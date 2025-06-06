-module(rest).

-export([request/6, request/7, parse/2]).

-include("eknife.hrl").

-spec request(atom(), atom() | {atom(), any()}, list(),
              [integer()], map(), map() | binary() | list()) -> {ok,
                                                                 integer(), map(),
                                                                 map() | list() | binary()} |
                                                                {error, integer(), map(),
                                                                 map() | list() | binary()} |
                                                                {error, any()}.

request(Method, Type, URL, Expect, InHeaders, Body) ->
    request(Method, Type, URL, Expect, InHeaders, Body, []).

-spec request(atom(), atom() | {atom(), any()}, list(),
              [integer()], map(), map() | binary() | list(),
              [tuple()]) -> {ok, integer(), map(),
                             map() | list() | binary()} |
                            {error, integer(), map(), map() | list() | binary()} |
                            {error, any()}.

request(Method, Type, URL, Expect, InHeaders, Body,
        TransportOptions) ->
    try request_throwable(Method,
                          Type,
                          URL,
                          Expect,
                          InHeaders,
                          Body,
                          TransportOptions)
    of
        Any -> Any
    catch
        Type:Reason:Stacktrace ->
            ?LOG_ERROR("Exception catched ~p:~p -> ~p. ~p",
                       [Type,
                        Reason,
                        utils:stacktrace(Stacktrace),
                        [Method, Type, URL, Expect, InHeaders]]),
            {error, exception}
    end.

request_throwable(Method, Type, URL, Expect, InHeaders,
                  Body, TransportOptions) ->
    case url_parse(URL) of
        undefined -> {error, invalid_url};
        {Scheme, Host, Port, Path, QS} ->
            Headers = maps:merge(#{<<"accept">> =>
                                       cast:to_binary(get_accept_type(Type) ++ "*/*;q=0.9")},
                                 keys_to_lower(InHeaders)),
            ?LOG_DEBUG("Make connection to the ~p:~p (~p)",
                       [Host, Port, Scheme]),
            {ok, ConnPid} = case TransportOptions of
                                [] when Scheme =:= https ->
                                    gun:open(Host,
                                             Port,
                                             #{protocols => [http], transport => tls});
                                [] -> gun:open(Host, Port, #{protocols => [http]});
                                _ ->
                                    gun:open(Host,
                                             Port,
                                             #{protocols => [http], transport => tls,
                                               transport_opts => TransportOptions})
                            end,
            {ok, Protocol} = gun:await_up(ConnPid, ?GUN_TIMEOUT),
            ?LOG_DEBUG("Connection is up - ~p", [Protocol]),
            StreamRef = case lists:any(fun (I) -> I =:= Method end,
                                       [post, put])
                            of
                            true ->
                                FullPath = case length(QS) of
                                               0 -> Path;
                                               _ -> Path ++ "?" ++ QS
                                           end,
                                EncBody = encode_body(Type, Body),
                                BodySize = byte_size(EncBody),
                                FullHeaders = case maps:is_key(<<"content-type">>,
                                                               Headers)
                                                  of
                                                  true ->
                                                      maps:to_list(Headers#{<<"content-length">> =>
                                                                                BodySize});
                                                  false ->
                                                      maps:to_list(Headers#{<<"content-type">> =>
                                                                                cast:to_binary(get_content_type(Type)),
                                                                            <<"content-length">> =>
                                                                                BodySize})
                                              end,
                                ?LOG_DEBUG("Make request ~p ~p with headers ~p and "
                                           "body size ~p",
                                           [Method, FullPath, FullHeaders, byte_size(EncBody)]),
                                case BodySize > 4 * 1024 of
                                    true -> ok;
                                    false -> ?LOG_DEBUG("Body: ~p", [EncBody])
                                end,
                                gun:Method(ConnPid, FullPath, FullHeaders, EncBody);
                            false when length(QS) =:= 0 ->
                                FullPath = case maps:size(Body) of
                                               0 -> Path;
                                               _ ->
                                                   lists:concat([Path,
                                                                 "?",
                                                                 binary_to_list(cow_qs:qs(maps:to_list(Body)))])
                                           end,
                                FullHeaders = maps:to_list(Headers),
                                ?LOG_DEBUG("Make request ~p ~p with headers ~p",
                                           [Method, FullPath, FullHeaders]),
                                gun:Method(ConnPid, FullPath, FullHeaders);
                            false when length(QS) =/= 0 ->
                                FullPath = lists:concat([Path,
                                                         "?",
                                                         QS,
                                                         "&",
                                                         binary_to_list(cow_qs:qs(maps:to_list(Body)))]),
                                FullHeaders = maps:to_list(Headers),
                                ?LOG_DEBUG("Make request ~p ~p with headers ~p",
                                           [Method, FullPath, FullHeaders]),
                                gun:Method(ConnPid, FullPath, FullHeaders);
                            false ->
                                FullPath = case length(QS) of
                                               0 -> Path;
                                               _ -> Path ++ "?" ++ QS
                                           end,
                                FullHeaders = maps:to_list(Headers),
                                ?LOG_DEBUG("Make request ~p ~p with headers ~p",
                                           [Method, FullPath, FullHeaders]),
                                gun:Method(ConnPid, FullPath, FullHeaders)
                        end,
            Resp = case gun:await(ConnPid, StreamRef, ?GUN_TIMEOUT)
                       of
                       {response, fin, Status, RespHeaders} ->
                           case lists:any(fun (I) -> I =:= Status end, Expect) of
                               true -> {ok, Status, RespHeaders, #{}};
                               false when Expect =:= [] ->
                                   {ok, Status, RespHeaders, #{}};
                               false -> {error, Status, RespHeaders, #{}}
                           end;
                       {response, nofin, Status, RespHeadersL} ->
                           RespHeaders =
                               maps:from_list(keys_to_lower(RespHeadersL)),
                           case gun:await_body(ConnPid, StreamRef, ?GUN_TIMEOUT) of
                               {ok, RespBody} ->
                                   case lists:any(fun (I) -> I =:= Status end, Expect) of
                                       true ->
                                           {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
                                       false when Expect =:= [] ->
                                           {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
                                       false ->
                                           {error,
                                            Status,
                                            RespHeaders,
                                            parse(RespHeaders, RespBody)}
                                   end;
                               Error -> {error, Error}
                           end;
                       Any -> Any
                   end,
            gun:shutdown(ConnPid),
            Resp
    end.

url_parse(URL) ->
    case uri_string:parse(cast:to_list(URL)) of
        URLMap when is_map(URLMap) ->
            {Scheme, DefaultPort} = case maps:get(scheme,
                                                  URLMap,
                                                  "http")
                                        of
                                        "https" -> {https, 443};
                                        _ -> {http, 80}
                                    end,
            {Scheme,
             maps:get(host, URLMap, "localhost"),
             maps:get(port, URLMap, DefaultPort),
             maps:get(path, URLMap, "/"),
             maps:get(query, URLMap, "")};
        Any ->
            ?LOG_WARNING("Can't parse URL ~p traditional way - "
                         "~p, down to custom",
                         [URL, Any]),
            case re:run(URL,
                        "([a-z0-9]*):\\/\\/([^:\\/]+)(:[0-9]+)?([^?]*)\\"
                        "??(.*)?",
                        [{capture, all, list}])
                of
                {match, [_, "https", Host, [], Path, QS]} ->
                    {https, Host, 443, Path, QS};
                {match, [_, _, Host, [], Path, QS]} ->
                    {http, Host, 80, Path, QS};
                {match, [_, "http", Host, PortS, Path, QS]} ->
                    {https, Host, port_to_num(PortS, 80), Path, QS};
                {match, [_, "https", Host, PortS, Path, QS]} ->
                    {https, Host, port_to_num(PortS, 443), Path, QS};
                Other ->
                    ?LOG_WARNING("Can't parse URL ~p - ~p", [URL, Other]),
                    undefined
            end
    end.

port_to_num(PortS, Default) ->
    cast:to_integer(binary:replace(cast:to_binary(PortS),
                                   <<":">>,
                                   <<>>,
                                   [global]),
                    Default).

keys_to_lower(L) when is_list(L) ->
    [{string:lowercase(K), V} || {K, V} <- L];
keys_to_lower(M) when is_map(M) ->
    maps:from_list(keys_to_lower(maps:to_list(M))).

get_accept_type(html) -> "text/html, ";
get_accept_type(qs) ->
    "application/x-www-form-urlencoded, ";
get_accept_type(json) -> "application/json, ";
get_accept_type({asis, _Out, In}) -> In ++ ", ";
get_accept_type(_) -> "".

get_content_type(video) -> "video/mp4";
get_content_type({multipart, _}) ->
    "multipart/form-data; boundary=xxxxxxxxXXXXXXXX";
get_content_type({multipart, _, _}) ->
    "multipart/form-data; boundary=xxxxxxxxXXXXXXXX";
get_content_type(html) -> "text/html";
get_content_type(qs) ->
    "application/x-www-form-urlencoded";
get_content_type(json) -> "application/json";
get_content_type({asis, Out, _In}) -> Out;
get_content_type(_) -> "text/plain".

encode_body(qs, Body) -> cow_qs:qs(maps:to_list(Body));
encode_body(html, Body) -> Body;
encode_body({multipart, FileName}, Body) ->
    {ok, FileType, Mime} =
        get_mime_from_extension(filename:extension(FileName)),
    <<<<"--xxxxxxxxXXXXXXXX\r\nContent-Disposition: "
        "form-data; name=\"">>/binary,
      FileType/binary, <<"\"; filename=\"">>/binary,
      FileName/binary, <<"\"\r\nContent-Type: ">>/binary,
      Mime/binary, <<"\r\n\r\n">>/binary, Body/binary,
      <<"\r\n--xxxxxxxxXXXXXXXX--">>/binary>>;
encode_body({multipart, FieldName, FileName}, Body) ->
    {ok, _FileType, Mime} =
        get_mime_from_extension(filename:extension(FileName)),
    <<<<"--xxxxxxxxXXXXXXXX\r\nContent-Disposition: "
        "form-data; name=\"">>/binary,
      FieldName/binary, <<"\"; filename=\"">>/binary,
      FileName/binary, <<"\"\r\nContent-Type: ">>/binary,
      Mime/binary, <<"\r\n\r\n">>/binary, Body/binary,
      <<"\r\n--xxxxxxxxXXXXXXXX--">>/binary>>;
encode_body({video, _FileName}, Body) -> Body;
encode_body({asis, _Out, _In}, Body) -> Body;
encode_body(_, Body) ->
    json_to_binary(utils:to_json(Body)).

json_to_binary(IOList) when is_list(IOList) ->
    iolist_to_binary(IOList);
json_to_binary(Any) when is_binary(Any) -> Any;
json_to_binary(Any) -> cast:to_binary(Any).

parse(Headers, Body) ->
    Unzip = case maps:get(<<"content-encoding">>,
                          Headers,
                          <<"plain">>)
                of
                <<"gzip">> -> zlib:gunzip(Body);
                _ -> Body
            end,
    case maps:get(<<"content-type">>, Headers, undefined) of
        undefined -> parse_body("application/json", Unzip);
        Type ->
            CType = hd(string:tokens(binary_to_list(Type), ";")),
            parse_body(CType, Unzip)
    end.

parse_body("application/json", Body) ->
    utils:from_json(Body, #{});
parse_body("text/javascript", Body) ->
    utils:from_json(Body, #{});
parse_body("application/x-www-form-urlencoded", Body) ->
    maps:from_list(cow_qs:parse_qs(Body));
% parse_body("text/plain", Body) ->
%   maps:from_list(cow_qs:parse_qs(Body)); % damn you, Facebook
parse_body(_, Body) -> Body.

get_mime_from_extension(Extension) ->
    case Extension of
        <<".png">> -> {ok, <<"photo">>, <<"image/png">>};
        <<".jpg">> -> {ok, <<"photo">>, <<"image/jpeg">>};
        <<".jpeg">> -> {ok, <<"photo">>, <<"image/jpeg">>};
        <<".mp4">> -> {ok, <<"video">>, <<"video/mp4">>};
        <<".mpg">> -> {ok, <<"video">>, <<"video/mpeg">>};
        <<".mp3">> -> {ok, <<"audio">>, <<"audio/mpeg">>};
        <<".m4a">> -> {ok, <<"audio">>, <<"audio/mp4">>};
        _ -> {error, undefined, <<"">>}
    end.
