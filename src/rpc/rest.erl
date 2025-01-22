-module(rest).
-export([
          request/6, 
          request/7, 
          request/8, 
          parse/2
        ]).

-include("eknife.hrl").

-define(REST_CLIENT_TIMEOUT, ?S2MS(60)).


-spec request(atom(), atom() | {atom(), any()}, list(), [integer()], map(), map() | binary() | list()) 
    -> {ok, integer(), map(), map() | list() | binary()} | {error, integer(), map(), map() | list() | binary()} | {error, any()}.
request(Method, Type, URL, Expect, InHeaders, Body) ->
  request(Method, Type, URL, Expect, InHeaders, Body, ?REST_CLIENT_TIMEOUT, []).

-spec request(atom(), atom() | {atom(), any()}, list(), [integer()], map(), map() | binary() | list(), [tuple()])
    -> {ok, integer(), map(), map() | list() | binary()} | {error, integer(), map(), map() | list() | binary()} | {error, any()}.
request(Method, Type, URL, Expect, InHeaders, Body, TransportOptions) ->
  request(Method, Type, URL, Expect, InHeaders, Body, ?REST_CLIENT_TIMEOUT, TransportOptions).

-spec request(atom(), atom() | {atom(), any()}, list(), [integer()], map(), map() | binary() | list(), integer(), [tuple()]) 
  -> {ok, integer(), map(), map() | list() | binary()} | {error, integer(), map(), map() | list() | binary()} | {error, any()}.
request(Method, Type, URL, Expect, InHeaders, Body, Timeout, TransportOptions) ->
  try 
    request_throwable(Method, Type, URL, Expect, InHeaders, Body, Timeout, TransportOptions)
  of
    Any -> 
      Any
  catch Type:Reason:Stacktrace ->
    ?LOG_ERROR("REST client exception ~p:~p -> ~p. ~p",
      [Type, Reason, tools:stacktrace(Stacktrace), [Method, Type, URL, Expect, InHeaders]]),
    {error, exception}
  end.

request_throwable(Method, Type, URL, Expect, InHeaders, Body, Timeout, TransportOptions) ->
  case url_parse(URL) of
    {Scheme, Host, Port, Path, QS} ->
      ?LOG_DEBUG("REST client make connection to the ~p:~p (~p)", [Host, Port, Scheme]),
      case gun:open(Host, Port, options(TransportOptions, Scheme)) of
        {ok, ConnPid} ->
          case gun:await_up(ConnPid, Timeout) of
            {ok, Protocol} ->
              ?LOG_DEBUG("REST client connection is up for ~p - ~p", [URL, Protocol]),
              StreamRef = method(ConnPid, Type, Method, Path, QS, headers(InHeaders, Type), Body),
              Resp = case gun:await(ConnPid, StreamRef, 2 * Timeout) of
                {response, fin, Status, RespHeadersL} ->
                  RespHeaders = maps:from_list(keys_to_lower(RespHeadersL)),
                  response(Status, Expect, RespHeaders, null);
                {response, nofin, Status, RespHeadersL} ->
                  RespHeaders = maps:from_list(keys_to_lower(RespHeadersL)),
                  case gun:await_body(ConnPid, StreamRef, 4 * Timeout) of
                    {ok, RespBody} ->
                      response(Status, Expect, RespHeaders, RespBody);
                    Error -> 
                      ?LOG_DEBUG("REST client got error on body await for ~p - ~p", [URL, Error]),
                      Error
                  end;
                Error ->
                  ?LOG_DEBUG("REST client got error on request await for ~p - ~p", [URL, Error]),
                  Error
              end,
              gun:shutdown(ConnPid),
              Resp;              
            Error ->
              ?LOG_DEBUG("REST client got error on connection await for ~p - ~p", [URL, Error]),
              Error
          end;
        Error ->
          ?LOG_DEBUG("REST client got error on connection for ~p - ~p", [URL, Error]),
          Error
      end;
    undefined -> 
      {error, invalid_url}
  end.

%
% income & encode
%

url_parse(URL) ->
  case uri_string:parse(cast:to_list(URL)) of
    URLMap when is_map(URLMap) ->
      {Scheme, DefaultPort} = case maps:get(scheme, URLMap, "http") of
        "https" -> 
          {https, 443};
        _ -> 
          {http, 80}
      end,
      {
        Scheme,
        maps:get(host, URLMap, "localhost"),
        maps:get(port, URLMap, DefaultPort),
        maps:get(path, URLMap, "/"),
        maps:get(query, URLMap, "")
      };
    Any ->
      ?LOG_DEBUG("REST client can't parse URL ~p traditional way - ~p, down to custom", [URL, Any]),
      case re:run(URL,"([a-z0-9]*):\\/\\/([^:\\/]+)(:[0-9]+)?([^?]*)\\??(.*)?", [{capture, all, list}]) of
        {match, [_, "https", Host, [], Path, QS]} ->
          {https, Host, 443, Path, QS};
        {match, [_, _, Host, [], Path, QS]} ->
          {http, Host, 80, Path, QS};
        {match, [_, "http", Host, PortS, Path, QS]} ->
          {https, Host, port_to_num(PortS, 80), Path, QS};
        {match, [_, "https", Host, PortS, Path, QS]} ->
          {https, Host, port_to_num(PortS, 443), Path, QS};
        Other ->
          ?LOG_WARNING("REST client can't parse URL ~p - ~p", [URL, Other]),
          undefined
      end
  end.

port_to_num(PortS, Default) ->
  cast:to_integer(binary:replace(PortS, <<":">>, <<>>, [global]), Default).

options([], https) ->
  #{ protocols => [http], transport => tls };
options(_TransportOptions, http) ->
  #{ protocols => [http] };
options(TransportOptions, _Scheme) ->
  #{ protocols => [http], transport => tls, transport_opts => TransportOptions}.

headers(InHeaders, Type) ->
  Headers = keys_to_lower(InHeaders),
  case maps:is_key(<<"accept">>, Headers) of
    false ->
      Headers#{ <<"accept">> => cast:to_binary(get_accept_type(Type) ++ "*/*;q=0.9") };
    true ->
      Headers
  end.

keys_to_lower(L) when is_list(L) ->
  [{string:lowercase(K), V} || {K, V} <- L];
keys_to_lower(M) when is_map(M) ->
  maps:from_list(keys_to_lower(maps:to_list(M))).

get_accept_type(text) -> 
  "text/plain, ";
get_accept_type(html) -> 
  "text/html, ";
get_accept_type(qs) ->
  "application/x-www-form-urlencoded, ";
get_accept_type(json) -> 
  "application/json, ";
get_accept_type(_) -> 
  "".

%
% making request
%

method(ConnPid, Type, Method, Path, QS, Headers, Body) ->
  {FullPath, FullHeaders, FullBody} = case lists:any(fun (I) -> I =:= Method end, [post, put]) of
    true ->
      EncBody = encode_body(Type, Body),
      BodySize = byte_size(EncBody),
      FHeaders = case maps:is_key(<<"content-type">>, Headers) of
        true ->
          maps:to_list(Headers#{ <<"content-length">> => BodySize});
        false ->
          maps:to_list(Headers#{
              <<"content-type">> => cast:to_binary(get_content_type(Type)),
              <<"content-length">> => BodySize
            })
      end,
      {
        case length(QS) of
          0 -> 
            Path;
          _ -> 
            Path ++ "?" ++ QS
        end,
        FHeaders,
        EncBody
      };
    false when length(QS) =:= 0 ->
      {
        case maps:size(Body) of
          0 -> 
            Path;
          _ ->
            lists:concat([Path, "?", binary_to_list(cow_qs:qs(maps:to_list(Body)))])
        end,
        maps:to_list(Headers),
        <<>>
      };
    false when (length(QS) =/= 0) and is_map(Body) ->
      {
        lists:concat([Path, "?", QS, "&", binary_to_list(cow_qs:qs(maps:to_list(Body)))]),
        maps:to_list(Headers),
        <<>>
      };
    false ->
      {
        case length(QS) of
          0 -> 
            Path;
          _ -> 
            Path ++ "?" ++ QS
        end,
        maps:to_list(Headers),
        <<>>
      }
  end,
  case FullBody of 
    <<>> ->
      ?LOG_DEBUG("REST client make request ~p ~p with headers ~p", [Method, FullPath, Headers]),
      gun:Method(ConnPid, FullPath, FullHeaders);
    _ ->
      ?LOG_DEBUG("REST client make request ~p ~p with headers ~p and body size ~p bytes", 
        [Method, FullPath, Headers, byte_size(FullBody)]),
      gun:Method(ConnPid, FullPath, FullHeaders, FullBody)
  end.

get_content_type(text) -> 
  "text/plain";
get_content_type(html) -> 
  "text/html";
get_content_type(qs) ->
  "application/x-www-form-urlencoded";
get_content_type(json) -> 
  "application/json";
get_content_type(_) -> 
  "application/octet-stream".

encode_body(any, Body) -> 
  cast:to_binary(Body);
encode_body(qs, Body) -> 
  cow_qs:qs(maps:to_list(Body));
encode_body(_, Body) when is_binary(Body) ->
  Body;
encode_body(_, Body) ->
  tools:to_json(Body, <<>>).

%
% response
%

response(Status, [], RespHeaders, null) ->
  {ok, Status, RespHeaders, #{}};
response(Status, [], RespHeaders, RespBody) ->
  {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
response(Status, Expect, RespHeaders, null) ->
  case lists:any(fun (I) -> I =:= Status end, Expect) of
    true -> 
      {ok, Status, RespHeaders, #{}};
    false -> 
      {error, Status, RespHeaders, #{}}
  end;
response(Status, Expect, RespHeaders, RespBody) ->
  case lists:any(fun (I) -> I =:= Status end, Expect) of
    true ->
      {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
    false ->
      {error, Status, RespHeaders, parse(RespHeaders, RespBody)}
  end. 

parse(Headers, Body) ->
  Unzip = case maps:get(<<"content-encoding">>, Headers, <<"plain">>) of
    <<"gzip">> -> 
      zlib:gunzip(Body);
    _ -> 
      Body
  end,
  case maps:get(<<"content-type">>, Headers, undefined) of
    undefined -> 
      parse_body("application/json", Unzip);
    Type ->
      CType = hd(string:tokens(binary_to_list(Type), ";")),
      parse_body(CType, Unzip)
  end.

parse_body("application/json", Body) ->
  tools:from_json(Body, #{});
parse_body("text/javascript", Body) ->
  tools:from_json(Body, #{});
parse_body("application/x-www-form-urlencoded", Body) ->
  maps:from_list(cow_qs:parse_qs(Body));
parse_body(_, Body) -> 
  Body.
