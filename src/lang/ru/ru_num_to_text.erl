-module(ru_num_to_text).

-export([convert/1, valid_form/2]).

-include("eknife.hrl").

-define(NULL, <<"ноль"/utf8>>).

-define(DIGITS,
        #{0 => <<""/utf8>>, 1 => <<"один"/utf8>>,
          2 => <<"два"/utf8>>, 3 => <<"три"/utf8>>,
          4 => <<"четыре"/utf8>>, 5 => <<"пять"/utf8>>,
          6 => <<"шесть"/utf8>>, 7 => <<"семь"/utf8>>,
          8 => <<"восемь"/utf8>>, 9 => <<"девять"/utf8>>}).

-define(TENS,
        #{1 => <<"десять"/utf8>>, 2 => <<"двадцать"/utf8>>,
          3 => <<"тридцать"/utf8>>, 4 => <<"сорок"/utf8>>,
          5 => <<"пятьдесят"/utf8>>, 6 => <<"шестьдесят"/utf8>>,
          7 => <<"семьдесят"/utf8>>, 8 => <<"восемьдесят"/utf8>>,
          9 => <<"девяносто"/utf8>>}).

-define(TEENS,
        #{10 => <<"десять"/utf8>>, 11 => <<"одиннадцать"/utf8>>,
          12 => <<"двенадцать"/utf8>>,
          13 => <<"тринадцать"/utf8>>,
          14 => <<"четырнадцать"/utf8>>,
          15 => <<"пятнадцать"/utf8>>,
          16 => <<"шестнадцать"/utf8>>,
          17 => <<"семнадцать"/utf8>>,
          18 => <<"восемнадцать"/utf8>>,
          19 => <<"девятнадцать"/utf8>>}).

-define(HUNDREDS,
        #{1 => <<"сто"/utf8>>, 2 => <<"двести"/utf8>>,
          3 => <<"триста"/utf8>>, 4 => <<"четыреста"/utf8>>,
          5 => <<"пятьсот"/utf8>>, 6 => <<"шестьсот"/utf8>>,
          7 => <<"семьсот"/utf8>>, 8 => <<"восемьсот"/utf8>>,
          9 => <<"девятьсот"/utf8>>}).

-define(THOUSANDS,
        #{0 => <<""/utf8>>, 1 => <<"одна"/utf8>>,
          2 => <<"две"/utf8>>, 3 => <<"три"/utf8>>,
          4 => <<"четыре"/utf8>>, 5 => <<"пять"/utf8>>,
          6 => <<"шесть"/utf8>>, 7 => <<"семь"/utf8>>,
          8 => <<"восемь"/utf8>>, 9 => <<"девять"/utf8>>}).

-define(RANGE_WORD,
        #{1 => {<<>>, <<>>, <<>>},
          2 =>
              {<<"тысяча"/utf8>>,
               <<"тысячи"/utf8>>,
               <<"тысяч"/utf8>>},
          3 =>
              {<<"миллион"/utf8>>,
               <<"миллиона"/utf8>>,
               <<"миллионов"/utf8>>},
          4 =>
              {<<"миллиард"/utf8>>,
               <<"миллиарда"/utf8>>,
               <<"миллиардов"/utf8>>},
          5 =>
              {<<"триллион"/utf8>>,
               <<"триллиона"/utf8>>,
               <<"триллионов"/utf8>>},
          6 =>
              {<<"квадриллион"/utf8>>,
               <<"квадриллиона"/utf8>>,
               <<"квадриллионов"/utf8>>},
          8 =>
              {<<"квинтиллион"/utf8>>,
               <<"квинтиллиона"/utf8>>,
               <<"квинтиллионов"/utf8>>}}).

-spec convert(integer() |
              list() |
              binary()) -> binary().

convert(Num) when is_integer(Num) ->
    convert(Num, 1, true);
convert(Num) -> convert(cast:to_integer(Num), 1, true).

convert(0, _Range, true) -> ?NULL;
convert(0, _Range, false) -> <<>>;
convert(Num, Range, ReturnZero) when Num < 0 ->
    TextNum = convert(abs(Num), Range, ReturnZero),
    <<"минус "/utf8, TextNum/binary>>;
convert(Num, 1, _ReturnZero) when Num < 1_000 ->
    up_to(Num, ?DIGITS);
convert(Num, Range, _ReturnZero) ->
    D = case Range =:= 2 of
            true -> ?THOUSANDS;
            false -> ?DIGITS
        end,
    {NumDiv, NumRem} = split(Num, 1_000),
    case range_word(Range, NumRem) of
        undefined -> <<>>;
        RangeWord when NumDiv =:= 0 ->
            case up_to(NumRem, D) of
                <<>> -> <<>>;
                NumRemText ->
                    <<NumRemText/binary, " ", RangeWord/binary>>
            end;
        RangeWord ->
            case up_to(NumRem, D) of
                <<>> -> convert(NumDiv, Range + 1, false);
                NumRemText ->
                    case convert(NumDiv, Range + 1, false) of
                        <<>> -> <<NumRemText/binary, " ", RangeWord/binary>>;
                        TextBefore ->
                            <<TextBefore/binary, " ", NumRemText/binary, " ",
                              RangeWord/binary>>
                    end
            end
    end.

split(Num, D) -> {Num div D, Num rem D}.

up_to(Num, D) when Num < 10 -> maps:get(Num, D);
up_to(Num, _D) when Num < 20 -> maps:get(Num, ?TEENS);
up_to(Num, D) when Num < 100 ->
    {Tens, Rest} = split(Num, 10),
    case up_to(Rest, D) of
        <<>> -> maps:get(Tens, ?TENS);
        RestText ->
            <<(maps:get(Tens, ?TENS))/binary, " ", RestText/binary>>
    end;
up_to(Num, D) when Num < 1_000 ->
    {Hundreds, Rest} = split(Num, 100),
    case up_to(Rest, D) of
        <<>> -> maps:get(Hundreds, ?HUNDREDS);
        RestText ->
            <<(maps:get(Hundreds, ?HUNDREDS))/binary, " ",
              RestText/binary>>
    end.

range_word(Range, Num) ->
    case maps:get(Range, ?RANGE_WORD, undefined) of
        undefined -> undefined;
        Forms -> valid_form(Forms, Num)
    end.

-spec valid_form({binary(), binary(), binary()},
                 integer()) -> binary().

valid_form(Forms, Num) ->
    LastDigit = abs(Num) rem 10,
    LastTwoDigits = abs(Num) rem 100,
    case Forms of
        {Result, _, _}
            when (LastDigit =:= 1) and (LastTwoDigits =/= 11) ->
            Result;
        {_, Result, _}
            when ((LastDigit =:= 2) or (LastDigit =:= 3) or
                      (LastDigit =:= 4))
                     and ((LastTwoDigits > 20) or (LastTwoDigits < 10)) ->
            Result;
        {_, _, Result} -> Result;
        _ -> undefined
    end.
