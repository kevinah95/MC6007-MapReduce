-module(readfile).

-export([readlines/1]).
-export([formatcontent/1]).

readlines(FileName) -> 
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
        after file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof -> [];
        Line -> formatcontent(Line) ++ get_all_lines(Device)
    end.

formatcontent(Line) ->
    {ok, Ts, _} = erl_scan:string(Line),
    extractTuple(Ts).

extractTuple(Ts) -> 
    {ok, Tup} = erl_parse:parse_term(Ts),
    [Tup].

% io:format("{~p,~p,~p}.~n", L).
% "{237,582,598}.\n"