%
% Sum Module
%
%
%%

-module(sum).
-export([gen_keys/1]).
-export([map/1]).
-export([reduce/1, process_tuple/1]).
-export([do_something/2]).

%Section to load tuples%
gen_keys(FileName) -> readlines(FileName).

%main load funct%
readlines(FileName) -> 
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
        after file:close(Device)
    end.

%load all lines%
get_all_lines(Device) ->
    case io:get_line(Device, "") of %load every single line%
        eof -> [];
        Line -> formatcontent(Line) ++ get_all_lines(Device) % creates the return list %
    end.

%formats the values from string into tuple%
formatcontent(Line) ->
    {ok, Ts, _} = erl_scan:string(Line),
    extractTuple(Ts).

% extract every single tuple as a list %
extractTuple(Ts) -> 
    {ok, Tup} = erl_parse:parse_term(Ts),
    [Tup].

map({_,Y,Z}) -> 
    Y + Z.


reduce(TuplesList) ->
    Mapped = do_something(TuplesList, maps:new()),
    List = map_to_list(Mapped, maps:iterator(Mapped), []). 

map_to_list(Mapped, Iterator, Result) when Iterator == none -> Result;

map_to_list(Mapped, Iterator, Result) -> 
    {K, V, I} = maps:next(Iterator), {K, V},
    map_to_list(Mapped, I, Result ++ [{K, V}]).



% process_chunck(Tuples_list) ->
    % Llaves = get_all_keys(Tuples_list),
    % io:format("En reduce ~p ", [Llaves]),
    % Llaves2 = from_list(List) -> .

 	% Processed_chunk = lists:map(fun process_tuple/1, Tuples_list),
    % Processed_chunk.

% get_all_keys(Tuples) ->
    % [X || {X, _} <- Tuples].

do_something([], Mapped) -> Mapped;

do_something([Tuple | Tuples], Mapped) ->
    {X, Y} = Tuple,
    case maps:is_key(X, Mapped) of
        true -> Existe = maps:get(X, Mapped) + Y, do_something(Tuples, maps:update(X, Existe, Mapped));
        false -> do_something(Tuples, maps:put(X, Y, Mapped))
    end.

process_tuple(Tuple) ->
	List = tuple_to_list(Tuple),
	%[llave, valor1, valor2, ..., valorN]
    Key = lists:nth(1,List),
    List1 = lists:sublist(List, 2, length(List)),
    Total = lists:foldl(fun(X, Sum) -> X + Sum end, 0, List1),
    %A=lists:nth(2,List),
    %B=lists:nth(3,List),
    {Key, Total}.