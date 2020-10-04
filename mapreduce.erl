%%%-------------------------------------------------------------------
%%% File    : mapreduce.erl
%%% Author  : Jose Castro <jose.r.castro@gmail.com>
%%% Description : 
%%%
%%% Created :  5 Sep 2009 by Jose Castro <>
%%%-------------------------------------------------------------------
-module(mapreduce).
-export([start/4, trabajador/3]).

start(ModuloTrabajo, SpecTrabajo, NumLotes, Cliente) ->
    jefe(ModuloTrabajo, SpecTrabajo, NumLotes, Cliente).

% Jefe ---------------------------------------------------------------
jefe(ModuloTrabajo, SpecTrabajo, SpecLotes, Cliente) ->

    {ListaTrabajadores, NumTrabajadores} = get_trabajadores(SpecTrabajo),

    Tuplas     = ModuloTrabajo:gen_keys(SpecLotes), %[{x, Y, Z}, {X, Y, Z}]%
    Repartidor = spawn_link(fun() -> repartidor(Tuplas, NumTrabajadores) end),
    Recolector = spawn_link(fun() -> recolector(ModuloTrabajo, length(Tuplas), Cliente, []) end),
    spawn_link(fun() -> spawn_trabajadores(ModuloTrabajo, Repartidor, Recolector, ListaTrabajadores) end).

get_trabajadores(SpecTrabajo) ->
    ListaTrabajadores = lista_trabajadores(SpecTrabajo),
    NumTrabajadores   = lists:sum(lists:map(fun({_,X}) -> X end, ListaTrabajadores)),
    {ListaTrabajadores, NumTrabajadores}.

lista_trabajadores(N) when is_integer(N) -> [{node(), N}];
lista_trabajadores(L) when is_list(L)    -> L.

spawn_trabajadores(Trabajo, Repartidor, Recolector, ListaTrabajadores) ->
    lists:map(
      fun({Host, N}) -> 
	      spawn(fun() -> 
			    io:format("generando ~p trabajadores en el nodo ~p\n", [N, Host]),
			    crear_trabajadores_nodo({Host, N}, Trabajo, Repartidor, Recolector) 
		    end) 
      end,
      ListaTrabajadores
     ).

crear_trabajadores_nodo({_,0}, _, _, _) -> ok;
crear_trabajadores_nodo({Host, N}, Trabajo, Repartidor, Recolector) when N > 0 ->
    spawn_link(Host, mapreduce, trabajador, [Trabajo, Repartidor, Recolector]),
    crear_trabajadores_nodo({Host, N-1}, Trabajo, Repartidor, Recolector).

% Repartidor ----------------------------------------------------------
repartidor([], 0) -> 
    io:format("repartidor termino\n"),
    finished;
repartidor([], N) when N > 0 ->
    receive
	{Worker, mas_trabajo} ->
	    Worker ! no_hay,
	    repartidor([], N-1)
    end;

%[{x, Y, Z}, {X, Y, Z}]%
repartidor([Llave|Llaves], NumTrabajadores) ->
    receive
	{Worker, mas_trabajo} ->
	    Worker ! Llave,
	    repartidor(Llaves, NumTrabajadores)
    end.

% Recolector ----------------------------------------------------------
recolector(Trabajo, 0, Cliente, Lotes) ->
    io:format("recolector termino, enviando paquete al Cliente\n"),
    % Result = ,
    % io:format("En recolector ~p", [Result]),
    Cliente ! {pedido, Trabajo:reduce(Lotes)};
recolector(Trabajo, Pendientes, Cliente, Lotes) when Pendientes > 0 ->
    receive
	{Llave, Lote} ->
        {X, _, _} = Llave,
	    recolector(Trabajo, Pendientes-1, Cliente, [{X, Lote}| Lotes])
    end.

% Trabajador ----------------------------------------------------------
trabajador(Trabajo, Repartidor, Recolector) ->
    Repartidor ! {self(), mas_trabajo},
    receive
	no_hay -> finished;
	Llave  -> %{X, Y, Z}%
	    Recolector ! {Llave, Trabajo:map(Llave)},
	    trabajador(Trabajo, Repartidor, Recolector)
    end.
