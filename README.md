# MC6007-MapReduce

## Uso

### Crear dos nodos
```bash
iex --sname nodoA --cookie secret_token -S mix 
iex --sname nodoB --cookie secret_token -S mix 
```

### Generar tuplas

Por ejemplo 100 líneas con números del 1 a n, en este caso 10:

```bash
iex(1)> Generator.tuples(100, 10, "tuples.data")
```

### MapReduce

```bash
iex(nodoB@host)> {:ok, pid} = GenServer.start_link(Parallel, [])
iex(nodoB@host)> GenServer.cast(pid, {:inicio, MapReduce, [{:nodoA@host, 2}], pid})
```

### Pagerank

```bash
iex(nodoB@host)> {:ok, pid} = GenServer.start_link(ParallelV2, [])
iex(nodoB@host)> GenServer.cast(pid, {:inicio, Pagerank, [{:nodoA@host, 1}], pid})
```
