# MapReduce

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

### MapReduce con nodos

```bash
iex(nodoB@host)> {:ok, pid} = GenServer.start_link(Parallel, [])
iex(nodoB@host)> GenServer.cast(pid, {:inicio, MapReduce, [{:nodoA@host, 2}], pid})
```

### Pagerank con nodos

```bash
iex(nodoB@host)> {:ok, pid} = GenServer.start_link(ParallelV2, [])
iex(nodoB@host)> GenServer.cast(pid, {:inicio, Pagerank, [{:nodoA@host, 1}], pid})
```

### Con streams

```
Pagerank.read_file() |> Stream.chunk_every(1) |> Stream.map(&(Pagerank.calculate_eigenvector_prime(Enum.at(&1, 0), Pagerank.read_eigenvector_file()))) |> Enum.to_list 
```
