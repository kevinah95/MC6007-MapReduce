# MapReduce

**TODO: Add description**

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `map_reduce` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:map_reduce, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/map_reduce](https://hexdocs.pm/map_reduce).

## Usage

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

### Crear el servidor 

```bash
iex(nodoB@host)> {:ok, pid} = GenServer.start_link(Parallel, [])
iex(nodoB@host)> GenServer.cast(pid, {:inicio, MapReduce, [{:nodoA@host, 2}], pid})
```
### Pagerank
Pagerank.read_file() |> Stream.chunk_every(1) |> Stream.map(&(Pagerank.calculate_eigenvector_prime(Enum.at(&1, 0), Pagerank.read_eigenvector_file()))) |> Enum.to_list 