defmodule ParallelV2 do
  use GenServer

  @doc """
  Starts the registry.
  """
  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
  end

  def send(server_pid, module_name, num_nodos) do
    GenServer.cast(server_pid, {:inicio, module_name, num_nodos, server_pid})
  end

  @doc """
  ## Examples

    iex(1)> Parallel.start(Primes, 10, self())

    generando 10 trabajadores en el nodo foo@khernandezr
    recolector termino, enviando paquete al client
    repartidor termino
    #PID<0.149.0>
  """
  def start(module_work, spec_work, client) do
    jefe(module_work, spec_work, client)
  end

  def jefe(module_work, spec_work, client) do
    {list_of_workers, num_workers} = get_trabajadores(spec_work)
    # TODO: change spec_work to work with generator
    llaves = module_work.read_file() |> Enum.chunk_every(num_workers)
    beta = 0.8
    n = length(llaves)
    dp = (1 - beta) / n
    repartidor = spawn_link(fn -> repartidor(llaves, num_workers) end)
    recolector = spawn_link(fn -> recolector(module_work, length(llaves), client, [], beta, dp) end)
    spawn_link(fn -> spawn_trabajadores(module_work, repartidor, recolector, list_of_workers) end)
  end

  def get_trabajadores(spec_work) do
    list_of_workers = lista_trabajadores(spec_work)

    num_workers =
      list_of_workers |> Enum.reduce([], fn x, acc -> [elem(x, 1) | acc] end) |> Enum.sum()

    {list_of_workers, num_workers}
  end

  def lista_trabajadores(n) when is_integer(n), do: [{node(), n}]
  def lista_trabajadores(l) when is_list(l), do: l

  def spawn_trabajadores(work, repartidor, recolector, list_of_workers) do
    Enum.map(list_of_workers, fn {host, n} ->
      spawn(fn ->
        :io.format("generando ~p trabajadores en el nodo ~p\n", [n, host])
        crear_trabajadores_nodo({host, n}, work, repartidor, recolector)
      end)
    end)
  end

  def crear_trabajadores_nodo({_, 0}, _, _, _), do: :ok

  def crear_trabajadores_nodo({host, n}, work, repartidor, recolector) when n > 0 do
    Node.spawn_link(host, __MODULE__, :worker, [work, repartidor, recolector])
    crear_trabajadores_nodo({host, n - 1}, work, repartidor, recolector)
  end

  def repartidor([], 0) do
    :io.format("repartidor termino\n")
    :finished
  end

  def repartidor([], n) when n > 0 do
    receive do
      {:mas_trabajo, worker} ->
        send(worker, :no_hay)
        repartidor([], n - 1)
    end
  end

  def repartidor([llave | llaves], num_workers) do
    receive do
      {:mas_trabajo, worker} ->
        # IO.puts("LLAVE")
        # IO.inspect(llave)
        # IO.inspect(worker)
        send(worker, llave)
        repartidor(llaves, num_workers)
    end
  end

  @doc """
  ## Examples
    iex(1)> receive do
    iex(1)>   {:pedido, l} -> l
    iex(1)> end
  """
  def recolector(work, 0, client, lotes, beta, dp) do
    :io.format("recolector termino, enviando paquete al client\n")
    #IO.inspect lotes
    send(client, {:pedido, Enum.reverse(work.reduceV2(lotes, beta, dp))})
    #grouped = Enum.group_by(lotes, fn {a, _} -> a end, &elem(&1, 1))

  end

  def recolector(work, pendientes, client, lotes, beta, dp) when pendientes > 0 do
    receive do
      chunk ->
        #IO.puts("CHUNK")
        #IO.inspect lotes
        recolector(work, pendientes - 1, client, [chunk | lotes], beta, dp)
    end
  end

  def worker(work, repartidor, recolector) do
    send(repartidor, {:mas_trabajo, self()})

    receive do
      :no_hay ->
        :finished

      llave ->
        send(recolector, work.map(hd(llave), Pagerank.read_eigenvector_file()))
        worker(work, repartidor, recolector)
    end
  end

  @impl true
  def init(default) do
    {:ok, default}
  end

  @impl true
  def handle_cast({:inicio, module, num_nodos, client}, state) do
    start(module, num_nodos, client)
    {:noreply, state}
  end

  @impl true
  def handle_info({:pedido, list}, state) do
    IO.inspect list
    {_, file} = :file.open("v.rank", [:write])
    IO.write(file, inspect(list))
    {:noreply, state}
  end
end
