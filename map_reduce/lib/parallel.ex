defmodule Parallel do
  @doc """
  ## Examples

    iex(1)> Parallel.start(Primes, 10, {10, 100}, self())

    generando 10 trabajadores en el nodo foo@khernandezr
    recolector termino, enviando paquete al client
    repartidor termino
    #PID<0.149.0>
  """
  def start(module_work, spec_work, num_batches, client) do
    jefe(module_work, spec_work, num_batches, client)
  end

  def jefe(module_work, spec_work, spec_lotes, client) do
    {list_of_workers, num_workers} = get_trabajadores(spec_work)

    llaves     = module_work.gen_keys(spec_lotes)
    repartidor = spawn_link(fn -> repartidor(llaves, num_workers) end)
    recolector = spawn_link(fn -> recolector(module_work, length(llaves), client, []) end)
    spawn_link(fn -> spawn_trabajadores(module_work, repartidor, recolector, list_of_workers) end)

  end

  def get_trabajadores(spec_work) do
    list_of_workers = lista_trabajadores(spec_work)

    num_workers = list_of_workers |> Enum.reduce([], fn(x, acc) -> [elem(x,1) | acc] end) \
                    |> Enum.sum
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

  def crear_trabajadores_nodo({_,0}, _, _, _), do: :ok

  def crear_trabajadores_nodo({host, n}, work, repartidor, recolector) when n > 0 do
      Node.spawn_link(host, __MODULE__, :worker, [work, repartidor, recolector])
      crear_trabajadores_nodo({host, n-1}, work, repartidor, recolector)
  end

  def repartidor([], 0) do
    :io.format("repartidor termino\n")
    :finished
  end

  def repartidor([], n) when n > 0 do
    receive do
      {:mas_trabajo, worker} ->
        send(worker, :no_hay)
        repartidor([], n-1)
    end
  end

  def repartidor([llave|llaves], num_workers) do
    receive do
	    {:mas_trabajo, worker} ->
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
  def recolector(work, 0, client, lotes) do
    :io.format("recolector termino, enviando paquete al client\n")
    send(client, {:pedido, work.reduce(lotes)})
  end

  def recolector(work, pendientes, client, lotes) when pendientes > 0 do
    receive do
      {llave, lote} ->
          recolector(work, pendientes-1, client, [{llave, lote}| lotes])
    end
  end

  def worker(work, repartidor, recolector) do
    send(repartidor, {:mas_trabajo, self()})
    receive do
      :no_hay -> :finished
      llave  ->
        send(recolector, {llave, work.map(llave)})
        worker(work, repartidor, recolector)
    end
  end

end
