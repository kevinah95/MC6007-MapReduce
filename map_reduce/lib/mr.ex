defmodule MR do
  use Application

  @impl true
  def start(_type, _args) do
    ParallelSupervisor.start_link(name: ParallelSupervisor)
  end
end
