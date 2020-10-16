defmodule MapReduce do
  @moduledoc """
  Documentation for `MapReduce`.
  """

  @doc """
  Map a list of tuples [{key, num1, num2}].

  ## Examples

      iex> list = [{1,875,790}, {10,215,373}, {1,534,980}]
      iex> MapReduce.map_list(list)
      [{1, 1665}, {1, 1514}, {10, 588}]

  """
  def map_list(list) do
    map = Enum.map(list, fn x -> {elem(x, 0), elem(x, 1) + elem(x, 2)} end)
    map |> List.keysort(0)
  end

  @doc """
  Reduce a list of tuple [{key, num}].

  ## Examples

      iex> list = [{1, 1665}, {1, 1514}, {10, 588}]
      iex> MapReduce.reduce_list(a, %{})
      %{1 => 3179, 10 => 588}

  """
  def reduce_list([], reduced), do: reduced

  def reduce_list([head | tail], reduced) do
    # Do what you need to do here and call the function again
    # with remaining list items and updated map.
    {fst, snd} = head
    reduce_list(tail, Map.update(reduced, fst, snd, &(&1 + snd)))
  end

  def read_file() do
    stream = File.stream!("tuples.data")
    stream |> Enum.map(&elem(Code.eval_string(&1), 0))
  end
end
