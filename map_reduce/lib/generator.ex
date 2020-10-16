
defmodule Generator do

  @doc """
  Create tuples.

  ## Examples

      iex> Generator.tuples(10, 10, "tuples.data")
      :ok
  """
  def tuples(num_tuples, num_keys, filename) do
    <<i1 :: integer-unsigned-32, i2 :: integer-unsigned-32, i3 :: integer-unsigned-32>> = :crypto.strong_rand_bytes(12)
    :rand.seed(:exsplus, {i1, i2, i3})
    {_, file} = :file.open(filename, [:write])
    loop(num_tuples, num_keys, file)
  end

  defp loop(0, _ , file), do: :file.close(file)

  defp loop(num_tuples, num_keys, file) when num_tuples > 0  do
    k1 = :rand.uniform(num_keys)
    k2 = :rand.uniform(1000)
    k3 = :rand.uniform(1000)
    IO.puts(file,["{#{k1},#{k2},#{k3}}"])
    loop(num_tuples-1,num_keys,file)
  end
end
