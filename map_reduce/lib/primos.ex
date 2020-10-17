defmodule Primes do
  @doc """
  ## Examples
    iex(12)> Primes.gen_keys({4,100})
    [{59, 82}, {40, 58}, {21, 39}, {2, 20}]
  """
  def gen_keys({num_strips, maximum}), do: gen_strips(2, maximum, 0, num_strips, [])

  defp gen_strips(_, _, num_strips, num_strips, result), do: result

  defp gen_strips(i, max, strip, num_strips, result) do
    reminder = rem(max - 2, num_strips)

    sixe =
      case strip <= reminder do
        true -> div(max - 2, num_strips + 1)
        false -> div(max - 2, num_strips)
      end

    gen_strips(i + sixe, max, strip + 1, num_strips, [{i, i + sixe - 1} | result])
  end

  @doc """
  ## Examples
    iex(12)> Primes.map({0,10})
    [7, 5, 3, 2, 1, 0]
  """
  def map({min, max}) do
    gen_primes(min, max, [])
  end

  defp gen_primes(i, max, primes) when i > max, do: primes

  defp gen_primes(i, max, primes) do
    case is_prime(i) do
      true -> gen_primes(i + 1, max, [i | primes])
      false -> gen_primes(i + 1, max, primes)
    end
  end

  defp is_prime(i), do: is_prime(2, :math.sqrt(i), i)

  defp is_prime(i, sqrt, _) when i > sqrt, do: true

  defp is_prime(i, sqrt, num) do
    if rem(num, i) == 0 do
      false
    else
      is_prime(i + 1, sqrt, num)
    end
  end

  @doc """
  ## Examples
    iex(10)> Primes.reduce([{59, 82}, {40, 58}, {21, 39}, {2, 20}])
    [2, 21, 40, 59]
  """

  def reduce(list) do
    Enum.reduce(list, [], fn x, acc -> [elem(x, 0) | acc] end)
  end
end
