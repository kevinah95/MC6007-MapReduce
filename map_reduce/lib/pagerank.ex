defmodule Pagerank do

  def calculate_eigenvector_prime(m_i, eigenvector) do
    n = Enum.count(m_i)

    beta = 0.8

    dp = (1 - beta) / n

    v = map(m_i, eigenvector)

    reduce(v, beta, dp)
  end

  def map(m_i, eigenvector) do
    m_i |> Enum.zip(eigenvector) |> Enum.map(fn {m_j, eigenvalue_j} -> (m_j * eigenvalue_j) end)
  end

  def reduce(v, beta, dp) do
    r = v |> Enum.sum
    r * beta + dp
  end

  def reduceV2(v, beta, dp) do
    r = Enum.map(v,fn e -> e |> Enum.sum |> Kernel.*(beta) |> Kernel.+(dp)  end)
  end

  def read_file() do
    stream = File.stream!("m.rank")
    stream |> Stream.map(&elem(Code.eval_string(&1), 0))
  end


  def read_eigenvector_file() do
    stream = File.stream!("v.rank")
    stream |> Stream.flat_map(&elem(Code.eval_string(&1), 0)) |> Enum.to_list
  end


end
