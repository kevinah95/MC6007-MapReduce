# import some stuff
import numpy as np
import pandas as pd
from fractions import Fraction


# keep it clean and tidy
def float_format(vector, decimal):
  return np.round((vector).astype(np.float), decimals=decimal)


# we have 3 webpages and probability of landing to each one is 1/3
#(defaultProbability)
dp = Fraction(1, 4)

# WWW matrix
M = np.matrix(
  [
    [0, Fraction(2, 5), 0, 0], 
    [Fraction(4, 15), 0, 0, Fraction(2, 5)], 
    [Fraction(4, 15), 0, Fraction(4, 5), Fraction(2, 5)],
    [Fraction(4, 15), Fraction(2, 5), 0, 0]
  ]
)

E = np.zeros((4, 4))


E[:] = dp
print(E)

# taxation
beta = 0.8

# WWW matrix
A = beta * M + ((1 - beta) * E)
print(A)

# initial vector
v = np.matrix([1, 1, 1, 1])
v = np.transpose(v)
print(v)
previous_v = v
for it in range(1, 5):
  v = A * v
  print(float_format(v, 3))
  #check if converged
  if (previous_v == v).all():
    break
  previous_v = v

print("Final:\n", float_format(v, 3))
print("sum", np.sum(v))