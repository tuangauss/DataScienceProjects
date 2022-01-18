import numpy as np
import matplotlib as plt

def get_3_std_estimates(x):
  return (x.mean() - 3 * x.std(), x.mean() + 3 * x.std())

def get_2_std_estimates(x):
  return (x.mean() - 2 * x.std(), x.mean() + 2 * x.std())

def get_1_std_estimates(x):
  return (x.mean() - x.std(), x.mean() + x.std())


N = 10000

def get_graph(n, title):
  """
  Draw a distribution histogram for a sample of N data from 
  n-dimensional Normal distribution
  """
  
  sample = np.random.normal(size=(N, n))
  dist = np.square(np.linalg.norm(sample, axis = 1))
  lower_bound, upper_bound = get_2_std_estimates(dist)
  n, bins, patches = plt.hist(dist, bins = 'auto', density = "true")
  plt.axvline(x = lower_bound, color = 'red')
  plt.axvline(x = upper_bound, color = 'red')
  plt.title(title, fontdict = {'fontsize': 20})
  plt.show()
  
get_graph(100, "Distribution of distance from origin for n = 100")

