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


def get_boundary(n):
  """
  For a dimension value n, sample N data points from a n-dimensional
  Normal distribution and find the 2 standard deviation boundary
  for the squared Euclidan norms.
  """
  
  sample = np.random.normal(size=(N, n))
  dist = np.square(np.linalg.norm(sample, axis = 1))
  lower_bound, upper_bound = get_2_std_estimates(dist)
  return (lower_bound, upper_bound)

### simulation
n_range = range(1, 5001)
lower_bounds = []
upper_bounds = []

for n in n_range:
  lower_bound, upper_bound = get_boundary(n)
  lower_bounds.append(lower_bound/n)
  upper_bounds.append(upper_bound/n)

plt.style.use('seaborn-notebook')
plt.plot(n_range, lower_bounds, label = 'lower_bounds\ndivided by n')
plt.plot(n_range, upper_bounds, label = 'upper_bounds\ndivided by n')
#plt.axvline(x=1000, color = 'red', linestyle = '--')
plt.legend(prop={'size': 13})
plt.xlim(1, 5000)
plt.xlabel("dimensions")
plt.title("Ratio between 2-standard devation boundaries and n as n increases", fontdict = {'fontsize': 16})
plt.show()
