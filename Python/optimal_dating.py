import random
import matplotlib as plt
import seaborn as sns

#################
# Top k algorithm
#################


def perm_rank(n):
  """create a ranked order list of n items"""
  return random.sample(range(1, n+1), n)


def top_k_selection_algo(array, m, k):
  """for any list of order, apply top-k algorithm
  
  Return whether we succeed (1) or failure (0) to
  identify top-k value
  """
  top_first_m = min(array[:(m-1)])
  # then for array[n:]
  # we pick first k values that is greater than max_first_m 
  inspect_array = np.array(array[m-1:])
  qualified_cand = inspect_array[inspect_array < top_first_m][:k]

  if len(qualified_cand) == k and max(qualified_cand) == k:
      return 1
  return 0


def simulation_top_k(n, k, iters):
  """
  for any value of k and n
  simulate all exploration cutoff from 2-> n
  and return a list of success probability at different cutoff
  """
  result = []
  for m in range(2, n+1):
      result_m = []
      for i in range(iters):
          order = perm_rank(n)
          success = selection_algo(order, m, k)
          result_m.append(success)
      result.append(np.mean(result_m))
  return result
