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


result = simulation_top_k(100, 1, iters)
result_3 = simulation_top_k(100, 3, iters)
result_5 = simulation_top_k(100, 5, iters)
result_10 = simulation_top_k(100, 10, iters)


plt.style.use('fivethirtyeight')
plt.figure(figsize=(13,6))
sns.scatterplot(np.arange(2, 101),y=result, label = "k = 1")
sns.scatterplot(np.arange(2, 101),y=result_3, label = "k = 3")
sns.scatterplot(np.arange(2, 101),y=result_5, label = "k = 5")
sns.scatterplot(np.arange(2, 101),y=result_10, label = "k = 10")
plt.grid(False)
plt.title("Probability of finding top k partners\n by exploring first r values")
plt.xlabel("r values")
plt.ylabel("Probability")


##############################
# Top candidate with p success
##############################

def selection_algo_with_success_rate(array, m, p):
  top_first_m = min(array[:(m-1)])
  available_array = np.random.binomial(1, p, len(array))
  #print(available_array)
  # then for array[n:]
  # we pick first k values that is greater than max_first_m 
  #print("top first m", top_first_m)
  #print(array[:(m-1)], array[m-1:])
  inspect_array = array[m-1:]
  inspect_available = available_array[m-1:]

  if top_first_m == 1:
      return 0
  available_idx = np.where(inspect_available == 1)[0]
  available_person = np.array(inspect_array)[available_idx]
  pass_cand =  available_person[available_person < top_first_m]
  #print(pass_cand)
  if len(pass_cand) == 0:
      return 0
  accept = pass_cand[0]
  if accept == 1:
      return 1
  return 0
 
def simulate_with_success_rate(n, p, iters):
  result = []
  for m in range(2, n+1):
      result_m = []
      for i in range(iters):
          order = perm_rank(n)
          success = selection_algo_with_success_rate(order, m, p)
          result_m.append(success)
      result.append(np.mean(result_m))
  return result

result_avail_1 = simulate_with_success_rate(100, 1, iters)
result_avail_2 = ssimulate_with_success_rate(100, 0.25, iters)
result_avail_5 = simulate_with_success_rate(100, 0.5, iters)
result_avail_7 = simulate_with_success_rate(100, 0.75, iters)

plt.style.use('fivethirtyeight')
plt.figure(figsize=(13,6))
sns.scatterplot(np.arange(2, 101),y=result_avail_1, label = "p = 1")
sns.scatterplot(np.arange(2, 101),y=result_avail_2, label = "p = 0.25")
sns.scatterplot(np.arange(2, 101),y=result_avail_5, label = "p = 0.5")
sns.scatterplot(np.arange(2, 101),y=result_avail_7, label = "p = 0.75")
plt.title("Probability of finding top partner at different success rate\n by exploring first r values")
plt.grid(False)
plt.xlabel("r values")
plt.ylabel("Probability")
