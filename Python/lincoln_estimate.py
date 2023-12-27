from random import random

def like_insta_post(p):
    "Find an error with probability p"
    return 1 if random.random() < p else 0

def simulate(true_audience, p1, p2, reps=10000):
    """Simulate Lincoln's method for estimating errors
    given the true number of errors, each person's probability
    of finding an error, and the number of simulations to run."""
    naive_estimates = []
    lincoln_estimates = []
    
    for rep in range(reps):
        like_post_1 = np.array([like_insta_post(p1) for _ in range(true_audience)])
        like_post_2 = np.array([like_insta_post(p2) for _ in range(true_audience)])
        like_post1_count = sum(like_post_1)
        like_post2_count = sum(like_post_2)
        overlap = np.sum(like_post_1 & like_post_2)
        
        naive_estimates.append(like_post1_count + like_post2_count - overlap)
        if overlap > 0:
            lincoln_estimates.append(like_post1_count*like_post2_count / float(overlap))
    
    return naive_estimates, lincoln_estimates

def calc_stats(arr):
    return (
            np.mean(arr),
            np.std(arr, ddof=1),
            np.mean(arr) - 1.96*np.std(arr, ddof=1),
            np.mean(arr) + 1.96*np.std(arr, ddof=1)
           )
