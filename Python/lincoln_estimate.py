from matplotlib import pyplot as plt

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

sims = [[0.3, 0.5], [0.6, 0.4], [0.7, 0.8], [0.9, 0.9]]
# create 2 lists, 1 of data frame of values, 1 of titles
res_arr = []
title_arr = []

for p in sims:
    naive_estimates, lincoln_estimates = simulate(100, p[0], p[1], reps=100000)
    naive_stats = calc_stats(naive_estimates)
    lincoln_stats = calc_stats(lincoln_estimates)
    naive_mean, naive_std = naive_stats[0], naive_stats[1]
    lincoln_mean, lincoln_std = lincoln_stats[0], lincoln_stats[1]
    
    pd_res = pd.DataFrame(
        {
            "method":["naive", "Lincoln"],
            "estimate":[naive_mean, lincoln_mean], 
            "std": [naive_std, lincoln_std]}
    )
    res_arr.append(pd_res)
    title_arr.append(f" p1={str(p[0])}\n p2={str(p[1])}")

colors = ['blue', 'orange']
fig, axes = plt.subplots(1, 4, figsize=(18, 6), sharey=True)
ax1, ax2, ax3, ax4 = axes
for dat_df, ax, title in zip(
                   res_arr, 
                   [ax1, ax2, ax3, ax4],
                   title_arr
):
    dat_df.plot(x='method', y='estimate', yerr = 'std', kind='bar', color = colors,
                ax=ax, legend=False, xlabel='', ylabel = 'mean of estimates').set_title(title)

for ax in axes:
    ax.set_xticklabels(ax.get_xticklabels(), rotation = 90)
    for side in ('right', 'top', 'left'):
        if (ax == ax1) and (side == 'left'):
            continue
        else:
            sp = ax.spines[side]
            sp.set_visible(False)
