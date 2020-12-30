import numpy as np
import pandas as pd
import io
from collections import defaultdict
from sklearn.metrics import mean_squared_error

from numpy.linalg import solve
np.random.seed(0)

# input is a dataframe with 3 columns
# user_id, item_id, rating
def create_rating_matrix(df):
    n_users = df.user_id.unique().shape[0]
    n_items = df.item_id.unique().shape[0]
    ratings = np.zeros((n_users, n_items))
    for row in df.itertuples():
        # row[1] - 1 is the user id readjusted to start by index 0
        # row[2] - 1 is the item id readjusted to start by index 0
        ratings[row[1]-1, row[2]-1] = row[3]
    ratings


# calculate sparsity of rating matrix
def calculate_sparsity(rating_matrix)
    sparsity = float(len(rating_matrixnonzero()[0])) * 100 / (rating_matrix.shape[0] * rating_matrix.shape[1])
    return sparsity


# function to split train, test data
def train_test_split(ratings, pct):
    test = np.zeros(ratings.shape)
    train = ratings.copy()
    for user in range(ratings.shape[0]):
        user_rating_idx = ratings[user, :].nonzero()[0]
        test_ratings = np.random.choice(user_rating_idx,
                                     size=int(len(user_rating_idx)*pct),
                                     replace=False)
        train[user, test_ratings] = 0.
        test[user, test_ratings] = ratings[user, test_ratings]
        
    # Test and training are truly disjoint
    assert(np.all((train * test) == 0)) 
    return train, test


# function to calculate MSE error
def get_mse(pred, actual):
    pred = pred[actual.nonzero()].flatten()
    actual = actual[actual.nonzero()].flatten()
    return mean_squared_error(pred, actual)
