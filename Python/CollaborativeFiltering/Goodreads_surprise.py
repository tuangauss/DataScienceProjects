"""
Code for Collaborative Filtering project

Instead of using module from Training module.py,
using surprise package which is a lot more efficient
in terms of sparse matrix handling.
"""

import numpy as np
import pandas as pd
from surprise import SVD
from surprise import Dataset
from surprise import Reader
from surprise import accuracy
from surprise import BaselineOnly
from surprise.model_selection import train_test_split

# 6 millions rows
df = pd.read_csv('/ratings.csv')
df['user_id'] = df['user_id'].astype(str)
df.head()

# 10k books
books = pd.read_csv('/books.csv')

# create a mapping between book_id and name
id_to_name = {}
for row in new_books.itertuples():
    id_to_name[row[1]] = row[11]

# add my onw book ratings
my_rating = {'user_id': [str(53425) for i in range(11)],
             'book_id': [13, 119, 240, 283, 479, 1100, 2205, 2246, 3227, 7210, 5],
             'rating': [5, 3, 4, 4, 4, 5, 2, 4, 4, 3, 3]}
print([id_to_name[id] for id in [13, 119, 240, 283, 479, 1100, 2205, 2246, 3227, 7210, 5]])
full_rating = pd.DataFrame(my_rating).append(new_rating)


# Load our data into DataSet class of surprise package
reader = Reader(rating_scale=(1, 5))
data = Dataset.load_from_df(full_rating[['user_id', 'book_id', 'rating']], reader)

# split into trainset and testset
trainset, testset = train_test_split(data, test_size=.10)
train_eval = trainset.build_testset()

# train a Funk SGD-SVD algorithms:
epochs = [1, 5, 10, 20, 40, 80, 100, 120, 150]
train_mse = []
test_mse = []
for n_epoch in epochs:
    print("Number of epochs trained", n_epoch)
    algo = SVD(n_factors = 40, lr_all = 0.001, n_epochs = n_epoch)
    algo.fit(trainset)
    train_predictions = algo.test(train_eval)
    test_predictions = algo.test(testset)
    train_mse.append(accuracy.mse(train_predictions))
    test_mse.append(accuracy.mse(test_predictions))
    print(accuracy.mse(train_predictions), accuracy.mse(test_predictions))

# function to plot the learning curve through epochs
def plot_learning_curve(iter_array, train_accuracy, test_accuracy, xlabel = 'iterations'):
    plt.plot(iter_array, train_accuracy,
             label='Train mse', linewidth=5)
    plt.plot(iter_array, test_accuracy,
             label='Test mse', linewidth=5)


    plt.xticks(fontsize=16);
    plt.yticks(fontsize=16);
    plt.xlabel(xlabel, fontsize=30);
    plt.ylabel('MSE', fontsize=30);
    plt.legend(loc='best', fontsize=20);

plot_learning_curve(epochs, train_mse, test_mse)

#train on a full dataset and make prediction
full_trainset = data.build_full_trainset()
algo = SVD(n_factors = 40, lr_all = 0.001, verbose=True, n_epochs = 100)
algo.fit(full_trainset)

# make prediction:
all_book_id = full_rating.book_id.unique()
top_n = []
for book_id in all_book_id:
    top_n.append(algo.predict(uid = str(53425), iid = book_id))
top_n.sort(key=lambda x: x.est, reverse=True)
print([id_to_name[pred.iid] for pred in top_n[:10]]
