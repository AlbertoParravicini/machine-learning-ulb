# -*- coding: utf-8 -*-
"""
Created on Tue May 16 23:08:16 2017

@author: albyr
"""

from sklearn.ensemble import GradientBoostingRegressor

import pandas as pd
import numpy as np
from sklearn.model_selection import cross_val_score, GridSearchCV, cross_val_predict
import matplotlib.pyplot as plt
from sklearn.metrics import r2_score, mean_squared_error
from sklearn.model_selection import train_test_split


# Courtesy of:
#   https://www.kaggle.com/neviadomski/how-to-get-to-top-25-with-simple-model-sklearn/comments/notebook
# Prints R2 and RMSE scores
def get_score(prediction, lables):    
    print('R2: {}'.format(r2_score(prediction, lables)))
    print('RMSE: {}'.format(np.sqrt(mean_squared_error(prediction, lables))))

# Shows scores for train and validation sets    
def train_test(estimator, x_trn, x_tst, y_trn, y_tst):
    prediction_train = estimator.predict(x_trn)
    # Printing estimator
    print(estimator)
    # Printing train scores
    get_score(prediction_train, y_trn)
    prediction_test = estimator.predict(x_tst)
    # Printing test scores
    print("Test")
    get_score(prediction_test, y_tst)


#%% 

if __name__ == '__main__':
    # File name
    file_name_train = "../data/train_fin.csv"
    file_name_test = "../data/test_fin.csv"
    
    # Read the train and test data
    train = pd.read_csv(file_name_train)
    test = pd.read_csv(file_name_test)
    
    x_train = train.iloc[:, range(1, train.shape[1]-1)]
    y_train = train.SalePrice
    
    y_mean = np.mean(y_train)
    y_sd = np.std(y_train)
    y_train = (y_train - y_mean) / y_sd
    
    
    #%%

    gbr = GradientBoostingRegressor(n_estimators=4000,\
                                    learning_rate=0.05,\
                                    max_depth=1,\
                                    max_features=90,\
                                    min_samples_leaf=1,\
                                    min_samples_split=2,\
                                    loss='huber')
    
    #%%
    
    predicted = cross_val_predict(gbr, x_train, y_train, cv=10)
    print('RMSE: {}'.format(np.sqrt(mean_squared_error(predicted * y_sd + y_mean, y_train * y_sd + y_mean))))
    
    gbr.fit(x_train, y_train)

    
    #%%
    pred = gbr.predict(test.iloc[:, range(1, test.shape[1])])
    
    pred = pred * y_sd + y_mean
    
    pred = np.expm1(pred)
    pred = np.round(pred / 500) * 500
    
    results = pd.DataFrame({"Id": test.Id, "SalePrice": pred})
    
    #%%
    
    
    results.to_csv("../data/predictions/gbr_py_fin_1_90_1_2.csv", index=False)
    

                        