from sklearn.ensemble import RandomForestRegressor

import pandas as pd
import numpy as np
from sklearn.model_selection import cross_val_score, GridSearchCV
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
    
    X_tr, X_val, y_tr, y_val = train_test_split(x_train, y_train, random_state = 3)
    
    #%%
    clf = RandomForestRegressor(n_estimators=1000,\
                                 n_jobs = 1,\
                                 criterion="mse",\
                                 bootstrap=True, \
                                 min_samples_split=2,\
                                 min_samples_leaf=1,\
                                 max_depth=None,\
                                 max_features=90) 
    
#    param_grid = {
#                    "n_estimators": [1000, 1200],
#                    "max_depth": [None],
#                    "max_features": [90],
#                    "min_samples_split": [2],
#                    "min_samples_leaf": [1],
#                    "bootstrap": [True]
#                 }
# 
#    grid_clf = GridSearchCV(clf, param_grid, cv=4, verbose=2, n_jobs=4)
#    grid_clf.fit(x_train, y_train)
#    
#    print("\n-------- BEST ESTIMATOR --------\n")
#    print(grid_clf.best_estimator_)
#    print("\n-------- BEST PARAMS --------\n")
#    print(grid_clf.best_params_)
#    print("\n-------- BEST SCORE --------\n")
#    print(grid_clf.best_score_)

    train_test(clf, x_train, x_test, y_train, y_test)
 
    clf.fit(x_train, y_train)
    
    # Feature importance plot of Random Forest;
    # see http://scikit-learn.org/stable/auto_examples/ensemble/plot_forest_importances.html
    importances = clf.feature_importances_
    std = np.std([tree.feature_importances_ for tree in clf.estimators_],
                 axis=0)
    indices = np.argsort(importances)[::-1]
    
    # Print the feature ranking
    print("Feature ranking:")
    
#    for f in range(x_train.shape[1]):
#        print("%d. feature %d (%f):" % (f + 1, indices[f], importances[indices[f]]), x_train.columns[indices[f]])
    
    # Plot the feature importances of the forest
    plt.figure()
    plt.title("Feature importances")
    plt.bar(range(x_train.shape[1]), importances[indices],
           color="#5A9ACC", yerr=std[indices], align="center")
    plt.xticks(range(x_train.shape[1]), x_train.columns[indices], rotation='vertical')
    plt.xlim([-1, x_train.shape[1]])
    plt.show()
    
    #%%
    pred = clf.predict(test.iloc[:, range(1, test.shape[1])])
    
    pred = pred * y_sd + y_mean
    
    pred = np.expm1(pred)
    
    results = pd.DataFrame({"Id": test.Id, "SalePrice": pred})
    
    #%%
    results.to_csv("../data/predictions/forest_py.csv", index=False)
    

                        