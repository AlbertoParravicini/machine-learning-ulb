import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib

import matplotlib.pyplot as plt
from scipy.stats import skew
from scipy.stats.stats import pearsonr


from keras.layers import Dense, Dropout, BatchNormalization, Activation
from keras.models import Sequential
from keras.regularizers import l1
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from keras.callbacks import ModelCheckpoint

#%% 

if __name__ == '__main__':
    # File name
    file_name_train = "../data/train_fin.csv"
    file_name_test = "../data/test_fin.csv"
    
    # Read the train and test data
    train = pd.read_csv(file_name_train)
    test = pd.read_csv(file_name_test)
    
    X_train = np.asmatrix(train.iloc[:, range(1, train.shape[1]-1)])
    X_test = np.asmatrix(test.iloc[:, range(1, test.shape[1])])
    y = train.SalePrice
    
#    X_train = StandardScaler().fit_transform(X_train)
#    X_test = StandardScaler().fit_transform(X_test)

    X_tr, X_val, y_tr, y_val = train_test_split(X_train, y, random_state = 3)
    
    #%% BUILD NN
    
    
    model = Sequential()
    model.add(Dense(32, input_shape = (X_train.shape[1], ), kernel_regularizer=l1(0.001)))
    model.add(BatchNormalization())
    model.add(Activation('relu'))
    model.add(Dropout(0.2))
    
    model.add(Dense(32, kernel_regularizer=l1(0.001)))
    model.add(BatchNormalization())
    model.add(Activation('relu'))
    model.add(Dropout(0.2))
    
    
    model.add(Dense(32, kernel_regularizer=l1(0.001)))
    model.add(BatchNormalization())
    model.add(Activation('relu'))
    model.add(Dropout(0.2))
    
    model.add(Dense(32, kernel_regularizer=l1(0.001)))
    model.add(BatchNormalization())
    model.add(Activation('relu'))
    model.add(Dropout(0.2))
    
    model.add(Dense(32, kernel_regularizer=l1(0.001)))
    model.add(BatchNormalization())
    model.add(Activation('relu'))
    
    model.add(Dense(1))
    
    model.compile(loss = "mse", optimizer = "adam")
    model.summary()
    
    #%% TRAIN NN
    filepath="../models/best-weights.hdf5"
    checkpoint = ModelCheckpoint(filepath, monitor='loss', verbose=1, save_best_only=True, mode='min')
    callbacks_list = [checkpoint]
    
    hist = model.fit(X_tr, y_tr, validation_data = (X_val, y_val), batch_size=32, epochs=2000, callbacks=callbacks_list)
    
    
    
    
    
    #%% SAFETY BREAK
    
    
    
    
    
    
    
    #%% PREDICT
    
    
    filepath = "../models/best-weights.hdf5"
    model.load_weights(filepath)
    
    model.compile(loss="mse", optimizer="adam")
    
    pred = model.predict(X_test)
    pred_set = pd.DataFrame({"Id":test.Id, "SalePrice":np.expm1(pred).reshape(-1)})
    
    #%% WRITE
    pred_set.to_csv("../data/predictions/nn_pred_3.csv", index=False)