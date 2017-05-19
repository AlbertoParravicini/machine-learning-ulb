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



#%% Read data

train = pd.read_csv("../data/train_fin.csv")
test = pd.read_csv("../data/test_fin.csv")

train_svm = pd.read_csv("../data/predictions/svm_xval.csv")
train_rlm = pd.read_csv("../data/predictions/ridge_xval.csv")

# Create a training set by combining the predictions.
train_nn = pd.DataFrame({"Id": train.Id,\
                         "svmSalePrice": train_svm.SalePrice,\
                         "rlmSalePrice": train_rlm.SalePrice,\
                         "SalePrice": train.SalePrice})

train.head()

# Log-scale the prices.
train_nn.svmSalePrice = np.log1p(train_nn.svmSalePrice)
train_nn.rlmSalePrice = np.log1p(train_nn.rlmSalePrice)



X_train = train_nn.iloc[:, 2:]
y = train_nn.SalePrice

x_scaler = StandardScaler().fit(X_train)
X_train = x_scaler.transform(X_train)

y_scaler = StandardScaler().fit(y.values.reshape(-1, 1))
y = y_scaler.transform(y.values.reshape(-1, 1))
X_tr, X_val, y_tr, y_val = train_test_split(X_train, y)

#%% BUILD NN


model = Sequential()
model.add(Dense(100, input_shape = (X_train.shape[1], ), kernel_regularizer=l1(0.001)))
model.add(BatchNormalization())
model.add(Activation('relu'))
model.add(Dropout(0.2))

model.add(Dense(40, kernel_regularizer=l1(0.001)))
model.add(BatchNormalization())
model.add(Activation('relu'))
model.add(Dropout(0.2))


model.add(Dense(40, kernel_regularizer=l1(0.001)))
model.add(BatchNormalization())
model.add(Activation('relu'))

model.add(Dense(1))

model.compile(loss = "mse", optimizer = "adam")
model.summary()

#%% TRAIN NN
filepath="../models/best-weights-stack.hdf5"
checkpoint = ModelCheckpoint(filepath, monitor='loss', verbose=1, save_best_only=True, mode='min')
callbacks_list = [checkpoint]

hist = model.fit(X_tr, y_tr, validation_data = (X_val, y_val), epochs=2000, batch_size=32, callbacks=callbacks_list)

#%% PREDICT
filepath = "../models/best-weights-stack.hdf5"
model.load_weights(filepath)

model.compile(loss="mse", optimizer="adam")

test_svm = pd.read_csv("../data/predictions/svm_fin.csv")
test_rlm = pd.read_csv("../data/predictions/ridge_fin_01.csv")

test_svm.SalePrice = np.log1p(test_svm.SalePrice)
test_rlm.SalePrice = np.log1p(test_rlm.SalePrice)

X_test = pd.DataFrame({"svmSalePrice": test_svm.SalePrice, "rlmSalePrice": test_rlm.SalePrice})
X_test = x_scaler.transform(X_test)


pred = model.predict(X_test)
pred = y_scaler.inverse_transform(pred)
pred = np.expm1(pred).reshape(-1)
pred = np.round(pred / 500) * 500
pred_set = pd.DataFrame({"Id": test.Id, "SalePrice": pred})

#%% WRITE
pred_set.to_csv("../data/predictions/nn_pred_stack.csv", index=False)