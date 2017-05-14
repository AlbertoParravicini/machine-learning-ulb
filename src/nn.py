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

train = pd.read_csv("../data/train.csv")
test = pd.read_csv("../data/test.csv")

train.head()


all_data = pd.concat((train.loc[:,'MSSubClass':'SaleCondition'],
                      test.loc[:,'MSSubClass':'SaleCondition']))

matplotlib.rcParams['figure.figsize'] = (12.0, 6.0)
prices = pd.DataFrame({"price":train["SalePrice"], "log(price + 1)":np.log1p(train["SalePrice"])})

#log transform the target:
train["SalePrice"] = np.log1p(train["SalePrice"])

#log transform skewed numeric features:
numeric_feats = all_data.dtypes[all_data.dtypes != "object"].index

skewed_feats = train[numeric_feats].apply(lambda x: skew(x.dropna())) #compute skewness
skewed_feats = skewed_feats[skewed_feats > 0.75]
skewed_feats = skewed_feats.index

all_data[skewed_feats] = np.log1p(all_data[skewed_feats])

all_data = pd.get_dummies(all_data)

all_data = all_data.fillna(train.mean())

#creating matrices for sklearn:
X_train = all_data[:train.shape[0]]
X_test = all_data[train.shape[0]:]
y = train.SalePrice

#%% BUILD NN
X_train = StandardScaler().fit_transform(X_train)
X_tr, X_val, y_tr, y_val = train_test_split(X_train, y, random_state = 3)

model = Sequential()
model.add(Dense(40, input_shape = (X_train.shape[1], ), kernel_regularizer=l1(0.001)))
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
filepath="../models/best-weights.hdf5"
checkpoint = ModelCheckpoint(filepath, monitor='loss', verbose=1, save_best_only=True, mode='min')
callbacks_list = [checkpoint]

hist = model.fit(X_tr, y_tr, validation_data = (X_val, y_val), epochs=2000, batch_size=32, callbacks=callbacks_list)

#%% PREDICT
filepath = "../models/best-weights.hdf5"
model.load_weights(filepath)

model.compile(loss="mse", optimizer="adam")

X_test = StandardScaler().fit_transform(X_test)
pred = model.predict(X_test)
pred_set = pd.DataFrame({"Id":test.Id, "SalePrice":np.expm1(pred).reshape(-1)})

#%% WRITE
pred_set.to_csv("../data/nn_pred.csv", index=False)