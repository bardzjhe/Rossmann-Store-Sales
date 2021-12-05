# Python 3 environment
# Rossmann Store Sales resource: https://www.kaggle.com/c/rossmann-store-sales

import numpy as np # linear algebra
import pandas as pd # .csv file I/O for data processing
import matplotlib.pyplot as plt
# Available data files are available in the "../input/" directory

train=pd.read_csv('../input/train.csv')
test=pd.read_csv('../input/test.csv')
store=pd.read_csv('../input/store.csv')

#train.sort_values(by=['Date']).head()
#train.head()



