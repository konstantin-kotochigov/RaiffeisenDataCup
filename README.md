# Raiffeisen Data Cup Contest (2018)

16th place solution (12th on public board)

## Problem setting
Card Transaction data is available for 20,000 bank clients. 

Data includes:
* GPS coordinates of transaction
* MCC code (merchant category)
* transaction date 
* parsed address in text format

and some other attributes

For train part of 10,000 clients there is also home address and (for 5,000 clients) work address avaliable

Task is to predict home and work for 10,000 clients in test dataset

Prediction is considered correct if error is less then 0.02 degress (~2km for local latitude)

Evaluation metric: Accuracy

## Challenges
* Poor data quality
* Time of transaction is not available, just date

## Considered approaches
* Point binary classification (1 - for points near home(work), 0 - for other points. Model qucikly performed in near top-10, so I chose it as the main approach)
* Point candidates generation (Tried in a simple form, showed 1% increase in performance)
* Convolutional neural network and Autoencoder (Not tried)
* Regression (Prediction as combination of other points, not tried)
* Clusterization (at first it showed normal results, but then I opted for regular classification which proved to be much better)

## Solution description
* Transactions are aggregated in single points (lonlat, mcc) with several attributes
* Some customer level attributes are also calculated
* Distance-based attributes are computed individually for each customer
* Catboost model performed best out of tried models
* Some averaging is done (very simple 5 model bagging combined with two top-scored points averaging)
