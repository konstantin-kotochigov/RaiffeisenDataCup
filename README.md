# Raiffeisen Data Cup Contest (2018)

15th place solution (11th on public board). See "app.R"

## Problem setting
Card Transaction data is available for 20,000 bank clients. 

Data includes:
* GPS coordinates
* MCC code (merchant category)
* Transaction date 
* Parsed address in text format

and some other features...

For the train part of 10,000 clients there is also home address and (for 5,000 clients) work address avaliable

#### Task 
To predict home and work for 10,000 clients in test dataset. Prediction is considered correct if error is less then 0.02 degress (~2km for local latitude)

#### Evaluation metric
Accuracy

## Considered approaches
| Idea |  Description | Result |
| :------------- |:-------------| :-----|
| binary classification | 1 - for points near home(work), 0 - for other points | qucikly got near top with it, so chose it as the main approach |
| point candidates generation | extend dataset with new points (as a grid or as cluster centers) | tried cluster centers, it showed slight imporvement |
| convolutional networks / autoencoders | treat transaction map as an image | not tried |
| regression techniques | prediction as a combination of other points | not tried |
| clusterization | use different MCC-cluster centers as predictions | mediocre results, opted for more prospective classification approach |


## Solution description
* Transactions are aggregated into single points by (lonlat, mcc)
* Some customer level and point-level attributes are calculated
* Distance-based attributes are computed individually for each customer
* Catboost performed best among tried models
* Some averaging is done (very simple 5 model bagging combined with two top-scored points averaging)
