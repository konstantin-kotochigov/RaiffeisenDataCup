# Raiffeisen Data Cup Contest (2018)

16th place solution (12th on public board)

## Problem setting
Card Transaction data is available for 20,000 bank clients. 

Data includes:
* GPS coordinates
* MCC code
* transaction date 
* parsed address

and some other attributes

For train part of 10,000 clients there is also home address and (for 5,000 clients) work address avaliable
Task: to predict home and work for 10,000 clients in test dataset

Prediction is correct if error is less then 0.02 degress (~2km for local latitude)
Evaluation metric: Accuracy

