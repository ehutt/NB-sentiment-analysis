# NB-sentiment-analysis
R Project; Sentiment analysis; Naive Bayes classifier; leave-one-out cross validation 

I wrote this code for a class project. We were only allowed to use a limited number of basic R functions, so all of the algorithms are written from scratch. 

## Data Sets

- `Tennis.csv` 
- `house-votes-84.data` 
- `sentiment`

## R scripts: 
- `NB.R` 
  - function performs Naive Bayes classification from a given dataset. 
  - takes as input a `formula`,  a dataframe `data`, and a `query`. 
  - returns the classification and probability of `query` belonging to that class.
- `LOOCV.R`
  - function that performs leave-one-out cross validation of NB.R.
  - input a `formula` and a dataframe `data`. 
  - returns the NB classification accuracy 
- `sentiment.R` 
  - function that uses NB.R and LOOCV.R to perform sentiment analysis
  - input a dataframe `data` containing text and sentiment labels 
  - returns the classification accuracy of the sentiment analysis via LOOCV 
  
## Examples 

1. Naive Bayes 
