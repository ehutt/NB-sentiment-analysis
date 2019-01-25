

sentiment <- function(data){
  source('NB.R')
  source('LOOCV.R')
  library(tm)
  library(SnowballC)
  
  #convert to corpus data type
  corp = Corpus(VectorSource(data$text))
  
  #clean corpus-- 
  #all lower case, use stemming, remove stop words, etc  
  corp = tm_map(corp, tolower) 
  corp = tm_map(corp, removeWords, stopwords('english')) 
  corp = tm_map(corp, stemDocument)
  #corp = tm_map(corp, stripWhitespace)
  
  #convert to binary matrix 
  dtm = DocumentTermMatrix(corp)
  dtm = weightBin(dtm) 
  
  #remove sparse words (appear only once) 
  #if corpus is sufficiently large 
  if (ncol(dtm) > 1000){
    proportion = 1 - (1/nrow(dtm)) - 0.00001
    dtm = removeSparseTerms(dtm,proportion)}
  
  #convert to data frame 
  df = data.frame(as.matrix(dtm))
  df['sentiment_label'] = data$sentiment
  df[names(df)] = lapply(df[names(df)], factor)
  
  #define formula to predict sentiment 
  form = sentiment_label ~.
  
  #perform cross validation with naive bayes
  return(LOOCV(form,df)) 
  
}