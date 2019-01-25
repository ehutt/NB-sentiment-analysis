#Leave One Out Cross Validation for NB 

LOOCV <- function(formula,data){
  library(DescTools)
  source('NB.R')
  
  start_loocv = Sys.time()
  
  #parse formula
  parsed = ParseFormula(formula,data)
  #get response variable ad query variables
  response = parsed$lhs[3][[1]]
  query_items = parsed$rhs[[3]]
  #initialize vector to track errors
  errors = vector(length=nrow(data))
  
  for (i in 1:nrow(data)){
   
    test = data[i,] 
   
    #only edit formula/query if large (as in sentiment analysis)
    if (length(query_items) > 25){
      train = data.frame(data[-i,response])
      colnames(train) = response
      query = c()
      vars = c()
      for (j in 1:length(query_items)){
        #remove columns with test=0 from formula, query, and training
        #to reduce size of input and unnecessary computation
        if (test[[query_items[j]]] != 0){ 
          query = c(query, as.vector(test[[query_items[j]]]))
          vars = c(vars, names(test)[j])
          train[names(test)[j]] = data[-i,j]
        }
      }
      #only use variables present in the query for the formula
      f = as.formula(paste(response, "~", paste(vars, collapse=" + ")))
    } else {
      #use full query/formula if small data set 
      train = data[-i,]
      query = vector(length=length(query_items))
      for (j in 1:length(query_items)){
        query[j] = as.vector(test[[query_items[j]]])
      }
      f = formula
    }
    
    #use naive bayes to predict test response based on training data
    prediction = NB(f,train,query)[1]
    if (i %% 50 == 0){ message(c("LOOCV Progress: ", i, " out of ", nrow(data)))}
    #track errors
    if (prediction == test[[response]]){errors[i] = 1
    } else {errors[i] = 0}
  }
  
  #calculate average accuracy 
  accuracy = sum(errors) / nrow(data)
  end_loocv = Sys.time()
  
  run_time = end_loocv - start_loocv
  message(c("Run time for LOOCV: ", run_time))
  
  return(accuracy)
}

  
  