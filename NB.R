
NB <- function(formula,data,query){
  library(plyr)
  library(DescTools)
  
  ## error checking 
  if (!is.formula(formula)) {stop("Invalid Formula.")
  } else if (!is.data.frame(data)){ stop("Data must be a data frame. ")
  } else if(!is.vector(query)){ stop("Query must be a vector.")
  } else if(any(is.na.data.frame(data))){stop("Null values detected.")} 
  
  #parse formula, extract response variable 
  parsed = ParseFormula(formula,data=data)
  response = parsed$lhs[3][[1]]
  
  #num observations and classes
  n <- nrow(data)
  classes = levels(data[[response]]) 
  numClasses = length(classes)
  #initialize empty for each class
  class_probs = array(0,dim=numClasses)
  
  for (c in 1:numClasses){
    label = classes[c]
    
    #probability of response = class c 
    prior_p <- sum(data[[response]]==label) / n 
    
    #initialize probabilities 
    p_x <- 1 #P(all features)
    cond_y <- prior_p #P(response==TRUE)
    x_given_y <- array(0,dim=length(query)) #P(features|response)

    #iterate through data frame and query to calculate probabilities 
    #assume data[,1] is an index so skip it 
    for (i in 1:length(query)){
      
      varName = parsed$rhs[[3]][i]
      
      if (match(query[[i]],levels(data[[varName]]),nomatch=0)==0) {
        stop("Query does not match the model.")}
      
      ## total probability of feature i in query 
      ## (not necessary for classification)
      ## P(xi) = # xi / # Observations
      # p_i <- (sum(data[[varName]]==query[[i]])) / n 
      ## running total for P(X) = P(x1x2...xn)
      # p_x <- p_x * p_i
      
      #probability of feature i given response 
      # P(X|Y) = P(Y)*P(x1|Y)*P(x2|Y)*...P(xn|Y)
      x = data[[varName]]==query[[i]]
      y = data[[response]]==label
      
      #add one to contingency table for large dataset 
      # (to prevent multiplying by 0) 
      # for sentiment analysis only 
      if (length(query) > 25){tab <- table(x,y) + 1
      } else {tab <- table(x,y)}
      x_given_y[i] <- tab[dim(tab)[1],dim(tab)[2]] / sum(tab[,dim(tab)[2]])
      
      #running total for P(X|Y)P(Y)
      cond_y <- cond_y * x_given_y[i] 
    }
    class_probs[c] <- cond_y
  }   
  
  #prediction = classes[which.max(class_probs)] 
  #p = max(class_probs)
  
  #by hand: 
  max_p = 0
  max_class = 0
  for (i in 1:length(class_probs)){
    if (class_probs[i] > max_p) {
      max_p = class_probs[i]
      max_class = i}
  }
  prediction = classes[max_class]
  p = max_p

  return(c(prediction,p))
}



  
