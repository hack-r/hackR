## File: hackR_functions.R
## Description: Functions called by hackR-package.R
## Copyright: (c) 2014, Jason D. Miller, MS, MS
## http://hack-r.com 
## http://hack-r.github.io
## http://github.com/hack-r


# string_to_month ---------------------------------------------------------

string_to_month=function(x)
{
  if(x=='January'){
    return("01");
  }
  else if(x=='February'){
    return("02");
  }
  else if(x=='March'){
    return("03");
  }
  else if(x=='April'){
    return("04");
  }
  else if(x=='May'){
    return("05");
  }
  else if(x=='June'){
    return("06");
  }
  else if(x=='July'){
    return("07");
  }
  else if(x=='August'){
    return("08");
  }
  else if(x=='September'){
    return("09");
  }
  else if(x=='October'){
    return("10");
  }
  else if(x=='November'){
    return("11");
  }
  else if(x=='December'){
    return("12");
  }
  else{
    return(999999);
  }
}


# Source2 -----------------------------------------------------------------
source2 <- function(file, start, end, ...) {
  # Sources just part of a file
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)}


# predictMNL --------------------------------------------------------------

predictMNL <- function(model, newdata) {
  
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
    
    # Draw random values
    vals <- runif(nrow(newdata))
    
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
    
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
    
    # Return the values
    return(ids)
  }}


# Log-Likelihood FN for Optimization --------------------------------------
ll <- function(params) {
  ## Log likelihood for y ~ x + student's t errors
  params <- as.list(params)
  return(sum(dt((df$y - params$const - params$beta*df$x) / params$scale, df=params$degrees.freedom, log=TRUE) -
               log(params$scale)))
}


# obj_size ----------------------------------------------------------------
obj_size <- function (x) {
  sort( sapply(ls(),function(x){format(object.size(get(x)), units = "Mb")}))
}
