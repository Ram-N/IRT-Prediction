#### Alec Stephenson ####
#### Metrics For Kaggle Competions ####
#### No Copyright Use At Own Risk ####
#### Code based on several posting on forums ####
#### by a number of different individuals. ####

#### The AUC Metric ####

auc <- function (pred, act) 
{
  if(!is.numeric(pred)) stop("'pred' must be numeric")
  if(!is.numeric(act)) stop("'act' must be numeric")
  if(!all(act %in% c(0,1))) stop("'act' must be binary")  
  if(length(pred) != length(act)) stop("'pred' and 'act' must have the same length")
  
  n0 <- sum(1-act); n1 <- sum(act)
  r <- rank(pred)[act==1]
  (sum(r) - n1 * (n1 + 1)/2)/(n0 * n1)
}

#### The Gini and Normalized Gini Metrics ####

gini <- function(pred, act, norm = FALSE) 
{
  if(!is.numeric(pred)) stop("'pred' must be numeric")
  if(!is.numeric(act)) stop("'act' must be numeric")
  if(!all(act %in% c(0,1))) stop("'act' must be binary")  
  if(length(pred) != length(act)) stop("'pred' and 'act' must have the same length")
  
  nn <- length(act); n1 <- sum(act)
  loss <- act[order(pred, decreasing=TRUE)]
  ginival <- (sum((nn:1) * loss/n1) - (nn + 1)/2)/nn
  if(norm) ginival <- 2*ginival/(1-n1/nn)
  ginival
}

#### Capped Binomial Deviance ####

cbinom <- function(pred, act, cap = c(0.01,0.99)) 
{
  if(!is.numeric(pred)) stop("'pred' must be numeric")
  if(!is.numeric(act)) stop("'act' must be numeric")
  if(!all(act %in% c(0,1))) stop("'act' must be binary")  
  if(length(pred) != length(act)) stop("'pred' and 'act' must have the same length")
  
  pred <- pmax.int(pmin.int(pred, cap[2]), cap[1])
  -sum(act*log10(pred) + (1-act)*log10(1-pred))/length(pred)
}

#### Root Mean Squared (Percentage) Error ####

rmse <- function(pred, act, norm = FALSE) 
{
  if(!is.numeric(pred)) stop("'pred' must be numeric")
  if(!is.numeric(act)) stop("'act' must be numeric") 
  if(length(pred) != length(act)) stop("'pred' and 'act' must have the same length")
  
  errs <- pred - act
  if(norm) errs <- errs/act
  sqrt(sum(errs^2)/length(pred))
}

#### Mean Absolute (Percentage) Error ####

mae <- function(pred, act, norm = FALSE) 
{
  if(!is.numeric(pred)) stop("'pred' must be numeric")
  if(!is.numeric(act)) stop("'act' must be numeric") 
  if(length(pred) != length(act)) stop("'pred' and 'act' must have the same length")
  
  errs <- pred - act
  if(norm) errs <- errs/act
  sum(abs(errs))/length(pred)
}
