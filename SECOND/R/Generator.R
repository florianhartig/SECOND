

# Binary data 

# http://stackoverflow.com/questions/10535235/generate-correlated-random-numbers-from-binomial-distributions-in-r

# library(copula)

# tmp <- normalCopula( 0.75, dim=2 )
# x <- rcopula(tmp, 1000)
# x2 <- cbind( qbinom(x[,1], 10, 0.5), qbinom(x[,2], 15, 0.7) )




#' Creates a matrix with predictors
#'
#' @author Florian Hartig
createPredictors <- function(numberPredictors, samplesize, cor = 0) {
  
  predictors = matrix(runif(numberPredictors*samplesize, min = -1), ncol = numberPredictors)
  
  if (cor != 0){
    predTemp <- runif(samplesize, min = -1)
    predictors  = (1-cor) * predictors + cor * matrix(rep(predTemp, numberPredictors), ncol = numberPredictors)
  }
  return(predictors)
}





#' Creates a lm or GLM response
#'
#' @author Florian Hartig
createResponse <- function(predictors, coefficients = 1, intercept = 0, family = gaussian() , scale = 1) {
  
  # checking inputs
  if (is.null(coefficients)) coefficients = rep(1,ncol(predictors))
  if (length(coefficients)==1) coefficients = rep(coefficients,ncol(predictors))
  if (ncol(predictors) != length(coefficients))stop("coefficients don't match ")
  
  n = nrow(predictors)

  linearResponse = intercept + predictors %*% coefficients
  linkResponse = family$linkinv(linearResponse)
  
  if (family$family == "gaussian") observedResponse = rnorm(n, mean = linkResponse, sd = scale)  
  else if (family$family == "binomial") observedResponse = rbinom(n, 1, prob = linkResponse)
  else if (family$family == "poisson") observedResponse = rpois(n, lambda = linkResponse)
  else stop("wrong link argument supplied")
  
  return(observedResponse)
}




