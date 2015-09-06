
x <- createPredictors(10, 100, cor = 0.5)

pairs(x)


createResponse(predictors = x, coefficients = 1, intercept = 0, family = poisson() , scale = 1)
