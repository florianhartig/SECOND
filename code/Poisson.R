





createPoissonData <- function(n=500, numGroups = 10, sampleSize = 2000){
  out = list()
  for (i in 1:n){
    environment1 = seq(-1,1,len = sampleSize)
    group = rep(1:numGroups, each = sampleSize/numGroups)
    groupRandom = rnorm(numGroups, sd = 1)
    counts = rpois(sampleSize, exp(environment1 + groupRandom[group] + rnorm(sampleSize, sd = 0.5)))
    out[[i]] <- data.frame(ID = 1:2000, counts, environment1, group)
  }
  return(out)
}
