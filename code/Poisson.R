

createDataList <- function(N = 500, datacreator, ...){
  out = list()  
  for (i in 1:N){
    out[[i]] <- datacreator(...)
  }
  return(out)  
}


createPoissonData <- function(numGroups = 10, sampleSize = 2000, seed = NULL){
  
    if (!is.null(seed)) set.seed(seed)
  
    environment1 = seq(-1,1,len = sampleSize)
    group = rep(1:numGroups, each = sampleSize/numGroups)
    groupRandom = rnorm(numGroups, sd = 1)
    counts = rpois(sampleSize, exp(environment1 + groupRandom[group] + rnorm(sampleSize, sd = 0.5)))
    return(data.frame(ID = 1:sampleSize, counts, environment1, group))
}


createDataList(10, createPoissonData, sampleSize = 10, seed= 1)


createBeetleData <- function(seed = NULL){

  if (!is.null(seed)) set.seed(seed)
  
  altitude = rep(seq(0,1,len = 50), each = 20)
  moisture = runif(1000, 0,1)
  temperature =  runif(1000, 0,1) - moisture - altitude + 2
  dataID = 1:1000
  spatialCoordinate = rep(seq(0,30, len = 50), each = 20)
  
  # random effects
  plot = rep(1:50, each = 20)
  year = rep(1:20, times = 50)
  
  #plotRandom = 0 - rexp(20, rate = 1)
  
  yearRandom = rtnorm(20, 0, 2, upper = 2)
  plotRandom = rtnorm(50,0,1, upper = 1)
  #overdispersion = rtnorm(1000, sd = 1, upper = 1)
  
  beetles <- rpois(1000, exp( 1 +   
                                
                                ( 2 + yearRandom[year]) * moisture 
                              
                              + 10*altitude - 10*altitude^2 
                              
                              #+ overdispersion 
                              + plotRandom[plot]) )
  
  # beetles[rbinom(1,200,0.1)] = 0  #zero-inflation
  data = data.frame(dataID, beetles, moisture, altitude, temperature, plot, year, spatialCoordinate)
}
