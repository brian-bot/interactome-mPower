
circumEarthInMiles <- 24901.55

findLatLong <- function(city, state, country){
  require(RCurl)
  
  stopifnot(length(city) == length(state) & length(city) == length(country))
  queryStr <- sapply(seq_along(city),function(i){
      gsub(" ","+",gsub(",NA,",",",paste(city[i],state[i],country[i],sep=",")))
  })
  location = t(sapply(queryStr, function(qs){
    R <- fromJSON(getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",qs,"&sensor=false&key=AIzaSyBmp_YrMp9wl6ZrgbpffNiS3Yw9a7aJ27o",sep="")))
    Sys.sleep(.5)
    if(length(R$results) > 0){
      return (unlist(R$results[[1]]$geometry$location))
    }else{
      return(c(NA,NA))
    }  
  }))
  return(location)
}

computeGeoDistanceMatrix <- function(lat, lng){
  require(spaa) # for geometric calculations
  n <- length(lat)
  M <- matrix(0, nrow=n, ncol=n)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      M[i,j] <- geodist(lat[i],lng[i],lat[j],lng[j])
    }
  }
  M[is.na(M)] <- 0
  M <- t(M) + M
  return(M)
}

geo2SimilarityMatrix <- function(M){
  circumEarthInMiles - M
}