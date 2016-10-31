library(rjson)
library(rgbif)
library(RCurl)
library(maptools)
library(raster)
require(ROAuth)

### API Key

apikey <- c('')

##API keys are provided by Google using through an individuals Google account.
#once you have yours, place it inside the single quotes.  

### Geocode Function
## This funcion will reutrn a lattitude and longitude coordinate for a named address. 
# EG: Try typing in a place of business. 
geocode <- function(location, apikey){
  
  api = c('https://maps.googleapis.com/maps/api/geocode/')
  output= c('json?')
  url <- paste(api,paste(output,location,sep="&address="), sep="")
  url.key <- paste(url,apikey, sep="&key=")
  result <- fromJSON(file=url.key)
  lat <- result$results[[1]]$geometry$location$lat
  lng <- result$results[[1]]$geometry$location$lng  
  cords <- cbind(lat,lng)
  cords <- as.data.frame(cords)
  return(cords)
}

### Geocode Example

city <- c("Atlanta, Ga")
state <- c("Georgia")
county <- c("houston county, Ga")

city.return <- geocode(city,apikey)
state.return <- geocode(state,apikey)
county.return <- geocode(county,apikey)


### Elevation Function
## If you provide teh latitude and longitude coordinates you can have google return
# an elevation value for that coordinate. 
elevation <- function(location, apikey){ #Returns Elevation in units of meters. 
 
  api = c('https://maps.googleapis.com/maps/api/elevation/')
  output= c('json?')
  coords = paste(location$lat,location$lng, sep=",")
  url <- paste(api,paste(output,coords,sep="&locations="), sep="")
  url.key <- paste(url,apikey, sep="&key=")
  result <- fromJSON(file=url.key)
  lat <- result$results[[1]]$location$lat
  lng <- result$results[[1]]$location$lng  
  elev <- result$results[[1]]$elevation
  elevation <- cbind(lat,lng,elev) 
  return(elevation)
}


### Elevation Example
## This reads in several lat,long points from around halfdome, Yosemite. 

halfdome <- read.csv('halfdomelatlon.csv')
halfdome.ele <- NULL

for(i in 1:length(halfdome$lat)){
  Sys.sleep(.25)
  location = halfdome[i,]
  ele = elevation(location,apikey)
  halfdome.ele <- rbind(halfdome.ele,ele)
}

halfdome.ele
write.csv(halfdome.ele, file="halfdome.ele.csv")


### Distance Matrix Function
## Provided two points, this funciton will return the average time of travel. 
# You can adjust for other options, such as mode of transportation and time of day. 

distmat <- function(origins,destinations,apikey){
  api = c('https://maps.googleapis.com/maps/api/distancematrix/')
  output= c('json?')
  unit = c('metric')
  mode = c('driving')#walking, driving, biking, transit is possible for some locations. 
  url <- paste(api,paste(output,unit,sep="&units="), sep="")
  url <- paste(url,mode,sep="&mode=")
  url <- paste(url,origins, sep="&origins=")
  url <- paste(url,destinations, sep="&destinations=")
  url.key <- paste(url,apikey, sep="&key=")
  result <- fromJSON(file=url.key)
  dist <- result$rows[[1]]$elements[[1]]$distance$value #meters
  time <- result$rows[[1]]$elements[[1]]$duration$value #seconds
  disttime<- cbind(dist,time) 
  return(disttime)
}

### Distance Matrix Example
locations <- read.csv("locations.csv")
locations[] <- lapply(locations,as.character)

origin <- locations$Location[1]
destinations <- locations$Location[2:15]

travelinfo <- NULL
for(i in 1:length(destinations)){
  Sys.sleep(.25)
  destination <- destinations[i]
  inf <- distmat(origin, destination, apikey)
  tinf <- cbind(origin, destination,inf)
  travelinfo <- rbind(travelinfo, tinf)
}

print(travelinfo)

