source("airplaneCrashData.R")
file = "airplaneCrashData.csv"
airplaneCrashData <- read.csv(file,header=T,stringsAsFactors=FALSE)



aboard <- gsub("\\D+"," ",airplaneCrashData$aboard) # extract 1st integer to get absolute counts
aboard_split <- strsplit(aboard," ")
airplaneCrashData$aboard <- sapply(aboard_split, "[[", 1) # extract 1st entry of sublist

fatalities <- gsub("\\D+"," ",airplaneCrashData$fatalities) # extract 1st integer to get absolute counts
fatalities_split <- strsplit(fatalities," ")
airplaneCrashData$fatalities <- sapply(fatalities_split, "[[", 1) # extract 1st entry of sublist

# extract countries

library(maps) #library to string match with countries 
##https://stackoverflow.com/questions/47999506/matching-an-extracting-country-name-from-character-string-in-r

extract_country <- function(s) {
  data(world.cities)
  raw <- s
  ###Removing punctuation
  raw <- gsub("[[:punct:]\n]","",raw)
  # Split data at word boundaries
  raw2 <- strsplit(raw, " ")
  # Match on country in world.countries
  CountryList_raw <- (lapply(raw2, function(x)x[which(toupper(x) %in% toupper(world.cities$country.etc))]))
  
  extracted_country <- do.call(rbind, lapply(CountryList_raw, as.data.frame))
  extracted_country <- unlist(extracted_country)
}

# routes
route_split <- strsplit(airplaneCrashData$route," - ")
index_shift<- sapply(origin_split,length) # find indices of each sublist when unlisted

route_split <- unlist(route_split) # flatten for cumsum to work
names(index_shift) <- c()
# extract origin
index_origin <- cumsum(index_shift) - (index_shift - 1)
origin_raw <- route_split[index_origin]

origin <- as.character(extract_country(origin_raw))
names(origin) <- c()


# extract destination
index_destination <- cumsum(index_shift)
destination_raw <- route_split[index_destination]

destination <- as.character(extract_country(destination_raw))
names(destination) <- c()

internationalFlight <- destination == location # check if its an international or domestic flight

location <- as.character(extract_country(airplaneCrashData$location))
names(location) <- c()
length(unique(location)) #155 countries which are less than 195 countries





airplane_uses <- function(x) {
  if(grepl("Military",x,ignore.case=TRUE)) {
    return("Military")
  } else {
    return("Civil")
  }
  return(x)
}

uses <- unlist(lapply(airplaneCrashData$operator,airplane_uses))
airplaneCrashData$uses <- uses

airplaneCrashData$date <- as.Date(airplaneCrashData$date, "%B %d, %Y")
airplaneCrashData$time <- as.numeric(airplaneCrashData$time)

is.na(airplaneCrashData) <- airplaneCrashData == "?"