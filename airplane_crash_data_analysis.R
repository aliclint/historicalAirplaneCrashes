source("airplane_crash_web_scrape.R")
file = "airplaneCrashData.csv"
airplaneCrashData <- read.csv(file,header=T,stringsAsFactors=FALSE)

source("airplane_crash_functions.R")

airplaneCrashData$date <- as.Date(airplaneCrashData$date, "%B %d, %Y")
airplaneCrashData$time <- as.numeric(airplaneCrashData$time)

aboard <- gsub("\\D+"," ",airplaneCrashData$aboard) # extract 1st integer to get absolute counts
aboard_split <- strsplit(aboard," ")
airplaneCrashData$aboard <- as.numeric(sapply(aboard_split, "[[", 1)) # extract 1st entry of sublist

fatalities <- gsub("\\D+"," ",airplaneCrashData$fatalities) # extract 1st integer to get absolute counts
fatalities_split <- strsplit(fatalities," ")
airplaneCrashData$fatalities <- as.numeric(sapply(fatalities_split, "[[", 1)) # extract 1st entry of sublist

airplaneCrashData$ground <- as.numeric(airplaneCrashData$ground)

# proportion dead
airplaneCrashData$deadProp <- (airplaneCrashData$fatalities + airplaneCrashData$ground) / airplaneCrashData$aboard

# extract countries
# routes
route_split <- strsplit(airplaneCrashData$route," - ")
index_shift<- sapply(route_split,length) # find indices of each sublist when unlisted

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

# determine if airplane is civil or military
uses <- unlist(lapply(airplaneCrashData$operator,airplane_uses))
airplaneCrashData$uses <- uses

# String search whether plane crash was known or unknown
known <- unlist(lapply(airplaneCrashData$summary,airplane_known))

is.na(airplaneCrashData) <- airplaneCrashData == "?"