ExtractAirplaneUses <- function(x) {
  if (grepl("Military",x,ignore.case=TRUE)) {
    return("Military")
  } else {
    return("Civil")
  }
  return(x)
}


ExtractCrashCauseKnown <- function(x) {
  if (grepl("unknown",x,ignore.case=TRUE)) {
    return("Known")
  } else {
    return("Unknown")
  }
  return(x)
}

library(maps) #library to string match with countries 
##https://stackoverflow.com/questions/47999506/matching-an-extracting-country-name-from-character-string-in-r

ExtractCountry <- function(s) {
  data(world.cities)
  raw <- s
  ###Removing punctuation
  raw <- gsub("[[:punct:]\n]","",raw)
  # Split data at word boundaries
  raw2 <- strsplit(raw, " ")
  # Match on country in world.countries
  
  extracted.country <- do.call(rbind, lapply(CountryList.Raw, as.data.frame))
  extracted.country <- unlist(extracted.country)
}

test <- airplane.crash$location
test <- gsub("[[:punct:]\n]","",test)


ExtractLocation <- function(s) {
  s <- sapply(s, as.character)
  s <- gsub(".*,", "", s)
  # remove white space trailing white space
  s <- str_trim(s, side = "both")
  s <- sapply(s, as.factor)
  return(s)
}