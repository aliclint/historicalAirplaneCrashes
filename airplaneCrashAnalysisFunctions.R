
airplane_uses <- function(x) {
  if (grepl("Military",x,ignore.case=TRUE)) {
    return("Military")
  } else {
    return("Civil")
  }
  return(x)
}


airplane_known <- function(x) {
  if (grepl("unknown",x,ignore.case=TRUE)) {
    return("Known")
  } else {
    return("Unknown")
  }
  return(x)
}

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