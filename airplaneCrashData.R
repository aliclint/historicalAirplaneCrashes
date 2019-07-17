library(stringr)
library(httr)
library(XML)

# empty dataframe
airplaneCrashData <- data.frame(date=character(), 
                                time=character(), 
                                location=character(), 
                                operator=character(), 
                                flight=character(),
                                route=character(),
                                ACtype=character(),
                                registration=character(),
                                cnln=character(),
                                aboard=character(),
                                fatalities=character(),
                                ground=numeric(),
                                summary=character(),
                                stringsAsFactors = FALSE)

page = 1
for (i in 1920:2019) {
  url <- paste("http://www.planecrashinfo.com/",i,"/",i,"-",page,".htm",sep="")
  while(!http_error(url)) { # checks if the page exists
    tempTable <- readHTMLTable(url, header=T, which=1,stringsAsFactors=F) # extracts teh 13 x 2 table
    tempTable <- data.frame(lapply(tempTable, trimws), stringsAsFactors = FALSE)
    names(tempTable) <- NULL
    airplaneCrashData <- rbind(airplaneCrashData,t(tempTable[,2]))
    page <- page + 1
    url <- paste("http://www.planecrashinfo.com/",i,"/",i,"-",page,".htm",sep="")
  }
  page <- 1
  if (i %% 10 == 9) {
    print(paste("Year ", i, " done.",sep=""))    
  }
}

airplaneCrashData[] <- lapply(airplaneCrashData, as.character)
colnames(airplaneCrashData) <- c("date","time","location","operator","flight","route","ACtype","registration","cnln","aboard","fatalities","ground","summary")

is.na(airplaneCrashData) <- airplaneCrashData == "?"

aboard <- gsub("\\D+"," ",airplaneCrashData$aboard) # extract 1st integer to get absolute counts
aboard_split <- strsplit(aboard," ")
airplaneCrashData$aboard <- sapply(aboard_split, "[[", 1) # extract 1st entry of sublist

fatalities <- gsub("\\D+"," ",airplaneCrashData$fatalities) # extract 1st integer to get absolute counts
fatalities_split <- strsplit(fatalities," ")
airplaneCrashData$fatalities <- sapply(fatalities_split, "[[", 1) # extract 1st entry of sublist

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


