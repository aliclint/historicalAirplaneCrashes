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

airplaneCrashData[] <- lapply(airplaneCrashData, as.character) # remove factor levels
colnames(airplaneCrashData) <- c("date","time","location","operator","flight","route","ACtype","registration","cnln","aboard","fatalities","ground","summary")
rm(list=ls())
write.csv(airplaneCrashData,col.names=TRUE,file="airplaneCrashData.csv",na="NA")





