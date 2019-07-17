source("airplaneCrashData.R")

airplaneCrashData$date <- as.Date(airplaneCrashData$date, "%B %d, %Y")
airplaneCrashData$time <- as.numeric(airplaneCrashData$time)