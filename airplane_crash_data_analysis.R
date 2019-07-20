library(tidyverse)
library(tidytext)
library(tidyr)
source("airplane_crash_functions.R")

file = "airplaneCrashData.csv"
if(!TRUE %in% (list.files() == file)) { # Checks if file exists in local directory.
  source("airplane_crash_web_scrape.R")
}

airplane.crash <- read.csv(file,header=T,stringsAsFactors=FALSE)



# Determines total number of people on the plane.
aboard <- gsub("\\D+"," ",airplane.crash$aboard) # extract 1st integer to get absolute counts
aboard.split <- strsplit(aboard," ")
airplane.crash$aboard <- sapply(aboard.split, "[[", 1) # extract 1st entry of sublist

# Determines fatalities onboard the plane.
fatalities <- gsub("\\D+"," ",airplane.crash$fatalities) # extract 1st integer to get absolute counts
fatalities.split <- strsplit(fatalities," ")
airplane.crash$fatalities <- sapply(fatalities.split, "[[", 1) # extract 1st entry of sublist

is.na(airplane.crash) <- airplane.crash == "?"

# Process the route feature.
route.split <- strsplit(airplane.crash$route," - ")
index.shift<- sapply(route.split,length) # find indices of each sublist when unlisted
route.split <- unlist(route.split) # flatten for cumsum algorithm to work
names(index.shift) <- c()

# count NA entries
sum(is.na(airplane.crash$location)/nrow(airplane.crash)) # less than 0.07% of locations are NA


# Determines if this is an international or domestic flight if applicable.
airplane.crash$international <- destination == origin 

# Determines the country of the crash.
location <- ExtractLocation(airplane.crash$location)
location <- as.character(location)
location[location %in% state.name] <- "United States of America"  # noticed alot of US states
location[location == "USSR"] <- "Russia" # 
airplane.crash$location <- location

# Determines whether the plane is civil or military.
uses <- unlist(lapply(airplane.crash$operator,ExtractAirplaneUses))
airplane.crash$uses <- uses

# Determines whether the plane crash was known or unknown.
known <- unlist(lapply(airplane.crash$summary,ExtractCrashCauseKnown))
airplane.crash$known <- known

rowSums(is.na(airplane.crash))

# Convert character to numeric
airplane.crash$fatalities <- as.numeric(airplane.crash$fatalities)

airplane.crash$aboard <- as.numeric(airplane.crash$aboard)
airplane.crash$ground <- as.numeric(airplane.crash$ground)
airp



# Processing date into month and year.
airplane.crash$date <- as.Date(airplane.crash$date, "%B %d, %Y")
airplane.crash$month <- format(as.Date(airplane.crash$date), "%m")
airplane.crash$year <- format(as.Date(airplane.crash$date), "%Y")



# 24H time of the crash. 
airplane.crash$time <- as.numeric(airplane.crash$time)

airplane.crash.cleaned <- airplane.crash %>% select(date, month, year, location, operator, ACtype, fatalities, aboard, uses, known, summary)
airplane.na.count <- colSums(is.na(airplane.crash.cleaned))
nrow(airplane.crash.cleaned)

airplane.crash.cleaned <- na.omit(airplane.crash.cleaned) # remove missing data
nrow(airplane.crash.cleaned)

#4934 final rows from 5002, lose 1.355 of data 
airplane.crash.cleaned <- airplane.crash.cleaned %>% mutate(deadProp = fatalities/aboard)

airplane.crash.cleaned %>% group_by(uses) %>% count()
airplane.crash.cleaned %>% filter(uses=="Civil") %>% group_by(location) %>% count() %>% arrange(desc(n)) # when controlled for civil planes, russia has the most airplane crashes
airplane.crash.cleaned %>% filter (uses=="Military") %>% group_by(location) %>% count() %>% arrange(desc(n))

#text mining


self_defined_stop_words <-tibble(word=c("crashed","cargo","en"))

tokens <- airplane.crash.cleaned %>% unnest_tokens(word, summary) %>% anti_join(stop_words) %>% anti_join(self_defined_stop_words)
unigram_count <- tokens %>%
count(word, sort = TRUE) # single words are not as meaningful, we need context

bigrams <- airplane.crash.cleaned %>% unnest_tokens(bigram, summary, token = "ngrams", n = 2)
bigrams_separated <- bigrams  %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% self_defined_stop_words$word) %>%
  filter(!word2 %in% self_defined_stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)


bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
  
top_bigrams <- bigrams_united %>% count(bigram, sort=TRUE) %>% top_n(15) # top 10 bigrams of reason of crash

# plot bigrams
top_bigrams %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
