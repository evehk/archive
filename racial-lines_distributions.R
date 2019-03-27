# - - - - - - - -  - -
# RL DISTRIBUTIONS
# Author: Eve Kraicer
# - - - - - - - -  - -

# setwd('~/Desktop/rf')

scripts  <- read.csv('scripts_metadata.csv')
acts  <- read.csv('actors_metadata.csv')

# helper function
num_func  <- function(x){as.numeric(as.character(x))}


# for combining east asian and south asian 
a_sub  <- acts[acts$CHARACTER_RACE == 's' | acts$CHARACTER_RACE == 'a',]
chars  <- nrow(a_sub)
words  <- sum(a_sub$EXTRACTED_WORDS)
demo.df <- data.frame(cbind('group'='a', chars, words))

# for the other races, append to demo.df
groups_five  <- c('w','b','l','i','n')
for (i in 1:length(groups_five)){
  group  <- groups_five[i]
  sub  <- acts[acts$CHARACTER_RACE == group,]
  chars <- nrow(sub)
  words  <- sum(sub$EXTRACTED_WORDS)
  temp.df  <- data.frame(cbind(group,chars,'words'=words))
  demo.df  <- data.frame(rbind(demo.df, temp.df))
}

demo.df$chars  <- num_func(demo.df$chars)
demo.df$words  <- num_func(demo.df$word)
demo.df$perc_chars  <- (demo.df$chars/nrow(acts))*100
demo.df$perc_words <- (demo.df$words/sum(demo.df$words))*100

# add column for census numbers
# pulled from US census bureau statistics from 2000 
demo.df$perc_pop  <- c(3.6,69.1,12.1,12.5,8,2)



