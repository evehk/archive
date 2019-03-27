# - - - - - - - -  - -
# Author: Eve Kraicer
# RL INTERSECTIONALITY
# - - - - - - - -  - -

scripts  <- read.csv('scripts_metadata.csv')
acts  <- read.csv('actors_metadata.csv')

# helper function
num_func  <- function(x){as.numeric(as.character(x))}


# white v non-white characters by genre
genres.df  <- NULL
for (i in 1:nlevels(scripts$GENRE)){
  genre  <- levels(scripts$GENRE)[i]
  ids  <- scripts$SCRIPT_ID[scripts$GENRE == genre]
  sub  <- acts[acts$SCRIPT_ID %in% ids,]
  w  <- length(which(sub$CHARACTER_RACE == 'w'))
  nw  <- length(which(sub$CHARACTER_RACE != 'w'))
  all  <- w+nw
  perc_nw  <- (nw/all)*100
  temp.df  <- data.frame(cbind(genre,w,nw,all, perc_nw))
  genres.df  <- data.frame(rbind(genres.df, temp.df))
}

genres.df$perc_nw  <- num_func(genres.df$perc_nw)


### intersectionality 
int.df  <- NULL
groups_int  <- c('b','l','a','n','s')
for (i in 1:length(groups_int)){
  group  <- groups_int[i]
  sub  <- acts[acts$ACTOR_RACE == group,]
  sub_m  <- sub[sub$GENDER == 'm',]
  sub_f  <- sub[sub$GENDER == 'f',]
  pw_m  <- length(which(sub_m$PART_WHITE == 'y'))
  m <- length(which(sub_m$PART_WHITE == 'n'))
  pw_f  <- length(which(sub_f$PART_WHITE == 'y'))
  f  <- length(which(sub_f$PART_WHITE == 'n'))
  temp.df  <- data.frame(cbind(group, pw_f, f, pw_m, m))
  int.df  <- data.frame(rbind(temp.df, int.df))
}

int.df[2:5] <- lapply(int.df[2:5], function(x) num_func(x))
all  <- data.frame(rbind(colSums(int.df[,2:5])))
all$group  <- 'all'
int.df  <- data.frame(rbind(int.df, all))


fish_func <- function(x){
  a  <- int.df[x,2]
  b  <- int.df[x,3]
  c  <- int.df[x,4]
  d  <- int.df[x,5]
  df  <- data.frame(c(a,b), c(c,d))
  result  <- fisher.test(df)
  return(result)
}

int_e <- fish_func(3) # east asian
int_l  <- fish_func(4) # latinx
int_b <- fish_func(5) # black 
int <- fish_func(6) # all 
