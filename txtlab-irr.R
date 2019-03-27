# - - - - - - - - -
# GENERALIZATION
# Author: Eve Kraicer
# - - - - - - - - -

# read in filename for each tagger and create data frames
# initials indicate annotator 
orig_ann1 <- data.frame(read.csv("trial5_sentences_AP.csv", row.names = NULL)) # predictor + coder
orig_ann2 <- data.frame(read.csv("trial5_sentences_VS.csv", row.names = NULL)) # coder
orig_ann3 <- data.frame(read.csv("trial5_sentences_EE.csv", row.names = NULL)) # coder

# neaten tables function (only data)
neaten <- function(x){
  df <- data.frame(x[1:120,1:4], row.names = NULL)
  df[is.na(df)] <- 0
  return(df)
}

# neaten data frames 
ann1 <- neaten(orig_ann1)
ann2 <- neaten(orig_ann2)
ann3 <- neaten(orig_ann3)

# helper function 
irr_calc <- function(x){
  a <- length(which(x == 2))
  n <- length(which(x == 0))
  d <- length(which(x == 1))
  pos <- (2*a)/(2*a+d)
  neg <- (2*n)/(2*n+d)
  irr <- c(pos,neg)
  return(irr)
}

### PART 2: (A)
# compare overall IRR for each pair 
compare <- function(x,y,z){
  df <- x[,2:4]+y[,2:4]
  pair <- z
  score <- c(irr_calc(df))
  results <- data.frame(cbind(pair, 'pos'=score[1], 'neg'=score[2]))
  return(results)
}

# get results of overall comparison 
all_12 <- compare(ann1, ann2, '1-2')
all_13 <- compare(ann1, ann3, '1-3')
all_23 <- compare(ann2, ann3, '2-3')

#combine and print csv
setwd(wd)
df1_v5 <- data.frame(rbind(all_12,all_13,all_23))

### PART 2: (B)
# compare IRR by type 

# also combine vectors of gen and ex for combined nonneutral analysis
ann1$nn <- ann1$generalization + ann1$exemplification 
ann2$nn <- ann2$generalization + ann2$exemplification
ann3$nn <- ann3$generalization + ann3$exemplification

# compare by type function 
compare_type <- function(x,y,z){
  df <- x[,2:5]+y[,2:5]
  pair <- z
  score_gen <- irr_calc(df$generalization)
  score_ex <- irr_calc(df$exemplification)
  score_nn <- irr_calc(df$nn)
  # combine all 
  results_gen <- data.frame(cbind(pair, 'type'='gen','pos'=score_gen[1], 'neg'=score_gen[2]))
  results_ex <- data.frame(cbind(pair, 'type'='ex', 'pos'=score_ex[1], 'neg'=score_ex[2]))
  results_nn <- data.frame(cbind(pair, 'type'='nn', 'pos'=score_nn[1], 'neg'=score_nn[2]))
  results <- data.frame(rbind(results_gen, results_ex, results_nn))
  return(results)
}

# calculate by type and by pair 
type_12 <- compare_type(ann1, ann2, '1-2')
type_13 <- compare_type(ann1, ann3, '1-2')
type_23 <- compare_type(ann2, ann3, '2-3')

# bind together into large df for printing 
df2_v5 <- data.frame(rbind(type_12,type_13,type_23))


### PART 2: (C)
# measuring where agreement is and where majority is

# FINDING AGREEMENT
# method 1 - looking at agreement of gen, neutral and exemplification
agree_1.df <- ann1[,2:4]+ann2[,2:4]+ann3[,2:4]
agree_1 <- vector()
for (i in 1:nrow(agree_1.df)){
  if (sum(agree_1.df[i,]) != 3) {agree_1 <- append(agree_1, 'amb')}
  else if ((length(which(agree_1.df[i,] == 3))) == 1){agree_1[i] <- "agreement"}
  else if ((length(which(agree_1.df[i,] == 2))) == 1) {agree_1[i] <-"two_agree"}
  else {agree_1[i] <- "disagreement"}
}

# method 2 - looking at agreement of neutral v nonneutral 
agree_2.df <- cbind(ann1$neutral + ann2$neutral + ann3$neutral, ann1$nn + ann2$nn + ann3$nn)
agree_2 <- vector()
for (i in 1:nrow(agree_2.df)){
  if (sum(agree_2.df[i,]) != 3) {agree_2[i] <- 'amb'}
  else if ((length(which(agree_2.df[i,] == 3))) == 1){agree_2[i] <- 'agreement'}
  else if ((length(which(agree_2.df[i,] == 2))) == 1) {agree_2[i] <- 'two_agree'}
  else {agree_2[i] <- 'disagreement'}
}

# FINDING MAJORITY 
# where are 2 or more people annotating
# bind together annotation columns 
agree_comb.df <- cbind(ann1[,2:5]+ann2[,2:5]+ann3[,2:5], agree_1, agree_2)

# method 1, gen and exemp as seperate
maj_1 <- vector()
for (i in 1:nrow(agree_comb.df)){
  if (agree_comb.df$agree_1[i] == 'amb' | agree_comb.df$agree_1[i] == 'disagreement') {maj_1[i] <- 'no_maj'}
  else if (agree_comb.df$generalization[i] > 1) {maj_1[i] <- 'generalization'}
  else if (agree_comb.df$exemplification[i] > 1) {maj_1[i] <- 'exemplification'}
  else if (agree_comb.df$neutral[i] > 1) {maj_1[i] <- 'neutral'}
}

# method 2, gen and exemp as collapsed 'nn'
maj_2 <- vector()
for (i in 1:nrow(agree_comb.df)){
  if (agree_comb.df$agree_2[i] == 'amb' | agree_comb.df$agree_2[i] == 'disagreement') {maj_2[i] <- 'no_maj'}
  else if (agree_comb.df$nn[i] > 1) {maj_2[i] <- 'nn'}
  else if (agree_comb.df$neutral[i] > 1) {maj_2[i] <- 'neutral'}
}


# find prediction using generalization types
prediction_1 <- vector()
for (i in 1:nrow(ann1)){
  if (sum(ann1[i,2:5]) == 0){prediction_1[i] <- 'amb'} else {
    # take the ***first instance*** of a column name where the annotation prediction is '1'
    # first to avoid nn labels 
    prediction_1[i] <- colnames(ann1[which(ann1[i,] == 1)],)[1]
  } 
}

# find using nn v neutral 
prediction_2 <- vector()
for (i in 1:nrow(ann1)){
  if (sum(ann1[i,2:5]) == 0){prediction_2[i] <- 'amb'} else {
    if (length(which(ann1[i,] == 1)) > 1) {prediction_2[i] <- 'nn'}
    else (prediction_2[i] <- colnames(ann1[which(ann1[i,] == 1)],))
  }
}

# append to df 
agree_comb.df$maj_1 <- maj_1
agree_comb.df$prediction_1 <- prediction_1
agree_comb.df$maj_2 <- maj_2
agree_comb.df$prediction_2 <- prediction_2

# append matches to determine when prediction matches majority
agree_comb.df$in_maj1 <- (agree_comb.df$prediction_1 == agree_comb.df$maj_1)
agree_comb.df$in_maj2 <- (agree_comb.df$prediction_2 == agree_comb.df$maj_2)

# master data.frame
df3_v5 <- data.frame(cbind('sentences'=ann1$sentences, agree_comb.df), row.names = NULL)

### PART 3: calculating sensitivity and specificity on data frame 

# build se/sp table using agree_comb.df results for tri and binary 
# tri annotations - letters start in the top left corner and read like the table is a book 
a <- length(which(agree_comb.df$maj_1 == 'neutral' & agree_comb.df$in_maj1 == 'TRUE'))
b <- length(which(agree_comb.df$maj_1 == 'exemplification' & agree_comb.df$prediction_1 == 'neutral'))
c <- length(which(agree_comb.df$maj_1 == 'generalization' & agree_comb.df$prediction_1 == 'neutral'))
d <- length(which(agree_comb.df$maj_1 == 'neutral' & agree_comb.df$prediction_1 == 'exemplification'))
e <- length(which(agree_comb.df$maj_1 == 'exemplification' & agree_comb.df$in_maj1 == 'TRUE'))
f <- length(which(agree_comb.df$maj_1 == 'generalization' & agree_comb.df$prediction_1 == 'exemplification'))
g <- length(which(agree_comb.df$maj_1 == 'neutral' & agree_comb.df$prediction_1 == 'generalzation'))
h <- length(which(agree_comb.df$maj_1 == 'exemplification' & agree_comb.df$prediction_1 == 'generalization'))
i <- length(which(agree_comb.df$maj_1 == 'generalization' & agree_comb.df$in_maj1 == 'TRUE'))

results1 <- data.frame('neutral_maj'=c(a,d,g), 'exemp_maj'=c(b,e,h), 'gen_maj'=c(c,f,i), 
                          row.names = c('neutral_pred', 'exemp_pred', 'gen_pred'))
# binary annotations 
a2 <- length(which(agree_comb.df$maj_2 == 'neutral' & agree_comb.df$in_maj2 == 'TRUE'))
b2 <- length(which(agree_comb.df$maj_2 == 'nn' & agree_comb.df$prediction_2 == 'neutral'))
c2 <- length(which(agree_comb.df$maj_2 == 'neutral' & agree_comb.df$prediction_2 == 'nn'))
d2 <- length(which(agree_comb.df$maj_2 == 'nn' & agree_comb.df$in_maj2 == 'TRUE'))

results2 <- data.frame('neutral_maj'=c(a2,c2), 'nn_maj'=c(b2,d2), row.names = c('neutral_pred', 'nn_pred'))

tp/tp+fn

tn/tn+fp

### calc se/sp 
#tri
# can't calculate for exemp bc tp = 0 
se_n1 <- a/a+(d+g)
sp_n1 <- (e+f+h+i)/((e+f+h+i)+(b+c))

se_g1 <- i/i+(d+f)
sp_g1 <- (a+b+d+e)/((a+b+d+e)+(g+h))

#bi 
se_n2 <- a2/a2+c2
sp_n2 <- d2/d2+b2

se_nn2 <-d2/d2+b2
sp_nn2 <- a2/a2+c2
