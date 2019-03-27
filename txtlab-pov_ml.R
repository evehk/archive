
# - - - - - - - - - -
# ML FOR POINT OF VIEW
# Author: Eve Kraicer
# - - - - - - - - - -

library("kernlab")
library("caret")
library("tm")
library("SnowballC")
library("splitstackshape")
library("e1071")

# set up data into corpus
# read in dissertation data from csv
setwd("~/Desktop/pov")

### prepare df
pov_train <- data.frame(read.csv('train.csv'), row.names=NULL)
rownames(pov_train)<-pov_train$filenames
pov_train <- pov_train[,3:8]
# only complete cases
pov_train <- pov_train[complete.cases(pov_train),]

# remove very high variance 
summary(pov_train$variance)
pov2_train <- pov_train #[which(pov_train$variance < 1),]

# remove variance
pov3_train <- pov2_train[,1:5] 


# PREDICT ON CONTEMPORARY 
# read in data 
pov_contemp <- data.frame(read.csv('pov_contemp.csv'), row.names=NULL)
# standardize names 
func <- function(x){paste(x, '.csv', sep='')}
rownames(pov_contemp) <- sapply(pov_contemp$filenames, func)
pov_contemp <- pov_contemp[,3:7]

# variance 
summary(pov_contemp$variance)
pov2 <- pov_contemp[,1:4]

# remove training data 
pov3 <- pov2[which(!(rownames(pov2) %in% rownames(train))),]

### CLASSIFICATION
# svm classification
train <- pov3_train
corpus <- pov3
model <- ksvm(labels ~ ., data=train, kernel="rbfdot", prob.model = TRUE)
predictions <- predict(model, corpus)
pred_prob  <-  predict(model, corpus, type = 'prob')

### RESULTS
# append predictions to pov3 
pov3$predictions <- predictions
pred_prob.df <- data.frame(pred_prob, row.names = rownames(pov3))

# add labels
pov_label <- vector()
for (i in 1:nrow(pred_prob.df)){
  if (pred_prob.df$FP[i] > .4 & pred_prob.df$FP[i] < .6){pov_label <- append(pov_label, "amb")}
  else if (pred_prob.df$FP[i] > pred_prob.df$TP[i]) {
    if (pred_prob.df$FP[i] > .9) {pov_label <- append(pov_label, "high_FP")} 
    else {pov_label <- append(pov_label, "low_FP")}}
  else {
    if (pred_prob.df$TP[i] > .9) {pov_label <- append(pov_label, "high_TP")} 
    else {pov_label <- append(pov_label, "low_TP")}}
}
pred_prob.df$label <- pov_label


# match files for gg 
setwd('~/Dropbox/eve/data')
gg  <- list.files('edges_ALL')
gg_match  <- read.csv('for_POV.csv')

# master files 
setwd('~/Dropbox/NovelEnglish_Contemporary')
master  <- read.csv('master2018_final.csv')
# so master$gg_match will match edges 
master$gg_match <- gg_match$files 
# master$pov_match will match povf
func <- function(x){paste(x, '.csv', sep='')}
master$pov_match <- sapply(master$ID, func)

# combine files to get all works labelled (predictions and trained data)
# for 3rd person
tp_pred  <- pred_prob.df[pred_prob.df$label == "high_TP"|pred_prob.df$label == "low_TP",]
tp_train  <- train[train$labels == 'TP',]
tp_files  <- c(rownames(tp_pred), paste(rownames(tp_train), '.csv', sep=''))

# for 1st person
fp_pred  <- pred_prob.df[pred_prob.df$label == "high_FP"|pred_prob.df$label == "low_FP",]
fp_train  <- train[train$labels == 'FP',]
fp_files  <- c(rownames(fp_pred),  paste(rownames(fp_train), '.csv', sep=''))

amb_files  <- rownames(pred_prob.df[pred_prob.df$label == 'amb',])

# label master
pov_v  <- vector()
for (i in 1:nrow(master)){
  if (master$pov_match[i] %in% tp_files) {pov_v[i]  <- 'TP'}
  else if (master$pov_match[i] %in% fp_files) {pov_v[i]  <- 'FP'}
  else if (master$pov_match[i] %in% amb_files){pov_v[i]  <- 'amb'}
  else {pov_v[i] <- 'remove'}
}

master$pov  <- pov_v
master_tp  <- master[master$pov == 'TP',]
master_fp  <- master[master$pov == 'FP',]

# take top chars data from gg paper 
top_chars_pov  <- top_chars
gg_pov_v  <- vector()
for (i in 1:nrow(top_chars_pov)){
  if (top_chars_pov$work[i] %in% master_fp$gg_match){gg_pov_v[i]  <- 'FP'}
  else if (top_chars_pov$work[i] %in% master_tp$gg_match){gg_pov_v[i]  <- 'TP'}
  else (gg_pov_v[i] <- 'amb')
}

table_4b <- NULL
# pov_mc_ag  <- NULL
top_chars_pov$pov  <- gg_pov_v
for (i in 1:nlevels(top_chars_pov$code)){
  sub <- top_chars_pov[which(as.character(top_chars_pov$code) == levels(top_chars_pov$code)[i]),]
  # sub  <- sub[sub$code != 'ROM',]
  group <- levels(top_chars_pov$code)[i]
  sub2  <- sub[sub$pov == 'TP',]
  titles <- nrow(sub2)
  results  <- NULL
  for (i in 1:10000){
    # shuffle second char column
    sample <- boot_func(sub2$adj_first, sub2)
    f  <- length(which(sample == 'F'| sample == 'FM'))
    f_perc  <- length(which(sample == 'F'| sample == 'FM'))/length(sample)
    temp  <- data.frame(cbind(f,f_perc))
    results  <- data.frame(rbind(results,temp))
  }
  mean_f  <- mean(results$f)
  mean_f_perc <- mean(results$f_perc)
  temp2 <- cbind(group, mean_f, mean_f_perc, titles)
  table_4b  <- data.frame(rbind(table_4b, temp2))
  # pov_mc_ag <- data.frame(rbind(pov_mc_ag, temp2))
}

# significance AG 
chsq_mc_pov <- t(data.frame(num_func(pov_mc_ag$mean_f[1:2]), num_func(pov_mc_ag$titles[1:2])))
chisq.test(chsq_mc_pov)
fisher.test(chsq_mc_pov)

## binomial test 
test <- rbinom(10000, size = 664, prob = 0.5)
quantile(test, c(.025, 0.975))
sum(num_func(table_4b$mean_f))/sum(num_func(table_4b$titles))
