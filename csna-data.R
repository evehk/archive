
# - - - - - - - -  - -
# CSNA DATA
# Author: Eve Kraicer
# - - - - - - - -  - -

library(stringr)

# setwd('~/Desktop/csna')

# get data
num_func <- function(x){as.numeric(as.character(x))}
data_all <- data.frame(read.csv('data_all.csv'))

# preprocessing
# get rid of spaces and other errors
age <- gsub('\\s', '', age)
nl_ccount <- gsub('Less than 10 timesLess than 10 times', 'Less than 10', nl_ccount)

# normalize institution names 
educ_inst_raw <- tolower(educ_inst_raw)
educ_inst_raw <- gsub('[[:punct:]]', '',educ_inst_raw)
educ_inst <- vector()
for (i in 1:length(educ_inst_raw)){
  if (grepl('mcgill',educ_inst_raw[i])){
    educ_inst[i] <- 'mcgill'
  } else if (grepl('concordia',educ_inst_raw[i])){
    educ_inst[i] <- 'concordia'
  } else if (grepl('queens',educ_inst_raw[i])){
    educ_inst[i] <- 'queens'
  } else if (grepl('toronto',educ_inst_raw[i])){
    educ_inst[i] <- 'u of t'
  } else if (grepl("('unsw'|'south wales')",educ_inst_raw[i])){
    educ_inst[i] <- 'unsw'
  } else if (grepl('ottawa',educ_inst_raw[i])){
    educ_inst[i] <- 'ottawa'
  } else if (grepl('ubc',educ_inst_raw[i])){
    educ_inst[i] <- 'ubc'
  } else {educ_inst[i] <- educ_inst_raw[i]}
}
# pull only grad year
educ_grad <- vector()
for (i in 1:length(educ_grad_raw)){
  if (grepl('\\d\\d\\d\\d',educ_grad_raw[i])){
    educ_grad[i] <- regmatches(educ_grad_raw[i],regexpr('\\d\\d\\d\\d',educ_grad_raw[i]))
  }
  else {educ_grad[i] <- educ_grad_raw[i]}
}
# combine multiple answers 
to_key <- function(key, v){
  for (i in 1:length(key)){
    v <- gsub(key[i], i, v)
  }
  new <- v
  return(new)
}

nl_calls_raw <- gsub('_+', '_', nl_calls_raw)
nl_calls_key <- as.character(t(data_all[1,48:57]))
nl_calls <- to_key(nl_calls_key, nl_calls_raw)

service_use_raw <- gsub('_+', '_', service_use_raw)
service_use_key <- as.character(t(data_all[1,37:41]))
service_use_key[4] <- "Hours \\(e.g.: not open in the evening/night, does not match with you schedule, etc.\\)"
service_use <- to_key(service_use_key, service_use_raw)

# who is a volunteer
nl_vol <- vector()
for (i in 1:length(nl_v)){
  if (nl_v[i] == '') {nl_vol[i] <- 'n'} else {nl_vol[i] <- 'y'}
}
                                               
## special cases
nl_info_free <- as.character(data_all[3:nrow(data_all),34])
service_use_free <- as.character(data_all[3:nrow(data_all),41])
nl_calls_free <- as.character(data_all[3:nrow(data_all),58])
nl_rec <- paste(data_all[3:nrow(data_all),64])
etc <- as.character(data_all[3:nrow(data_all), 123])

# word use 
nl_disc_key <- as.character(t(data_all[1,65:74]))
nl_disc.df <- NULL
for (j in 65:74){
  sub <- data_all[,j]
  temp2 <- NULL
  for (i in 1:length(nl_disc_key)){
    word <- nl_disc_key[i]
    count <- length(which(sub == as.character(word)))
    temp <- data.frame(cbind(word,count))
    temp2 <- rbind(temp, temp2)
  }
  nl_disc.df <- rbind(temp2, nl_disc.df)
  nl_disc.df <- nl_disc.df[nl_disc.df$count != 0,]
}

calls_sum <- NULL
for (i in 1:10){
  a <- i
  r <- length(which(grepl(i, nl_calls)))
  temp <- cbind.data.frame(a,r)
  calls_sum <- rbind.data.frame(calls_sum, temp)
}
calls_sum$use <- nl_calls_key
calls_sum$freq <- calls_sum$r

calls_sum_m <- NULL
calls_sum_w <- NULL
for (i in 1:10){
  sub <- data_clean[data_clean$gender == 'Female',]
  use <- i
  freq <- length(which(grepl(i, sub$nl_calls)))
  temp <- cbind.data.frame(use,freq)
  calls_sum_w <- rbind.data.frame(calls_sum_w, temp)
}
calls_sum_w$use <- nl_calls_key
calls_sum_m$use <- nl_calls_key

notes <- data.frame(cbind(etc, nl_vol))
notes_v <- notes[notes$nl_vol == 'n',]

