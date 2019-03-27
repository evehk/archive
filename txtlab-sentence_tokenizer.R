
# - - - - - - - - - -
# SENTENCE TOKENIZER
# Author: Eve Kraicer
# - - - - - - - - - -

# install libraries
library(tokenizers)
library(stringi)

# set wd
wd_in <- ('~/Desktop/generalization/generalization_trials/articles')
wd_out <- ('~/Desktop/generalization/generalization_trials/outputs')

# function for cleaning and tokenizing 
get_sentences <- function(x){
  # empty df 
  sent.df<-NULL
  for (i in 1:length(files)){
    # setwd 
    setwd(wd_in)
    files <- list.files(x)
    setwd(paste(getwd(),(x),sep = "/"))
    # list of files to be converted
    text <- scan(files[i], what = "character", quote = "")
    # for beginning 1500 words 
    # set to 1530 to account for subtraction of the last row (to make sure all sentences are complete)
    text_b <- head(text,1530)
    text_str_b <- paste(text.b, collapse = ' ')
    sent_b <- data.frame("sentences"=tokenize_sentences(text_str_b))
    # for last 1000 words
    text_e <- tail(text,1030)
    text_str_e <- paste(text.e, collapse = ' ')
    sent_e <- data.frame(tokenize_sentences(text_str_e))
    # clean
    # remove first row to avoid cut offs 
    sent_b<-data.frame(sent_b[-nrow(sent_b),])
    sent_e <- data.frame(sent_e[-1,])
    # append label 
    sent_b$section <- "b"
    sent_e$section <- "e"
    # match colnames & combine
    colnames(sent_b)<-c("sentences", "section")
    colnames(sent_e) <- colnames(sent_b)
    sent_all <- data.frame(rbind(sent_b, sent_e))
    # remove numbers in specific instances
    sent_all$sentences <- gsub('\\d\\d\\d\\d', 'YEAR', sent_all$sentences)
    sent_all$sentences <- gsub('\\d', '', sentences.all$sentences)
    sent_all$sentences <- gsub('\\(\\)', '', sent_all$sentences)
    # control for length
    sent_all$length <- sapply(gregexpr("\\W+", sent_all$sentences), length)
    sent_sub <- sent_all[sent_all$length > 5 & sent_all$length < 100,]
    sent_sub$sent.no<-(nrow(sent_sub)*3) 
    # add empty columns for generalization types 
    sentences<-cbind(data.frame("sentences"=sentences.sub$sentences, "neutral"=" ", "generalization"=" ",
                                "exemplification"=" ", "attribution"= " ", "ambiguous"=" ", "section"=sentences.sub$section, "sent no"=sentences.sub$sent.no))
    sent_temp <- sentences[sample(nrow(sentences),30),]
    sent_temp$filename <- files[i]
    # combine to store 
    sent.df<-data.frame(rbind(sent.df, sent_temp))
  }
  # change directory to output folder
  # write file to folder 
  # use UTF-8 to avoid weird characters
  setwd(wd_out)
  setwd(paste(getwd(),(x),sep = "/"))
  # shuffle order
  sent_final<-data.frame(sentences.df[sample(nrow(sentences.df)),])
  write.csv(sent_final, "sentences_corpus.csv", row.names = F, fileEncoding = "utf-8")
}

# get_sentences('trial_1')
# get_sentences('trial_2')
# get_sentences('trial_3')
# get_sentences('trial_4')
# get_sentences('trial_5')

get_sentences('corpus')
