# - - - - - - - -  - -
# GENDER CLUSTERING
# Author: Eve Kraicer
# - - - - - - - -  - -

# this code runs through three different corpuses (BS, PW, and NYT) and classifies based on features 
# feature sets: 1 = stopwords only, 2 = top words, 3 = bottom words 
# the goal of this project is look at the different ways features influence the gender markers in texts 

### libraries
library("tm")
library("proxy")
library("cluster")
library("ggplot2")

### cleaning and collecting data
# set wd
wd <- "~/Desktop/eve/g_clustering"
setwd(wd)

# metadata
# pull out correct files from master contemporary list 
metadata <- data.frame(read.csv("master2018_final.csv", row.names = NULL))
# only from 3 source folders listed above
metadata.sub <- data.frame(metadata[metadata$SOURCE.FOLDER == "BS" | metadata$SOURCE.FOLDER == "NYT " 
                         | metadata$SOURCE.FOLDER == "PW" | metadata$SOURCE.FOLDER == "ebooks 2016",])
# only where genre is unique (no overlap with other genres)
metadata.sub <- data.frame(metadata.sub[metadata.sub$Genre == "BS" | metadata.sub$Genre == "CT" | metadata.sub$Genre == "PW",])
metadata.m <- metadata.sub[metadata.sub$Author_Gender == "M",]
metadata.w <- metadata.sub[metadata.sub$Author_Gender == "F",]

# files 
# pull out only files from metadata.sub
files <- list.files("NovelEnglish_Contemporary")
files.rem <- files[!(files %in% metadata.sub$ID)]

# make a folder for those files
# file.copy("NovelEnglish_Contemporary", "contemp_sub")
# setwd("~/Desktop/eve/g_clustering") 
# file.remove(files.rem)
# new files for only subset
files.sub <- list.files("contemp_sub")


### making dtms for all 

# read in subsetted corpus
corpus <- VCorpus(DirSource("contemp_sub", encoding = "UTF-8"), readerControl=list(language="English"))

# clean data for all features
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, content_transformer(removePunctuation))
# corpus <- tm_map(corpus, stemDocument, language = "english") 

# clean data for features 1 and 2
# stop words removed 
corpus.swr <- tm_map(corpus, removeWords, c(stopwords("english"), "one", "said")) 
corpus.sw <- corpus


# make dtm for features 1 and 2
corpus.swr.dtm<-DocumentTermMatrix(corpus.swr, control=list(wordLengths=c(1,Inf)))
corpus.swr.matrix<-as.matrix(corpus.swr.dtm, stringsAsFactors=F)

# make dtm for feature 3
corpus.sw.dtm<-DocumentTermMatrix(corpus.sw, control=list(wordLengths=c(1,Inf)))
corpus.sw.matrix<-as.matrix(corpus.sw.dtm, stringsAsFactors=F)

### feature- specific dtms

# feature 1: only stopwords
corpus.1.dtm <- corpus.sw.dtm[,colnames(corpus.sw.dtm) %in% stopwords("en")]
corpus.1.matrix <- as.matrix(corpus.1.dtm, stringsAsFactors=F)

# feature 1: only words appearing in at least 60% of documents 
# remove sparse terms
corpus.2.dtm <- removeSparseTerms(corpus.swr.dtm, 0.4) 
corpus.2.matrix <- as.matrix(corpus.2.dtm, stringsAsFactors=F)

# feature 3: only words appearing below common words and stop, but above 10 percent of documents
# words that appear in a minimum of 10 percent 
tail.dtm <-  removeSparseTerms(corpus.swr.dtm, 0.9)  
tail.dtm <- tail.dtm[,(!(colnames(tail.dtm) %in% colnames(corpus.2.matrix)))]
corpus.3.matrix <- as.matrix(tail.dtm, stringsAsFactors=F)

# feature 4: only 3rd person pronouns - he, him, his, she, her, hers, they, them, theirs
pronouns <- c("he", "him", "his", "she", "her", "hers")
corpus.4.dtm <- corpus.sw.dtm[,colnames(corpus.sw.dtm) %in% pronouns]
corpus.4.matrix <- as.matrix(corpus.4.dtm, stringsAsFactors=F)

# feature 5: mdw for each from 
corpus.5.dtm <- corpus.swr.dtm[,colnames(corpus.swr.dtm) %in% mdw.gender]
corpus.5.matrix <- as.matrix(corpus.5.dtm, stringsAsFactors=F)


# scale data for meaningful measures
scaling<- rowSums(corpus.sw.matrix) 

# scale for all matrices 
corpus.1.scaled <- corpus.1.matrix/scaling
corpus.2.scaled <- corpus.2.matrix/scaling
corpus.3.scaled <- corpus.3.matrix/scaling
corpus.4.scaled <- corpus.4.matrix/scaling
corpus.5.scaled <- corpus.5.matrix/scaling

# distance corporas 
# feature 1 
dist.1<-simil(corpus.1.scaled, method = "correlation")
dist.1<-pr_simil2dist(dist.1)
# feature 2
dist.2<-simil(corpus.2.scaled, method = "correlation")
dist.2<-pr_simil2dist(dist.2)
# feature 3
dist.3<-simil(corpus.3.scaled, method = "correlation")
dist.3<-pr_simil2dist(dist.3)
# feature 4 
dist.4<-simil(corpus.4.scaled, method = "correlation")
dist.4<-pr_simil2dist(dist.4)
# feature 5 
dist.5<-simil(corpus.5.scaled, method = "correlation")
dist.5<-pr_simil2dist(dist.5)


### kmeans clustering 
# output a table with feature, k, purity, silhouette
purity <- function(x,y){
  pam.df<-NULL
  for (i in 2:10){
    k <- i
    pam.k<-pam(x,k,diss=TRUE)
    avg.sil<-pam.k$silinfo$avg.width
    purity.v<-vector()
    for (j in 1:k){
      clus.sub<-pam.k$clustering[pam.k$clustering == j]
      sub<-metadata.sub[as.character(metadata.sub$ID) %in% names(clus.sub),]
      purity<-max(c(length(which(sub$Author_Gender == "F"))/nrow(sub), length(which(sub$Author_Gender == "M"))/nrow(sub)))
      purity.v<-append(purity.v, purity)
    }
    avg.pur<-mean(purity.v)
    temp.df<-data.frame("feature"=y,k,avg.pur,avg.sil)
    pam.df<-rbind(pam.df, temp.df)
  }
  return(pam.df)
}

pur.1 <- purity(dist.1, "feature_1")
pur.2 <- purity(dist.2, "feature_2")
pur.3 <- purity(dist.3, "feature_3")
pur.4 <- purity(dist.4, "feature_4")
pur.5 <- purity(dist.5, "feature_5")

clust.df <- data.frame(rbind(pur.1, pur.2, pur.3, pur.4, pur.5))
write.csv(clust.df, "clusters.csv")

# max sil and ideal k 
max.ideal <- function(x,y){
  max.sil.df <- NULL
  asw <- array(0,10)
  for (i in 2:10){
    asw[i] <- pam(x,i,diss=TRUE)$silinfo$avg.width #the silhouette value is in the pam function
    pam(x,i,diss=TRUE)$
  }
  ideal.k<-which.max(asw) #ideal k
  max.sil <- max(asw)
  max.sil.df <- data.frame(y, ideal.k, max.sil)
  return(max.sil.df)
}

max.ideal.1 <- max.ideal(dist.1, "feature_1")
max.ideal.2 <- max.ideal(dist.2, "feature_2")
max.ideal.3 <- max.ideal(dist.3, "feature_3")
max.ideal.4 <- max.ideal(dist.4, "feature_4")
max.ideal.5 <- max.ideal(dist.5, "feature_5")

max.ideal.df <- data.frame(rbind(max.ideal.1, max.ideal.2, max.ideal.3, max.ideal.4, max.ideal.5))
write.csv(max.ideal.df, "max_ideal.csv")


### plotting
# put in max.ideal and dist for each feature
plots <- function(x,y){
  k <- x$ideal.k
  corp.pam <- pam(y, k)
  mds<-cmdscale(y, eig=TRUE, k=k)
  #extract coordinates
  df <- NULL
  for (i in 1:k){
    coord <- mds$points[,i]
    df <- data.frame(cbind(df, coord))
  }
  #x<-mds$points[,1]
  #y<-mds$points[,2]
  #df<-data.frame(x,y)
  #add cluster assignments
  #check alignment
  which(row.names(df) != names(corp.pam$clustering))
  df$clus.info<-corp.pam$clustering
  #add gender assigments
  #check alignment
  df<-df[order(row.names(df)),]
  aa<-metadata.sub[order(as.character(metadata.sub$ID)),]
  which(row.names(df) != aa$ID)
  df$gender<-aa$Author_Gender
  return(df)
}

plot.df.1 <- plots(max.ideal.1, dist.1)
plot.df.2 <- plots(max.ideal.2, dist.2)
plot.df.3 <- plots(max.ideal.3, dist.3)
plot.df.4 <- plots(max.ideal.4, dist.4)
plot.df.5 <- plots(max.ideal.5, dist.5)

# plot clusters using ellipses and points colored by gender
# this allows you to observe how well aligned genders are with clusters
# ********how does this work with max.ideal > 2*************
plot.1 <- ggplot(plot.df.1, aes(x=coord, y=coord.1, color=gender)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_point(size=1) +
  stat_ellipse(aes(color=factor(clus.info)))
plot.2 <- ggplot(plot.df.2, aes(x=coord, y=coord.1, color=gender)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_point(size=1) +
  stat_ellipse(aes(color=factor(clus.info)))
plot.4 <- ggplot(plot.df.4, aes(x=coord, y=coord.1, color=gender)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_point(size=1) +
  stat_ellipse(aes(color=factor(clus.info)))
plot.5 <- ggplot(plot.df.5, aes(x=coord, y=coord.1, color=gender)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_point(size=1) +
  stat_ellipse(aes(color=factor(clus.info)))





