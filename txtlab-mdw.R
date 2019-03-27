# - - - - - - - - -
# MDW BY GENDER
# Author: Eve Kraicer
# - - - - - - - - - 

# find top words, stop words removed 
# create df using corpus.swr.matrix
df3 <- corpus.swr.matrix
top.words<-sort(colSums(df3), decreasing = T)[1:3000]

# create matching tables of raw counts
# df1 is men, df2 is women 
men.matrix <- df3[row.names(df3) %in% metadata.m$ID,]
men.raw <- men.matrix[,colnames(men.matrix) %in% names(top.words)]
women.matrix <- df3[row.names(df3) %in% metadata.w$ID,]
women.raw <- women.matrix[,colnames(women.matrix) %in% names(top.words)]


### Dunning's Log Likelihood + Fisher's ###
H = function(k) {N = sum(k); return(sum(k/N*log(k/N+(k==0))))}
men<-colSums(men.raw)
women<-colSums(women.raw)
all.m<-sum(men)
all.w<-sum(women)
results <- data.frame(word = colnames(men.raw), 
                      group1=men,
                      group2=women,
                      G2 = 0,
                      fisher.OR = 0,
                      fisher.p = 0)
for (j in 1:ncol(men.raw)){
  cont.table<-data.frame(c(men[j], all.m-men[j]), c(women[j], all.w-women[j]))
  fish<-fisher.test(cont.table)
  LLR = 2*sum(cont.table)*(H(cont.table)-H(rowSums(cont.table))-H(colSums(cont.table)))
  results$G2[j] = LLR
  results$fisher.OR[j] = fish$estimate
  results$fisher.p[j] = fish$p.value
}
# sort by G2
dunning.df<-results[order(-results$G2),]
# establish correction
cut<-0.05/ncol(men.raw)
# remove non-significant words
dunning.df<-dunning.df[dunning.df$fisher.p < cut,] 
# the above ranks by strength either over or under expected values
# if you want to sort by above and below, then run the following code
dunning.sort<-dunning.df
dunning.sort$diff<-dunning.sort$group1-dunning.sort$group2
G2_Sort.v<-vector()
for (i in 1:nrow(dunning.sort)){
  if (dunning.sort$diff[i] <= 0){
    G2_Sort<--dunning.sort$G2[i]
  } else {
    G2_Sort<-dunning.sort$G2[i]
  }
  G2_Sort.v<-append(G2_Sort.v, G2_Sort)
}
dunning.sort<-cbind(dunning.sort, G2_Sort.v)
dunning.sort<-dunning.sort[order(-dunning.sort$G2_Sort.v),]

mdw.men <- dunning.sort[1:50,]
mdw.women <- dunning.sort[1849:nrow(dunning.sort),]
mdw.gender <- c(as.character(mdw.men$word), as.character(mdw.women$word))




