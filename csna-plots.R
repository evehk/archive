# - - - - - - - - - -
# CSNA PLOTS
# Author: Eve Kraicer
# - - - - - - - -  - -

library(ggplot2)
library(reshape2)

data_clean
data_v <- data_clean[which(data_clean$nl_vol == 'y'),]
data_u <- data_clean[which(data_clean$nl_vol == 'n'),]

### sub for gender 
data_g <- data_clean[data_clean$gender == 'Female'|data_clean$gender == 'Male',]

my_palette <- c('#CE7F00','#274156', '#B8C4BB', '#1C6E8C', '#295923','#797A7A','#CE7F00','#274156', '#B8C4BB', '#1C6E8C', '#295923')

# gender x age 
plot_5 <- ggplot(data = data_g, aes(x = gender, ..count..)) + 
  geom_bar(aes(fill = age), position="dodge") + 
  theme_bw() +
  scale_fill_manual(values = my_palette) + 
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))

# gender x educ_level
plot_2 <- ggplot(data = data_g, aes(x = gender, ..count..)) + 
  geom_bar(aes(fill = educ_lev), position="dodge") + 
  theme_bw() +
  scale_fill_manual(values = my_palette) + 
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))

### sub for mcgill and concordia (active nl)
### who has a nightline and if so how well known is it 
# mcgill x nl_knowledge
# n = 117
data_mcg <- data_clean[data_clean$educ_inst == 'mcgill',]
data_mcg_u <- data_clean[data_clean$educ_inst == 'mcgill' & data_clean$nl_vol == 'n',]
data_conc <- data_clean[data_clean$educ_inst == 'concordia',]
data_conc_u <- data_clean[data_clean$educ_inst == 'concordia' & data_clean$nl_vol == 'n',]
bar_a <- ggplot(data = data_mcg, aes(x = nl_active[1], fill = nl_active)) + geom_bar(width = 1) +
  theme_bw() +
  scale_fill_manual(values = my_palette) + 
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        # axis.text.x=element_text(size = 11),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())
plot_3a <- bar_a + coord_polar(theta = "y")

# concordia x nl_knowledge
# n = 33
bar_b <- ggplot(data = data_conc_u, aes(x = nl_active[1], fill = nl_active)) + geom_bar(width = 1) +
  theme_bw() +
  scale_fill_manual(values = my_palette) + 
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        # axis.text.x=element_text(size = 11), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())
plot_4b <- bar_b + coord_polar(theta = "y")

# satisfaction 
plot_5 <- ggplot(data = data_clean[data_clean$nl_satis != '',], aes(x = nl_vol, ..count..)) + 
  geom_bar(aes(fill = nl_satis), position="dodge") + 
  theme_bw() +
  scale_fill_manual(values = my_palette) + 
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))


### use and use x gender
plot_7 <-  ggplot(data = calls_sum, aes(x = use, y = freq, fill = use)) + 
  geom_col() + 
  theme_bw() +
  scale_fill_manual(values = my_palette) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(size = , family = "Times New Roman", face = "bold"),
        text = element_text(size = 10, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 10, angle = -270))

plot_7b <-  ggplot(data = calls_sum_m, aes(x = use, y = freq, fill = use)) + 
  geom_col() + 
  theme_bw() +
  scale_fill_manual(values = my_palette) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(size = , family = "Times New Roman", face = "bold"),
        text = element_text(size = 10, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 10, angle = -270))

plot_7c <-  ggplot(data = calls_sum_w, aes(x = use, y = freq, fill = use)) + 
  geom_col() + 
  theme_bw() +
  scale_fill_manual(values = my_palette) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(size = , family = "Times New Roman", face = "bold"),
        text = element_text(size = 10, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 10, angle = -270))

# wordcloud
# turn to dtm 
txt <- vector()
for (i in 1:nrow(nl_disc.df)){
  txt <- append(txt, replicate(num_func(nl_disc.df$count[i]), as.character(nl_disc.df$word[i]))) 
}
txt <- gsub("Empathic or \"understanding\"", 'Empathetic', txt)
txt <- paste(txt, collapse = ' ')
wc_dtm <- Corpus(VectorSource(txt))

dtm <- TermDocumentMatrix(wc_dtm)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

# make cloud 
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, scale = c(3.5, 0.5),
        max.words=30, random.order=FALSE, rot.per=0.34, 
        colors=my_palette)
