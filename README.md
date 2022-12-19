#Contract Management Analysis - FM MBA SENAI 2022
#Prof Dr Robson Quinello

#packages

library(pdftools)
library(tm) 
library(topicmodels)
library(textdata)
library(rvest)
library(tidyverse)
library(tidytext)
library(dplyr)
library(tidyr)
library(reshape2)
library(forcats)
library(scales)
library(stringr)
library(ggplot2)
library (ggplot)
library(wordcloud)
library(igraph)
library(ggraph)
library(antiword)
library(stopwords)
library(syuzhet) 
library(RColorBrewer)
library(wordcloud)
library(tm)
library(nplr)
library(quanteda)
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
library(syuzhet)
library(ape)
library(cluster) 
library(dendextend)

#read text (baixar arquivo fonte e substituir dirretorio)

artigo_TX <- pdftools::pdf_text(pdf = "C:/Users/GUERREIRO/Desktop/FCD/BigDataRAzure/Cap05/InsightsR.pdf")
class(artigo_TX)
View(artigo_TX)

#create corpus

corpus <- Corpus(VectorSource(artigo_TX))
skipWords <- function(x) removeWords(x, stopwords("portuguese"))
funcs <- list(tolower, removePunctuation, removeNumbers, stripWhitespace, skipWords)
a <- tm_map(a, PlainTextDocument)
a <- tm_map(corpus, FUN = tm_reduce, tmFuns = funcs)
a.dtm1 <- TermDocumentMatrix(a, control = list(wordLengths = c(3,10))) 
View (corpus)

# remove stopwords
c(tm::stopwords("portuguese"))
newstopwords = stopwords ("portuguese")
newstopwords <- findFreqTerms(a.dtm1, lowfreq=15) 
View (newstopwords)


# remove most frequent words for this corpus
a.dtm2 <- a.dtm1[!(a.dtm1$dimnames$Terms) %in% newstopwords,] 
inspect(a.dtm2)
a.dtm3 <- removeSparseTerms(a.dtm2, sparse=0.5)
a.dtm.df <- as.data.frame(inspect(a.dtm3))
a.dtm.df.scale <- scale(a.dtm.df)
d <- dist(a.dtm.df.scale, method = "euclidean") 
fit <- hclust(d, method="ward.D")
plot(fit)

#dendogram
par(mar=c(0,0,1,0))   
clus=cutree(fit,3)
colors=c("red","blue")
plot(as.phylo(fit), 
     cex=0.7,
     hang=-1, 
     type="fan", 
     tip.color=colors[clus])

# get word counts in decreasing order
m = as.matrix(t(a.dtm1))
word_freqs = sort(colSums(m), decreasing=TRUE) 

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
count(dm)
view (dm)

#plot
dm %>%
  filter(!is.na(freq)) %>%
  arrange(freq) %>%
  tail(20) %>%
  mutate(word=factor(word, word)) %>%
  ggplot( aes(x=word, y=freq) ) +
  geom_bar(stat="identity", fill="#69b3a2") +
  coord_flip() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") +
  ylab("Major words")

# plot wordcloud
m = as.matrix(t(a.dtm1))
view (m)

# get word counts in decreasing order
word_freqs = sort(colSums(m), decreasing=TRUE) 
View (word_freqs)

# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#sentiments (could be slow)

texto <- scan (file = "C:/Users/GUERREIRO/Desktop/FCD/BigDataRAzure/Cap05/insightsR.txt", fileEncoding = "UTF-8", what = character(), sep = "\n", allowEscapes = T)
texto_words <- get_tokens(texto)
head(texto_words)
class (texto_words)
length(texto_words)
oracoes_vetor <- get_sentences(texto)
length(oracoes_vetor)

sentimentos_df <- get_nrc_sentiment(texto_palavras, lang="portuguese")

head(sentimentos_df)

summary(sentimentos_df)

barplot(
  colSums(prop.table(sentimentos_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 10, name = "Set3"),
  main = "Contract",
  sub = "prepared by RQ",
  xlab="sentiments", ylab = NULL)

sentimentos_valencia <- (sentimentos_df$negative * -1) + sentimentos_df$positive

simple_plot(sentimentos_valencia)

plot(
  sentimentos_valencia, 
  type="h", 
  main="Narrative Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

#END__________________________
