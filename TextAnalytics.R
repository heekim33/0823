# 1. ÆÐÅ°Áö ¼³Ä¡
# rJava,KoNLP, wordcloud, tm 
install.packages("rJava")
#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-15.0.2')

install.packages(c("tm", "ggplot2", "wordcloud2", "fpc","rvest","stringr","qgraph","dplyr","networkD3"))
install.packages("KoNLP")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"),force = T)
install.packages('tidyverse')
install.packages("RColorBrewer")
install.packages('Unicode')

library(tidyverse)
library(KoNLP)
library(tm)    
library(wordcloud2) 
library(ggplot2)
library(fpc)
library(RColorBrewer)
library(rJava)
library(rvest)
library(stringr)
library(qgraph)
library(dplyr)
library(networkD3)
library(Unicode)
useNIADic()


#2. ºÐ¼®ÇÒ µ¥ÀÌÅÍ °¡Á®¿À±â

library(readxl) 
data <- read_excel("data2.xlsx") 
keyword<-data$keyword
keyword<- gsub(',',' ',keyword)
str(keyword)


#3.Å°¿öµå Áß ¸í»çÃßÃâ
ko_words <- function(doc) {
  d <- as.character(doc)
  pos <- unlist(SimplePos22(d))
  
  extracted <- str_match(pos, '([°¡-ÆR]+)/[NP][A-Z]')
  
  keyword <- extracted[, 2]
  keyword[!is.na(keyword)]
}

texts <- keyword %>%
  str_replace_all(pattern="\r", replacement="") %>%
  str_replace_all(pattern="\n", replacement=" ") %>%
  str_replace_all(pattern="[[:punct:]]", replacement=" ") %>%
  str_replace_all(pattern="[¤¡-¤¾¤¿-¤Ó]+", replacement="") %>%
  str_replace_all(pattern="/", replacement=" ") %>%
  str_trim(side="both")

texts <- texts[texts != ""]

pos <- Map(ko_words, texts)

#4. µ¿½ÃÃâÇö ¸ÅÆ®¸¯½º »ý¼º
options(encoding = 'UTF-8')
corpus <- VCorpus(VectorSource(pos))
stopWord <- c("¾î¸°ÀÌ", "½º¸¶Æ®Æù")

tdm <- TermDocumentMatrix(corpus, control=list(removePunctuation = TRUE,
removeNumbers=FALSE, wordLengths=c(2, 6),stopwords=stopWord, weighting=weightTf))
memory.limit(size = 100000) 
tdm.matrix <- as.matrix(tdm)

word.count <- rowSums(tdm.matrix)
word.order <- order(word.count, decreasing=TRUE)
freq.words <- tdm.matrix[word.order[1:20], ]
rownames(tdm.matrix)[word.order[1:20]]

co.matrix <- freq.words %*% t(freq.words)

#5. µ¿½ÃÃâÇö ´Ü¾î ½Ã°¢È­
qg<-qgraph(co.matrix, labels=rownames(co.matrix),
       diag=FALSE, layout='spring', threshold=1,
       vsize=log(diag(co.matrix)) * 0.9)
plot(qg)

#6. ¿öµå Å¬¶ó¿ìµå ±×¸®±â
dtm = as.DocumentTermMatrix(tdm)
freq=sort(colSums(as.matrix(dtm)), decreasing=TRUE) 
wf=data.frame(word=names(freq), freq=freq)

#´Ü¼ø ºóµµ ¸·´ë ±×·¡ÇÁ
p=ggplot(subset(wf, freq>5000), aes(word, freq))
p=p+geom_bar(stat="identity")
p=p+theme(axis.text.x=element_text(angle=45, hjust=1))
p

#¿öµå Å¬¶ó¿ìµå (TF)
wordcloud2(wf[1:1000,],fontFamily="NanumGothic", minRotation=0, maxRotation=0)


#¿öµå Å¬¶ó¿ìµå (TF-IDF) 
tdm_tfidf <- TermDocumentMatrix(corpus, control=list(removePunctuation = TRUE,
                                               removeNumbers=FALSE, wordLengths=c(2, 6),stopwords=stopWord, weighting= weightTfIdf))
tdm_tfidf.matrix <- as.matrix(tdm_tfidf)
word.count <- rowSums(tdm_tfidf.matrix)
word.order <- order(word.count, decreasing=TRUE)
freq.words <- tdm_tfidf.matrix[word.order[1:20], ]

dtm = as.DocumentTermMatrix(tdm_tfidf)
freq2=sort(colSums(as.matrix(dtm)), decreasing=TRUE) 
wf=data.frame(word=names(freq2), freq=freq2)

wordcloud2(wf[1:1000,],fontFamily="NanumGothic", minRotation=0, maxRotation=0)


#7. ÅäÇÈ¸ðµ¨¸µ
install.packages('lda')
install.packages('topicmodels')
install.packages('LDAvis')
install.packages('servr')
install.packages('LDAvisData')
install.packages("tidytext")
install.packages("devtools")
devtools::install_github("cpsievert/LDAvisData")
install.packages("stringi")
install.packages("lubridate")
install.packages("Rmpfr")
install.packages("SentimentAnalysis")

library(lda)
library(stringr)
library(topicmodels)
library(LDAvis)
library(servr)
library(LDAvisData)
library(MASS)
library(tidytext)

library(stringi)
library(lubridate)
library(Rmpfr)
library(SentimentAnalysis)

# Document Matrix »ý¼º
dtm2 <- DocumentTermMatrix(corpus, control=list(
  removePunctuation=TRUE, stopwords=stopWord,
  removeNumbers=TRUE, wordLengths=c(4, 10), weighting=weightTf))

rowTotals <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm2 <- dtm2[rowTotals> 0, ]

# LDA ÇÔ¼ö Àû¿ë
q2_lda <- LDA(dtm2, k=4, seed=1234)

q2_topics <- tidy(q2_lda, matrix="beta")

top_terms <- q2_topics %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  theme(axis.text.y=element_text(family="NanumGothic")) +
  facet_wrap(~ topic, scales="free") +
  coord_flip()

# ÃÖÀûÀÇ K°ª Ã£±â
harmonicMean <- function(logLikelihoods, precision=2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec=precision) + llMed))))
}

seqk <- seq(2, 40, 1) # k¸¦ 2ºÎÅÍ 40±îÁö ¹Ù²Ù¾î°¡¸é¼­ ½ÇÇà 
burnin <- 1000
iter <- 500
keep <- 50

fitted_many <- lapply(seqk, function(k) LDA(dtm2, k=k, method="Gibbs", control=list(burnin=burnin, iter=iter, keep=keep)))

logLiks_many <- lapply(fitted_many, function(L) L@logLiks[-c(1:(burnin/keep))])

hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

#±×·¡ÇÁ ½Ã°¢È­
ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) +
  geom_path(lwd=1.5) +
  theme(text=element_text(family=NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  ggplot2::annotate("text", x=9, y=-199000, label=paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
  labs(title="Latent Dirichlet Allocation Analysis",
       subtitle="How many distinct topics?")

# k=6À¸·Î ÅäÇÈ ¸ðµ¨¸µ
q_model <- LDA(dtm2, k=6, method="Gibbs", control=list(iter=2000))

q_topics <- topics(q_model, 1)
q_terms <- as.data.frame(terms(q_model, 20), stringsAsFactors=FALSE)
q_terms[1:6]


# 8.°¨¼ººÐ¼®

# µ¥ÀÌÅÍ ºÒ·¯¿À±â
data <- read_excel("data2.xlsx") 
contents<-data$contents

# µ¥ÀÌÅÍÀüÃ³¸®
contents <- str_replace_all(contents, "[^0-9a-zA-Z¤¡-¤¾¤¿-¤Ó°¡-ÆR[:space:]]", " ")
contents <- str_replace_all(contents, "[\n\t]", " ")
contents <- str_trim(contents)
contents <- str_replace_all(contents, "\\s+", " ")

# Tokenizer
corp <- VCorpus(VectorSource(contents))
tdm <- TermDocumentMatrix(corp, 
                          control = list(wordLengths = c(2, Inf), 
                                         tokenize = function(x) {
                                           ngram_tokenize(x, char = F)
                                         }))

tail(Terms(tdm))

#Èñ¼Ò ´Ü¾î Á¦°Å
tdm_r <- removeSparseTerms(tdm, sparse = 0.95) #?¬?†Œ ?‹¨?–´ ? œê±?
head(Terms(tdm_r), 20)

wordFreq <- slam::row_sums(tdm_r)
wordFreq_df <- data.frame(words = names(wordFreq), 
                          freq  = wordFreq)

remove_chars <- c("t", "co", "t co", "https", "https t", "https t co")
wordFreq_df2 <- wordFreq_df %>% 
  filter(!(words %in% remove_chars))

#±º»ê´ëÇÐ±³ °¨¼º»çÀü »ç¿ë
# »çÀü ºÒ·¯¿À±â
senti_words_kr <- readr::read_delim("SentiWord_Dict.txt", delim='\t', col_names=c("term", "score"))
head(senti_words_kr)
dim(senti_words_kr)

#»çÀü ÁØºñ
x <- duplicated(senti_words_kr$term)
senti_words_kr2 <- senti_words_kr[!x, ]
senti_dic_kr <- SentimentDictionaryWeighted(words = senti_words_kr2$term, 
                                            scores = senti_words_kr2$score)
senti_dic_kr <- SentimentDictionary(senti_words_kr2$term[senti_words_kr2$score > 0], 
                                    senti_words_kr2$term[senti_words_kr2$score < 0])

summary(senti_dic_kr)

#°¨¼º Á¡¼ö °è»ê
res_sentiment <- analyzeSentiment(corp, 
                                  language="korean",
                                  rules=list("KoreanSentiment"=list(ruleSentiment, senti_dic_kr)),
                                  removeStopwords = F, stemming = F)

df2 <- data.frame(round(res_sentiment, 3), data$contents)

theme_set(theme_minimal(base_family = "NanumGothic"))

df3 <- df2 %>% 
  mutate(pos_neg = ifelse(KoreanSentiment > 0, "Positive",
                          ifelse(KoreanSentiment == 0, "Neutral","Negative")))
         
df4 <- data.frame(head(df3, n = 500))

DT::datatable(head(df3, n = 500), 
              class = 'cell-border stripe', 
              options = list(pageLength = 5, autoWidth = TRUE, scrollX = TRUE))
ggplot(df3, aes(x = factor(pos_neg))) + 
  geom_bar(stat = "count", width = 0.7, fill = "steelblue") + 
  theme_minimal()
