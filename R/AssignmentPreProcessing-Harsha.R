##################################### ASSIGNMENT ON TEXT MINING #############################################################

#Remove existing global variables
rm(list = ls(all=TRUE))

#Setting of working directory
setwd("C:\\Users\\Harshavardhan Reddy\\Desktop\\TX-Assignment")

#Importing required libraries
library(tm)
library(magrittr)
library(mclust)
library(wordcloud)
library(ggplot2)

#Assigining the file path of corpus text
corpus_path_name = file.path(getwd(),"corpus","txt")

#Corpus loding
corp_docs = Corpus(DirSource(corpus_path_name))
summary(corp_docs)

#Viewing the documents through function call
viewDocs = function(d,n){d %>% extract2(n) %>% as.character() %>% writeLines()}
viewDocs(corp_docs, 1)
viewDocs(corp_docs, 22)


#--------------------- PRE-PROCESSING ----------------------#

#REMOVING SPECIAL PATTERNS IN THE DOCUMENTS
toSpace = content_transformer(function(x,pattern) gsub(pattern, " ", x))
corp_docs = tm_map(corp_docs, toSpace, "/|@|\\|")

#CONVERT ALL DOCUMENTS TEXT INTO LOWER CASE
corp_docs = tm_map(corp_docs, content_transformer(tolower))

#REMOVE THE NUMBERS IN THE DOCUMENTS
corp_docs = tm_map(corp_docs, removeNumbers)

#REMOVE THE PUNCTUATIONS IN THE DOCUMENTS
corp_docs = tm_map(corp_docs, removePunctuation)

#REMOVE STOP WORDS IN THE DOCUMENTS
corp_docs = tm_map(corp_docs, removeWords, stopwords("english"))

#REMOVING OWN STOP WORDS IN THE DOCUMENTS
corp_docs = tm_map(corp_docs,removeWords, c("can","will"))

#REMOVING THE WHITE SPACE IN THE DOCUMENTS
corp_docs = tm_map(corp_docs, stripWhitespace)

#CONVERT SPECIFIC TERMS AS OTHER TERMS IN THE DOCUMENTS
#toString = content_transformer(function(x, from, to) gsub(from, to, x))
#docs = tm_map(docs, toString, "america", "united states")

#STEMMING THE DOCUMENTS
corp_docs = tm_map(corp_docs, stemDocument)

#CONVERT INTO DOCUMENT TERM MATRIX
corp_dtm = DocumentTermMatrix(corp_docs)
inspect(corp_dtm[1:5, 1000:1005])
class(corp_dtm)
dim(corp_dtm)

#FINDING THE ORDER AND FREQUENCY OF THE VOCABULARY IN THE DOCUMENT TERM MATRIX
freq <- colSums(as.matrix(corp_dtm))
length(freq)
ord <- order(freq)

#LEAST FREQUENT TERMS
freq[head(ord)]
#HIGHEST FREQUENT TERMS
freq[tail(ord)]

#REMOVING SPARCE TERMS EXCEPT ALLOWING 20 PERCENT
dim(corp_dtm)
corp_dtms <- removeSparseTerms(corp_dtm, 0.2)
dim(corp_dtms)
freq <- colSums(as.matrix(corp_dtms))
freq

#FREQUENCY TERMS 200 AND MORE
findFreqTerms(corp_dtms, lowfreq=200)

#FREQUENCY TERMS 1 AND MORE
findFreqTerms(corp_dtms, lowfreq=1)

#CONVERTING THE DTMS INTO MATRIX
docs_mat = data.frame(as.matrix(corp_dtms))


#---------------------- CLUSTERING USING EM -----------------------------#
#CLUSTER USING MCLUST
doc_clus = Mclust(docs_mat)

#ASSIGN CLUSTER NUMBERS TO THE MATRIX DATA
docs_mat$Cluster = doc_clus$classification
head(docs_mat)

#CHECKING THE PROBABILITIES OF THE CLUSTERS
doc_clus$z
round(doc_clus$z,2)

#------------------------- WORD CLOUD ON CLUSTERS -------------------------#

# 1. FINDING WORD COUNT ON ALL CLUSTERS DATA
freq <- sort(colSums(docs_mat[,-10]), decreasing=TRUE)
head(freq, 9)
wf   <- data.frame(word=names(freq), freq=freq)
rownames(wf)<- NULL
head(wf)

#CREATING A BAR CHART FOR WORDS
subset(wf, freq>50)                                                  %>%
  ggplot(aes(word, freq))                                              +
  geom_bar(stat="identity")                                            +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#USING MINIMUM 40 FREQUNCY IN WORD CLOUD
set.seed(123)
wordcloud(names(freq), freq, min.freq=40)

#CREATING WORD CLOUD WITH MAX WORDS
set.seed(142)
wordcloud(names(freq), freq, max.words=100)

#WORD CLOUD WITH HIGHER FREQUENCY
set.seed(142)
wordcloud(names(freq), freq, min.freq=100)

#APPLYING COLOURS TO THE WORD CLOUD
set.seed(142)
wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))

#APPLY SCALING TO THE WORD CLOUD
set.seed(142)
wordcloud(names(freq), freq, min.freq=100, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

#APPLYING 20 PERCENT ROTAION IN THE WORD COLUD
set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, min.freq=100, rot.per=0.2, colors=dark2)

# 2. FINDING WORD COUNT WITH CLUSTER 1
clust1DF = docs_mat[docs_mat$Cluster == 1,]
freq <- sort(colSums(clust1DF[,-32]), decreasing=TRUE)
head(freq, 14)
wf   <- data.frame(word=names(freq), freq=freq)
rownames(wf)<- NULL
head(wf)

#CREATING A BAR CHART FOR WORDS
subset(wf, freq>50)                                                  %>%
  ggplot(aes(word, freq))                                              +
  geom_bar(stat="identity")                                            +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#USING MINIMUM 40 FREQUNCY IN WORD CLOUD
set.seed(123)
wordcloud(names(freq), freq, min.freq=40)

#CREATING WORD CLOUD WITH MAX WORDS
set.seed(142)
wordcloud(names(freq), freq, max.words=100)

#WORD CLOUD WITH HIGHER FREQUENCY
set.seed(142)
wordcloud(names(freq), freq, min.freq=40)

#APPLYING COLOURS TO THE WORD CLOUD
set.seed(142)
wordcloud(names(freq), freq, min.freq=40, colors=brewer.pal(6, "Dark2"))

#APPLY SCALING TO THE WORD CLOUD
set.seed(142)
wordcloud(names(freq), freq, min.freq=40, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

#APPLYING 20 PERCENT ROTAION IN THE WORD COLUD
set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, min.freq=40, rot.per=0.2, colors=dark2)


# 3. FINDING WORD COUNT WITH CLUSTER 2
clust2DF = docs_mat[docs_mat$Cluster == 2,]
freq <- sort(colSums(clust2DF[,-32]), decreasing=TRUE)
head(freq, 14)
wf   <- data.frame(word=names(freq), freq=freq)
rownames(wf)<- NULL
head(wf)

#CREATING A BAR CHART FOR WORDS
subset(wf, freq>50)                                                  %>%
  ggplot(aes(word, freq))                                              +
  geom_bar(stat="identity")                                            +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#USING MINIMUM 40 FREQUNCY IN WORD CLOUD
set.seed(123)
wordcloud(names(freq), freq, min.freq=40)

#CREATING WORD CLOUD WITH MAX WORDS
set.seed(142)
wordcloud(names(freq), freq, max.words=100)

#WORD CLOUD WITH HIGHER FREQUENCY
set.seed(142)
wordcloud(names(freq), freq, min.freq=40)

#APPLYING COLOURS TO THE WORD CLOUD
set.seed(142)
wordcloud(names(freq), freq, min.freq=40, colors=brewer.pal(6, "Dark2"))

#APPLY SCALING TO THE WORD CLOUD
set.seed(142)
wordcloud(names(freq), freq, min.freq=40, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

#APPLYING 20 PERCENT ROTAION IN THE WORD COLUD
set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, min.freq=40, rot.per=0.2, colors=dark2)


# 4. FINDING WORD COUNT WITH CLUSTER 3
clust3DF = docs_mat[docs_mat$Cluster == 3,]
freq <- sort(colSums(clust3DF[,-32]), decreasing=TRUE)
head(freq, 14)
wf   <- data.frame(word=names(freq), freq=freq)
rownames(wf)<- NULL
head(wf)

#CREATING A BAR CHART FOR WORDS
subset(wf, freq>25)                                                  %>%
  ggplot(aes(word, freq))                                              +
  geom_bar(stat="identity")                                            +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#USING MINIMUM 40 FREQUNCY IN WORD CLOUD
set.seed(123)
wordcloud(names(freq), freq, min.freq=40)

#CREATING WORD CLOUD WITH MAX WORDS
set.seed(143)
wordcloud(names(freq), freq, max.words=100)

#WORD CLOUD WITH HIGHER FREQUENCY
set.seed(143)
wordcloud(names(freq), freq, min.freq=40)

#APPLYING COLOURS TO THE WORD CLOUD
set.seed(143)
wordcloud(names(freq), freq, min.freq=40, colors=brewer.pal(6, "Dark2"))

#APPLY SCALING TO THE WORD CLOUD
set.seed(143)
wordcloud(names(freq), freq, min.freq=40, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

#APPLYING 20 PERCENT ROTAION IN THE WORD COLUD
set.seed(143)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, min.freq=100, rot.per=0.2, colors=dark2)

# 5. FINDING WORD COUNT WITH CLUSTER 4
clust4DF = docs_mat[docs_mat$Cluster == 4,]
freq <- sort(colSums(clust4DF[,-32]), decreasing=TRUE)
head(freq, 14)
wf   <- data.frame(word=names(freq), freq=freq)
rownames(wf)<- NULL
head(wf)

#CREATING A BAR CHART FOR WORDS
subset(wf, freq>50)                                                  %>%
  ggplot(aes(word, freq))                                              +
  geom_bar(stat="identity")                                            +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#USING MINIMUM 40 FREQUNCY IN WORD CLOUD
set.seed(123)
wordcloud(names(freq), freq, min.freq=40)

#CREATING WORD CLOUD WITH MAX WORDS
set.seed(143)
wordcloud(names(freq), freq, max.words=100)

#WORD CLOUD WITH HIGHER FREQUENCY
set.seed(143)
wordcloud(names(freq), freq, min.freq=40)

#APPLYING COLOURS TO THE WORD CLOUD
set.seed(143)
wordcloud(names(freq), freq, min.freq=40, colors=brewer.pal(6, "Dark2"))

#APPLY SCALING TO THE WORD CLOUD
set.seed(143)
wordcloud(names(freq), freq, min.freq=40, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

#APPLYING 20 PERCENT ROTAION IN THE WORD COLUD
set.seed(143)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, min.freq=40, rot.per=0.2, colors=dark2)

#----------------------- CREATING A CSV FILE WITH THE DOCS_MAT DATA FRAME ----------------------------#

# GENERATING CSV FILE WITH NAME Obama.CSV
write.csv(x = docs_mat,file = "obama.csv",row.names=T)
