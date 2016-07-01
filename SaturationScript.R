# Set working directory
#setwd("C:/Users/mrc-user/Documents/CSSS/Qual saturation/M_2000tab_61f0763afdcfdf130469c8f3f9636bd5/UKDA-2000-tab/rtf")
setwd("/home/xander/Complexity/Projects/Saturation/UKDA-2000-tab/rtf")

# Load required packages
require(tm)
require(corpora)
#install.packages("corpora")
require("corpora")
require("magrittr")
<<<<<<< HEAD
require("topicmodels")
install.packages("SnowballC")
require("SnowballC")

rm(list=ls())
setwd("C:/Users/mrc-user/Documents/CSSS/Qual saturation/M_2000tab_61f0763afdcfdf130469c8f3f9636bd5/UKDA-2000-tab/rtf")
raw_001 <- readLines("2000int001.rtf")
prep_001 <- Corpus(VectorSource(raw_001))  
  rawb_001 <- tm_map(prep_001,content_transformer(tolower))
clean_001 <- rawb_001 %>%
  tm_map(., removePunctuation) %>% 
  tm_map(., stripWhitespace) 

#%>%
#  tm_map(., removeWords, stopwords("english"))

###In prep
dtm_001 <- DocumentTermMatrix(clean_001) %>%
    as.matrix(.)
freq_001 <- colSums(dtm_001) %>%
           sort(. , decreasing=TRUE)
length(freq_001)
freq_001 <- freq_001[7:2002]

install.packages("SnowballC")
require("SnowballC")
install.packages('wordcloud')
require('wordcloud')
words <- names(freq_001)
wordcloud(words[1:100], freq_001[1:100])

lda <- LDA(freq_001, 5, method = "VEM", control = NULL, model = NULL)

terms(lda)
head(words)
head(freq_001)



length(table(freq_001))
max(table(freq_001))
=======
require("dplyr")
install.packages("SnowballC")
require("SnowballC")
install.packages('wordcloud')
require('wordcloud')


# This loop will go through all of the interview files and output plain text files.
# First we generate a list of the names of files to loop throug.
file.names <- list.files(".", pattern="*.rtf", full.names=T, recursive=FALSE)
savedCleaned <- vector("list", length(file.names))
index <- 1
for (i in file.names){
  raw <- readLines(i) 
  prep <- Corpus(VectorSource(raw))  
  rawb <- tm_map(prep,content_transformer(tolower))
  clean <- rawb %>%
    tm_map(., removePunctuation) %>% 
    tm_map(., stripWhitespace)   %>%
    tm_map(., removeWords, stopwords("english"))
  savedCleaned[[index]] <- clean
  index = index + 1
}


words <- names(freq_001)
wordcloud(words[1:100], freq_001[1:100])

lda <- LDA(freq_001, 5, method = "VEM", control = NULL, model = NULL)
>>>>>>> 786f1de9dee0d97761a5eb76d0ed6753588c6771


#######################Mark's topic model changes

require(tm)
install.packages("corpora")
require("corpora")
require("magrittr")
install.packages("dplyr")
require("dplyr")
install.packages("SnowballC")
require("SnowballC")
install.packages('wordcloud')
require('wordcloud')
require("topicmodels")


rm(list=ls())
setwd("C:/Users/mrc-user/Documents/CSSS/Qual saturation/M_2000tab_61f0763afdcfdf130469c8f3f9636bd5/UKDA-2000-tab/rtf/txt")
raw_001 <- readLines("2000int001.txt") %>% 
  clean_001 <-Corpus(VectorSource(raw_001))  %>%
  tm_map(.,content_transformer(tolower)) %>%
  tm_map(., removePunctuation) %>% 
  tm_map(., stripWhitespace)   %>%
  tm_map(., removeWords, stopwords("english"))

dtm_001 <- DocumentTermMatrix(clean_001,control = list(stemming = TRUE))

Sys.setlocale("LC_COLLATE", "C")
"C"

library("slam")
summary(col_sums(dtm_001))

##The mean term frequency-inverse document frequency (tf-idf) over documents containing
#this term is used to select the vocabulary. This measure allows to omit terms which have low
#frequency as well as those occurring in many documents. We only include terms which have
#a tf-idf value of at least 0.1 which is a bit more than the median and ensures that the very
#frequent terms are omitted.
term_tfidf <- tapply(dtm_001$v/row_sums(dtm_001)[dtm_001$i], dtm_001$j, mean) *
  log2(nDocs(dtm_001)/col_sums(dtm_001 > 0))

summary(term_tfidf)

dtm_001 <- dtm_001[,term_tfidf >= 0.1]
dtm_001 <- dtm_001[row_sums(dtm_001) > 0,]
summary(col_sums(dtm_001))

dim(dtm_001)

k <- 30
SEED <- 2010
TM_001 <-
  list(VEM = LDA(dtm_001, k = k, control = list(seed = SEED)),
       VEM_fixed = LDA(dtm_001, k = k,control = list(estimate.alpha = FALSE, seed = SEED)))
#         + Gibbs = LDA(dtm_001, k = k, method = "Gibbs",
#                       + control = list(seed = SEED, burnin = 1000,
#                                        + thin = 100, iter = 1000)),
#         + CTM = CTM(dtm_001, k = k,
#                     + control = list(seed = SEED,
#                                      + var = list(tol = 10^-4), em = list(tol = 10^-3))))

sapply(TM_001[1:2], slot, "alpha")

sapply(TM_001, function(x)
  mean(apply(posterior(x)$topics,
             1, function(z) - sum(z * log(z)))))

Topic <- topics(TM_001[["VEM"]], 1)

Terms <- terms(TM_001[["VEM"]], 5)
Terms[,1:10]

#(topics_v24 <-+ topics(TM_001[["VEM"]])[grep("/v24/", JSS_papers[, "identifier"])])
#named integer(0)
#most_frequent_v24 <- which.max(tabulate(topics_v24))

#The similarity between these papers is indicated by the fact that the majority of the papers
#have the same topic as their most likely topic. The ten most likely terms for topic 1 are given
#by
#terms(TM_001[["VEM"]], 10)[, most_frequent_v24]


##########Not used terms

#k <- 30
#SEED <- 2010
#TM_001 <-
#  list(VEM = LDA(dtm_001, k = k, control = list(seed = SEED)),
#       VEM_fixed = LDA(dtm_001, k = k,control = list(estimate.alpha = FALSE, seed = SEED)))
#         + Gibbs = LDA(dtm_001, k = k, method = "Gibbs",
#                       + control = list(seed = SEED, burnin = 1000,
#                                        + thin = 100, iter = 1000)),
#         + CTM = CTM(dtm_001, k = k,
#                     + control = list(seed = SEED,
#                                      + var = list(tol = 10^-4), em = list(tol = 10^-3))))

