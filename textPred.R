library(tm)
library(RWeka)
set.seed(212)

fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if(!file.exists("Coursera-SwiftKey.zip")) {
    download.file(fileUrl, "Coursera-SwiftKey.zip")
    unzip("Coursera-SwiftKey.zip")
}

blogs   <- readLines("Coursera-SwiftKey/final/en_US/en_US.blogs.txt", encoding="UTF-8")
news    <- readLines("Coursera-SwiftKey/final/en_US/en_US.news.txt", encoding="UTF-8")
twitter <- readLines("Coursera-SwiftKey/final/en_US/en_US.twitter.txt", encoding="UTF-8")

sampleBlogs <- blogs[sample(1:length(blogs), 0.5 * length(blogs))]
sampleNews <- news[sample(1:length(news), 0.5 * length(news))]
sampleTwitter <- twitter[sample(1:length(twitter), 0.5 * length(twitter))]

rm(blogs)
rm(news)
rm(twitter)

sampleData <- c(sampleBlogs, sampleNews, sampleTwitter)

writeLines(sampleData, "sampleData.txt")

sampleData <- Corpus(VectorSource(sampleData))
sampleData <- tm_map(sampleData, content_transformer(tolower))
sampleData <- tm_map(sampleData, stripWhitespace)
sampleData <- tm_map(sampleData, removePunctuation)
sampleData <- tm_map(sampleData, removeNumbers)

# Function to make N grams
tdmNgram <- function (corpus, n) {
    ngramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = n, max = n))}
    tdm_ngram <- TermDocumentMatrix(corpus, control = list(tokenizer = ngramTokenizer))
    tdm_ngram
}

# Function to extract the N grams and sort
sortNgram <- function (tdm_ngram) {
    ngram_mat <- as.matrix(tdm_ngram)
    ngram_df <- as.data.frame(ngram_mat)
    colnames(ngram_df) <- "Count"
    ngram_df <- ngram_df[order(-ngram_df$Count), , drop = FALSE]
    ngram_df
}

# Calculate N-Grams
bigram <- tdmNgram(sampleData, 2)
trigram <- tdmNgram(sampleData, 3)
quadgram <- tdmNgram(sampleData, 4)

# Extract term-count tables from N-Grams and sort 
bigram <- sortNgram(bigram)
trigram <- sortNgram(trigram)
quadgram <- sortNgram(quadgram)

quadgram <- data.frame(words=rownames(quadgram), count=quadgram$Count)
quadgram$words <- as.character(quadgram$words)
quadgram_split <- strsplit(as.character(quadgram$words), split=" ")
quadgram <- transform(quadgram, first = sapply(quadgram_split, head, 1), second = sapply(quadgram_split, head, 2), third = sapply(quadgram_split, head, 3), fourth = sapply(quadgram_split, head, 4))
quadgram <- data.frame(unigram = quadgram$first, bigram = quadgram$second, trigram = quadgram$third, quadgram = quadgram$fourth, freq = quadgram$count, stringsAsFactors=FALSE)
quadgram <- quadgram[quadgram$freq > 1,]
rownames(quadgram) <- c()
saveRDS(quadgram,"quadgram.RData")

trigram <- data.frame(words=rownames(trigram), count=trigram$Count)
trigram$words <- as.character(trigram$words)
trigram_split <- strsplit(as.character(trigram$words), split=" ")
trigram <- transform(trigram, first = sapply(trigram_split, head, 1), second = sapply(trigram_split, head, 2), third = sapply(trigram_split, head, 3))
trigram <- data.frame(unigram = trigram$first, bigram = trigram$second, trigram = trigram$third, freq = trigram$count, stringsAsFactors=FALSE)
trigram <- trigram[trigram$freq > 1,]
rownames(trigram) <- c()
saveRDS(trigram,"trigram.RData")

bigram <- data.frame(words=rownames(bigram), count=bigram$Count)
bigram$words <- as.character(bigram$words)
bigram_split <- strsplit(as.character(bigram$words), split=" ")
bigram <- transform(bigram, first = sapply(bigram_split, head, 1), second = sapply(bigram_split, head, 2))
bigram <- data.frame(unigram = bigram$first, bigram = bigram$second, freq = bigram$count, stringsAsFactors=FALSE)
bigram <- bigram[bigram$freq > 1,]
rownames(bigram) <- c()
saveRDS(bigram,"bigram.RData")
