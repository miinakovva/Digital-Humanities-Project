options(stringsAsFactors = FALSE)

library(rdracor)
library(quanteda)
require(topicmodels)

lemma_data <- read.delim("./data/lemmatization-de.txt", header = FALSE, encoding = "UTF-8")

set_dracor_api_url("https://dracor.org/api")

load <- function() {
  play_metadata <-
    read.csv("./data/dracor_play_metadata.csv", sep = ",", encoding = "UTF-8")
  #play_metadata_head <- head(play_metadata, n = 683L)
  play_metadata_head <- play_metadata
  message(sprintf("Load 1: %s %d", play_metadata_head$name[1], play_metadata_head$yearNormalized[1]))
  text_df <- get_text_df(play = play_metadata_head$name[1], corpus = "ger")
  text_df$year_normalized <- play_metadata_head$yearNormalized[1]
  for (i in 2:nrow(play_metadata_head)) {
    message(sprintf("Load %d: %s %d", i, play_metadata_head$name[i], play_metadata_head$yearNormalized[i]))
    tryCatch({
      row_text_df <- get_text_df(play = play_metadata_head$name[i], corpus = "ger")
      row_text_df$year_normalized <- play_metadata_head$yearNormalized[i]      
      text_df <- rbind(text_df, row_text_df)
    }, warning = function(w) {
      message('Caught an warning!')      
      print(w)
    }, error = function(e) {
      message('Caught an error!')
      print(e)
    })
  }
  return(text_df)
}

dracor_texts <- load()

library(udpipe)
library(stringr)
library(dplyr)


m_ger <- udpipe::udpipe_download_model(language = "german-gsd") 
m_ger <- udpipe_load_model(file = m_ger$file_model)

for (i in 1:nrow(dracor_texts)) {
  text <- dracor_texts$text[i]
  text_prepared <- str_squish(text)
  
  df_text_annot <- udpipe::udpipe_annotate(m_ger, x = text_prepared) %>% 
    as.data.frame() %>%
    dplyr::select(-sentence)
  
  options(width = 60)
  knitr::opts_chunk$set(tidy.opts=list(width.cutoff=80), tidy=TRUE)
  text_tagged <- df_text_annot %>% filter(upos %in% c('NOUN'))
  text_filtered <- paste(text_tagged$token, collapse = " ", sep = "")
  
  dracor_texts$text[i] <- text_filtered
}

dracor_corpus <- corpus(dracor_texts$text)

dracor_tokens <- dracor_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_replace(lemma_data$V2, lemma_data$V1, valuetype = "fixed") %>%
  tokens_remove(stopwords("de")) %>%
  tokens_tolower()

dracor_collocations <- quanteda.textstats::textstat_collocations(dracor_tokens, min_count = 25)

dracor_tokens <- tokens_compound(dracor_tokens, dracor_collocations)

DTM <- dracor_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 3)

sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
dracor_texts <- dracor_texts[sel_idx, ]

K <- 20

topicModel <- LDA(DTM, K, method="Gibbs", control=list(
  iter = 2500,
  seed = 1,
  verbose = 25,
  alpha = 0.0008))

tmResult <- posterior(topicModel)

attributes(tmResult)

beta <- tmResult$terms 

dim(beta)
rowSums(beta) 

theta <- tmResult$topics
rowSums(theta)[1:20]

terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")

library(LDAvis)
library("tsne")

svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
                   vocab = colnames(DTM), term.frequency = colSums(DTM), mds.method = svd_tsne,
                   plot.opts = list(xlab = "", ylab = ""))
serVis(json)

library(reshape2)
library(ggplot2)


topic_proportion_per_decade <- aggregate(theta, by = list(decade = dracor_texts$year_normalized), mean)

colnames(topic_proportion_per_decade)[2:(K+1)] <- topicNames

vizDataFrame <- melt(topic_proportion_per_decade, id.vars = "decade")

require(pals)
ggplot(vizDataFrame,
       aes(x=decade, y=value, fill=variable)) +
  geom_bar(stat = "identity") + ylab("proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "decade") +
  theme(axis.text.x = element_text(angle = 90, hjust = 10))

