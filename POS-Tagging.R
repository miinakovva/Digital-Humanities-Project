library(dplyr)
library(quanteda)
library(udpipe)

# Lade das POS_Tagging Model herunter
# für Tokenisierung, Tagging, Lemmatisierung and Dependency Parsing
m_ger <- udpipe::udpipe_download_model(language = "german-gsd") # detailierter und granularer
m_ger <- udpipe_load_model(file = m_ger$file_model)


# Annotiere jeden einzelnen Text (Wahlprogramm)
for (i in 1:nrow(result))
{
  text <- result$text[i]
  text <- gsub("[[:punct:]]", "", text)
  text <- gsub("[ß]", "ss", text)
  text_prepared <- str_squish(text)
  
  # Annotiere am Originaltext (weil POS-Tagging abh. von Groß-/Kleinschreibung)
  # und speichere als data.frame
  df_text_annot <- udpipe::udpipe_annotate(m_ger, x = text_prepared) %>% 
    as.data.frame() %>%
    dplyr::select(-sentence)
  
  # Filtere alle Nomen und Eigennamen heraus
  options(width = 60)
  knitr::opts_chunk$set(tidy.opts=list(width.cutoff=80), tidy=TRUE)
  text_tagged <- df_text_annot %>% filter(upos %in% c('NOUN','PROPN'))
  # nur Nomen und Eigennamen oder auch Adjektive? token oder lemma?
  text_filtered <- paste(text_tagged$token, collapse = " ", sep = "")
  
  result$text[i] <- text_filtered
}

# neuer data.frame
#abschnitt_id <- result$abschnitt_id
#wahljahr <- result$wahljahr
#partei <- result$partei
#df_text_prep <- data.frame(abschnitt_id, partei, wahljahr, text_prep)
