# załadowanie bibliotek
library(tm)
library(hunspell)

# zmiana katalogu roboczego
work_dir <- "/Users/jakubkret/Edu/uek/s4/PJN/pjn"
setwd(work_dir)

# zdefiniowanie katalogów funkcjonalnych
input_dir <- "./data"
output_dir <- "./results"
scripts_dir <- "./scripts"
workspaces_dir <- "./workspaces"

# utworzenie katalogów wynikowych
dir.create(output_dir, showWarnings = F)
dir.create(workspaces_dir, showWarnings = F)

# wykonaie skryptu z definicjami funkcji
source_file <- paste(
  scripts_dir,
  "functions.R",
  sep = "/"
)
source(source_file)

# utworzenie korpusu dokumentów
corpus_dir <- create_path(
  input_dir,
  "documents_truncated"
)

corpus <- VCorpus(
  DirSource(
    corpus_dir,
    #		pattern = "*.txt",
    encoding = "UTF-8"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

# wstępne przetwarzanie
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

stoplist_file <- create_path(
  input_dir,
  "stopwords_pl.txt"
)
stoplist <- readLines(
  stoplist_file, 
  encoding = "UTF-8"
)
corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace)

# wywołanie własnych funkcji transformujących
corpus <- tm_map(corpus, remove_char, intToUtf8(8722))
corpus <- tm_map(corpus, remove_char, intToUtf8(190))
corpus <- tm_map(corpus, content_transformer(paste_paragraphs))
corpus <- tm_map(corpus, cut_extensions)
corpus <- tm_map(corpus, stripWhitespace)

# zdefiniowanie funkcji do lematyzacji
polish <- dictionary("pl_PL")
lemmatize <- function(text){
  vectorized_text <- unlist(hunspell_parse(text, dict = polish))
  lemmatized_vectorized_text <- hunspell_stem(vectorized_text, dict = polish)
  for (i in 1:length(lemmatized_vectorized_text)) {
    if (length(lemmatized_vectorized_text[[i]]) == 0){
      lemmatized_vectorized_text[i] <- vectorized_text[i]
    }
    if (length(lemmatized_vectorized_text[[i]])  > 1){
      lemmatized_vectorized_text[i] <- lemmatized_vectorized_text[[i]][1]
    }
  }
  lemmatized_vectorized_text <- unlist(lemmatized_vectorized_text)
  lemmatized_text <- paste(lemmatized_vectorized_text, collapse = " ")
  return(lemmatized_text)
}
corpus <- tm_map(corpus, content_transformer(lemmatize))

# eksport zawartości kurpusu do plików tekstowych
preprocessed_dir <- create_path(
  input_dir,
  "documents_processed"
)
dir.create(preprocessed_dir, showWarnings = F)
writeCorpus(corpus, preprocessed_dir)
