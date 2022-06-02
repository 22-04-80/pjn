# załadowanie bibliotek
library(tm)

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
  "documents_processed"
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

# usunięcie rozszerzeń z nazw plików w korpusie
corpus <- tm_map(corpus, cut_extensions)

# utworzenie macierzy częstości
tdm_tf_all <- TermDocumentMatrix(corpus)
tdm_tfidf_all <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
tdm_tf_2_16 <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
tdm_tfidf_2_16 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)
dtm_tf_all <- DocumentTermMatrix(corpus)
dtm_tfidf_all <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
dtm_tf_2_16 <- DocumentTermMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
dtm_tfidf_2_16 <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)

# przekształcenie macierzy rzadkich do macierzy klasycznych
tdm_tf_all_m <- as.matrix(tdm_tf_all)
tdm_tfidf_all_m <- as.matrix(tdm_tfidf_all)
tdm_tf_2_16_m <- as.matrix(tdm_tf_2_16)
tdm_tfidf_2_16_m <- as.matrix(tdm_tfidf_2_16)
dtm_tf_all_m <- as.matrix(dtm_tf_all)
dtm_tfidf_all_m <- as.matrix(dtm_tfidf_all)
dtm_tf_2_16_m <- as.matrix(dtm_tf_2_16)
dtm_tfidf_2_16_m <- as.matrix(dtm_tfidf_2_16)

# utworzenie katalogu na macierze
matrixes_dir <- create_path(
  output_dir,
  "matrices"
)
dir.create(matrixes_dir, showWarnings = F)

# eksport macierzy do pliku
matrix_file <- create_path(
  matrixes_dir,
  "tdm_tf_all.csv"
)
write.table(
  tdm_tf_all_m,
  matrix_file,
  sep = ";",
  dec = ",",
  col.names = NA
)
matrix_file <- create_path(
  matrixes_dir,
  "dtm_tf_all.csv"
)
write.table(
  dtm_tf_all_m,
  matrix_file,
  sep = ";",
  dec = ",",
  col.names = NA
)