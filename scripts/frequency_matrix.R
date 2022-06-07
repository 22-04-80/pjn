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


# utworzenie katalogu na macierze
matrixes_dir <- create_path(
  output_dir,
  "matrices"
)
dir.create(matrixes_dir, showWarnings = F)

# 2_16_tf DTM i TDM

tdm_tf_2_16 <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
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

tdm_tf_2_16_m <- as.matrix(tdm_tf_2_16)
dtm_tf_2_16_m <- as.matrix(dtm_tf_2_16)

# eksport macierzy do pliku
matrix_file <- create_path(
  matrixes_dir,
  "tdm_tf_2_16.csv"
)
write.table(
  tdm_tf_2_16_m,
  matrix_file,
  sep = ";",
  dec = ",",
  col.names = NA
)

matrix_file <- create_path(
  matrixes_dir,
  "dtm_tf_2_16.csv"
)
write.table(
  dtm_tf_2_16_m,
  matrix_file,
  sep = ";",
  dec = ",",
  col.names = NA
)

# 3_15_weightTfIdf DTM i TDM

tdm_tfidf_3_15 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(3,15)
    )
  )
)

dtm_tfidf_3_15 <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(3,15)
    )
  )
)

tdm_tfidf_3_15_m <- as.matrix(tdm_tfidf_3_15)
dtm_tfidf_3_15_m <- as.matrix(dtm_tfidf_3_15)

# eksport macierzy do pliku
matrix_file <- create_path(
  matrixes_dir,
  "tdm_tfidf_3_15.csv"
)
write.table(
  tdm_tfidf_3_15_m,
  matrix_file,
  sep = ";",
  dec = ",",
  col.names = NA
)

matrix_file <- create_path(
  matrixes_dir,
  "dtm_tfidf_3_15.csv"
)
write.table(
  dtm_tfidf_3_15_m,
  matrix_file,
  sep = ";",
  dec = ",",
  col.names = NA
)


# 4_14_tf DTM i TDM

tdm_tf_4_14 <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(4,14)
    )
  )
)

dtm_tf_4_14 <- DocumentTermMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(4,14)
    )
  )
)

tdm_tf_4_14_m <- as.matrix(tdm_tf_4_14)
dtm_tf_4_14_m <- as.matrix(dtm_tf_4_14)

# eksport macierzy do pliku
matrix_file <- create_path(
  matrixes_dir,
  "tdm_tf_4_14.csv"
)
write.table(
  tdm_tf_4_14_m,
  matrix_file,
  sep = ";",
  dec = ",",
  col.names = NA
)

matrix_file <- create_path(
  matrixes_dir,
  "dtm_tf_4_14.csv"
)
write.table(
  dtm_tf_4_14_m,
  matrix_file,
  sep = ";",
  dec = ",",
  col.names = NA
)
