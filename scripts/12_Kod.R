### FUNCTIONS.R
# zdefiniowanie funkcji do tworzenia ścieżek dostępu
create_path <- function(parent, child){
  paste(
    parent,
    child,
    sep = "/"
  )
}

# zdefiniowanie własnych funkcji transformujących
# funkcja do usuwania pojedynczych znaków
remove_char <- content_transformer(
  function(text, char){
    gsub(char, "", text)
  }
)

# funkcja do usuwania z tekstu podziału na akapity
paste_paragraphs <- function(text){
  paste(text, collapse = " ")
}

# funkcja do usuwania rozszerzeń z nazw plików
cut_extensions <- function(document, ext = "txt"){
  meta(document, "id") <- gsub(
    paste("\\.", ext, "$", sep = ""),
    "",
    meta(document, "id")
  )
  return(document)
}

### PREPROCESSING.R
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

### FREEQUENCY_MATRIX.R
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

### REDUCTION.R
# załadowanie bibliotek
library(lsa)

# zmiana katalogu roboczego
work_dir <- "/Users/jakubkret/Edu/uek/s4/PJN/pjn"
setwd(work_dir)

# zdefiniowanie lokalizacji katalogu ze skryptami
scripts_dir <- "./scripts"

# wykonanie skryptu frequency_matrix.R
source_file <- paste(
  scripts_dir,
  "frequency_matrix.R",
  sep = "/"
)
eval(
  parse(
    source_file,
    encoding = "UTF-8"
  )
)

# utworzenie katalogu na wykresy
plots_dir <- create_path(
  output_dir,
  "plots"
)
dir.create(plots_dir, showWarnings = F)

# analiza głównych składowych
pca <- prcomp(dtm_tfidf_3_15)

# wykres dokumentów w przestrzeni dwuwymiarowej
x <- pca$x[,1]
y <- pca$x[,2]
legend <- paste(
  paste(
    "d",
    1:length(rownames(dtm_tfidf_3_15)),
    sep = ""
  ),
  rownames(dtm_tfidf_3_15),
  sep = " - "
)

plot_file <- create_path(
  plots_dir,
  "pca.png"
)
png(plot_file, width = 800, height = 600)
plot(
  x, 
  y
)
text(
  x, 
  y, 
  paste(
    "d",
    1:length(rownames(dtm_tfidf_3_15)),
    sep = ""
  ),
  pos = 4
)
legend(
  "bottomright",
  legend,
  cex = 0.6
)

dev.off()

# analiza ukrytych wymiarów semantycznych
# dekompozycja wg wartości osobliwych
lsa <- lsa(tdm_tf_2_16)

# wykres dokumentów w przestrzeni dwuwymiarowej
coord_docs <- lsa$dk%*%diag(lsa$sk)
coord_terms <- lsa$tk%*%diag(lsa$sk)
terms_importance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
important_terms <- names(tail(sort(terms_importance),30))
coord_important_terms <- coord_terms[important_terms,]
own_terms <- c("antenat", "achilles", "wojna", "mars", "ziemianin", "myrton", "bóstwo", "grunwald", "bogini", "bóg", "holden", "naomi", "kamień", "matematyka", "egipt", "filozofia")
coord_own_terms <- coord_terms[own_terms,]
coord_plot_terms <- coord_own_terms

x1 <- coord_docs[,1]
y1 <- coord_docs[,2]

x2 <- coord_plot_terms[,1]
y2 <- coord_plot_terms[,2]

legend <- paste(
  paste(
    "d",
    1:length(colnames(tdm_tf_2_16)),
    sep = ""
  ),
  colnames(tdm_tf_2_16),
  sep = " - "
)

plot_file <- create_path(
  plots_dir,
  "lsa.png"
)
png(plot_file, width = 800, height = 600)
plot(
  x1, 
  y1,
  xlim = c(-20,20)
)
text(
  x1, 
  y1, 
  paste(
    "d",
    1:length(rownames(tdm_tf_2_16)),
    sep = ""
  ),
  pos = 4
)
points(
  x2,
  y2,
  pch = 2
)
text(
  x2,
  y2,
  rownames(coord_plot_terms)
)
legend(
  "top",
  legend,
  cex = 0.6
)

dev.off()

### CLUSTERING.R
# załadowanie bibliotek
library(proxy)
library(dendextend)
library(corrplot)
library(flexclust)

# zmiana katalogu roboczego
work_dir <- "/Users/jakubkret/Edu/uek/s4/PJN/pjn"
setwd(work_dir)

# zdefiniowanie lokalizacji katalogu ze skryptami
scripts_dir <- "./scripts"

# wykonanie skryptu frequency_matrix.R
source_file <- paste(
  scripts_dir,
  "frequency_matrix.R",
  sep = "/"
)
eval(
  parse(
    source_file,
    encoding = "UTF-8"
  )
)

# utworzenie katalogu na wykresy
clusters_dir <- create_path(
  output_dir,
  "clusters"
)
dir.create(clusters_dir, showWarnings = F)

# analiza skupień
# metoda hierarchiczna
# parametry:
# 1. macierz częstości
# a. waga (tf, tfidf, bin, log)
# b. zakres zmiennych (bounds)
# 2. miara odległości (euclidean, manhatan, jaccard, cosine)
# 3. sposób wyznaczania odległości skupień (single, complete, ward.D2)

# przygotowanie
doc_names <- rownames(dtm_tf_2_16)
doc_count <- length(doc_names)
legend <- paste(
  paste(
    "d",
    1:doc_count,
    sep = ""
  ),
  doc_names,
  sep = " - "
)
rownames(dtm_tf_2_16_m) <-  paste(
  "d",
  1:doc_count,
  sep = ""
)
rownames(dtm_tf_3_15_m) <-  paste(
  "d",
  1:doc_count,
  sep = ""
)
rownames(dtm_tf_4_14_m) <-  paste(
  "d",
  1:doc_count,
  sep = ""
)

clusters_pattern <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3)
colors <- c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen", "hotpink", "khaki")
colors_pattern <- c()
for (d in 1:doc_count) {
  colors_pattern[d] <- colors[clusters_pattern[d]]
}
names(clusters_pattern) <- paste(
  "d",
  1:doc_count,
  sep = ""
)

names(colors_pattern) <- paste(
  "d",
  1:doc_count,
  sep = ""
)
par(mai=c(1,1,1,1))

# eksperyment 1
dist_matrix_1 <- dist(dtm_tf_2_16_m, method = "euclidean")
h_clust_1 <- hclust(dist_matrix_1, method = "complete")

plot_file1 <- create_path(
  clusters_dir,
  "e1.png"
)
png(plot_file1, width = 800, height = 600)

plot(h_clust_1)
legend(
  "topright",
  legend,
  cex = 0.4
)
dendrogram_1 <- as.dendrogram(h_clust_1)
clust_count_1 <- find_k(dendrogram_1)$k
colored_dendrogram_1 <- color_branches(
  dendrogram_1,
  k = clust_count_1
)
plot(colored_dendrogram_1)
clusters_1 <- cutree(h_clust_1, k = clust_count_1)
clusters_matrix_1 <- matrix(
  0,
  doc_count,
  clust_count_1
)
rownames(clusters_matrix_1) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_1[doc_no, clusters_1[doc_no]] <- 1
}
corrplot(clusters_matrix_1)

# eksperyment 2
dist_matrix_2 <- dist(dtm_tf_2_16_m, method = "jaccard")
h_clust_2 <- hclust(dist_matrix_2, method = "single")

plot_file2 <- create_path(
  clusters_dir,
  "e2.png"
)
png(plot_file2, width = 800, height = 600)

plot(h_clust_2)
legend(
  "topright",
  legend,
  cex = 0.4
)
dendrogram_2 <- as.dendrogram(h_clust_2)
clust_count_2 <- find_k(dendrogram_2)$k
colored_dendrogram_2 <- color_branches(
  dendrogram_2,
  k = clust_count_2
)

plot(colored_dendrogram_2)
clusters_2 <- cutree(h_clust_2, k = clust_count_2)
clusters_matrix_2 <- matrix(
  0,
  doc_count,
  clust_count_2
)
rownames(clusters_matrix_2) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_2[doc_no, clusters_2[doc_no]] <- 1
}
corrplot(clusters_matrix_2)

# eksperyment 3
dist_matrix_3 <- dist(dtm_tf_2_16_m, method = "cosine")
h_clust_3 <- hclust(dist_matrix_3, method = "ward.D2")

plot_file3 <- create_path(
  clusters_dir,
  "e3.png"
)
png(plot_file3, width = 800, height = 600)

plot(h_clust_3)
legend(
  "topright",
  legend,
  cex = 0.4
)
dendrogram_3 <- as.dendrogram(h_clust_3)
clust_count_3 <- find_k(dendrogram_3)$k
colored_dendrogram_3 <- color_branches(
  dendrogram_3,
  k = clust_count_3
)
plot(colored_dendrogram_3)
clusters_3 <- cutree(h_clust_3, k = clust_count_3)
clusters_matrix_3 <- matrix(
  0,
  doc_count,
  clust_count_3
)
rownames(clusters_matrix_3) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_3[doc_no, clusters_3[doc_no]] <- 1
}
corrplot(clusters_matrix_3)

### TODO METODY NIEHIERARHICZNE

### TOPIC_MODELING.R
# załadowanie bibliotek
library(topicmodels)

# zmiana katalogu roboczego
work_dir <- "/Users/jakubkret/Edu/uek/s4/PJN/pjn"
setwd(work_dir)

# zdefiniowanie lokalizacji katalogu ze skryptami
scripts_dir <- "./scripts"

# wykonanie skryptu frequency_matrix.R
source_file <- paste(
  scripts_dir,
  "frequency_matrix.R",
  sep = "/"
)
eval(
  parse(
    source_file,
    encoding = "UTF-8"
  )
)

# utworzenie katalogu na wykresy
topics_dir <- create_path(
  output_dir,
  "topics"
)
dir.create(topics_dir, showWarnings = F)

# analiza ukrytej alokacji Dirichlet'a dla 4 tematów
# utworzenie katalogu na wykresy
topics_4_dir <- create_path(
  topics_dir,
  "topics_4"
)
dir.create(topics_4_dir, showWarnings = F)

topics_count <- 4
lda <- LDA(
  dtm_tf_2_16,
  k = topics_count,
  method = "Gibbs",
  control = list(
    burnin = 2000, 
    thin = 100,
    iter = 3000
  )
)

results <- posterior(lda)
topics_colors <- c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen", "hotpink")

for (t in 1:topics_count) {
  topic_file <- create_path(
    topics_4_dir,
    paste("temat_", t, ".png", sep = "")
  )
  png(topic_file)
  par(mai=c(1,2,1,1))
  topic <- tail(sort(results$terms[t,]),20)
  barplot(
    topic,
    horiz = T,
    las = 1,
    main = paste("Temat", t),
    xlab = "Prawdopodobieństwo",
    col = topics_colors[t]
  )
  dev.off()
}

for (d in 1:length(lda@documents)) {
  doc_file <- create_path(
    topics_4_dir,
    paste(rownames(results$topics)[d],".png", sep = "")
  )
  png(doc_file)
  par(mai=c(1,2,1,1))
  document <- results$topics[d,]
  barplot(
    document,
    las = 1,
    main = rownames(results$topics)[d],
    xlab = "Prawdopodobieństwo",
    col = topics_colors
  )
  dev.off()
}

par(mai=c(1,4,1,1))
barplot(
  t(results$topics),
  horiz = T,
  col = topics_colors,
  las = 1
)

# analiza ukrytej alokacji Dirichlet'a dla 3 tematów
topics_3_dir <- create_path(
  topics_dir,
  "topics_3"
)
dir.create(topics_3_dir, showWarnings = F)

topics_count <- 3
lda <- LDA(
  dtm_tf_2_16,
  k = topics_count,
  method = "Gibbs",
  control = list(
    burnin = 2000, 
    thin = 100,
    iter = 3000
  )
)

results <- posterior(lda)
topics_colors <- c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen", "hotpink")

for (t in 1:topics_count) {
  topic_file <- create_path(
    topics_3_dir,
    paste("temat_", t, ".png", sep = "")
  )
  png(topic_file)
  par(mai=c(1,2,1,1))
  topic <- tail(sort(results$terms[t,]),20)
  barplot(
    topic,
    horiz = T,
    las = 1,
    main = paste("Temat", t),
    xlab = "Prawdopodobieństwo",
    col = topics_colors[t]
  )
  dev.off()
}

for (d in 1:length(lda@documents)) {
  doc_file <- create_path(
    topics_3_dir,
    paste(rownames(results$topics)[d],".png", sep = "")
  )
  png(doc_file)
  par(mai=c(1,2,1,1))
  document <- results$topics[d,]
  barplot(
    document,
    las = 1,
    main = rownames(results$topics)[d],
    xlab = "Prawdopodobieństwo",
    col = topics_colors
  )
  dev.off()
}

par(mai=c(1,4,1,1))
barplot(
  t(results$topics),
  horiz = T,
  col = topics_colors,
  las = 1
)

# analiza ukrytej alokacji Dirichlet'a dla 5 tematów
topics_5_dir <- create_path(
  topics_dir,
  "topics_5"
)
dir.create(topics_5_dir, showWarnings = F)

topics_count <- 5
lda <- LDA(
  dtm_tf_2_16,
  k = topics_count,
  method = "Gibbs",
  control = list(
    burnin = 2000, 
    thin = 100,
    iter = 3000
  )
)

results <- posterior(lda)
topics_colors <- c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen", "hotpink")

for (t in 1:topics_count) {
  topic_file <- create_path(
    topics_5_dir,
    paste("temat_", t, ".png", sep = "")
  )
  png(topic_file)
  par(mai=c(1,2,1,1))
  topic <- tail(sort(results$terms[t,]),20)
  barplot(
    topic,
    horiz = T,
    las = 1,
    main = paste("Temat", t),
    xlab = "Prawdopodobieństwo",
    col = topics_colors[t]
  )
  dev.off()
}

for (d in 1:length(lda@documents)) {
  doc_file <- create_path(
    topics_5_dir,
    paste(rownames(results$topics)[d],".png", sep = "")
  )
  png(doc_file)
  par(mai=c(1,2,1,1))
  document <- results$topics[d,]
  barplot(
    document,
    las = 1,
    main = rownames(results$topics)[d],
    xlab = "Prawdopodobieństwo",
    col = topics_colors
  )
  dev.off()
}

par(mai=c(1,4,1,1))
barplot(
  t(results$topics),
  horiz = T,
  col = topics_colors,
  las = 1
)

### KEYWORDS.R
# załadowanie bibliotek
library(wordcloud)

# zmiana katalogu roboczego
work_dir <- "/Users/jakubkret/Edu/uek/s4/PJN/pjn"
setwd(work_dir)

# zdefiniowanie lokalizacji katalogu ze skryptami
scripts_dir <- "./scripts"

# wykonanie skryptu frequency_matrix.R
source_file <- paste(
  scripts_dir,
  "frequency_matrix.R",
  sep = "/"
)
eval(
  parse(
    source_file,
    encoding = "UTF-8"
  )
)

# utworzenie katalogu na wykresy
clouds_dir <- create_path(
  output_dir,
  "clouds"
)
dir.create(clouds_dir, showWarnings = F)

# waga tf jako miara istotności słów
for (d in 1:length(corpus)) {
  print(rownames(dtm_tf_2_16_m)[d])
  print(head(sort(dtm_tf_2_16_m[d,], decreasing = T)))
}

# waga tfidf jako miara istotności słów
for (d in 1:length(corpus)) {
  print(rownames(tdm_tfidf_3_15_m)[d])
  print(head(sort(tdm_tfidf_3_15_m[d,], decreasing = T)))
}

# prawdopodobieństwo w LDA jako miara istotności słów
for (d in 1:length(corpus)) {
  terms_importance <- c(results$topics[d,]%*%results$terms)
  names(terms_importance) <- colnames(results$terms)
  print(rownames(results$topics)[d])
  print(head(sort(terms_importance, decreasing = T)))
}

# chmury tagów
for (d in 1:length(corpus)) {
  cloud_file <- create_path(
    clouds_dir,
    paste(corpus[[d]]$meta$id,".png", sep = "")
  )
  png(cloud_file)
  par(mai=c(0,0,0,0))
  wordcloud(
    corpus[d],
    max.words = 200,
    colors = brewer.pal(8,"Spectral")
  )
  dev.off()
}

