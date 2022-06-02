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
doc_names <- rownames(dtm_tf_all)
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
rownames(dtm_tf_all_m) <-  paste(
  "d",
  1:doc_count,
  sep = ""
)
rownames(dtm_bin_all_m) <-  paste(
  "d",
  1:doc_count,
  sep = ""
)
rownames(dtm_tfidf_2_16_m) <-  paste(
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
dist_matrix_1 <- dist(dtm_tf_all_m, method = "euclidean")
h_clust_1 <- hclust(dist_matrix_1, method = "complete")

plot_file1 <- create_path(
  clusters_dir,
  "e1.png"
)
png(plot_file1, width = 1920, height = 1024)

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
dist_matrix_2 <- dist(dtm_bin_all_m, method = "jaccard")
h_clust_2 <- hclust(dist_matrix_2, method = "single")

plot_file2 <- create_path(
  clusters_dir,
  "e2.png"
)
png(plot_file2, width = 1920, height = 1024)

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
dist_matrix_3 <- dist(dtm_tf_all_m, method = "cosine")
h_clust_3 <- hclust(dist_matrix_3, method = "ward.D2")

plot_file3 <- create_path(
  clusters_dir,
  "e3.png"
)
png(plot_file3, width = 1920, height = 1024)

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

# metoda niehierarchiczna
# parametry metody:
# 1. macierz częstości
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. liczba skupień

# eksperyment 4
plot_file4 <- create_path(
  clusters_dir,
  "e4.png"
)
png(plot_file4, width = 1920, height = 1024)

clust_count_4 <- 3
k_means_4 <- kmeans(dtm_tfidf_2_16, centers = clust_count_4)
clusters_4 <- k_means_4$cluster
clusters_matrix_4 <- matrix(
  0,
  doc_count,
  clust_count_4
)
rownames(clusters_matrix_4) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_4[doc_no, clusters_4[doc_no]] <- 1
}
corrplot(clusters_matrix_4)

# porównanie wyników eksperymentów
Bk_plot(
  dendrogram_1,
  dendrogram_2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a Mallows'a",
  ylab = "Indeks Fawlkes'a Mallows'a",
)
Bk_plot(
  dendrogram_1,
  dendrogram_3,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a Mallows'a",
  ylab = "Indeks Fawlkes'a Mallows'a",
)
Bk_plot(
  dendrogram_2,
  dendrogram_3,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a Mallows'a",
  ylab = "Indeks Fawlkes'a Mallows'a",
)

rand_exp1_pattern <- comPart(clusters_1, clusters_pattern)
rand_exp2_pattern <- comPart(clusters_2, clusters_pattern)
rand_exp3_pattern <- comPart(clusters_3, clusters_pattern)
rand_exp4_pattern <- comPart(clusters_4, clusters_pattern)
rand_exp1_exp2 <- comPart(clusters_1, clusters_2)
rand_exp1_exp4 <- comPart(clusters_1, clusters_4)
rand_exp2_exp4 <- comPart(clusters_2, clusters_4)
