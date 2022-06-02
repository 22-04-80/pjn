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
pca <- prcomp(dtm_tfidf_2_16)

# wykres dokumentów w przestrzeni dwuwymiarowej
x <- pca$x[,1]
y <- pca$x[,2]
legend <- paste(
  paste(
    "d",
    1:length(rownames(dtm_tfidf_2_16)),
    sep = ""
  ),
  rownames(dtm_tfidf_2_16),
  sep = " - "
)

plot_file <- create_path(
  plots_dir,
  "pca.png"
)
png(plot_file, width = 1920, height = 1024)
plot(
  x, 
  y
)
text(
  x, 
  y, 
  paste(
    "d",
    1:length(rownames(dtm_tfidf_2_16)),
    sep = ""
  ),
  pos = 4
)
legend(
  "top",
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
png(plot_file, width = 3656, height = 2664)
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
    1:length(rownames(dtm_tfidf_2_16)),
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
