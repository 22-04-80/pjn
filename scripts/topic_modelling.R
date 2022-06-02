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

# analiza ukrytej alokacji Dirichlet'a
topics_count <- 6
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
    topics_dir,
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
    topics_dir,
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
