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
