library(tm)

work_dir <- "D:/JK/PJN"
setwd(work_dir)

input_dir <- "./data"
output_dir <- "./results"
scripts_dir <- "./scripts"
workspaces_dir <- "./workspaces"

dir.create(output_dir, showWarnings=F)
dir.create(workspaces_dir, showWarnings=F)

create_path <- function(parent, child) {
    paste(parent, child, sep = "/")
}

corpus_dir <- create_path(
    input_dir,
    "Literatura - streszczenia - oryginał"
)

corpus <- VCorpus(
    DirSource(corpus_dir, pattern="*.txt", encoding="UTF-8"),
    readerControl=list(language="pl_PL")
)

# ------------------------

corpus <- tm_map(
    corpus,
    content_transformer(tolower)
)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

stoplist_file <- create_path(input_dir, "stopwords_pl.txt")
stoplist <- readLines(stoplist_file, encoding="UTF-8")
corpus <- tm_map(corpus, removeWords, stoplist)

corpus <- tm_map(corpus, stripWhitespace)