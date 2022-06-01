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