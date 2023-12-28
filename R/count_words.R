library(yaml)
library(tokenizers)

get_section_info <- function(section) {

  # Get the section name
  section_name <- gsub("\\.(qmd|Rmd)", "", section)

  # Extract prose
  text <- extract_prose(section)

  # Count the words
  word_count <-
    count_words(text) |>
    format(big.mark = ",")

  # Data frame
  section_info_df <- data.frame(section = section_name, word_count)
  # Return the word counts
  return(section_info_df)
}

get_sections <- function(type = "book") {

  if(type == "book") {
    input <- "_quarto.yml"
  } else {
    stop("Invalid type")
  }

  # Read the YAML file
  yaml <- yaml::yaml.load_file(input)

  # Get the sections
  sections <-
    yaml$book$chapters |>
    unlist() |>
    as.character()

  # Return the sections
  return(sections)
}

count_words <- function(text) {
  # Split the text into words
  words <-
    tokenizers::tokenize_words(
      x = text,
      lowercase = FALSE,
      simplify = TRUE
    )

  # Count the words
  word_count <- length(words)

  # Return the word counts
  return(word_count)
}
