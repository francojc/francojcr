#' Function to count the number of words in files in a quarto book project
#'
#' This function takes the path to the `_quarto.yml` file and the average number of words
#' a person can read per minute as input. It reads the `_quarto.yml` file to get the list
#' of chapters in the book project. For each chapter, it calculates the word count and
#' the estimated reading time in minutes based on the average words per minute. It then
#' returns a summary of the book, including the total word count, average reading time,
#' and total reading time in hours, as well as a table with detailed information for each chapter.
#'
#' @param quarto_yml_path The path to the `_quarto.yml` file (default: "_quarto.yml").
#' @param words_per_minute The average number of words a person can read per minute (default: 200).
#'
#' @return None (prints the book summary and chapter info).
#'
#' @examples
#' \dontrun{
#' count_words_in_book("_quarto.yml", 200)
#' }
#'
#' @importFrom yaml yaml.load_file
#' @importFrom wordcountaddin word_count
#' @importFrom knitr kable
#'
#' @export
book_length <- function(quarto_yml_path = "_quarto.yml", words_per_minute = 200) {
  # Read the _quarto.yml file
  yml_file <- yaml::yaml.load_file(input = quarto_yml_path)

  chapters_vec <- unlist(yml_file$book$chapters)

  # Initialize vectors to store chapter information
  chapters_names <- vector("character", length(chapters_vec))
  word_counts <- numeric(length(chapters_vec))
  read_times <- numeric(length(chapters_vec))

  # Loop through each chapter to get information
  for (i in seq_along(chapters_vec)) {
    chapter <- chapters_vec[i]
    # Get the chapter name without extension
    chapters_names[i] <- tools::file_path_sans_ext(basename(chapter))
    # Get the chapter word count
    word_counts[i] <- wordcountaddin::word_count(filename = chapter)
    # Calculate the chapter read time and round it
    read_times[i] <- round(word_counts[i] / words_per_minute, 1)
  }

  # Create a data frame with all chapter information
  chapters_tbl <- data.frame(
    chapter = chapters_names,
    word_count = word_counts,
    read_time_mins = read_times
  )

  # Calculate book summary statistics
  total_words <- sum(word_counts)
  avg_read_mins <- round(mean(read_times), 1)
  total_read_hours <- round(sum(read_times) / 60, 1)

  # Create a data frame for book summary
  book_summary <- data.frame(
    total_words = total_words,
    avg_read_mins = avg_read_mins,
    total_read_hours = total_read_hours
  )

  # Print the book summary
  message("\nBook summary: ")
  print(knitr::kable(book_summary, format.args = list(big.mark = ",")))

  # Print the chapter info
  message("\nChapter info: ")
  print(knitr::kable(chapters_tbl, digits = 1, format.args = list(big.mark = ",")))
}
