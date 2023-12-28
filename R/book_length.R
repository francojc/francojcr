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
#' @importFrom fs path_ext_remove
#' @importFrom wordcountaddin word_count
#' @importFrom purrr map_dfr
#' @importFrom dplyr summarize
#' @importFrom knitr kable
#' @importFrom tibble tibble
#'
#' @export

# Function to count the number of words in files in a quarto book project
book_length <- function(quarto_yml_path = "_quarto.yml", words_per_minute = 200) {

  # Read the _quarto.yml file
  yml_file <- yaml::yaml.load_file(input = quarto_yml_path)

  chapters_vec <-
    yml_file$book$chapters |>
    unlist() |>
    as.character()

  get_chapter_info <- function(chapter) {
    # Get the chapter name
    chapter_name <- fs::path_ext_remove(chapter)
    # Get the chapter word count
    chapter_wc <- wordcountaddin::word_count(filename = chapter)
    chapter_time <-
      chapter_wc / words_per_minute |> # Divide by specified words per minute
        as.numeric() |> # Convert to numeric
        round(digits = 1) # Round to 1 decimal place
    # Combine the results into a tibble
    chapter_info <-
      tibble::tibble(
        chapter = chapter_name,
        word_count = chapter_wc,
        read_time_mins = chapter_time
      )
    # Return the results
    return(chapter_info)
  }

  chapters_tbl <-
    purrr::map_dfr(chapters_vec, get_chapter_info)

  # Print the book summary
  message("\nBook summary: ")
  book_summary <- chapters_tbl |>
    dplyr::summarise(
      total_words = sum(word_count),
      avg_read_mins = round(mean(read_time_mins), 1),
      total_read_hours = round(sum(read_time_mins) / 60, 1)
    ) |>
    knitr::kable(format.args = list(big.mark = ","))

  print(book_summary)

  # Print the chapter info
  message("\nChapter info: ")

  chapter_info_table <- chapters_tbl |>
    knitr::kable(format.args = list(big.mark = ",", digits = 1))

  print(chapter_info_table)
}
