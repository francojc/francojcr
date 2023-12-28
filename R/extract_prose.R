
#' Extracts prose from an R Markdown or Quarto file, removing yaml header, code, comments, and callout formatting.
#'
#' This function takes a file path as input and reads the contents of the file.
#' It then removes code chunks, HTML comments, YAML metadata, inline code,
#' inline attributes, colons, and excess whitespace to extract the prose from
#' the file.
#'
#' @param file_path The path to the file to extract prose from.
#' @return A character vector containing the extracted prose.
#' @examples
#' \dontrun{
#' file_path <- "/path/to/file.Rmd"
#' prose <- extract_prose(file_path)
#' print(prose)
#' }
#' @importFrom stringr str_c str_replace_all str_remove_all str_replace_all str_trim
extract_prose <- function(file_path) {
  prose <-
    file_path |>
    readLines() |>
    stringr::str_c(collapse = "  ") |>
    stringr::str_remove_all(pattern = "\\s*```\\{.*?```")  |> # code chunks
    stringr::str_remove_all(pattern = "<!--.*?-->") |> # html comments
    stringr::str_remove_all(pattern = "---.*?---") |> # yaml
    stringr::str_remove_all(pattern = "`+r.*?`+\\s*") |> # inline code
    stringr::str_remove_all(pattern = "\\{.*?\\}") |> # inline attributes
    stringr::str_remove_all(pattern = "[:]+") |> # colons
    stringr::str_replace_all(pattern = "\\s{2}", "\n") |>
    stringr::str_replace_all(pattern = "\\n{3,}", "\n\n") |>
    stringr::str_trim("both")

  return(prose)
}
