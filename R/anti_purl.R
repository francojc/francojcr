#' Extracts contents of an R Markdown or Quarto file
#'
#' This function extracts the prose from a file and optionally copies it to the clipboard. In this way, it is the opposite of the purl function.
#'
#' @param file_path The path to the file.
#' @param clip Logical value indicating whether to copy the prose to the clipboard. Default is FALSE.
#'
#' @return If clip is FALSE, the function returns the extracted prose. If clip is TRUE, the function copies the prose to the clipboard.
#'
#' @examples
#' \dontrun{
#' anti_purl("/path/to/file.Rmd", clip = TRUE)
#' }
#' @importFrom clipr clipr_available write_clip
#'
#' @export
anti_purl <- function(file_path, clip = FALSE) {
  # Extract prose
  prose <- extract_prose(file_path)

  # Write prose to clipboard
  if (clip) {
    if (clipr::clipr_available()) {
      clipr::write_clip(prose)
      message("Prose copied to clipboard")
    } else {
      message("clipr not available. Prose not copied to clipboard")
    }
  } else {
    return(prose)
    message("Prose extracted")
  }
}
