#' Get the path to a language-specific `udpipe` model
#'
#' This function will search for the desired udpipe language model path and
#'  download it if it is not found.
#' @param model_name The name of the language model to identify
#' @keywords cleanNLP, udpipe
#' @export
#' @examples
#' \dontrun{
#' # Search and return the path to the Spanish language model
#' spanish_model <- get_udpipe_model_path(model_lang = "spanish")
#' }
#' get_udpipe_model_path()

get_udpipe_model_path <- function(model_lang) {
  pacman::p_load(magrittr, stringr, cleanNLP) # load required packages
  model_lang <- tolower(model_lang) # lowercase the model_name
  base_path <- system.file("extdata", package = "cleanNLP") # get base path
  message("Checking for existing model")
  model_name <- # get model file name
    base_path %>% dir() %>% str_subset(paste0("^", model_lang))
  if(length(model_name) == 0) { 
    message(" Model not found, downloading now...")
    cnlp_download_udpipe(model_lang) # download udpipe model, cleanNLP dir
    model_name <- # get model file name
      base_path %>% dir() %>% str_subset(paste0("^", model_lang))
    message("Verifying model path")
    if(length(model_name) == 0) { 
      stop("Error finding downloaded model")
    }
  }
  model_path <- file.path(base_path, model_name, fsep = "/") # create model path
  message(" Model found")
  return(model_path)
}
