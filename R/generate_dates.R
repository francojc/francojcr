#' Generate Dates
#'
#' This function generates a sequence of dates based on the specified start and end dates,
#' and filters the dates to include only the selected days of the week.
#'
#' @param start_date The start date of the date sequence in "YYYY-MM-DD" format.
#' @param end_date The end date of the date sequence in "YYYY-MM-DD" format.
#' @param days_of_week A string specifying the days of the week to include in the date sequence.
#' Each character represents a day of the week (e.g., "m" for Monday, "t" for Tuesday).
#' @param locale The locale to use for determining the day names. Default is "us" (United States).
#' If set to "es" (Spanish), the dates will take European format (DD-MM-YYYY) and the day names will be translated to Spanish.
#'
#' @return A data frame with two columns: 'date' (formatted dates) and 'day' (day names).
#'
#' @importFrom lubridate ymd wday
#' @importFrom stats setNames
#' @examples
#' \dontrun{
#' generate_dates("2024-01-17", "2024-05-01", "wf", locale = "es")
#' }
#' @export
generate_dates <- function(start_date, end_date, days_of_week, locale = "us") {
  # Create a sequence of dates from start_date to end_date
  date_sequence <- seq(from = lubridate::ymd(start_date), to = lubridate::ymd(end_date), by = "day")

  # Define a named vector to match days_of_week with lubridate::wday() values
  days_lookup <- c(m = 2, t = 3, w = 4, r = 5, f = 6, s = 7, u = 1)

  # Convert the days_of_week string to a vector of lubridate::wday() values
  selected_days <- unlist(strsplit(days_of_week, ""))
  selected_wdays <- days_lookup[selected_days]

  # Filter the date_sequence to include only the selected days of the week
  filtered_dates <- date_sequence[lubridate::wday(date_sequence) %in% selected_wdays]

  # Determine the day names based on locale
  if (locale == "es") {
    day_names <- weekdays(filtered_dates, abbreviate = FALSE)
    # Translate English day names to Spanish
    day_translation <- setNames(
      c("domingo", "lunes", "martes", "mi\\u00e9rcoles", "jueves", "viernes", "s\\u00e1bado"),
      c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    )
    day_names <- day_translation[day_names]
    # Format the dates as DD-MM-YYYY
    formatted_dates <- format(filtered_dates, "%d-%m-%Y")
  } else { # Default to US locale
    day_names <- lubridate::wday(filtered_dates, label = TRUE, abbr = FALSE)
    # Keep the dates in YYYY-MM-DD format
    formatted_dates <- as.character(filtered_dates)
  }

  # Create a data frame with the 'date' and 'day' columns
  results_df <- data.frame(date = formatted_dates, day = day_names)

  return(results_df)
}
