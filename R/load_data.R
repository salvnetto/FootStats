#' Load Football Data from the repository
#'
#' This function loads football data from the repository on GitHub based on region, file type, and data processing preference.
#'
#' @param region A character string indicating the region. Valid options are "br".
#' @param file A character string specifying the data file to load. Options include "match_history", "standings", and "squads".
#'
#' @return A data frame containing the football data.
#' @export
#'
#' @examples
#' # Load processed match history data for Brazil
#' df = load_data(region = "br", file = "match_history")

load_data = function(
    region = c("br"),
    file = c("match_history", "standings", "squads")
) {
  region = match.arg(region)
  file = match.arg(file)
  data_type = "processed_data"
  #data_type = ifelse(raw, "raw_data", "processed_data")

  leagues = list(
    br = "brasileirao"
  )
  league = leagues[[region]]
  if (is.null(league)) {
    stop("Invalid region specified. Please choose from: br.")
  }

  url = paste0(
    "https://raw.githubusercontent.com/salvnetto/FootStats-Data/main/datasets/",
    data_type, "/",
    league, "/",
    file, ".csv"
  )

  df = tryCatch(
    {
      utils::read.csv(url(url))
    },
    error = function(e) {
      stop("Failed to load data from the provided URL: ", e$message)
    }
  )

  return(df)
}
