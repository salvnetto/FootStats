#' Load Football Data from the Repository
#'
#' This function retrieves football data from an online repository based on the selected country, league, and data file type.
#'
#' @param country A character string specifying the country of the league. Must be one of the following:
#'   - "brazil"
#'   - "england"
#'   - "spain"
#'   - "italy"
#'   - "germany"
#'
#' @param league A character string specifying the league within the chosen country. Valid options per country:
#'   - **brazil**: "brasileirao_a", "brasileirao_b"
#'   - **england**: "premier_league"
#'   - **spain**: "laliga"
#'   - **italy**: "serie_a"
#'   - **germany**: "bundesliga"
#'
#' @param file A character string indicating the type of data file to load. Valid options include "match_history" and "squad".
#'
#' @details
#' The function constructs a URL to retrieve the selected dataset from the repository and attempts to load the data into a data frame.
#'
#' @return A data frame containing the requested football data.
#'
#' @examples
#' # Load processed match history data for Brazil's Série A
#' df <- load_data(country = "brazil", league = "brasileirao_a", file = "match_history")
#'
#' # Load squad data for the English Premier League
#' df <- load_data(country = "england", league = "premier_league", file = "squad")
#'
#' @export

load_data = function(
    country,
    league,
    file = "match_history"
) {
  # Load JSON structure
  json_url <- "https://raw.githubusercontent.com/salvnetto/FootStats-Data/main/database/leagues.json"
  json_data <- jsonlite::fromJSON(httr::content(httr::GET(json_url), "text", encoding = "UTF-8"))

  # Validate country
  valid_countries <- names(json_data)
  country <- match.arg(country, choices = valid_countries)

  # Validate league
  valid_leagues <- names(json_data[[country]])
  league <- match.arg(league, choices = valid_leagues)

  # Validate file type
  valid_file = c("match_history", "squad")
  file <- match.arg(file, choices = valid_file)

  data_type <- "processed_data"

  url <- paste0(
    "https://raw.githubusercontent.com/salvnetto/FootStats-Data/main/datasets/",
    data_type, "/",
    league, "/",
    file, ".csv"
  )

  df <- tryCatch(
    {
      utils::read.csv(url(url))
    },
    error = function(e) {
      stop("Failed to load data from the provided URL: ", e$message)
    }
  )

  return(df)
}
