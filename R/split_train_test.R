#' Split Dataset into Training and Testing Sets for Sports Analytics
#'
#' This function splits a sports dataset into training and testing sets based on
#' specified season and rounds, with home venue filtering and team indexing.
#'
#' @param data A data frame containing sports match data from load_data()
#' @param season The specific season to filter data from (must be a character)
#' @param rounds_train A vector of rounds to use for training data
#' @param include_seasons A vector of other seasons to include in the training data
#' @param gen_data If the data frame is from generate_data
#'
#' @return A list containing two data frames: train and test
#'
#' @examples
#' \dontrun{
#' result <- split_train_test(
#'   data = match_data,
#'   season = '2022-2023',
#'   rounds_train = 19,
#'   include_seasons = c('2021-2022', '2020-2021')
#' )
#' train_data <- result$train
#' test_data <- result$test
#' }
#'
#' @export

split_train_test <- function(data, season, rounds_train, include_seasons = NULL, gen_data = FALSE) {
  available_seasons <- unique(as.character(data$season))
  season_filter <- match.arg(as.character(season), available_seasons)
  if (!is.null(include_seasons)) {
    include_season_filter <- available_seasons[available_seasons %in% include_seasons]
    all_seasons <- c(season_filter, include_season_filter)
  } else {
    all_seasons <- season_filter
  }

  if (!is.numeric(rounds_train)) {
    stop("'rounds_train' must be numeric.")
  }

  min_round <- min(data$round, na.rm = TRUE)
  max_round <- max(data$round, na.rm = TRUE)

  if (rounds_train < min_round || rounds_train > max_round) {
    stop(paste("'rounds_train' must be between", min_round, "and", max_round))
  }

  data_unique <- data[data$season %in% all_seasons, ]
  if (gen_data == FALSE) {
    unique_teams <- unique(c(data_unique$team_name, data_unique$opponent))
    
    data$venue <- ifelse(data$venue == "Home", 1, 0)
    data$team_name_idx <- match(data$team_name, unique_teams)
    data$opponent_idx <- match(data$opponent, unique_teams)
    data$season <- as.character(data$season)
  }

  data <- data[data$venue == 1 & data$season %in% all_seasons, ]

  r_train <- seq_len(rounds_train)
  r_test <- seq(rounds_train + 1, max(data$round, na.rm = TRUE))

  data_train <- data[data$round %in% r_train, ]
  data_test <- data[data$round %in% r_test & data$season == season_filter, ]

  return(list(train = data_train, test = data_test))
}
