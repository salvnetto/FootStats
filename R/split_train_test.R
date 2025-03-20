#' Split Dataset into Training and Testing Sets for Sports Analytics
#'
#' This function splits a sports dataset into training and testing sets based on
#' specified season and rounds, with home venue filtering and team indexing.
#'
#' @param data A data frame containing sports match data from load_data()
#' @param season The specific season to filter data from (must be a character)
#' @param rounds_train A vector of rounds to use for training data
#'
#' @return A list containing two data frames: train and test
#'
#' @examples
#' \dontrun{
#' result <- split_train_test(
#'   data = match_data,
#'   season = '2022-2023',
#'   rounds_train = 19
#' )
#' train_data <- result$train
#' test_data <- result$test
#' }
#'
#' @export

split_train_test <- function(data, season, rounds_train) {
  available_seasons <- unique(as.character(data$season))
  season_filter <- match.arg(as.character(season), available_seasons)

  if (!is.numeric(rounds_train)) {
    stop("'rounds_train' must be numeric.")
  }

  min_round <- min(data$round, na.rm = TRUE)
  max_round <- max(data$round, na.rm = TRUE)

  if (rounds_train < min_round || rounds_train > max_round) {
    stop(paste("'rounds_train' must be between", min_round, "and", max_round))
  }

  data$venue <- ifelse(data$venue == "Home", 1, 0)
  data$season <- as.character(data$season)

  data <- data[data$venue == 1 & as.character(data$season) == season_filter, ]

  r_train <- seq_len(rounds_train)
  r_test <- seq(rounds_train + 1, max(data$round, na.rm = TRUE))

  data_train <- data[data$round %in% r_train, ]
  data_test <- data[data$round %in% r_test, ]

  return(list(train = data_train, test = data_test))
}
