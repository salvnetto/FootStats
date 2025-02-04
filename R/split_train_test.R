#' Split Dataset into Training and Testing Sets for Sports Analytics
#'
#' This function splits a sports dataset into training and testing sets based on
#' specified season and rounds, with home venue filtering and team indexing.
#'
#' @param df A data frame containing sports match data from load_data()
#' @param season The specific season to filter data from (must be a character)
#' @param rounds_train A vector of rounds to use for training data
#'
#' @return A list containing two data frames: train and test
#'
#' @examples
#' \dontrun{
#' result <- split_train_test(
#'   df = match_data,
#'   season = '2022-2023',
#'   rounds_train = 19
#' )
#' train_data <- result$train
#' test_data <- result$test
#' }
#'
#' @export

split_train_test = function(df, season, rounds_train) {
  available_seasons = unique(as.character(df$season))
  season_filter = match.arg(season, available_seasons)

  if (!is.numeric(rounds_train)) {
    stop("'rounds_train' must be numeric.")
  }

  min_round <- min(df$round)
  max_round <- max(df$round)

  if (rounds_train < min_round || rounds_train > max_round) {
    stop(paste("'rounds_train' must be between", min_round, "and", max_round))
  }

  unique_teams = unique(c(df$team_name, df$opponent))

  df = df %>%
    dplyr::mutate(
      venue = ifelse(venue == 'Home', 1, 0),
      team_name_idx = match(team_name, unique_teams),
      opponent_idx = match(opponent, unique_teams),
      season = as.character(season)
    ) %>%
    dplyr::filter(venue == 1) %>%
    dplyr::filter(season == season_filter)

  r_train = 1:rounds_train
  r_test = (rounds_train+1):max(df$round)

  df_train = df %>%
    dplyr::filter(round %in% r_train)
  df_test = df %>%
    dplyr::filter(round %in% r_test)

  return(
    list(
      train = df_train,
      test = df_test
    )
  )
}
