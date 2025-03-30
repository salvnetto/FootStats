#' Generate Data Based on Model
#'
#' This function simulates match results between teams using a specified model.
#' The model requires a set of parameters to generate the goals for each team.
#'
#' @param num_teams Integer. The number of teams.
#' @param model Character or function. The model to use for generating the data (e.g., "poisson").
#' @param params List. A list of parameters required for the specified model.
#'   The available models are:
#'   \itemize{
#'     \item poisson: Basic poisson model with intercept and home effect.
#'   }
#'   To see a full list of parameters required for each model, call `print_model_params("poisson")` (or replace `"poisson"` with your chosen model name).
#' @param team_names Vector of strings. Optional. A vector containing team names.
#'
#' @return A list containing:
#' \itemize{
#'   \item data: A data frame with the simulation results (home team, away team, goals scored).
#'   \item params: A list containing the parameters used for the simulation.
#' }
#'
#' @examples
#' \dontrun{
#'   my_params = list(
#'     "mu_int" = 0, "sd_int" = 10,
#'     "mu_home" = 0, "sd_home" = 10,
#'     "mu_att" = 0, "sd_att" = 10,
#'     "mu_def" = 0, "sd_def" = 10
#'   )
#'   my_team_names = c("Team_name1", "Team_name2")
#'   gen_data <- generate_data(
#'     num_teams = 2, model = "poisson",
#'     params = my_params, team_names = my_team_names
#'   )
#'   print(gen_data$params)
#' }
#'
#' @export


generate_data <- function(num_teams, model, params, team_names = NULL) {
  if (!is.numeric(num_teams) || num_teams < 1) {
    stop("'num_teams' must be numeric and bigger than 1.")
  }
  if (num_teams %% 2 != 0) {
    stop("'num_teams' must be even, not odd.")
  }
  if (!is.list(params)) {
    stop("'params' must be a list.")
  }
  if (!is.null(team_names)) {
    if (!is.atomic(team_names)) {
      stop("'team_names' must be a vector.")
    }
    if (length(team_names) != num_teams) {
      stop("'team_names' must be a vector of the same length as 'num_teams'.")
    }
  }
  
  # Generate fixture schedule with proper rounds
  fixture_schedule <- generate_fixtures(num_teams)
  
  # Create games dataframe from fixture schedule
  games <- data.frame(
    home_index = fixture_schedule$Home,
    away_index = fixture_schedule$Away,
    round = fixture_schedule$Round
  )
  
  # Add team names if provided
  if (!is.null(team_names)) {
    games$home_team <- team_names[games$home_index]
    games$away_team <- team_names[games$away_index]
  }
  
  ## Model-based goal generation
  if (is.character(model)) {
    model <- get(paste0("fs_", model), mode = 'function', envir = asNamespace("FootStats"))
  }
  
  model_params <- model(params, num_teams, games)
  
  # Add goals to games dataframe
  games$gf <- model_params$goals_home
  games$ga <- model_params$goals_away
  
  # Add metadata columns
  games$season <- rep('2025', nrow(games))
  games$venue <- rep(1, nrow(games))  # 1 indicates home venue for the listed home team
  
  # Rearrange columns
  if (!is.null(team_names)) {
    games <- games[, c("season", "venue", "round", "home_index", "home_team", 
                       "goals_home", "goals_away", "away_team", "away_index")]
  } else {
    games <- games[, c("season", "venue", "round", "home_index", 
                       "gf", "ga", "away_index")]
  }
  
  games <- games |> 
    dplyr::mutate(
      result = dplyr::case_when(
        gf > ga ~ 'W',
        gf < ga ~ 'L',
        TRUE ~ 'D'
      ) |> factor()
    )
  
  output <- list(
    data = games,
    params = model_params
  )
  
  return(output)
}

# Helper function for fixture generation (internal use)
generate_fixtures <- function(num_teams) {
  teams <- 1:num_teams
  num_rounds <- (num_teams - 1) * 2
  schedule <- data.frame(Round = integer(), Home = integer(), Away = integer())
  
  # First half of season
  rotating <- teams[-1]
  for (round in 1:(num_teams - 1)) {
    matches <- data.frame(Round = round, Home = teams[1], Away = rotating[1])
    others <- rotating[-1]
    home_teams <- others[1:(length(others)/2)]
    away_teams <- rev(others)[1:(length(others)/2)]
    matches <- rbind(matches, data.frame(Round = round, Home = home_teams, Away = away_teams))
    schedule <- rbind(schedule, matches)
    rotating <- c(rotating[-1], rotating[1])
  }
  
  # Second half (reverse fixtures)
  second_half <- schedule
  second_half$Round <- second_half$Round + (num_teams - 1)
  second_half <- data.frame(Round = second_half$Round,
                            Home = second_half$Away,
                            Away = second_half$Home)
  
  full_schedule <- rbind(schedule, second_half)
  full_schedule[order(full_schedule$Round), ]
}