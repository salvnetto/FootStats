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

  games <- data.frame(
    home_index = rep(1:num_teams, each = num_teams),
    away_index = rep(1:num_teams, times = num_teams)
  )

  games <- games[games$home_index != games$away_index, ]

  if (!is.null(team_names)){
    games$home_team <- team_names[games$home]
    games$away_team <- team_names[games$away]
  }

  ## model based generated goals
  if (is.character(model))
    model <- get(paste0("fs_", model), mode = 'function', envir = asNamespace("FootStats"))

  model_params <- model(params, num_teams, games)

  games$goals_home <- model_params$goals_home
  games$goals_away <- model_params$goals_away

  ## rearrange column order
  if (!is.null(team_names)){
    games <- games[, c("home_index", "home_team", "goals_home", "goals_away", "away_team", "away_index")]
  } else {
    games <- games[, c("home_index", "goals_home", "goals_away", "away_index")]
  }

  output <- list(
    data = games,
    params = model_params
  )

  return(output)
}
