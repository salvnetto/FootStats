#' Models to Simulate Football Match Results
#'
#' @param params List. A list of parameters required for the Poisson model
#' @param num_teams Integer. The number of teams in the league.
#' @param games Data frame. A data frame containing the home and away team indices. 
#'   The data frame must have columns: home_index, away_index.
#'
#' @return A list containing the simulated paramaters

fs_poisson <- function(params, num_teams, games) {
  params_req = c("mu_int", "sd_int", "mu_home", "sd_home", "mu_att", "sd_att", "mu_def", "sd_def")
  missing <- setdiff(params_req, names(params))
  if (length(missing) > 0) {
    stop("Missing required parameters: ", paste(missing, collapse = ", "))
  }

  intercept = stats::rnorm(1, params$mu_int, params$sd_int)
  home = stats::rnorm(1, params$mu_home, params$sd_home)
  att = stats::rnorm(num_teams, params$mu_att, params$sd_att)
  def = stats::rnorm(num_teams, params$mu_def, params$sd_def)

  theta_home = intercept + att[games$home_index] - def[games$away_index] + home
  theta_away = intercept + att[games$away_index] - def[games$home_index]

  goals_home = stats::rpois(nrow(games), exp(theta_home))
  goals_away = stats::rpois(nrow(games), exp(theta_away))

  model_params <- list(
    intercept  = intercept,
    home       = home,
    att        = att,
    def        = def,
    theta_home = theta_home,
    theta_away = theta_away,
    goals_home = goals_home,
    goals_away = goals_away
  )

  return(model_params)
}
