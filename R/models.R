fs_poisson <- function(params, num_teams, games) {
  params_req = c("intercept", "home", "sd_att", "sd_def")
  missing <- setdiff(params_req, names(params))
  if (length(missing) > 0) {
    stop("Missing required parameters: ", paste(missing, collapse = ", "))
  }

  intercept = params$intercept
  home = params$home
  att_raw = stats::rnorm(num_teams, 0, params$sd_att)
  def_raw = stats::rnorm(num_teams, 0, params$sd_def)
  
  att = att_raw - mean(att_raw)
  def = def_raw - mean(def_raw)

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



fs_poisson_covariates <- function(params, num_teams, games) {
  params_req = c("intercept", "home", "sd_att", "sd_def", "beta")
  missing <- setdiff(params_req, names(params))
  if (length(missing) > 0) {
    stop("Missing required parameters: ", paste(missing, collapse = ", "))
  }
  
  X = matrix(stats::rgamma(dim(games)[1], scale = 1, shape = 1), dim(games)[1], 1)
  
  intercept = params$intercept
  home = params$home
  beta = params$beta
  att_raw = stats::rnorm(num_teams, 0, params$sd_att)
  def_raw = stats::rnorm(num_teams, 0, params$sd_def)
  
  att = att_raw - mean(att_raw)
  def = def_raw - mean(def_raw)
  
  theta_home = intercept + att[games$home_index] - def[games$away_index] + X*beta + home 
  theta_away = intercept + att[games$away_index] - def[games$home_index] + X*beta
  
  goals_home = stats::rpois(nrow(games), exp(theta_home))
  goals_away = stats::rpois(nrow(games), exp(theta_away))
  
  model_params <- list(
    intercept  = intercept,
    home       = home,
    att        = att,
    def        = def,
    X          = X,
    beta       = beta,
    theta_home = theta_home,
    theta_away = theta_away,
    goals_home = goals_home,
    goals_away = goals_away
  )
  
  return(model_params)
}
