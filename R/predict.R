#' Predict Match Outcomes Using Posterior Draws
#'
#' This function calculates probabilities of home win, draw, and away win for matches in test data using posterior draws from a model. It predicts results by selecting the outcome with the highest probability and checks prediction accuracy against actual results.
#'
#' @param test_data A data frame containing match data. Must include columns:
#'   \itemize{
#'     \item `result`: Actual match outcome (values "W" for home win, "D" for draw, "L" for away win).
#'   }
#' @param draws A posterior draws object containing generated goals for home (`gf_new`) and away (`ga_new`) teams.
#'
#' @return A modified version of `test_data` with added columns:
#' \itemize{
#'   \item `home_win`: Probability of home win (0-1).
#'   \item `draw`: Probability of draw (0-1).
#'   \item `home_lost`: Probability of away win (0-1).
#'   \item `result_predicted`: Predicted outcome ("W", "D", "L").
#'   \item `success`: 1 if prediction matches actual result, 0 otherwise.
#' }
#'
#' @examples
#' \dontrun{
#'   # Example using posterior draws from a Bayesian model
#'   test_data <- data.frame(
#'     home_team = c("Team A", "Team B"),
#'     away_team = c("Team B", "Team A"),
#'     result = c("W", "L")
#'   )
#'   # Assume 'draws' is a posterior draws object with gf_new and ga_new variables
#'   predicted_data <- predict(test_data, draws)
#'   print(predicted_data[, c("home_team", "away_team", "result_predicted", "success")])
#' }
#'
#' @importFrom posterior draws_of
#' @export
predicted_results <- function(test_data, draws) {
  test_data$home_win <- NA
  test_data$draw <- NA
  test_data$home_lost <- NA
  test_data$lambda_home <- NA
  test_data$lambda_away <- NA
  
  
  for (i in 1:nrow(test_data)) {
    y1 <- posterior::draws_of(draws$y1_pred)[, i]  # Posterior samples for X (home), length 10000
    y2 <- posterior::draws_of(draws$y2_pred)[, i]  # Posterior samples for Y (away), length 10000
    n_preds <- length(y1)                         # Number of samples
    
    test_data$home_win[i] <- sum(y1 > y2) / n_preds   # P(X > Y): Probability of home win
    test_data$draw[i] <- sum(y1 == y2) / n_preds      # P(X == Y): Probability of draw
    test_data$home_lost[i] <- sum(y1 < y2) / n_preds  # P(X < Y): Probability of home loss
    
    test_data$lambda_home[i] <- mean(y1)  # Expected goals (home)
    test_data$lambda_away[i] <- mean(y2)  # Expected goals (away)
    
  }
  
  return(test_data)
}

