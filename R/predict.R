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
predict <- function(test_data, draws) {
  test_data$home_win <- NA
  test_data$draw <- NA
  test_data$home_lost <- NA
  
  for (i in 1:nrow(test_data)) {
    x <- posterior::draws_of(draws$gf_new)[, i]
    y <- posterior::draws_of(draws$ga_new)[, i]
    n_preds <- length(x)
    
    test_data$home_win[i] <- sum(x > y) / n_preds
    test_data$draw[i] <- sum(x == y) / n_preds
    test_data$home_lost[i] <- sum(x < y) / n_preds
  }
  
  max_vals <- pmax(test_data$home_win, test_data$draw, test_data$home_lost)
  test_data$result_predicted <- ifelse(
    test_data$home_win == max_vals, "W",
    ifelse(test_data$draw == max_vals, "D", "L")
  )
  
  test_data$success <- as.integer(test_data$result_predicted == test_data$result)
  
  return(test_data)
}
