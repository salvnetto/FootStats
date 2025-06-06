% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predicted_results}
\alias{predicted_results}
\title{Predict Match Outcomes Using Posterior Draws}
\usage{
predicted_results(test_data, draws)
}
\arguments{
\item{test_data}{A data frame containing match data. Must include columns:
\itemize{
\item \code{result}: Actual match outcome (values "W" for home win, "D" for draw, "L" for away win).
}}

\item{draws}{A posterior draws object containing generated goals for home (\code{gf_new}) and away (\code{ga_new}) teams.}
}
\value{
A modified version of \code{test_data} with added columns:
\itemize{
\item \code{home_win}: Probability of home win (0-1).
\item \code{draw}: Probability of draw (0-1).
\item \code{home_lost}: Probability of away win (0-1).
\item \code{result_predicted}: Predicted outcome ("W", "D", "L").
\item \code{success}: 1 if prediction matches actual result, 0 otherwise.
}
}
\description{
This function calculates probabilities of home win, draw, and away win for matches in test data using posterior draws from a model. It predicts results by selecting the outcome with the highest probability and checks prediction accuracy against actual results.
}
\examples{
\dontrun{
  # Example using posterior draws from a Bayesian model
  test_data <- data.frame(
    home_team = c("Team A", "Team B"),
    away_team = c("Team B", "Team A"),
    result = c("W", "L")
  )
  # Assume 'draws' is a posterior draws object with gf_new and ga_new variables
  predicted_data <- predict(test_data, draws)
  print(predicted_data[, c("home_team", "away_team", "result_predicted", "success")])
}

}
