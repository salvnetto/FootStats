% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{confusion_matrix}
\alias{confusion_matrix}
\title{Generate Confusion Matrix}
\usage{
confusion_matrix(df, type)
}
\arguments{
\item{df}{A data frame containing match results and predictions.}

\item{type}{Type of the matrix: Binary (Home win or Not), Three-Class (Win, Draw and Lost).}
}
\value{
A confusion matrix object from caret package or contingency table if insufficient data
}
\description{
Creates a confusion matrix comparing predicted and actual match outcomes.
}
\examples{
# Create proper test data
test_data <- data.frame(
  result = c("W", "D", "L", "W", "L"),
  home_win = c(0.7, 0.2, 0.3, 0.6, 0.4),
  home_lost = c(0.2, 0.3, 0.6, 0.3, 0.5),
  draw = c(0.1, 0.5, 0.1, 0.1, 0.1)
)

# Run with actual data
confusion_matrix(test_data, "binary")
}
