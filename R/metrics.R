#' Calculate Model Performance Score
#' 
#' @description Computes a weighted performance score based on match outcomes and predicted probabilities.
#' 
#' @param df A data frame containing match results and predictions. Must contain columns:
#'   \itemize{
#'     \item result (character) - Match outcomes with values "W", "D", or "L"
#'     \item home_win (numeric) - Predicted probability of home win
#'     \item draw (numeric) - Predicted probability of draw
#'     \item home_lost (numeric) - Predicted probability of home loss
#'   }
#'   
#' @return Invisibly returns the numeric score value. Prints formatted message with score.
#' @export
#' 
#' @examples
#' # Create sample data
#' test_data <- data.frame(
#'   result = c("W", "D", "L", "W", "L"),
#'   home_win = c(0.7, 0.2, 0.3, 0.6, 0.4),
#'   home_lost = c(0.2, 0.3, 0.6, 0.3, 0.5),
#'   draw = c(0.1, 0.5, 0.1, 0.1, 0.1)
#' )
#' score(test_data)
score <- function(df) {
  df$score <- NA_real_
  df$score[df$result == "W"] <- df$home_win[df$result == "W"]
  df$score[df$result == "D"] <- df$draw[df$result == "D"]
  df$score[df$result == "L"] <- df$home_lost[df$result == "L"]
  
  final_score <- sum(df$score) / nrow(df)
  
  message(paste0("The model score is: ", round(final_score, 4)))
  invisible(final_score)
}


#' Compare Expected vs Actual Results
#' 
#' @description Analyzes model performance by comparing predicted probabilities with actual outcomes.
#' 
#' @param df A data frame containing match results and predictions. Same structure as required for score().
#' @param favorite Character specifying favorite team filtering: 
#'   \itemize{
#'     \item "home" - Filter matches where home team was predicted favorite
#'     \item "away" - Filter matches where away team was predicted favorite
#'     \item NULL (default) - No filtering, use all matches
#'   }
#'   
#' @return A data frame with columns:
#'   \itemize{
#'     \item result (character) - Match outcomes
#'     \item real_result (integer) - Count of actual outcomes
#'     \item expected_result (numeric) - Sum of predicted probabilities
#'   }
#' @export
#' 
#' @examples
#' # Create proper test data
#' test_data <- data.frame(
#'   result = c("W", "D", "L", "W", "D"),
#'   home_win = c(0.7, 0.2, 0.1, 0.6, 0.3),
#'   home_lost = c(0.1, 0.3, 0.6, 0.2, 0.4),
#'   draw = c(0.2, 0.5, 0.3, 0.2, 0.3)
#' )
#' 
#' # Run with actual data
#' expected_results(test_data, favorite = "home")
expected_results <- function(df, favorite = "none") {
  df$favorite <- if (favorite == "home") {
    ifelse(df$home_win > df$home_lost, 1, 
           ifelse(df$home_win < df$home_lost, 0, 1))
  } else if (favorite == "away") {
    ifelse(df$home_win < df$home_lost, 1, 
           ifelse(df$home_win > df$home_lost, 0, 1))
  } else {
    1
  }
  
  df_filtered <- df[df$favorite == 1, ]
  
  if (nrow(df_filtered) == 0) {
    return(data.frame(result = character(),
                      real_result = numeric(),
                      expected_result = numeric()))
  }
  
  real_counts <- vapply(c("W", "D", "L"), function(x) {
    sum(df_filtered$result == x, na.rm = TRUE)
  }, numeric(1))
  
  expected <- c(
    W = round(sum(df_filtered$home_win, na.rm = TRUE), 2),
    D = round(sum(df_filtered$draw, na.rm = TRUE), 2),
    L = round(sum(df_filtered$home_lost, na.rm = TRUE), 2)
  )
  
  result_df <- data.frame(
    result = names(real_counts),
    real_result = unname(real_counts),
    expected_result = unname(expected)
  )
  
  result_df <- result_df[match(c("W", "D", "L"), result_df$result), ]
  
  rownames(result_df) <- NULL
  return(result_df)
}


#' Compare Actual vs Predicted Goals
#' 
#' @description Compares total goals scored/conceded with predicted goal expectations.
#' 
#' @param df A data frame containing goal data. Must contain columns:
#'   \itemize{
#'     \item gf (numeric) - Goals for
#'     \item ga (numeric) - Goals against
#'     \item lambda_home (numeric) - Predicted home goals
#'     \item lambda_away (numeric) - Predicted away goals
#'   }
#'   
#' @return A data frame with 2 rows ("real", "preview") and columns (GF, GA)
#' @export
#' 
#' @examples
#' goal_data <- data.frame(
#'   gf = c(2, 1, 3),
#'   ga = c(1, 1, 2),
#'   lambda_home = c(1.8, 1.2, 2.5),
#'   lambda_away = c(0.9, 1.1, 1.8)
#' )
#' real_vs_preview_goals(goal_data)
real_vs_preview_goals <- function(df) {
  real_gf <- round(sum(df$gf, na.rm = TRUE), 2)
  real_ga <- round(sum(df$ga, na.rm = TRUE), 2)
  preview_gf <- round(sum(df$lambda_home, na.rm = TRUE), 2)
  preview_ga <- round(sum(df$lambda_away, na.rm = TRUE), 2)
  
  goals <- data.frame(
    GF = c(real_gf, preview_gf),
    GA = c(real_ga, preview_ga),
    row.names = c("real", "preview")
  )
  
  return(goals)
}


#' Generate Confusion Matrix
#' 
#' @description Creates a confusion matrix comparing predicted and actual match outcomes.
#' 
#' @param df A data frame containing match results and predictions.
#' @param type Type of the matrix: Binary (Home win or Not), Three-Class (Win, Draw and Lost).
#' 
#' @return A confusion matrix object from caret package or contingency table if insufficient data
#' @export
#' @importFrom caret confusionMatrix
#' 
#' @examples
#' # Create proper test data
#' test_data <- data.frame(
#'   result = c("W", "D", "L", "W", "L"),
#'   home_win = c(0.7, 0.2, 0.3, 0.6, 0.4),
#'   home_lost = c(0.2, 0.3, 0.6, 0.3, 0.5),
#'   draw = c(0.1, 0.5, 0.1, 0.1, 0.1)
#' )
#' 
#' # Run with actual data
#' confusion_matrix(test_data, "binary")
confusion_matrix <- function(df, type) {
  type_arg <- match.arg(type, choices = c("binary", "three-class"))
  
  if(type_arg == "binary") {
    df <- df |> 
      dplyr::mutate(
        preview = dplyr::case_when(
          home_win > draw & home_win > home_lost ~ "W",
          TRUE ~ "DL"
        ) |> factor(),
        result = dplyr::case_when(
          result == "W" ~ "W",
          TRUE ~ "DL"
          ) |> factor()
      ) 
    
    caret::confusionMatrix(df$preview, df$result)
  }
  
  if(type_arg == "three-class") {
    df <- df |> 
      dplyr::mutate(
        preview = dplyr::case_when(
          home_win > draw & home_win > home_lost ~ "W",
          home_lost > draw & home_lost > home_win ~ "L",
          TRUE ~ "D"
        ) |> factor()
      )
    
    caret::confusionMatrix(df$preview, df$result)
  }
  

}

#' Generate Posterior Predictive Check Plots
#' 
#' @description Creates diagnostic plots comparing observed goals with posterior predictions 
#' from a Bayesian model. Produces two sets of plots:
#' 1. Goal distribution comparisons using bar plots
#' 2. Mean goal comparisons using statistical plots
#'
#' @param draws A stanfit object or list containing posterior predictions. Must contain:
#' \itemize{
#'   \item gf_new (matrix) - Posterior predictions for home goals
#'   \item ga_new (matrix) - Posterior predictions for away goals
#' }
#' @param df_test Test Data frame containing observed goal data. Must contain:
#' \itemize{
#'   \item gf (numeric) - Observed home goals
#'   \item ga (numeric) - Observed away goals
#' }
#'
#' @return A bayesplot grid object containing the combined plots
#' @export
#' 
#' @examples
#' \dontrun{
#' # Requires Stan model output
#' library(bayesplot)
#' 
#' # Sample test data
#' test_data <- data.frame(
#'   gf = c(2, 1, 3, 0, 2),
#'   ga = c(1, 1, 2, 2, 0)
#' )
#' 
#' # Sample posterior draws (typically from stanfit object)
#' sample_draws <- list(
#'   gf_new = matrix(sample(0:4, 500, replace = TRUE), 
#'   ga_new = matrix(sample(0:4, 500, replace = TRUE))
#' )
#' 
#' # Generate plots
#' plots(sample_draws, test_data)
#' }
#' 
#' @importFrom bayesplot ppc_bars ppc_stat bayesplot_grid
plots <- function(draws, df_test) {
  # Input validation
  if(!"gf_new" %in% names(draws) || !"ga_new" %in% names(draws)) {
    stop("draws must contain gf_new and ga_new matrices")
  }
  if(!all(c("gf", "ga") %in% colnames(df_test))) {
    stop("df_test must contain 'gf' and 'ga' columns")
  }
  
  gf_rep <- posterior::draws_of(draws$gf_new)
  gf_obs <- df_test$gf
  
  ga_rep <- posterior::draws_of(draws$ga_new)
  ga_obs <- df_test$ga
  
  pgf <- bayesplot::ppc_bars(gf_obs, gf_rep) + 
    ggplot2::labs(title = "Home Goals")
  pga <- bayesplot::ppc_bars(ga_obs, ga_rep) + 
    ggplot2::labs(title = "Away Goals")
  
  pgf_mean <- bayesplot::ppc_stat(gf_obs, gf_rep, stat = "mean") + 
    ggplot2::labs(title = "Home Goal Means")
  pga_mean <- bayesplot::ppc_stat(ga_obs, ga_rep, stat = "mean") + 
    ggplot2::labs(title = "Away Goal Means")
  
  bayesplot::bayesplot_grid(
    plots = list(pgf, pga, pgf_mean, pga_mean),
    titles = c("Goal Distribution Comparison", 
               "Goal Distribution Comparison",
               "Mean Goal Comparison",
               "Mean Goal Comparison")
  )
}