#' Print the required parameters for a specific model
#'
#' This function prints the parameters expected for each model.
#'
#' @param model A character string with the name of the model (e.g., "poisson").
#'
#' @return None. Prints the expected parameters to the console.
#' @export
print_model_params <- function(model) {
  if (model == "poisson") {
    cat("Required parameters for Poisson model:\n")
    cat("- mu_int: The intercept for the model\n")
    cat("- sd_int: Standard deviation for the intercept\n")
    cat("- mu_home: Home team attack parameter\n")
    cat("- sd_home: Standard deviation for home attack\n")
    cat("- mu_att: Attack strength of each team\n")
    cat("- sd_att: Standard deviation for attack strength\n")
    cat("- mu_def: Defensive strength of each team\n")
    cat("- sd_def: Standard deviation for defense strength\n")
  } else {
    cat("Model not recognized. Please refer to documentation.\n")
  }
}
