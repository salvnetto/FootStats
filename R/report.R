#' Generate a Quarto Markdown (.qmd) File for Bayesian Model Analysis
#'
#' This function creates a `.qmd` file for analyzing Bayesian models using
#' posterior samples from a fitted model. It includes trace plots, convergence
#' diagnostics, autocorrelation plots, and performance metrics.
#'
#' @param model A character string specifying the model name.
#'   This determines the file paths for the fitted model and data.
#' @param title A character string specifying the title of the report.
#' @param param_list A character vector containing parameter names to be included in the analysis.
#' @param output_path A character string specifying the full path for saving the `.qmd` file.
#'
#' @return Writes a `.qmd` file to the specified output path.
#'
#' @details
#' - The function reads the fitted model from `results/{model}.rds` and the data from `data/{model}.rds`.
#' - Generates trace plots, effective sample size diagnostics, and autocorrelation plots.
#' - Uses the `FootStats` package to compute and visualize model evaluation metrics.
#'
#' @examples
#' \dontrun{
#' generate_qmd(
#'   model = "indep_poisson(pl)",
#'   title = "Poisson Premier League",
#'   param_list = c("beta_0", "home", "sd_att", "sd_def", "mu_att", "beta"),
#'   output_path = "relatorios/indep_poisson(pl).qmd"
#' )
#' }
#'
#' @export
generate_qmd <- function (model, title, league, param_list, params_extra, pasta_arquivo, nome_arquivo, output_path)
{
  
  fit_path <- here::here(paste0(pasta_arquivo, "/", nome_arquivo, ".rds"))
  df_path <- here::here(paste0(pasta_arquivo, "/", nome_arquivo, ".xlsx"))
  
  output_path <- glue::glue(output_path)
  
  qmd_content <- glue::glue("\n---\ntitle: \"{title}\"\nformat:\n  html:\n    embed-resources: true\n---\n\n```{{r setup}}\n#| output: false\n#| echo: false\n#| warning: false\n\nlibrary(tidyverse)\nlibrary(cmdstanr)\nlibrary(posterior)\nlibrary(bayesplot)\nlibrary(shinystan)\nlibrary(FootStats)\nlibrary(readxl)\n```\n\n```{{r}}\n#| output: false\n#| echo: false\n#| warning: false\n\nfit = readRDS(\"{fit_path}\")\ndraws = posterior::as_draws_rvars(fit$draws())\ndf = read_excel(\"{df_path}\")\n```\n\n```{{r}}\nfit$summary(list({paste(shQuote(param_list, type='cmd'), collapse=', ')}))\n```\n\n# Convergencia\n\n```{{r traceplot}}\n#| output: true\n#| echo: false\n#| warning: false\n\n#ataque\nsample_att <- sample(1:20,4)\natt1 = mcmc_trace(draws, pars = c(paste0('att[',sample_att[1], ']')))\natt2 = mcmc_trace(draws, pars = c(paste0('att[',sample_att[2], ']')))\natt3 = mcmc_trace(draws, pars = c(paste0('att[',sample_att[3], ']')))\natt4 = mcmc_trace(draws, pars = c(paste0('att[',sample_att[4], ']')))\nbayesplot_grid(plots = list(att1, att2, att3, att4))\n\n#defesa\nsample_def <- sample(1:20,4)\ndef1 = mcmc_trace(draws, pars = c(paste0('def[',sample_def[1], ']')))\ndef2 = mcmc_trace(draws, pars = c(paste0('def[',sample_def[2], ']')))\ndef3 = mcmc_trace(draws, pars = c(paste0('def[',sample_def[3], ']')))\ndef4 = mcmc_trace(draws, pars = c(paste0('def[',sample_def[4], ']')))\nbayesplot_grid(plots = list(def1, def2, def3, def4))\n\n#outros\nplots_list <- list({paste0('mcmc_trace(draws, pars = c(\"', param_list, '\"))', collapse=', ')})\nbayesplot_grid(plots = plots_list)\n```\n\n```{{r rhat and neff}}\n#| output: true\n#| echo: false\n#| warning: false\n\n#extract\nrhats = bayesplot::rhat(fit)\nratios_cp = neff_ratio(fit)\n#plot\nprhat = mcmc_rhat_hist(rhats)\npnepp = mcmc_neff_hist(ratios_cp)\nbayesplot_grid(plots = list(prhat, pnepp))\n```\n\n```{{r acf}}\n#| output: true\n#| echo: false\n#| warning: false\n\n#ataque\natt1 = mcmc_acf(draws, pars = c(paste0('att[',sample_att[1], ']')))\natt2 = mcmc_acf(draws, pars = c(paste0('att[',sample_att[2], ']')))\natt3 = mcmc_acf(draws, pars = c(paste0('att[',sample_att[3], ']')))\natt4 = mcmc_acf(draws, pars = c(paste0('att[',sample_att[4], ']')))\nbayesplot_grid(plots = list(att1, att2, att3, att4))\n\n#defesa\ndef1 = mcmc_acf(draws, pars = c(paste0('def[',sample_def[1], ']')))\ndef2 = mcmc_acf(draws, pars = c(paste0('def[',sample_def[2], ']')))\ndef3 = mcmc_acf(draws, pars = c(paste0('def[',sample_def[3], ']')))\ndef4 = mcmc_acf(draws, pars = c(paste0('def[',sample_def[4], ']')))\nbayesplot_grid(plots = list(def1, def2, def3, def4))\n\n#outros\nacf_plots <- list({paste0('mcmc_acf(draws, pars = c(\"', param_list, '\"))', collapse=', ')})\nbayesplot_grid(plots = acf_plots)\n```\n\n# Metricas\n\n```{{r}}\n#| output: true\n#| echo: true\n#| warning: false\n\nFootStats::score(df)\nFootStats::expected_results(df)\nFootStats::expected_results(df, favorite = 'home')\nFootStats::expected_results(df, favorite = 'away')\nFootStats::real_vs_preview_goals(df)\nFootStats::confusion_matrix(df, type = \"binary\")\nFootStats::confusion_matrix(df, type = \"three-class\")\nFootStats::table_results(df)\n```\n\n```{{r}}\n#| output: true\n#| echo: false\n#| warning: false\n\nFootStats::plots(draws, df)\n```\n  ")
  
  writeLines(qmd_content, output_path)
  message("Quarto file saved to: ", output_path)
}
