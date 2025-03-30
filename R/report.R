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
generate_qmd <- function(model, title, param_list, output_path) {
  fit_path <- glue::glue("../results/{model}.rds")
  df_path <- glue::glue("../data/{model}.xlsx")
  output_path <- glue::glue(output_path)

  qmd_content <- glue::glue("
---
title: \"{title}\"
format: html
---

```{{r setup}}
#| output: false
#| echo: false
#| warning: false

library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(shinystan)
library(FootStats)
library(readxl)
```

```{{r}}
#| output: false
#| echo: false
#| warning: false

fit = readRDS(\"{fit_path}\")
draws = posterior::as_draws_rvars(fit$draws())
df = read_excel(\"{df_path}\")
```

```{{r}}
fit$summary(list({paste(shQuote(param_list, type='cmd'), collapse=', ')}))
```

# Convergencia

```{{r traceplot}}
#| output: true
#| echo: false
#| warning: false

#ataque
att1 = mcmc_trace(draws, pars = c('att[1]'))
att2 = mcmc_trace(draws, pars = c('att[7]'))
att3 = mcmc_trace(draws, pars = c('att[12]'))
att4 = mcmc_trace(draws, pars = c('att[20]'))
bayesplot_grid(plots = list(att1, att2, att3, att4))
#defesa
def1 = mcmc_trace(draws, pars = c('def[1]'))
def2 = mcmc_trace(draws, pars = c('def[7]'))
def3 = mcmc_trace(draws, pars = c('def[12]'))
def4 = mcmc_trace(draws, pars = c('def[20]'))
bayesplot_grid(plots = list(def1, def2, def3, def4))
#outros
plots_list <- list({paste0('mcmc_trace(draws, pars = c(\"', param_list, '\"))', collapse=', ')})
bayesplot_grid(plots = plots_list)
```

```{{r rhat and neff}}
#| output: true
#| echo: false
#| warning: false

#extract
rhats = bayesplot::rhat(fit)
ratios_cp = neff_ratio(fit)
#plot
prhat = mcmc_rhat_hist(rhats)
pnepp = mcmc_neff_hist(ratios_cp)
bayesplot_grid(plots = list(prhat, pnepp))
```

```{{r acf}}
#| output: true
#| echo: false
#| warning: false

#ataque
att1 = mcmc_acf(draws, pars = c('att[1]'))
att2 = mcmc_acf(draws, pars = c('att[7]'))
att3 = mcmc_acf(draws, pars = c('att[12]'))
att4 = mcmc_acf(draws, pars = c('att[20]'))
bayesplot_grid(plots = list(att1, att2, att3, att4))
#defesa
def1 = mcmc_acf(draws, pars = c('def[1]'))
def2 = mcmc_acf(draws, pars = c('def[7]'))
def3 = mcmc_acf(draws, pars = c('def[12]'))
def4 = mcmc_acf(draws, pars = c('def[20]'))
bayesplot_grid(plots = list(def1, def2, def3, def4))
#outros
acf_plots <- list({paste0('mcmc_acf(draws, pars = c(\"', param_list, '\"))', collapse=', ')})
bayesplot_grid(plots = acf_plots)
```

# Metricas

```{{r}}
#| output: true
#| echo: true
#| warning: false

FootStats::score(df)
FootStats::expected_results(df)
FootStats::expected_results(df, favorite = 'home')
FootStats::expected_results(df, favorite = 'away')
FootStats::real_vs_preview_goals(df)
FootStats::confusion_matrix(df, type = \"binary\")
FootStats::confusion_matrix(df, type = \"three-class\")
```

```{{r}}
#| output: true
#| echo: false
#| warning: false

FootStats::plots(draws, df)
```
  ")
                                       
  writeLines(qmd_content, output_path)
  message("Quarto file saved to: ", output_path)
}
