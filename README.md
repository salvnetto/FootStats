# DYNMO - Datasets and Predictive Models for Football

## Usage

To use the `DYNMO` package, install it via github:

```r
# install the package from this Github repository

devtools::install_github("salvnetto/DYNMO")
```

```r
library(DYNMO)

# Load processed match history data for Brasileirão
df = load_data("br", "match_history")
```
