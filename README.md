# DCA
<!-- badges: start -->
<!-- badges: end -->
This package executes properly the entire methodology of Data Concentration Analysis, which was derived from Alegre Ferreira (2018) PhD's dissertation and posteriorly enhanced by de França and Sosa (2021) and finally consolidated by Neres da Conceição (2023) in his master's thesis.

This is an initial version of the package and it will be aprimored in near future with vingnettes and a brief explication of the methodology. For now, anyone who's interessed may find the whole methodology within these authors transcripts.

## Installation
You can install this package with using
``` r
remotes::install_github("VinNeres/DCA")
```

## Example
To use the function provided by this package, you can use the integrated example data, called `brz_gdp` to calculate all DCA components for Brazilian sectoral and regional GDP (all brute production, aggregate and intermediate GDP).

```r
library(DCA)

data(brz_gdp)
brz_gdp <- brz_gdp[,3:20]
brz_gdp_dca <- dca(brz_gdp, 27, 15, 4)
```
## License
This package is licensed under GNU3 license.
