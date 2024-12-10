#' Brazilian GDP Example Data
#'
#' This dataset contains information on the Brazilian GDP, measured as Total Aggregate
#' Production, for the period 2003 to 2020. The data is segregated by region
#' and economic activity.
#'
#' @format A data frame with 405 rows and 20 columns:
#' \describe{
#'   \item{state}{It is the first dimension of the database, referencing the states}
#'   \item{sector}{It is the second dimension of the database, referencing the economic activity}
#'   \item{...}{The rest of the columns reference production in R$ millions. Each column correponds to a year.}
#' }
#' @source Brazilian Institute of Geography and Statistics.
#'
#' @examples
#' data(brz_gdp)
#' head(brz_gdp)
"brz_gdp"
