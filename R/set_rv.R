#' Declare Random Variables
#'
#' Defines one or more random variables using formula syntax (e.g., `x ~ normal(0, 1)`).
#'
#' @param x An object from "sim"
#' @param ... Formulas declaring random variables (e.g., `u ~ uniform(0, 1)`).
#' @param params A named list of local constants or parameters.
#' @param overwrite Logical; whether to overwrite previously defined variables.
#' @param verbose Logical; whether to display the defined random variables.
#' @param list_dist Built-in distribution in `gamblr` package to be used.
#'
#' @return Updated simulation context with declared random variables.
#' @export
set_rv <- function(
    x,
    ...,
    params = list(),
    overwrite = FALSE,
    verbose = FALSE,
    list_dist = pdist
) {

}
