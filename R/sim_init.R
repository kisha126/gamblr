#' Initialize a Simulation Context
#'
#' Starts a new Monte Carlo simulation pipeline with optional constants and a random seed.
#'
#' @param constants A named list of global constants (e.g., `list(two_pi = 2 * pi)`).
#' @param seed An optional integer to set the random seed for reproducibility.
#' @param device Target device for simulation, currently `"cpu"` or `"gpu"` (future support).
#' @param name Optional simulation name.
#' @param verbose Logical; whether to print diagnostic messages during simulation.
#' @param store_history Logical; if `TRUE`, stores pipeline steps for reproducibility or debugging.
#' @param backend Optional parallel backend, e.g., `"local"`, `"multicore"`, `"slurm"`, or custom.
#' @param n_workers Optional integer specifying the number of parallel workers. Ignored if
#'   `backend = "local"` or if parallelization is not supported. Default is `NULL`.
#'
#' @return A simulation context object used for chaining simulation steps.
#' @export
sim_init <- function(
    constants = list(),
    seed = NULL,
    device = c("cpu", "gpu"),
    name = NULL,
    verbose = FALSE,
    store_history = FALSE,
    backend = "local",
    n_workers = NULL
) {

}
