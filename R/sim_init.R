#' Initialize a gamblr Simulation Specification
#'
#' Sets up the foundational parameters for a Monte Carlo simulation run. This is
#' typically the first function called in a `gamblr` pipeline.
#'
#' @param n_trials (integer) The number of Monte Carlo iterations to perform.
#' @param seed (integer, optional) An optional integer to set the random seed
#'   for reproducibility. Defaults to `NULL` (no seed set explicitly).
#' @param constants (named list, optional) A named list of global constants
#'   to be used in the simulation (e.g., `list(fixed_cost = 500, tax_rate = 0.2)`).
#'   These constants can be accessed by RV definitions and model expressions.
#'   Defaults to an empty list.
#' @param device (character, optional) Target device for simulation core
#'   computations. Currently, only `"cpu"` is supported. Defaults to `"cpu"`.
#' @param name (character, optional) An optional name for the simulation, useful
#'   for tracking or titling plots. Defaults to `NULL`.
#' @param verbose (logical, optional) If `TRUE`, prints diagnostic messages
#'   during various stages of specification and execution. Defaults to `FALSE`.
#' @param store_history (logical, optional) If `TRUE`, records the sequence of
#'   `gamblr` function calls that construct the specification. Defaults to `FALSE`.
#' @param backend (character or list, optional) Specifies the parallel
#'   execution backend.
#'   Common values:
#'   - `"local"` (default, serial execution),
#'   - `"multicore"` (local parallel processing, often using the `future` package).
#'   For HPC systems, this could be a string like `"slurm"`, `"pbs"`, or a
#'   list providing detailed configuration for job submission.
#' @param n_workers (integer, optional) Number of parallel workers for
#'   backends like `"multicore"`. If `NULL` (default), the backend might choose
#'   a sensible default (e.g., number of available cores).
#'
#' @return A `gambl_spec` object. This object stores the complete simulation plan
#'   and is passed through the `gamblr` pipeline.
#' @export
#' @examples
#' # Basic initialization
#' sim_plan_1 <- sim_init(n_trials = 1000)
#' sim_plan_1
#'
#' # Initialization with constants
#' sim_plan_const <- sim_init(
#'  n_trials = 500,
#'  constants = list(gravity = 9.81, pi_approx = 3.14)
#' )
#' sim_plan_const
#'
#' # More detailed initialization
#' sim_plan_2 <- sim_init(
#'   n_trials = 10000,
#'   seed = 42,
#'   constants = list(conversion_factor = 2.20462),
#'   name = "Profitability Analysis",
#'   verbose = TRUE,
#'   store_history = TRUE,
#'   backend = "multicore",
#'   n_workers = 4
#' )
#' sim_plan_2
sim_init <- function(n_trials,
                     seed = NULL,
                     constants = list(),
                     device = "cpu",
                     name = NULL,
                     verbose = FALSE,
                     store_history = FALSE,
                     backend = "local",
                     n_workers = NULL) {

    # --- Input Validations ---
    if (!is.numeric(n_trials) || length(n_trials) != 1 || n_trials <= 0 || floor(n_trials) != n_trials) {
        stop("`n_trials` must be a single positive integer.", call. = FALSE)
    }
    n_trials <- as.integer(n_trials)

    if (!is.null(seed)) {
        if (!is.numeric(seed) || length(seed) != 1 || floor(seed) != seed) {
            stop("`seed` must be a single integer or NULL.", call. = FALSE)
        }
        seed <- as.integer(seed)
    }

    # Validation for constants
    if (!is.list(constants)) {
        stop("`constants` must be a list.", call. = FALSE)
    }
    if (length(constants) > 0 && (is.null(names(constants)) || any(names(constants) == ""))) {
        stop("All elements in `constants` list must be named.", call. = FALSE)
    }
    # Could add more checks, e.g., no duplicate names, basic types, etc.

    if (!is.character(device) || length(device) != 1 || !tolower(device) %in% c("cpu")) {
        stop("`device` must be 'cpu' (support for 'gpu' is planned for the future).", call. = FALSE)
    }
    device <- tolower(device)

    if (!is.null(name) && (!is.character(name) || length(name) != 1)) {
        stop("`name` must be a single character string or NULL.", call. = FALSE)
    }

    if (!is.logical(verbose) || length(verbose) != 1) {
        stop("`verbose` must be a single logical value (TRUE/FALSE).", call. = FALSE)
    }

    if (!is.logical(store_history) || length(store_history) != 1) {
        stop("`store_history` must be a single logical value (TRUE/FALSE).", call. = FALSE)
    }

    if (!((is.character(backend) && length(backend) == 1) || is.list(backend))) {
        stop("`backend` must be a single character string (e.g., 'local', 'multicore') or a list for detailed configuration.", call. = FALSE)
    }

    if (!is.null(n_workers)) {
        if (!is.numeric(n_workers) || length(n_workers) != 1 || n_workers <= 0 || floor(n_workers) != n_workers) {
            stop("`n_workers` must be a single positive integer or NULL.", call. = FALSE)
        }
        n_workers <- as.integer(n_workers)
    }

    # --- Construct Settings List ---
    settings <- list(
        device = device,
        name = name,
        verbose = verbose,
        store_history = store_history,
        backend = backend,
        n_workers = n_workers
    )

    # --- Initialize History ---
    current_call_str <- ""
    tryCatch({
        current_call_str <- rlang::expr_deparse(match.call())
    }, error = function(e) {
        current_call_str <- "sim_init(...)"
    })

    history_list <- list()
    if (store_history) {
        history_list <- list(
            list(
                call = current_call_str,
                timestamp = Sys.time()
            )
        )
    }

    # --- Create the gambl_spec object ---
    spec <- new_gambl_spec(
        n_trials = n_trials,
        seed = seed,
        settings = settings,
        inputs = list(),
        constants = constants,
        model_expressions = list(),
        dist_env = create_dist_env(),
        history = history_list
    )

    if (verbose) {
        cat("gamblr specification initialized.\n")
        if (!is.null(name)) cat("Simulation name:", name, "\n")
        cat("Number of trials:", n_trials, "\n")
        if (!is.null(seed)) cat("Seed set to:", seed, "\n")
        if (length(constants) > 0) {
            cat("Initial constants defined:", paste(names(constants), collapse = ", "), "\n")
        }
    }

    spec
}

#' Create a new gambl_spec object (Internal Constructor)
#'
#' This is an internal function to construct `gambl_spec` objects with proper
#' validation.
#'
#' @param n_trials Integer, number of trials.
#' @param seed Integer or NULL, RNG seed.
#' @param settings List, settings from `sim_init`.
#' @param inputs Named list, initially empty, for `GamblInputR6` objects.
#' @param constants Named list, for fixed values.
#' @param model_expressions List, initially empty, for `rlang` quosures of model outputs.
#' @param dist_env Environment, containing DSL functions for distributions.
#' @param history List, initially empty or with the `sim_init` call, for call history.
#' @return A `gambl_spec` object.
#' @noRd
new_gambl_spec <- function(n_trials,
                           seed = NULL,
                           settings = list(),
                           inputs = list(),
                           constants = list(),
                           model_expressions = list(),
                           dist_env = NULL,
                           history = list()) {

    stopifnot(is.integer(n_trials) && n_trials > 0)

    if (!is.null(seed)) {
        stopifnot(is.integer(seed) && length(seed) == 1)
    }

    stopifnot(is.list(settings))
    stopifnot(is.list(inputs) && (length(inputs) == 0 || !is.null(names(inputs))))

    # ---Validation for constants---
    stopifnot(is.list(constants))
    if (length(constants) > 0) {
        stopifnot(!is.null(names(constants)) && all(names(constants) != ""))
    }
    stopifnot(is.list(model_expressions))

    # ---Distribution specs and validation---
    # if (is.null(dist_env)) {
    #     dist_env <- create_dist_env() # Defined in distributions_dsl.R
    # }
    # stopifnot(is.environment(dist_env))
    # stopifnot(is.list(history))

    spec <- structure(
        list(
            n_trials = n_trials,
            seed = seed,
            settings = settings,
            inputs = inputs,
            constants = constants,
            model_expressions = model_expressions,
            # dist_env = dist_env,
            history = history
        ),
        class = "gambl_spec"
    )

    spec
}


#' Print method for `gambl_spec` objects
#'
#' Provides a concise summary of the simulation specification.
#'
#' @param x A `gambl_spec` object.
#' @param ... Additional arguments (unused).
#' @export
print.gambl_spec <- function(x, ...) {
    cat("--- gamblr Simulation Specification ---\n")
    if (!is.null(x$settings$name)) {
        cat("Name:", x$settings$name, "\n")
    } else {
        cat("Name: <unnamed>\n")
    }
    cat("Number of trials:", x$n_trials, "\n")
    if (!is.null(x$seed)) {
        cat("Seed:", x$seed, "\n")
    }

    cat("Settings:\n")
    cat("  Device:", x$settings$device, "\n")
    backend_str <- if(is.list(x$settings$backend)) {
        paste0("list (type: ", x$settings$backend$type %||% "unknown", ")")
    } else {
        as.character(x$settings$backend)
    }
    cat("  Backend:", backend_str, "\n")
    if (!is.null(x$settings$n_workers)) {
        cat("  Workers:", x$settings$n_workers, "\n")
    }
    cat("  Verbose:", x$settings$verbose, "\n")
    cat("  Store History:", x$settings$store_history, "\n")

    cat("Inputs (RVs defined):", length(x$inputs), "\n")
    if (length(x$inputs) > 0) {
        cat("  Names:", paste(names(x$inputs), collapse = ", "), "\n")
    }
    cat("Constants defined:", length(x$constants), "\n") # This line was already good
    if (length(x$constants) > 0) {
        cat("  Names:", paste(names(x$constants), collapse = ", "), "\n")
    }
    cat("Model expressions (Outputs):", length(x$model_expressions), "\n")
    if (length(x$model_expressions) > 0) {
        model_names <- names(x$model_expressions)
        if(is.null(model_names) || any(model_names == "")) model_names <- paste0("output_", seq_along(x$model_expressions))
        cat("  Names:", paste(model_names, collapse = ", "), "\n")
    }

    if (isTRUE(x$settings$store_history) && length(x$history) > 0) {
        cat("History log entries:", length(x$history), "\n")
        # Potentially show the first call if it's sim_init
        # first_call_summary <- substr(x$history[[1]]$call, 1, 60)
        # if (nchar(x$history[[1]]$call) > 60) first_call_summary <- paste0(first_call_summary, "...")
        # cat("  Initial call:", first_call_summary, "\n")
    }
    cat("--- End of Specification ---\n")
    invisible(x)
}



