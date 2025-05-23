sim_init <- function(n_trials,
                     seed = NULL,
                     constants = list(),
                     device = "cpu",
                     name = NULL,
                     verbose = FALSE,
                     store_history = FALSE,
                     backend = "local",
                     n_workers = NULL) {

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
    if (!is.list(constants)) {
        stop("`constants` must be a list.", call. = FALSE)
    }
    if (length(constants) > 0 && (is.null(names(constants)) || any(names(constants) == ""))) {
        stop("All elements in `constants` list must be named.", call. = FALSE)
    }
    # Add other input validations as in your original blueprint if needed

    settings <- list(
        device = device,
        name = name,
        verbose = verbose,
        store_history = store_history,
        backend = backend,
        n_workers = n_workers
    )

    current_call_str <- tryCatch(rlang::expr_deparse(match.call()), error = function(e) "sim_init(...)")
    history_list <- if (store_history) list(list(call = current_call_str, timestamp = Sys.time())) else list()

    cat("--- DEBUG: Inside sim_init --- Before create_dist_env ---\n")
    dist_env_for_spec <- create_dist_env()
    cat("--- DEBUG: Inside sim_init --- After create_dist_env ---\n")
    cat(paste("  Is dist_env_for_spec an environment?", is.environment(dist_env_for_spec)), "\n")
    if(is.environment(dist_env_for_spec)) {
        cat("  Contents of dist_env_for_spec (first few):\n")
        print(head(ls(dist_env_for_spec)))
    }

    spec <- new_gambl_spec(
        n_trials = n_trials,
        seed = seed,
        settings = settings,
        inputs = list(),
        constants = constants,
        model_expressions = list(),
        dist_env = dist_env_for_spec,
        history = history_list
    )

    cat("--- DEBUG: Inside sim_init --- After new_gambl_spec ---\n")
    cat(paste("  Is spec$dist_env an environment?", is.environment(spec$dist_env)), "\n")
    if(!is.null(spec$dist_env) && is.environment(spec$dist_env)) {
        cat("  Contents of spec$dist_env (first few):\n")
        print(head(ls(spec$dist_env)))
    }
    cat("--- DEBUG: Exiting sim_init ---\n")

    if (verbose) {
        cli::cli_inform("gamblr specification initialized.")
        if (!is.null(name)) cli::cli_inform("Simulation name: {.val {name}}")
        cli::cli_inform("Number of trials: {.val {n_trials}}")
        if(!is.null(seed)) cli::cli_inform("Seed: {.val {seed}}")
        if(length(constants)>0) cli::cli_inform("Constants: {.val {names(constants)}}")
    }
    spec
}

# --- Print method for gambl_spec ---
print.gambl_spec <- function(x, ...) {
    cat("--- gamblr Simulation Specification ---\n")
    if (!is.null(x$settings$name)) cat("Name:", x$settings$name, "\n") else cat("Name: <unnamed>\n")
    cat("Number of trials:", x$n_trials, "\n")
    if (!is.null(x$seed)) cat("Seed:", x$seed, "\n")
    cat("Settings:\n")
    cat("  Device:", x$settings$device, "\n")
    backend_str <- if(is.list(x$settings$backend)) paste0("list (type: ", x$settings$backend$type %||% "unknown", ")") else as.character(x$settings$backend)
    cat("  Backend:", backend_str, "\n")
    if (!is.null(x$settings$n_workers)) cat("  Workers:", x$settings$n_workers, "\n")
    cat("  Verbose:", x$settings$verbose, "\n")
    cat("  Store History:", x$settings$store_history, "\n")
    cat("Inputs (RVs defined):", length(x$inputs), "\n")
    if (length(x$inputs) > 0) cat("  Names:", paste(names(x$inputs), collapse = ", "), "\n")
    cat("Constants defined:", length(x$constants), "\n")
    if (length(x$constants) > 0) cat("  Names:", paste(names(x$constants), collapse = ", "), "\n")
    cat("Model expressions (Outputs):", length(x$model_expressions), "\n")
    if (length(x$model_expressions) > 0) {
        model_names <- names(x$model_expressions)
        if(is.null(model_names) || any(model_names == "")) model_names <- paste0("output_", seq_along(x$model_expressions))
        cat("  Names:", paste(model_names, collapse = ", "), "\n")
    }
    if (isTRUE(x$settings$store_history) && length(x$history) > 0) cat("History log entries:", length(x$history), "\n")
    cat("--- End of Specification ---\n")
    invisible(x)
}

set_rv <- function(spec, ...) {
    if (!inherits(spec, "gambl_spec")) {
        cli::cli_abort("`spec` must be a {.cls gambl_spec} object.")
    }

    formulas_quos_list <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")

    if (length(formulas_quos_list) == 0) {
        if (isTRUE(spec$settings$verbose)) {
            cli::cli_inform("No RV definitions provided to {.fn set_rv}.")
        }
        return(spec)
    }

    if (isTRUE(spec$settings$store_history)) {
        current_call_str <- tryCatch(rlang::expr_deparse(match.call()), error = function(e) "set_rv(...)")
        spec$history <- append(spec$history, list(list(call = current_call_str, timestamp = Sys.time())))
    }

    dist_env <- spec$dist_env # This comes from the spec object
    cat("--- DEBUG: Inside set_rv --- Checking spec$dist_env ---\n") # DEBUG
    cat(paste("  Is spec$dist_env an environment in set_rv?", is.environment(dist_env)), "\n") # DEBUG
    if (is.null(dist_env) || !is.environment(dist_env)) {
        cli::cli_abort(
            c("{.arg spec$dist_env} is not initialized or is not an environment.",
              "i" = "Ensure {.fn sim_init} correctly creates and assigns {.code spec$dist_env} using {.fn create_dist_env}."),
            call = NULL
        )
    }
    if(is.environment(dist_env)) { # DEBUG
        cat("  Contents of spec$dist_env in set_rv (first few):\n") # DEBUG
        print(head(ls(dist_env))) # DEBUG
    }

    new_rv_names_in_this_call <- character()
    for (i in seq_along(formulas_quos_list)) {
        original_formula_quo <- formulas_quos_list[[i]]
        formula_expr <- rlang::quo_get_expr(original_formula_quo)
        if (!rlang::is_formula(formula_expr, lhs = TRUE)) {
            arg_name_attr <- names(formulas_quos_list)[i]
            err_msg <- paste0("Argument ", if (nzchar(arg_name_attr)) paste0("{.arg ", arg_name_attr, "}") else paste0("#", i)," to {.fn set_rv} must be a two-sided formula like {.code name ~ dist(...)}")
            cli::cli_abort(c(err_msg, "x" = paste0("You provided: ", rlang::expr_deparse(formula_expr))))
        }
        rv_name_expr <- rlang::f_lhs(formula_expr)
        if (!rlang::is_symbol(rv_name_expr)) cli::cli_abort("LHS of formula must be a simple name.")
        rv_name <- rlang::as_string(rv_name_expr)

        if (rv_name %in% names(spec$inputs)) cli::cli_abort("RV {.val {rv_name}} already defined.")
        if (rv_name %in% names(spec$constants)) cli::cli_abort("RV {.val {rv_name}} conflicts with constant.")
        if (rv_name %in% new_rv_names_in_this_call) cli::cli_abort("RV {.val {rv_name}} defined multiple times in this call.")
        new_rv_names_in_this_call <- c(new_rv_names_in_this_call, rv_name)

        formula_rhs_expr <- rlang::f_rhs(formula_expr)
        eval_env_for_rhs <- rlang::quo_get_env(original_formula_quo)
        dist_descriptor <- tryCatch(rlang::eval_tidy(formula_rhs_expr, data = dist_env, env = eval_env_for_rhs),
                                    error = function(e) cli::cli_abort(c("Error evaluating dist call for RV {.val {rv_name}}:", "i" = paste0("Expression: ", rlang::expr_deparse(formula_rhs_expr)), "x" = e$message), parent = e))
        if (!inherits(dist_descriptor, "gambl_distribution_descriptor")) cli::cli_abort(c("RHS for RV {.val {rv_name}} not valid dist descriptor.", "i" = paste0("Expression: ", rlang::expr_deparse(formula_rhs_expr))))

        dist_r6_class_name <- dist_descriptor$dist_r6_class_name
        if (!rlang::env_has(dist_env, dist_r6_class_name)) cli::cli_abort("R6 class {.val {dist_r6_class_name}} not in dist_env.")
        R6_class_generator <- rlang::env_get(dist_env, dist_r6_class_name)
        if (!inherits(R6_class_generator, "R6ClassGenerator")) cli::cli_abort("{.val {dist_r6_class_name}} not R6 generator.")

        dist_obj <- tryCatch(R6_class_generator$new(params_def = dist_descriptor$params_quos),
                             error = function(e) cli::cli_abort(c("Error instantiating dist {.val {dist_descriptor$user_dist_name}} for RV {.val {rv_name}}:", "x" = e$message), parent = e))
        captured_formula_rhs_quo <- rlang::new_quosure(formula_rhs_expr, env = eval_env_for_rhs)
        input_rv <- GamblInputR6$new(name = rv_name, distribution_obj = dist_obj, formula_rhs = captured_formula_rhs_quo)
        spec$inputs[[rv_name]] <- input_rv
        if (isTRUE(spec$settings$verbose)) {
            param_strings <- paste(names(dist_descriptor$params_quos), "=", sapply(lapply(dist_descriptor$params_quos, rlang::quo_get_expr), rlang::expr_deparse, width=60L), collapse=", ")
            cli::cli_inform("Defined RV: {.val {rv_name}} ~ {dist_descriptor$user_dist_name}({param_strings})")
        }
    }

    spec
}
