#' Define Random Variables for the Simulation
#'
#' Adds one or more random variables (RVs) to a `gambl_spec` object.
#' RVs are defined using a formula syntax, e.g., `rv_name ~ distribution_call(...)`
#' or `rv_name ~ calc(expression(...))`. Parameters of a distribution can be
#' constants or expressions involving other previously defined RVs or constants
#' from the `gambl_spec`.
#'
#' @param spec A `gambl_spec` object, as created by `sim_init()`.
#' @param ... One or more formulas defining random variables. Each formula should be
#'   of the form `variable_name ~ distribution_function(param1 = value, ...)` or
#'   `variable_name ~ calc(some_expression)`.
#'   `distribution_function` (e.g., `normal`, `uniform`, `calc`) must be a function
#'   available in `spec$dist_env` that returns a "distribution descriptor".
#'
#' @return The modified `gambl_spec` object with the new RVs added to `spec$inputs`.
#' @export
#' @examples
#' # This example assumes sim_init, new_gambl_spec, R6 classes, and create_dist_env
#' # are correctly defined and loaded as per the previous detailed answer.
#' #
#' # `%||%` <- function(a, b) if (is.null(a)) b else a # Helper for print.gambl_spec
#' # source("path/to/your/sim_init.R") # (which calls create_dist_env)
#' # source("path/to/your/r6_classes.R")
#' # source("path/to/your/create_dist_env.R")
#'
#' # sim_plan_example <- sim_init(
#' #   n_trials = 100,
#' #   constants = list(base_adj = 0.8, price_sens = 0.1)
#' # )
#' #
#' # if (interactive() && exists("sim_plan_example")) {
#' #   sim_plan_example <- sim_plan_example %>%
#' #     set_rv(
#' #       price ~ uniform(min = 10, max = 20),
#' #       demand_base ~ normal(mean = 100, sd = 10)
#' #     )
#' #
#' #   sim_plan_example <- sim_plan_example %>%
#' #     set_rv(
#' #       # Dependent RV: sd of demand_adj depends on 'price' (an existing RV)
#' #       # and base_adj, price_sens (constants)
#' #       demand_adj ~ normal(mean = demand_base * base_adj, sd = price * price_sens),
#' #       # Calculated RV
#' #       revenue_potential ~ calc(price * demand_adj)
#' #     )
#' #   print(sim_plan_example)
#' #   # To inspect (requires R6 classes to be loaded):
#' #   # print(sim_plan_example$inputs$demand_adj$distribution_obj$params_def)
#' #   # print(sim_plan_example$inputs$revenue_potential$distribution_obj$params_def)
#' # }
set_rv <- function(spec, ...) {
  # --- Input Validations ---
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

  # --- History Tracking ---
  if (isTRUE(spec$settings$store_history)) {
    current_call_str <- tryCatch(rlang::expr_deparse(match.call()), error = function(e) "set_rv(...)")
    spec$history <- append(spec$history, list(list(call = current_call_str, timestamp = Sys.time())))
  }

  dist_env <- spec$dist_env
  if (is.null(dist_env) || !is.environment(dist_env)) {
    cli::cli_abort(
      c("{.arg spec$dist_env} is not initialized or is not an environment.",
        "i" = "Ensure {.fn sim_init} correctly creates and assigns {.code spec$dist_env} using {.fn create_dist_env}."),
      call = NULL
    )
  }

  new_rv_names_in_this_call <- character()

  for (i in seq_along(formulas_quos_list)) {
    original_formula_quo <- formulas_quos_list[[i]]
    arg_name_attr <- names(formulas_quos_list)[i]

    formula_expr <- rlang::quo_get_expr(original_formula_quo)

    if (!rlang::is_formula(formula_expr, lhs = TRUE)) {
      err_msg <- paste0(
        "Argument ", if (nzchar(arg_name_attr)) paste0("{.arg ", arg_name_attr, "}") else paste0("#", i),
        " to {.fn set_rv} must be a two-sided formula like {.code variable_name ~ distribution_call(...)}."
      )
      provided_expr_str <- rlang::expr_deparse(formula_expr)
      cli::cli_abort(c(err_msg, "x" = "You provided: {.code {provided_expr_str}}"))
    }

    rv_name_expr <- rlang::f_lhs(formula_expr)
    if (!rlang::is_symbol(rv_name_expr)) {
      err_msg <- paste0(
        "The Left-Hand Side (LHS) of formula for argument ",
        if (nzchar(arg_name_attr)) paste0("{.arg ", arg_name_attr, "}") else paste0("#", i),
        " must be a simple variable name."
      )
      lhs_str <- rlang::expr_deparse(rv_name_expr)
      cli::cli_abort(c(err_msg, "x" = "Found LHS: {.code {lhs_str}}"))
    }
    rv_name <- rlang::as_string(rv_name_expr)

    # --- Name Conflict Checks ---
    if (rv_name %in% names(spec$inputs)) {
      cli::cli_abort("Random variable {.val {rv_name}} is already defined in {.code spec$inputs}.")
    }
    if (rv_name %in% names(spec$constants)) {
      cli::cli_abort("Random variable {.val {rv_name}} conflicts with an existing constant name.")
    }
    if (rv_name %in% new_rv_names_in_this_call) {
      cli::cli_abort("Random variable {.val {rv_name}} is defined multiple times in this call to {.fn set_rv}.")
    }
    new_rv_names_in_this_call <- c(new_rv_names_in_this_call, rv_name)

    # --- Evaluate Distribution Call (RHS) to get Descriptor ---
    formula_rhs_expr <- rlang::f_rhs(formula_expr)
    eval_env_for_rhs <- rlang::quo_get_env(original_formula_quo)

    dist_descriptor <- tryCatch({
      rlang::eval_tidy(formula_rhs_expr, data = dist_env, env = eval_env_for_rhs)
    },
    error = function(e) {
      rhs_str <- rlang::expr_deparse(formula_rhs_expr)
      cli::cli_abort(
        c("Error evaluating the distribution call for RV {.val {rv_name}}:",
          "i" = "Expression: {.code {rhs_str}}",
          "x" = "Original error: {e$message}"),
        parent = e
      )
    }
    )

    if (!inherits(dist_descriptor, "gambl_distribution_descriptor")) {
      rhs_str <- rlang::expr_deparse(formula_rhs_expr)
      cli::cli_abort(
        c("The RHS for RV {.val {rv_name}} did not return a valid distribution descriptor.",
          "i" = "Expression: {.code {rhs_str}}",
          "i" = "Ensure functions like {.fn normal}, {.fn uniform}, or {.fn calc} are used correctly from {.code spec$dist_env}.")
      )
    }

    # --- Instantiate GamblDistributionR6 child object ---
    dist_r6_class_name <- dist_descriptor$dist_r6_class_name
    if (!rlang::env_has(dist_env, dist_r6_class_name)) {
      cli::cli_abort(
        "R6 class generator {.val {dist_r6_class_name}} for distribution {.val {dist_descriptor$user_dist_name}}
           not found in {.code spec$dist_env}."
      )
    }
    R6_class_generator <- rlang::env_get(dist_env, dist_r6_class_name)

    if (!inherits(R6_class_generator, "R6ClassGenerator")) { # R6::is.R6Class also works
      cli::cli_abort("{.val {dist_r6_class_name}} in {.code spec$dist_env} is not an R6 class generator.")
    }

    dist_obj <- tryCatch({
      R6_class_generator$new(params_def = dist_descriptor$params_quos)
    },
    error = function(e) {
      cli::cli_abort(
        c("Error instantiating distribution {.val {dist_descriptor$user_dist_name}} for RV {.val {rv_name}}:",
          "x" = "{e$message}"),
        parent = e
      )
    }
    )

    # --- Instantiate GamblInputR6 object ---
    captured_formula_rhs_quo <- rlang::new_quosure(formula_rhs_expr, env = eval_env_for_rhs)

    input_rv <- GamblInputR6$new(
      name = rv_name,
      distribution_obj = dist_obj,
      formula_rhs = captured_formula_rhs_quo
    )

    # --- Store GamblInputR6 object ---
    spec$inputs[[rv_name]] <- input_rv

    if (isTRUE(spec$settings$verbose)) {
      param_summary_exprs <- lapply(dist_descriptor$params_quos, rlang::quo_get_expr)
      param_strings <- paste(
        names(param_summary_exprs), "=",
        sapply(param_summary_exprs, rlang::expr_deparse, width = 60L),
        collapse = ", "
      )
      cli::cli_inform("Defined RV: {.val {rv_name}} ~ {dist_descriptor$user_dist_name}({param_strings})")
    }
  }

  return(spec)
}
