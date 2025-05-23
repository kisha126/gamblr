# --- Base GamblDistributionR6 Class ---
GamblDistributionR6 <- R6::R6Class("GamblDistributionR6",
                                   public = list(
                                       name = NULL,
                                       params_def = NULL,
                                       required_params = NULL,
                                       initialize = function(dist_name, params_def, required_params = character(0)) {
                                           stopifnot(is.character(dist_name), length(dist_name) == 1)
                                           stopifnot(is.list(params_def)) # Should be list of quosures
                                           stopifnot(is.character(required_params))
                                           if (!all(required_params %in% names(params_def))) {
                                               missing_params <- setdiff(required_params, names(params_def))
                                               cli::cli_abort("Distribution {.val {dist_name}} missing required parameter{?s}: {.val {missing_params}}.", call = NULL)
                                           }
                                           self$name <- dist_name
                                           self$params_def <- params_def
                                           self$required_params <- required_params
                                           invisible(self)
                                       },
                                       get_parameter_definitions = function() { self$params_def },
                                       sample = function(n, dynamic_param_values) { cli::cli_abort("Sample method must be implemented by child classes.", call = NULL) }
                                   )
)

# --- NormalDistributionR6 ---
NormalDistributionR6 <- R6::R6Class("NormalDistributionR6",
                                    inherit = GamblDistributionR6,
                                    public = list(
                                        initialize = function(params_def) { super$initialize(dist_name = "normal", params_def = params_def, required_params = c("mean", "sd")) },
                                        sample = function(n, dynamic_param_values) {
                                            mean_val <- dynamic_param_values$mean; sd_val <- dynamic_param_values$sd
                                            if (length(mean_val) == 1 && n > 1) mean_val <- rep(mean_val, n)
                                            if (length(sd_val) == 1 && n > 1) sd_val <- rep(sd_val, n)
                                            if (length(mean_val) != n || length(sd_val) != n) cli::cli_abort("NormalDist: params wrong length for n_trials={n}.", call=NULL)
                                            if (any(sd_val <= 0, na.rm = TRUE)) cli::cli_abort("NormalDist: 'sd' must be positive.", call=NULL)
                                            stats::rnorm(n, mean = mean_val, sd = sd_val)
                                        }
                                    )
)

# --- UniformDistributionR6 ---
UniformDistributionR6 <- R6::R6Class("UniformDistributionR6",
                                     inherit = GamblDistributionR6,
                                     public = list(
                                         initialize = function(params_def) { super$initialize(dist_name = "uniform", params_def = params_def, required_params = c("min", "max")) },
                                         sample = function(n, dynamic_param_values) {
                                             min_val <- dynamic_param_values$min; max_val <- dynamic_param_values$max
                                             if (length(min_val) == 1 && n > 1) min_val <- rep(min_val, n)
                                             if (length(max_val) == 1 && n > 1) max_val <- rep(max_val, n)
                                             if (length(min_val) != n || length(max_val) != n) cli::cli_abort("UniformDist: params wrong length for n_trials={n}.", call=NULL)
                                             if (any(min_val >= max_val, na.rm = TRUE)) cli::cli_abort("UniformDist: 'min' must be < 'max'.", call=NULL)
                                             stats::runif(n, min = min_val, max = max_val)
                                         }
                                     )
)

# --- BetaDistributionR6 ---
BetaDistributionR6 <- R6::R6Class("BetaDistributionR6",
                                  inherit = GamblDistributionR6,
                                  public = list(
                                      initialize = function(params_def) { super$initialize(dist_name = "beta", params_def = params_def, required_params = c("shape1", "shape2")) },
                                      sample = function(n, dynamic_param_values) {
                                          s1 <- dynamic_param_values$shape1; s2 <- dynamic_param_values$shape2
                                          if (length(s1) == 1 && n > 1) s1 <- rep(s1, n)
                                          if (length(s2) == 1 && n > 1) s2 <- rep(s2, n)
                                          if (length(s1) != n || length(s2) != n) cli::cli_abort("BetaDist: params wrong length for n_trials={n}.", call=NULL)
                                          if (any(s1 <= 0, na.rm=TRUE) || any(s2 <= 0, na.rm=TRUE)) cli::cli_abort("BetaDist: shapes must be positive.", call=NULL)
                                          stats::rbeta(n, shape1 = s1, shape2 = s2)
                                      }
                                  )
)

# --- LognormalDistributionR6 ---
LognormalDistributionR6 <- R6::R6Class("LognormalDistributionR6",
                                       inherit = GamblDistributionR6,
                                       public = list(
                                           initialize = function(params_def) { super$initialize(dist_name = "lognormal", params_def = params_def, required_params = c("meanlog", "sdlog")) },
                                           sample = function(n, dynamic_param_values) {
                                               ml <- dynamic_param_values$meanlog; sl <- dynamic_param_values$sdlog
                                               if (length(ml) == 1 && n > 1) ml <- rep(ml, n)
                                               if (length(sl) == 1 && n > 1) sl <- rep(sl, n)
                                               if (length(ml) != n || length(sl) != n) cli::cli_abort("LognormalDist: params wrong length for n_trials={n}.", call=NULL)
                                               if (any(sl <= 0, na.rm=TRUE)) cli::cli_abort("LognormalDist: sdlog must be positive.", call=NULL)
                                               stats::rlnorm(n, meanlog = ml, sdlog = sl)
                                           }
                                       )
)

# --- CalculatedDistributionR6 ---
CalculatedDistributionR6 <- R6::R6Class("CalculatedDistributionR6",
                                        inherit = GamblDistributionR6,
                                        public = list(
                                            initialize = function(params_def) { # params_def will be list(expr = quo(actual_expression))
                                                super$initialize(dist_name = "calc", params_def = params_def, required_params = "expr")
                                            },
                                            sample = function(n, dynamic_param_values) {
                                                expr_quo <- self$params_def$expr
                                                result <- rlang::eval_tidy(expr_quo, data = dynamic_param_values)
                                                if (length(result) == 1 && n > 1) {
                                                    result <- rep(result, n)
                                                } else if (length(result) != n) {
                                                    cli::cli_abort(
                                                        paste0("Calculated RV expression for '", rlang::quo_text(expr_quo),
                                                               "' did not evaluate to a vector of length n_trials ({n}) or length 1. ",
                                                               "Actual length: {length(result)}."),
                                                        call = NULL
                                                    )
                                                }
                                                return(result)
                                            }
                                        )
)

# --- GamblInputR6 Class ---
GamblInputR6 <- R6::R6Class("GamblInputR6",
                            public = list(
                                name = NULL,
                                distribution_obj = NULL,
                                formula_rhs = NULL,
                                initialize = function(name, distribution_obj, formula_rhs) {
                                    stopifnot(is.character(name), length(name) == 1)
                                    if (!inherits(distribution_obj, "GamblDistributionR6")) cli::cli_abort("`distribution_obj` must inherit from `GamblDistributionR6`.", call=NULL)
                                    stopifnot(rlang::is_quosure(formula_rhs))
                                    self$name <- name
                                    self$distribution_obj <- distribution_obj
                                    self$formula_rhs <- formula_rhs
                                    invisible(self)
                                },
                                get_dependencies = function() {
                                    all_dep_vars <- c()
                                    if (!is.null(self$distribution_obj) && !is.null(self$distribution_obj$params_def)) {
                                        for (param_quo in self$distribution_obj$params_def) {
                                            all_dep_vars <- c(all_dep_vars, rlang::all_vars(rlang::quo_get_expr(param_quo)))
                                        }
                                    }
                                    return(unique(all_dep_vars))
                                }
                            )
)


# --- II. Define create_dist_env() (needs R6 classes above) ---
create_dist_env <- function() {
    env <- rlang::new_environment()
    make_dist_descriptor <- function(dist_r6_class_name, user_dist_name, ...) {
        params_quos <- rlang::enquos(...)
        if (length(params_quos) > 0 && (is.null(names(params_quos)) || any(names(params_quos) == ""))) {
            cli::cli_abort("All parameters for distribution {.val {user_dist_name}} must be named.", call=NULL)
        }
        structure(list(dist_r6_class_name = dist_r6_class_name, user_dist_name = user_dist_name, params_quos = params_quos),
                  class = "gambl_distribution_descriptor")
    }

    env$normal    <- function(mean, sd) make_dist_descriptor("NormalDistributionR6", "normal", mean = {{mean}}, sd = {{sd}})
    env$uniform   <- function(min, max) make_dist_descriptor("UniformDistributionR6", "uniform", min = {{min}}, max = {{max}})
    env$beta      <- function(shape1, shape2) make_dist_descriptor("BetaDistributionR6", "beta", shape1 = {{shape1}}, shape2 = {{shape2}})
    env$lognormal <- function(meanlog, sdlog) make_dist_descriptor("LognormalDistributionR6", "lognormal", meanlog = {{meanlog}}, sdlog = {{sdlog}})
    env$calc      <- function(expr) make_dist_descriptor("CalculatedDistributionR6", "calc", expr = {{expr}})

    env$NormalDistributionR6    <- NormalDistributionR6
    env$UniformDistributionR6   <- UniformDistributionR6
    env$BetaDistributionR6      <- BetaDistributionR6
    env$LognormalDistributionR6 <- LognormalDistributionR6
    env$CalculatedDistributionR6 <- CalculatedDistributionR6

    return(env)
}


# --- III. Define new_gambl_spec(), sim_init() ---
`%||%` <- function(a, b) if (is.null(a)) b else a

new_gambl_spec <- function(n_trials, seed = NULL, settings = list(), inputs = list(), constants = list(),
                           model_expressions = list(), dist_env, history = list()) {
    stopifnot(is.integer(n_trials) && n_trials > 0)
    if (!is.null(seed)) stopifnot(is.integer(seed) && length(seed) == 1)
    stopifnot(is.list(settings), is.list(inputs), is.list(constants), is.list(model_expressions), is.environment(dist_env), is.list(history))
    if (length(inputs) > 0 && (is.null(names(inputs)) || any(names(inputs)==""))) stop("All elements in `inputs` must be named if list is not empty.")
    if (length(constants) > 0 && (is.null(names(constants)) || any(names(constants) == ""))) stop("All elements in `constants` list must be named.")

    structure(list(n_trials = n_trials, seed = seed, settings = settings, inputs = inputs, constants = constants,
                   model_expressions = model_expressions, dist_env = dist_env, history = history), class = "gambl_spec")
}
