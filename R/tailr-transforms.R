
#' Transformation hook for tailr
#'
#' @param fun   The function used to dispatch the generic function on.
#'              This would be the \code{\link{cases}} function.
#' @param expr  The call expression to transform.
#' @return Transformed expression.
#' @export
transform_call.tailr_pmatch_cases <- function(fun, expr) {
    stopifnot(rlang::call_name(expr) == "cases")

    args <- rlang::call_args(expr)
    value <- args[[1]]
    patterns <- args[-1]
    eval(rlang::expr(cases_expr(!!value, !!!patterns)))
}

