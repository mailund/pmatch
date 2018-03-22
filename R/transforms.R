# FIXME: Replace these with `foolbox` code when going to 0.1.4 id:1 gh:34 ic:gh

#' Recursive function for transforming a call `cases`.
#'
#' @param expr The expression to transform.
#' @return Updated expression.
transform_cases_call <- function(expr) {
    stopifnot(rlang::call_name(expr) == "cases")

    args <- rlang::call_args(expr)
    value <- args[[1]]
    patterns <- args[-1]
    eval(rlang::expr(cases_expr(!!value, !!!patterns)))
}

#' Recursive function for transforming a function that uses `cases`.
#'
#' @param expr The expression to transform.
#' @return Updated expression.
transform_cases_function_rec <- function(expr) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        expr
    } else {
        stopifnot(rlang::is_lang(expr))
        call_args <- rlang::call_args(expr)
        for (i in seq_along(call_args)) {
            expr[[i + 1]] <- transform_cases_function_rec(call_args[[i]])
        }
        # FIXME: This assumes that if we see `cases` it is pmatch::cases. id:0 gh:27 ic:gh
        # There could be other `cases`, from other scopes, so really,
        # we should carry the environment along the recursive calls and check.
        if (rlang::call_name(expr) == "cases") {
            expr <- transform_cases_call(expr)
        }
        expr
    }
}

#' Transform a function containing a `cases` call into one that
#' instead has if-statements.
#'
#' @param fun A function
#' @return Another function with a transformed body
#'
#' @seealso cases
#' @export
transform_cases_function <- function(fun) {
    if (!rlang::is_closure(fun)) {
        err <- simpleError("Function must be a closure to be transformed")
        stop(err)
    }
    body(fun) <- transform_cases_function_rec(body(fun))
    fun
}

## tailr transformer
attr(cases, "tailr_transform") <- transform_cases_call
