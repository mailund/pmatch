#' Recursive function for transforming a call `cases`.
#'
#' @param expr The expression to transform.
#' @param ...  Additional callback arguments to make this work with `foolbox`
#' @return Updated expression.
transform_cases_call <- function(expr, ...) {
    args <- rlang::call_args(expr)
    value <- args[[1]]
    patterns <- args[-1]
    eval(rlang::expr(cases_expr(!!value, !!!patterns)))
}

#' Transform a function containing a `cases` call into one that
#' instead has if-statements.
#'
#' @param fun A function
#' @return Another function with a transformed body
#'
#' @seealso cases
#' @import foolbox
#' @export
transform_cases_function <- function(fun) {
    fun %>% rewrite() %>% foolbox::rewrite_with(
        rewrite_callbacks() %>%
            add_call_callback(cases, transform_cases_call)
    )
}

## tailr transformer
attr(cases, "tailr_transform") <- transform_cases_call
