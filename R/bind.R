
#' Move the bound variables from one environment into another.
#'
#' @param from  The environment we want to copy from.
#' @param to    The environment where we want to bind the variables.
#' @param names Names of the variables to copy. By default, all of them.
copy_env <- function(from, to, names=ls(from, all.names = TRUE)) {
    mapply(
        assign, names, mget(names, from), list(to),
        SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
    invisible(NULL)
}

#' Dummy object used for generic function dispatching.
#' @export
bind <- structure(NA, class = "pmatch_bind")

#' Bind variables to pattern-matched expressions.
#'
#' The \code{bind} object itself doesn't do anything. It simply exists in order
#' to define notation for binding variables using the sub-script operator.
#'
#' @param dummy The \code{bind} object. Only used to dispatch to the right
#'   subscript operator.
#' @param ...   Patterns to assign to.
#' @param value Actual values to assign
#'
#' @examples
#' bind[x, y] <- c(2,4)
#' x == 2
#' y == 4
#'
#' llist := NIL | CONS(car, cdr : llist)
#' L <- CONS(1, CONS(2, CONS(3, NIL)))
#' bind[CONS(first, CONS(second, rest))] <- L
#' first == 1
#' second == 2
#'
#' @export
`[<-.pmatch_bind` <- function(dummy, ..., value) {
    force(value)
    var_env <- rlang::caller_env()
    patterns <- eval(substitute(alist(...)))

    # we have to treat single values special because a single constructor-value
    # is a list and we don't want to index into it as such.
    if (length(patterns) == 1) {
        value <- list(value)
    }

    for (i in seq_along(patterns)) {
        var_bindings <- test_pattern_(value[[i]], patterns[[i]], eval_env = var_env)
        if (is.null(var_bindings)) {
            msg <- simpleError(
                glue::glue(
                    "The pattern {deparse(patterns[[i]])} ",
                    "does not match its value."
                )
            )
            stop(msg)
        }
        copy_env(from = var_bindings, to = var_env)
    }

    dummy # always remember to return this so we don't set the bind object to NULL
}
