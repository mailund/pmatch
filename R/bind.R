
#' Recursive comparison of expression and pattern.
#'
#' @param escape     Continuation from callCC, used to escape if we cannot
#'   match.
#' @param expr       The expression to match again.
#' @param test_expr  The pattern we are trying to match.
#' @param eval_env   The environment where we get constructors from.
#' @param match_env  The environment to put matched variables in.
#'
#' @return An environment containing bound variables from the expression, if
#'   matching. If the pattern doesn't match, the function escapes through the
#'   \code{escape} continuation.
test_pattern_rec <- function(escape, expr, test_expr, eval_env, match_env) {

    # Is this a function-constructor?
    if (rlang::is_call(test_expr)) {
        func <- get(rlang::as_string(test_expr[[1]]), eval_env)
        if (inherits(func, "constructor")) {
            # This is a constructor.  Check if it is the right kind
            constructor <- rlang::as_string(test_expr[[1]])
            expr_constructor <- attr(expr, "constructor")
            if (rlang::is_null(expr_constructor) ||
                constructor != expr_constructor) {
                escape(NULL)
            } # wrong type

            # Now check recursively
            for (i in seq_along(expr)) {
                test_pattern_rec(
                    escape, expr[[i]], test_expr[[i + 1]],
                    eval_env, match_env
                )
            }

            # If we get here, the matching was successfull
            return(match_env)
        }
    }

    # Is this a constant-constructor?
    if (rlang::is_symbol(test_expr) &&
        exists(rlang::as_string(test_expr), eval_env)) {
        constructor <- rlang::as_string(test_expr)
        val <- get(constructor, eval_env)
        val_constructor <- attr(val, "constructor_constant")
        if (!rlang::is_null(val_constructor)) {
            # we have a constructor but is it the actual constant?
            if (val_constructor == constructor) {
                # the symbol refers to a literal constructor so
                # we treat it as such -- if it wasn't, we treat it
                # as a variable below.
                expr_constructor <- attr(expr, "constructor")
                if (rlang::is_null(expr_constructor) ||
                    constructor != expr_constructor) {
                    escape(NULL) # wrong type
                } else {
                    return(match_env) # Successfull match
                }
            }
        }
    }

    # Not a constructor.  Must be a value to compare with or a variable to bind
    # to
    if (rlang::is_symbol(test_expr)) {
        assign(rlang::as_string(test_expr), expr, match_env)
    } else {
        stop(glue::glue(
            "You cannot match against a constant in bind expressions ({deparse(expr)})"
        ))
    }

    match_env
}

#' @describeIn test_pattern Version that quotes \code{test_expr} itself.
#' @export
test_pattern_ <- function(expr, test_expr,
                          eval_env = rlang::caller_env(),
                          match_parent_env = rlang::caller_env()) {
    # Environment in which to store matched variables
    match_env <- rlang::child_env(.parent = match_parent_env)

    # Test pattern
    tester <- function(escape) {
        test_pattern_rec(escape, expr, test_expr, eval_env, match_env)
    }
    callCC(tester)
}

#' Test if a pattern matches an expression
#'
#' Test if a value, \code{expr}, created from constructors matches a pattern of
#' constructors. The \code{test_pattern_} function requires that
#' \code{test_expr} is a quoted expression while the \code{test_pattern}
#' function expects a bare expression and will quote it itself.
#'
#' @param expr             A value created using constructors.
#' @param test_expr        A constructor pattern to test \code{expr} against.
#' @param eval_env         The environment where constructors can be found.
#' @param match_parent_env Environment to use as the parent of the match
#'   bindings we return. This parameter enables you provide additional values to
#'   the environment where match-expressions are evaluated.
#'
#' @return \code{NULL} if the pattern does not match or an environment with
#'   bound variables if it does.
#'
#' @examples
#' type := ZERO | ONE(x) | TWO(x, y)
#' zero <- ZERO
#' one <- ONE(1)
#' two <- TWO(1, 2)
#'
#' as.list(test_pattern(zero, ZERO)) # returns an empty binding
#' test_pattern_(one, quote(ZERO)) # returns NULL
#' as.list(test_pattern_(one, quote(ONE(v)))) # returns a binding for v
#' as.list(test_pattern(two, TWO(v, w))) # returns a binding for v and w
#' @describeIn test_pattern Version that quotes \code{test_expr} itself.
#' @export
test_pattern <- function(expr, test_expr,
                         eval_env = rlang::caller_env(),
                         match_parent_env = rlang::caller_env()) {
    test_pattern_(expr, rlang::enexpr(test_expr), eval_env, match_parent_env)
}

#' Move the bound variables from one environment into another.
#'
#' @param from  The environment we want to copy from.
#' @param to    The environment where we want to bind the variables.
#' @param names Names of the variables to copy. By default, all of them.
copy_env <- function(from, to, names = ls(from, all.names = TRUE)) {
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
#' bind[x, y] <- c(2, 4)
#' x == 2
#' y == 4
#'
#' llist := NIL | CONS(car, cdr:llist)
#' L <- CONS(1, CONS(2, CONS(3, NIL)))
#' bind[CONS(first, CONS(second, rest))] <- L
#' first == 1
#' second == 2
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
