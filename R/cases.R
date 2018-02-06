
test_pattern_rec <- function(escape, expr, test_expr, eval_env, match_env) {
    # Is this a function-constructor?
    if (rlang::is_lang(test_expr)) {
        func <- get(rlang::as_string(test_expr[[1]]), eval_env)
        if (inherits(func, "constructor")) {
            # This is a constructor.  Check if it is the right kind
            constructor <- rlang::as_string(test_expr[[1]])
            expr_constructor <- attr(expr, "constructor")
            if (rlang::is_null(expr_constructor) || constructor != expr_constructor) {
                escape(NULL)
            } # wrong type

            # Now check recursively
            for (i in seq_along(expr)) {
                test_pattern_rec(escape, expr[[i]], test_expr[[i + 1]], eval_env, match_env)
            }

            # If we get here, the matching was successfull
            return(match_env)
        }
    }

    # Is this a constant-constructor?
    if (rlang::is_symbol(test_expr) && exists(rlang::as_string(test_expr), eval_env)) {
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
                if (rlang::is_null(expr_constructor) || constructor != expr_constructor) {
                    escape(NULL) # wrong type
                } else {
                    return(match_env) # Successfull match
                }
            }
        }
    }

    # Not a constructor.  Must be a value to compare with or a variable to bind to
    if (rlang::is_symbol(test_expr)) {
        assign(rlang::as_string(test_expr), expr, match_env)
    } else {
        value <- rlang::eval_tidy(test_expr, eval_env)
        if (expr != value) {
            escape(NULL)
        }
    }

    match_env
}

test_pattern <- function(expr, test_expr, eval_env) {
    # Environment in which to store matched variables
    match_env <- rlang::env()

    if (test_expr == quote(otherwise)) {
        return(match_env)
    }

    # Test pattern
    tester <- function(escape) {
        test_pattern_rec(escape, expr, test_expr, eval_env, match_env)
    }
    callCC(tester)
}

#' Dispatches from an expression to a matching pattern
#'
#' Given an expression of a type defined by the \code{\link{:=}} operator, \code{cases}
#' matches it against patterns until it find one that has the same structure as \code{expr}.
#' When it does, it evaluates the expression the pattern is associated with. During matching,
#' any symbol that is not quasi-quoted will be considered a variable, and matching
#' values will be bound to such variables and be available when an expression is evaluated.
#'
#' @param expr The value the patterns will be matched against.
#' @param ...  A list of \code{pattern -> expression} statements.
#' @return The value of the expression associated with the first matching pattern.
#'
#' @seealso \code{\link{:=}}
#'
#' @examples
#' linked_list := NIL | CONS(car, cdr : linked_list)
#' lst <- CONS(1, CONS(2, CONS(3, NIL)))
#' len <- function(lst, acc = 0) {
#'     cases(lst,
#'           NIL -> acc,
#'           CONS(car,cdr) -> len(cdr, acc + 1))
#' }
#' len(lst)
#'
#' list_sum <- function(lst, acc = 0) {
#'     cases(lst,
#'           NIL -> acc,
#'           CONS(car,cdr) -> list_sum(cdr, acc + car))
#' }
#' list_sum(lst)
#'
#' @importFrom rlang eval_tidy
#' @export
cases <- function(expr, ...) {
    matchings <- rlang::quos(...)

    for (i in seq_along(matchings)) {
        eval_env <- rlang::get_env(matchings[[i]])
        match_expr <- rlang::quo_expr(matchings[[i]])
        if (!rlang::is_lang(match_expr) || match_expr[[1]] != "<-") {
            error_msg <- glue::glue(
                "Malformed matching rule. Rules must be on the form 'pattern -> expression'."
            )
            stop(simpleError(error_msg, call = match_expr))
        }

        test_expr <- match_expr[[3]]
        result_expr <- match_expr[[2]]

        match <- test_pattern(expr, test_expr, eval_env)
        if (!rlang::is_null(match)) {
            return(rlang::eval_tidy(result_expr, data = match, env = eval_env))
        }
    }

    error_msg <- glue::glue(
        "None of the patterns matched the expression."
    )
    stop(simpleError(error_msg, call = match.call()))
}
