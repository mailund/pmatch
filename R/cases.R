
#' Recursive comparison of expression and pattern.
#'
#' @param escape     Continuation from callCC, used to escape if we cannot match.
#' @param expr       The expression to match again.
#' @param test_expr  The pattern we are trying to match.
#' @param eval_env   The environment where we get constructors from.
#' @param match_env  The environment to put matched variables in.
#'
#' @return An environment containing bound variables from the expression, if matching.
#'         If the pattern doesn't match, the function escapes through the \code{escape}
#'         continuation.
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

#' @describeIn test_pattern Version that quotes \code{test_expr} itself.
#' @export
test_pattern_ <- function(expr, test_expr,
                          eval_env = rlang::caller_env(),
                          match_parent_env = rlang::caller_env()) {
    # Environment in which to store matched variables
    match_env <- rlang::child_env(.parent = match_parent_env)

    if (test_expr == quote(otherwise)) {
        return(match_env)
    }

    # Test pattern
    tester <- function(escape) {
        test_pattern_rec(escape, expr, test_expr, eval_env, match_env)
    }
    callCC(tester)
}

#' Test if a pattern matches an expression
#'
#' Test if a value, \code{expr}, created from constructors matches a pattern of constructors.
#' The \code{test_pattern_} function requires that \code{test_expr} is a quoted expression
#' while the \code{test_pattern} function expects a bare expression and will quote it
#' itself.
#'
#' @param expr             A value created using constructors.
#' @param test_expr        A constructor pattern to test \code{expr} against.
#' @param eval_env         The environment where constructors can be found.
#' @param match_parent_env Environment to use as the parent of the match bindings we return.
#'                         This parameter enables you provide additional values to
#'                         the environment where match-expressions are evaluated.
#'
#' @return \code{NULL} if the pattern does not match or an environment with bound
#'         variables if it does.
#'
#' @examples
#' type := ZERO | ONE(x) | TWO(x,y)
#' zero <- ZERO
#' one <- ONE(1)
#' two <- TWO(1,2)
#'
#' as.list(test_pattern(zero, ZERO)) # returns an empty binding
#' test_pattern_(one, quote(ZERO)) # returns NULL
#' as.list(test_pattern_(one, quote(ONE(v)))) # returns a binding for v
#' as.list(test_pattern(two, TWO(v,w))) # returns a binding for v and w
#'
#' @describeIn test_pattern Version that quotes \code{test_expr} itself.
#' @export
test_pattern <- function(expr, test_expr,
                         eval_env = rlang::caller_env(),
                         match_parent_env = rlang::caller_env()) {
    test_pattern_(expr, rlang::enexpr(test_expr), eval_env, match_parent_env)
}

#' Raise an error if a match expression is malformed.
#'
#' @param match_expr The match expression
assert_correctly_formed_pattern_expression <- function(match_expr) {
    if (!rlang::is_lang(match_expr) || match_expr[[1]] != "<-") {
        error_msg <- glue::glue(
            "Malformed matching rule. Rules must be on the form 'pattern -> expression'."
        )
        stop(simpleError(error_msg, call = match_expr))
    }
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
#' @export
cases <- function(expr, ...) {
    matchings <- rlang::quos(...)

    for (i in seq_along(matchings)) {
        eval_env <- rlang::get_env(matchings[[i]])
        match_expr <- rlang::quo_expr(matchings[[i]])
        assert_correctly_formed_pattern_expression(match_expr)

        test_expr <- match_expr[[3]]
        result_expr <- match_expr[[2]]

        match <- test_pattern_(expr, test_expr, eval_env)
        if (!rlang::is_null(match)) {
            return(rlang::eval_tidy(result_expr, data = match, env = eval_env))
        }
    }

    error_msg <- glue::glue(
        "None of the patterns matched the expression."
    )
    stop(simpleError(error_msg, call = match.call()))
}

#' Create an if-statement for \code{\link{cases_expr}} and \code{\link{cases_expr_}} functions
#'
#' @param expr        The expression we pattern match against.
#' @param match_expr  The pattern specification, on the form pattern -> expression
#' @param continue    The expression that goes in the \code{else} part of the \code{if}
#'                    expression. If this is \code{NULL}, we create an \code{if}-expression
#'                    instead of an \code{if-else}-expression.
#'
#' @return A new if-expression
make_match_expr <- function(expr, match_expr, continue) {
    assert_correctly_formed_pattern_expression(match_expr)
    pattern_test <-
        rlang::expr(!rlang::is_null(..match_env <- pmatch::test_pattern(!! expr, !! match_expr[[3]])))
    eval_match <-
        rlang::expr(with(..match_env, !! match_expr[[2]]))

    if (rlang::is_null(continue)) {
        rlang::call2("if", pattern_test, eval_match)
    } else {
        rlang::call2("if", pattern_test, eval_match, continue)
    }
}

#' @describeIn cases_expr Version that expects \code{expr} to be quoted.
#' @export
cases_expr_ <- function(expr, ...) {
    matchings <- rlang::exprs(...)

    if (length(matchings) < 1) {
        error_msg <- glue::glue(
            "At least one pattern must be provided."
        )
        stop(simpleError(error_msg, call = match.call()))
    }

    rev_match_indices <- rev(seq_along(matchings))
    last_idx <- rev_match_indices[1]
    continue_expr <- make_match_expr(expr, matchings[[last_idx]], NULL)
    for (i in rev_match_indices[c(-1)]) {
        continue_expr <- make_match_expr(expr, matchings[[i]], continue_expr)
    }
    return(continue_expr)
}

#' Create an expression that tests patterns against an expression in turn
#'
#' Where \code{\link{cases}} evaluates expressions based on pattern matches, this function
#' creates a long if-else expression that tests patterns in turn and evaluate the expression
#' for a matching pattern. This function is intended for meta-programming rather than
#' usual pattern matching.
#'
#' @param expr The expression to test against. This is usually a bare symbol.
#' @param ... Pattern matching rules as in \code{\link{cases}}.
#'
#' @examples
#' linked_list := NIL | CONS(car, cdr : linked_list)
#'
#' length_body <- cases_expr(lst, NIL -> acc, CONS(car, cdr) -> ll_length(cdr, acc + 1))
#' length_body
#'
#' ll_length <- rlang::new_function(alist(lst=, acc = 0), length_body)
#' ll_length(CONS(1, CONS(2, CONS(3, CONS(4, NIL)))))
#'
#' @describeIn cases_expr Version that quotes \code{expr} itself.
#' @export
cases_expr <- function(expr, ...) {
    expr <- rlang::enexpr(expr)
    cases_expr_(expr, ...)
}

## tailr transformer
tailr_transform_call <- function(expr) {
    stopifnot(rlang::call_name(expr) == "cases")

    args <- rlang::call_args(expr)
    value <- args[[1]]
    patterns <- args[-1]
    eval(rlang::expr(cases_expr(!! value, !!! patterns)))
}
attr(cases, "tailr_transform") <- tailr_transform_call
