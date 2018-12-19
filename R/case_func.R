

func_constructor_test <- function(pattern_expr, nesting, eval_env) {
    if (rlang::is_call(pattern_expr)) {
        name <- rlang::as_string(pattern_expr[[1]])
        constructor <- get(name, eval_env)

        constructor_vars <- names(formals(constructor))

        if (!rlang::is_null(constructor) && inherits(constructor, "constructor")) {
            test_exprs <- rlang::expr({
                .cons <- attr(!!nesting, "constructor")
                !is.null(.cons) && .cons == !!name
            })

            for (i in 2:length(pattern_expr)) {
                var <- rlang::sym(constructor_vars[i - 1])
                nesting_nesting <- call("$", nesting, var)
                test_exprs <- call(
                    "&&",
                    test_exprs,
                    transform_match(
                        pattern_expr[[i]],
                        nesting_nesting,
                        eval_env
                    )
                )
            }
            return(test_exprs)
        }
    }
    NULL
}

const_constructor_test <- function(pattern_expr, nesting, eval_env) {
    # Is it a constructor?
    if (rlang::is_symbol(pattern_expr) &&
        exists(rlang::as_string(pattern_expr), eval_env)) {
        name <- rlang::as_string(pattern_expr)
        val <- get(name, eval_env)
        val_constructor <- attr(val, "constructor_constant")

        if (!rlang::is_null(val_constructor)) {
            # We have a constructor but is it the actual constant?
            if (val_constructor == name) {
                test_expr <- rlang::expr(
                    is.na(!!nesting) && attr(!!nesting, "constructor") == !!name
                )
                return(test_expr)
            }
        }
    }

    NULL
}

const_test <- function(pattern_expr, nesting, eval_env) {

    # This function *must* be tested after the const constructor test
    if (rlang::is_atomic(pattern_expr)) {
        rlang::expr({
            !!pattern_expr == !!nesting
        })
    } else {
        NULL
    }
}

var_test <- function(pattern_expr, nesting, eval_env) {

    # to silent lint
    `!<-` <- function() {
        NULL # nocov
    }

    # This function *must* be tested after the const constructor test
    if (rlang::is_symbol(pattern_expr)) {
        rlang::expr({
            !!pattern_expr <- !!nesting
            TRUE
        })
    } else {
        NULL # nocov
    }
}


transform_match <- function(pattern_expr, nesting, eval_env) {
    test_funcs <- c(
        func_constructor_test,
        const_constructor_test,
        const_test,
        var_test
    )
    for (func in test_funcs) {
        test <- func(pattern_expr, nesting, eval_env)
        if (!rlang::is_null(test)) {
            return(test)
        }
    }
    stop(glue::glue("Malformed pattern {pattern_expr}")) # nocov
}


#' Creates a pattern matching function.
#'
#' Creates a function that can contain expressions of a type defined by the
#' \code{\link{:=}} operator. The first argument of the generated function
#' will be matched against patterns provided in the \code{...} parameter of
#' this function.
#'
#' When you call the generated function, and the first argument is matching
#' a pattern, it evaluates the expression the
#' pattern is associated with. During matching, any symbol that is not
#' quasi-quoted will be considered a variable, and matching values will be bound
#' to such variables and be available when an expression is evaluated.
#'
#' Functions created with \code{case_func} do not support the `..` operator,
#' but you can always create constructors for fixed-number tuples, e.g.
#'
#' \code{
#' tuples := ..(first, second) | ...(first, second, third)
#' }
#'
#' Be careful not to use \code{.} here, if you use dot as a generic
#' variable.
#'
#' @param ...  A list of variables for the function in addition
#'  the data to be matched against which will automatically added
#'  plus \code{pattern -> expression} statements.
#' @return A function that can pattern match
#'
#' @seealso \code{\link{:=}} \code{\link{case_func}}
#'
#' @examples
#' linked_list := NIL | CONS(car, cdr : linked_list)
#' lst <- CONS(1, CONS(2, CONS(3, NIL)))
#' len <- case_func(acc = 0,
#'    NIL -> acc,
#'    CONS(car,cdr) -> len(cdr, acc + 1)
#' )
#' len(lst)
#'
#' list_sum <- case_func(acc = 0,
#'    NIL -> acc,
#'    CONS(car,cdr) -> list_sum(cdr, acc + car)
#' )
#' list_sum(lst)
#'
#' tuples := ..(first, second) | ...(first, second, third)
#' f <- case_func(..(.,.) -> 2, ...(.,.,.) -> 3)
#' f(..(1, 2))
#' f(...(1, 2, 3))
#'
#' @export
case_func <- function(...) {
    matchings <- rlang::quos(...)
    func_args <- c()
    eval_env <- rlang::caller_env()

    match_cases <- rlang::expr({
        stop("None of the patterns match.")
    })
    for (i in rev(seq_along(matchings))) {
        match_expr <- rlang::quo_expr(matchings[[i]])
        if (rlang::is_symbol(match_expr)) {
            if (names(matchings[i]) == "") {
                x <- list(rlang::missing_arg())
                names(x) <- rlang::as_string(match_expr)
                func_args <- c(func_args, x)
            } else {
                func_args <- c(func_args, matchings[i])
            }

            next
        }
        # the order of test and result depend on the syntax... for `->` the
        # R parser will switch the two; for `~` it will not.
        switch(as.character(match_expr[[1]]),
            "<-" = {
                pattern_expr <- match_expr[[3]]
                eval_expr <- match_expr[[2]]
            },
            "~" = {
                pattern_expr <- match_expr[[2]]
                eval_expr <- match_expr[[3]]
            }, {
                # not a pattern, make it an argument
                func_args <- c(func_args, matchings[i])
                next # not a pattern
            }
        )

        match_cases <-
            call(
                "if",
                transform_match(pattern_expr, quote(.match_expr), eval_env),
                eval_expr,
                match_cases
            )
    }

    func_args <- rev(lapply(func_args, rlang::quo_squash))
    func_args <- c(list(.match_expr = rlang::missing_arg()), func_args)
    rlang::new_function(
        func_args,
        match_cases,
        eval_env
    )
}


tr_transform_expr <- function(expr, dummy_func) {
    if (!rlang::is_call(expr) || rlang::call_name(expr) != "Recall") {
        rlang::expr(return(!!expr))
    } else {
        stopifnot(rlang::call_name(expr) == "Recall")
        expanded_call <- match.call(dummy_func, expr)
        call_args <- rlang::call_args(expanded_call)
        arg_names <- names(call_args)
        arg_symbols <- rlang::syms(arg_names)
        dummy_names <- rlang::syms(purrr::map(arg_names, ~ paste0(".", .x, collaps = "")))

        first <- purrr::map(seq_along(call_args), ~ rlang::expr(!!dummy_names[[.x]] <- !!call_args[[.x]]))
        first
        second <- purrr::map(seq_along(call_args), ~ rlang::expr(!!arg_symbols[[.x]] <- !!dummy_names[[.x]]))
        second

        assignments <- c(rlang::exprs(!!!first), rlang::exprs(!!!second))
        rlang::expr({!!!assignments})
    }
}



#' Creates a pattern matching function for tail-recursive functions.
#'
#' Creates a function that can contain expressions of a type defined by the
#' \code{\link{:=}} operator. The first argument of the generated function
#' will be matched against patterns provided in the \code{...} parameter of
#' this function.
#'
#' When you call the generated function, and the first argument is matching
#' a pattern, it evaluates the expression the
#' pattern is associated with. During matching, any symbol that is not
#' quasi-quoted will be considered a variable, and matching values will be bound
#' to such variables and be available when an expression is evaluated.
#'
#' Unlike the \code{\link{case_func}} function, you cannot use this function
#' to define pattern matching functions that are not tail recursive. Furthermore,
#' you must use \code{\link{Recall}} for recursive calls.
#'
#' Functions created with \code{case_trfunc} do not support the `..` operator,
#' but you can always create constructors for fixed-number tuples, e.g.
#'
#' \code{
#' tuples := ..(first, second) | ...(first, second, third)
#' }
#'
#' Be careful not to use \code{.} here, if you use dot as a generic
#' variable.
#'
#' @param ...  A list of variables for the function in addition
#'  the data to be matched against which will automatically added
#'  plus \code{pattern -> expression} statements.
#' @return A function that can pattern match
#'
#' @seealso \code{\link{:=}} \code{\link{case_func}}
#'
#' @examples
#' linked_list := NIL | CONS(car, cdr : linked_list)
#' lst <- CONS(1, CONS(2, CONS(3, NIL)))
#' len <- case_trfunc(acc = 0,
#'    NIL -> acc,
#'    CONS(car,cdr) -> len(cdr, acc + 1)
#' )
#' len(lst)
#'
#' list_sum <- case_trfunc(acc = 0,
#'    NIL -> acc,
#'    CONS(car,cdr) -> list_sum(cdr, acc + car)
#' )
#' list_sum(lst)
#'
#'
#' @export
case_trfunc <- function(...) {
    matchings <- rlang::quos(...)
    func_args <- c()
    eval_env <- rlang::caller_env()

    for (i in rev(seq_along(matchings))) {
        match_expr <- rlang::quo_expr(matchings[[i]])
        if (rlang::is_symbol(match_expr)) {
            if (names(matchings[i]) == "") {
                x <- list(rlang::missing_arg())
                names(x) <- rlang::as_string(match_expr)
                func_args <- c(func_args, x)
            } else {
                func_args <- c(func_args, matchings[i])
            }

            next
        }
        # the order of test and result depend on the syntax... for `->` the
        # R parser will switch the two; for `~` it will not.
        switch(as.character(match_expr[[1]]),
               "<-" = {
                   next
               },
               "~" = {
                   next
               }, {
                   # not a pattern, make it an argument
                   func_args <- c(func_args, matchings[i])
                   next # not a pattern
               }
        )
    }

    func_args <- rev(lapply(func_args, rlang::quo_squash))
    func_args <- c(list(.match_expr = rlang::missing_arg()), func_args)

    # used for translating Recall calls.
    dummy_func <- rlang::new_function(
        func_args,
        NULL,
        eval_env
    )

    match_cases <- rlang::expr({
        stop("None of the patterns match.")
    })
    for (i in rev(seq_along(matchings))) {
        match_expr <- rlang::quo_expr(matchings[[i]])
        if (rlang::is_symbol(match_expr)) {
            next
        }
        # the order of test and result depend on the syntax... for `->` the
        # R parser will switch the two; for `~` it will not.
        switch(as.character(match_expr[[1]]),
               "<-" = {
                   pattern_expr <- match_expr[[3]]
                   eval_expr <- match_expr[[2]]
               },
               "~" = {
                   pattern_expr <- match_expr[[2]]
                   eval_expr <- match_expr[[3]]
               }, {
                   # not a pattern, make it an argument
                   next # not a pattern
               }
        )

        match_cases <-
            call(
                "if",
                transform_match(pattern_expr, quote(.match_expr), eval_env),
                tr_transform_expr(eval_expr, dummy_func),
                match_cases
            )
    }

    body <- rlang::expr(
        repeat { !!match_cases }
    )

    rlang::new_function(
        func_args,
        body,
        eval_env
    )
}


