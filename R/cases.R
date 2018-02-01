
test_pattern_rec <- function(escape, expr, test_expr, eval_env, match_env) {
    # Is this a function-constructor?
    if (rlang::is_lang(test_expr)) {
        func <- get(rlang::as_string(test_expr[[1]]), eval_env)
        if ("constructor" %in% class(func)) {
            # This is a constructor.  Check if it is the right kind
            constructor <- rlang::as_string(test_expr[[1]])
            expr_constructor <- attr(expr, "constructor")
            if (rlang::is_null(expr_constructor) || constructor != expr_constructor) 
                escape(NULL)  # wrong type
            
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
            expr_constructor <- attr(expr, "constructor")
            if (rlang::is_null(expr) || constructor != expr_constructor) {
                escape(NULL)  # wrong type
            } else {
                return(match_env)  # Successfull match
            }
        }
    }
    
    # Not a constructor.  Must be a value to compare with or a variable to bind to
    if (rlang::is_symbol(test_expr)) {
        assign(rlang::as_string(test_expr), expr, match_env)
    } else {
        value <- rlang::eval_tidy(test_expr, eval_env)
        if (expr != value) 
            escape(NULL)
    }
    
    match_env
}

test_pattern <- function(expr, test_expr, eval_env) {
    # Environment in which to store matched variables
    match_env <- rlang::env()
    
    if (test_expr == quote(otherwise)) 
        return(match_env)
    
    # Test pattern
    tester <- function(escape) {
        test_pattern_rec(escape, expr, test_expr, eval_env, match_env)
    }
    callCC(tester)
}

cases <- function(expr, ...) {
    matchings <- rlang::quos(...)
    matchings[[1]]
    
    for (i in seq_along(matchings)) {
        eval_env <- rlang::get_env(matchings[[i]])
        match_expr <- rlang::quo_expr(matchings[[i]])
        stopifnot(match_expr[[1]] == "<-")
        
        test_expr <- match_expr[[3]]
        result_expr <- match_expr[[2]]
        
        match <- test_pattern(expr, test_expr, eval_env)
        if (!rlang::is_null(match)) 
            return(rlang::eval_tidy(result_expr, data = match, env = eval_env))
    }
    
    stop("No matching pattern!")
}


