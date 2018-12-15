## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(pmatch)

## ------------------------------------------------------------------------
OPT := NONE | VALUE(val) | ERROR(err)

## ------------------------------------------------------------------------
try <- function(expr) {
    rlang::enquo(expr)
    tryCatch(VALUE(rlang::eval_tidy(expr)), 
             error = function(e) ERROR(e))
}

## ------------------------------------------------------------------------
cases(try(42),
      VALUE(val) -> val,
      ERROR(err) -> err,
      NONE -> "NOTHING")

## ------------------------------------------------------------------------
cases(try(x + 42), # x isn't defined...
      VALUE(val) -> val,
      ERROR(err) -> err,
      NONE -> "NOTHING")

## ---- error=TRUE---------------------------------------------------------
value <- function(x) {
    quoted_x <- rlang::enexpr(x)
    cases(x,
          VALUE(val) -> val,
          . -> stop(simpleError(
                paste(deparse(quoted_x), " is not a value."), 
              call = quoted_x
        )))
}
    

value(try(42))
value(try(42 + x))

## ------------------------------------------------------------------------
Ops.OPT <- function(e1, e2) {
    cases(..(e1, e2),
          ..(ERROR(err), .)        -> ERROR(err),
          ..(., ERROR(err))        -> ERROR(err),
          ..(NONE, .)              -> NONE,
          ..(., NONE)              -> NONE,
          ..(VALUE(v1), VALUE(v2)) -> VALUE(do.call(.Generic, list(v1, v2))),
          ..(VALUE(v1), v2)        -> VALUE(do.call(.Generic, list(v1, v2))),
          ..(v1, VALUE(v2))        -> VALUE(do.call(.Generic, list(v1, v2)))
    )
}

## ------------------------------------------------------------------------
VALUE(12) + VALUE(6)
NONE + VALUE(6)
ERROR("foo") + NONE
VALUE(12) + ERROR("bar")
VALUE(12) + 12
12 + NONE
12 + try(42 + x)

## ------------------------------------------------------------------------
Math.OPT <- function(x, ...) {
    cases(x,
          ERROR(err) -> ERROR(err),
          NONE       -> NONE,
          VALUE(v)   -> do.call(.Generic, list(x)),
          v          -> do.call(.Generic, list(x))
    )
}

## ------------------------------------------------------------------------
log(ERROR("foo"))
exp(NONE)

## ------------------------------------------------------------------------
with_values <- function(...) {
    optionals <- rlang::enquos(...)
    n <- length(optionals)
    body <- optionals[[n]]
    optionals <- optionals[-n]
    ev <- rlang::child_env(rlang::get_env(body))
    
    try_ <- function(q, ev) {
        suppressWarnings(
            tryCatch(VALUE(rlang::eval_tidy(q, data = ev)), 
                 error = function(e) ERROR(e))
        )
    }
    to_val <- function(opt, escape) {
        cases(
            opt,
            ERROR(err) -> escape(ERROR(err)),
            NONE       -> escape(NONE),
            VALUE(val) -> val,
            val        -> val
        )
    }
    to_opt <- function(val) {
        cases(
            val,
            ERROR(err) -> ERROR(err),
            NONE       -> NONE,
            VALUE(val) -> VALUE(val),
            val        -> VALUE(val)
        )
    }
    
    callCC(function(escape) {
        for (q in optionals) {
            if (rlang::quo_is_symbol(q)) {
                var_name <- as.character(q[[2]])
                ev[[var_name]] <- to_val(
                    rlang::eval_tidy(q, data = ev), 
                    escape
                )
                
            } else if (rlang::quo_is_call(q)) {
                stopifnot(rlang::call_name(q) == "<-" ||
                          rlang::call_name(q) == "==")
                var_name <- as.character(q[[2]][[2]])
                val_expr <- q[[2]][[3]]
                ev[[var_name]] <- to_val(
                    try_(val_expr, ev),
                    escape
                )
                
            }  else {
                stop("Optional values must be names of assignments.")
            }
        }
        to_opt(rlang::eval_tidy(body, data = ev))
    })
    
}

x <- VALUE(1)
y <- VALUE(2)
with_values(
    x, y,
    z <- VALUE(3),
    w <- x + y + z,
    (w - x - y) / z
)

## ------------------------------------------------------------------------
with_values(
    f <- file("no such file", "r"),
    readLines(f)
)

