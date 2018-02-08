

#' Construct a pair-list of arguments that can be used to create a new function
#'
#' Given a list of argument names, construct a list of arguments with empty defaults.
#'
#' @param args List of variable names.
#' @return A pair list that can be used with rlang::new_function.
make_args_list <- function(args) {
    res <- replicate(length(args), substitute())
    names(res) <- args
    as.pairlist(res)
}

#' Build a tibble form a list of constructor arguments.
#'
#' @param argument The argument provided to a constructor in its definition
#' @return A tibble with a single row, the first column holds the argument name, the second its type.
process_arg <- function(argument) {
    error_msg <- glue::glue(
        "The constructor argument is malformed.\n",
        "The expression {deparse(argument)} should either be a bare symbol or on the form 'variable : type'."
    )
    if (rlang::is_lang(argument)) {
        if (argument[[1]] != ":") {
            stop(simpleError(error_msg, call = argument))
        }
        arg <- rlang::quo_name(argument[[2]])
        type <- rlang::quo_name(argument[[3]])
        tibble::tibble(arg = arg, type = type)
    } else if (rlang::is_symbol(argument)) {
        arg <- rlang::quo_name(argument)
        tibble::tibble(arg = arg, type = NA)
    } else {
        stop(simpleError(error_msg, call = argument))
    }
}

#' Construct a tibble from all the arguments of a constructor
#'
#' @param constructor_arguments The arguments provided in the constructor specification
#' @return The arguments represented as a tibble. The first column contain argument names, the second their types.
process_arguments <- function(constructor_arguments) {
    dplyr::bind_rows(purrr::map(as.list(constructor_arguments), process_arg))
}

#' Create a function constructor and put it in an environment.
#'
#' @param constructor The construct specification
#' @param data_type_name The type the constructor should generate
#' @param env The environment where we define the constructor
process_constructor_function <- function(constructor, data_type_name, env) {
    constructor_name <- rlang::quo_name(constructor[[1]])
    constructor_arguments <- process_arguments(constructor[-1])

    # create the constructor function
    constructor <- function() {
        args <- rlang::as_list(environment())

        # type check!
        for (i in seq_along(args)) {
            arg <- args[[constructor_arguments$arg[i]]]
            type <- constructor_arguments$type[i]
            if (!rlang::is_na(type) && !inherits(arg, type)) {
                error_msg <- glue::glue(
                    "The argument {arg} is of type {class(arg)} but should be of type {type}."
                )
                stop(simpleError(error_msg, call = match.call()))
            }
        }

        structure(args, constructor = constructor_name, class = data_type_name)
    }
    formals(constructor) <- make_args_list(constructor_arguments$arg)

    # set meta information about the constructor
    class(constructor) <- c("constructor", "function")

    # put the constructor in the binding scope
    assign(constructor_name, constructor, envir = env)
}

#' Create a constant constructor and put it in an environment.
#'
#' @param constructor The construct specification
#' @param data_type_name The type the constructor should generate
#' @param env The environment where we define the constructor
process_constructor_constant <- function(constructor, data_type_name, env) {
    constructor_name <- rlang::as_string(constructor)
    constructor_object <- structure(NA, constructor_constant = constructor_name, class = data_type_name)
    assign(constructor_name, constructor_object, envir = env)
}

#' Create a constructor and put it in an environment.
#'
#' @param constructor The construct specification
#' @param data_type_name The type the constructor should generate
#' @param env The environment where we define the constructor
process_constructor <- function(constructor, data_type_name, env) {
    if (rlang::is_lang(constructor)) {
        process_constructor_function(constructor, data_type_name, env)
    } else if (rlang::is_symbol(constructor)) {
        process_constructor_constant(constructor, data_type_name, env)
    } else {
        error_msg <- glue::glue(
            "The constructor is malformed.\n",
            "Constructors must either be constanst, i.e. bare symbols, or in the form of a function call."
        )
        stop(simpleError(error_msg, call = constructor))
    }
}

#' Goes through a list of |-separated expressions and define them as constructors
#'
#' @param constructors The constructs specification
#' @param data_type_name The type the constructor should generate
#' @param env The environment where we define the constructor
process_alternatives <- function(constructors, data_type_name, env) {
    if (rlang::is_lang(constructors) && constructors[[1]] == "|") {
        process_alternatives(constructors[[2]], data_type_name, env)
        process_alternatives(constructors[[3]], data_type_name, env)
    } else {
        process_constructor(constructors, data_type_name, env)
    }
}

#' Create a string representation from a constructed object
#'
#' @param object The object to translate into a string
#' @return A string representation of \code{object}
deparse_construction <- function(object) {
    constructor_name <- attr(object, "constructor")
    if (rlang::is_null(constructor_name)) {
        # this is not a constructor, so just get the value
        return(toString(object))
    }

    if (rlang::is_list(object)) {
        components <- names(object)
        values <- purrr::map(rlang::as_list(object), deparse_construction)

        print_args <- vector("character", length = length(components))
        for (i in seq_along(components)) {
            print_args[i] <- paste0(components[i], " = ", values[i])
        }
        print_args <- paste0(print_args, collapse = ", ")
        paste0(constructor_name, "(", print_args, ")")
    } else {
        constructor_name
    }
}


#' Print a constructed value
#'
#' @param x Object to print
#' @param ... Additional parameters; not used.
construction_printer <- function(x, ...) {
    cat(deparse_construction(x), "\n")
}

#' Define a new data type from a sequence of constructors.
#'
#' This assignment operator introduces a domain-specific language for specifying
#' new types. Types are defined by the ways they can be constructed. This is provided
#' as a sequence of \code{|}-separated constructors, where a constructor is either
#' a constant, i.e., a bare symbol, or a function.
#'
#' We can construct an enumeration like this:
#'
#' \code{
#' numbers := ONE | TWO | THREE
#' }
#'
#' This will create the type \code{numbers} and three constants, \code{ONE}, \code{TWO},
#' and \code{THREE} that can be matched against using the \code{\link{cases}} function
#'
#' \code{
#' x <- TWO
#' cases(x,
#'       ONE -> 1,
#'       TWO -> 2,
#'       THREE -> 3)
#' }
#'
#' Evaluating the \code{\link{cases}} function will compare the value in \code{x} against the three
#' patterns and recognize that \code{x} holds the constant \code{TWO} and it will then return
#' \code{2}.
#'
#' With function constructors we can create more interesting data types. For example, we can create
#' a linked list like this
#'
#' \code{linked_list := NIL | CONS(car, cdr : linked_list)}
#'
#' This expression defines constant \code{NIL} and function \code{CONS}. The function takes
#' two arguments, \code{car} and \code{cdr}, and requires that \code{cdr} has type
#' \code{linked_list}. We can create a list with three elements, 1, 2, and 3, by writing
#'
#' \code{CONS(1, CONS(2, CONS(3, NIL)))}
#'
#' and we can, e.g., test if a list is empty using
#'
#'
#' \code{cases(lst, NIL -> TRUE, CONS(car,cdr) -> FALSE)}
#'
#' A special pattern, \code{otherwise},can be used to capture all patterns, so the
#' emptiness test can also be written
#'
#' \code{cases(lst, NIL -> TRUE, otherwise -> FALSE)}
#'
#' Arguments to a constructor function can be typed. To specify typed variables,
#' we use the \code{:}-operator. The syntax is then \code{var : type}. The type
#' will be checked when you construct a value using the constructor.
#'
#' @param data_type The name of the new data type. Should be given as a bare symbol.
#' @param constructors A list of \code{|}-separated constructor specifications.
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
`:=` <- function(data_type, constructors) {
    data_type <- rlang::enquo(data_type)
    constructors <- rlang::enexpr(constructors)

    if (!rlang::quo_is_symbol(data_type)) {
        error_msg <- glue::glue(
            "Incorrect type specification: {rlang::quo_expr(data_type)}. ",
            "The type must be a bare symbol."
        )
        stop(simpleError(error_msg, call = match.call()))
    }

    data_type_name <- rlang::quo_name(data_type)
    process_alternatives(constructors, data_type_name, rlang::get_env(data_type))

    assign(paste0("toString.", data_type_name), deparse_construction, envir = rlang::get_env(data_type))
    assign(paste0("print.", data_type_name), construction_printer, envir = rlang::get_env(data_type))
}
