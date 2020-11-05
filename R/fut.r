# -*- coding:utf-8 -*-

## funciones de utilidad general, privadas

#' length
#' @description vector has length greater than zero?
#' @param x vector
#' @return logical
#' @author eddy castellón
filled <- function(x) {
    lenght(x) > 0
}

#' character type
#' @description vector is of character type and has elements?
#' @param x vector
#' @return logical
filled_char <- function(x) {
    is.character(x) && length(x)
}

#' numeric mode
#' @description vector is of numeric mode and has elements?
#' @param x vector
#' @return logical
filled_num <- function(x) {
    is.numeric(x) && length(x)
}

#' integer type
#' @description vector is of integer type and has elements?
#' @param x vector
#' @return logical
filled_int <- function(x) {
    is.integer(x) && length(x)
}

#' path maybe
#' @description test string begins with one o more alphanumeric
#'     characters followed by file path separators, or it begins with
#'     "./" or ".\\" followed by one or more alpanumeric characters;
#'     in both cases ending in alphanumeric character.
#' @param x character
#' @return TRUE if x has at least one slash followed by and ended by
#'     an alphanumeric character
#' @examples
#' is_path(".aa/bb") -> FALSE
#' is_path("aa/bb") -> TRUE
#' is_path("aa/bb.") -> FALSE
#' @author eddy castellón
is_path <- function(x) {
    filled_char(x) && grepl("^((\\w+[/\\])|(\\.?[/\\]\\w+)).+\\w$", x)
}

#' file name
#' @description check file's name is valid using the function
#' \code{file.create} of base R. If any directory in the path chain
#' doesn't exists, the file's name is invalid.
#' @param x character; the file's name
#' @return logical
#' @examples
#' @author eddy castellón
ok_fname <- function(x = character()) {
    ok <- file.exists(x)
    if (!ok) {
        ok <- file.create(x)
        if (ok) {
            unlink(x)
        }
    }
    return(ok)
}

#' Caracter
#' @description es vector de caracteres y tiene elementos
#' @param x vector
#' @return TRUE si es vector de tipo character y tiene elementos
ok_chr <- function(x) {
    is.character(x) && length(x)
}

#' Numerico
#' @description es vector modo numérico y con elementos
#' @param x vector
#' @return TRUE si es vector numérico y tiene elementos
ok_num <- function(x) {
    is.numeric(x) && length(x)
}

#' Entero
#' @description es vector tipo entero y con elementos
#' @param x vector
#' @return TRUE si es vector tipo entero y tiene elementos
ok_int <- function(x) {
    is.integer(x) && length(x)
}

#' !!
chk_vector_call <- function(x) TRUE

#' dots argument
#' @description arguments in ... returned as a character or integer
#'     vector
#' @param ...
#' @return character or integer vector or NULL
#' @examples
#' dots_arg(a, b) -> c("a", "b")
#' dots_arg("a", "b") -> c("a", "b")
#' dots_arg(c("a", "b")) -> c("a", "b")
#' dots_arg(1:3) -> c(1, 2, 3)
#' @author eddy castellón
dots_arg <- function(...){
    xp <- eval(substitute(alist(...)))
    nn <- length(xp)

    if (nn > 1L) {
        vapply(xp, as.character, "a")
    } else {
        if (nn == 1L) {
            if (inherits(xp[[1]], "call")) {
                ## !!!
                ## debería verificar es c(..) o seq(., .)
                ## que log(.) u otra similar es error
                if (chk_vector_call(xp[[1]])) {
                    ex <- eval(xp[[1]])
                    if (is.numeric(ex)) {
                        return(NULL)
                    } else {
                        return(ex)
                    }
                } else {
                    NULL
                }
            } else {
                as.character(xp[[1]])
            }
        } else {
            message("\n!!! with out arguments")
            NULL
        }
    }
}
