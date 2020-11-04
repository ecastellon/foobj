# -*- coding:utf-8 -*-

## funciones de utilidad general, privadas

#'
#' @description vector has elements?
#' @param x
#' @return TRUE or FALSE
#' @author eddy castellón
filled <- function(x) {
    lenght(x) > 0
}

#' character type
#' @description test x is of character type and has elements
#' @param x vector
#' @return TRUE or FALSE
filled_char <- function(x) {
    is.character(x) && length(x)
}

#' numeric mode
#' @description test x is of numeric mode and has elements
#' @param x vector
#' @return TRUE or FALSE
filled_num <- function(x) {
    is.numeric(x) && length(x)
}

#' integer type
#' @description test x is of integer type and has elements
#' @param x vector
#' @return TRUE or FALSE
filled_int <- function(x) {
    is.integer(x) && length(x)
}

#' double type
#' @description test x is of double type and has elements
#' @param x vector
#' @return TRUE or FALSE
filled_dou <- function(x) {
    is.double(x) && length(x)
}

#' path file
#' @description test if a string is a valid path to a file; that is,
#'     it begins with one o more alphanumeric character followed by a
#'     slash, or it begins with "./" or ".\\" followed by one or more
#'     alpanumeric characters and in both cases ended with an
#'     alphanumeric character
#' @param x character
#' @return TRUE if x has at least one slash followed by and ended by
#'     an alphanumeric character
#' @examples
#' is_path(".aa/bb") -> FALSE
#' is_path("aa/bb") -> TRUE
#' 
#' @author eddy castellón
is_path <- function(x) {
    bb <- filled_char(x) && nzchar(x)
    if (bb) {
        bb <- grepl("^((\\w+[/\\])|(\\.?[/\\]\\w+)).+\\w$", x)
    }
    bb
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
