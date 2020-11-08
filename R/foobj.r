# -*- coding: utf-8 -*-

#' attribute \code{meta}
#' @description Add the attribute \code{meta} to an object for
#'     recording a short description of the data or the object; or the
#'     path of a text file where that description (of any length) has
#'     been recorded. In the last case, the name of the file must have
#'     at least a file path separator (/) to be recognized as a file's
#'     name and not as a simple comment. This is an alternative to the
#'     function \code{comment} of base R.
#' @param x object that will hold the attribute
#' @param value character; metadata
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' meta(df) <- "./metadata.txt"
#' meta(df) <- "other_path/metadata.txt"
#'
#' rm(df)
#' @author Eddy Castellón
`meta<-` <- function(x, value){
    attr(x, "meta") <- value
    invisible(x)
}

#' attribute \code{meta}
#' @description read the value of the attribute \code{meta} or the
#'     content of the file recorded in the attribute
#' @param x name of the object
#' @param read_me logical; read the content of a meta file if it
#'     exists?; FALSE by default
#' @return string or \code{NA} if the object hasn't the attribute
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' meta(df)
#'
#' rm(df)
#' @author Eddy Castellón
meta <- function(x, read_me = FALSE){
    cc <- attr(x, "meta")
    if (is.null(cc)) {
        cc <- NA_character_
    } else {
        if (read_me && is_path(cc) &&
            (file.access(cc, mode = 4L) == 0)) {
            cc <- readLines(cc, ok = FALSE)
        } else {
            message("\n... access denied !!!")
        }
    }
    cc
}

#' objects
#' @description return the object's names (a character vector) and the
#'     values of its attributes \code{meta} (as a data.frame), of the
#'     objects saved in a file
#' @param file character; file's name
#' @param meta logical; the value of the attribute \code{meta}
#'     returned as a column of a data.frame? (TRUE by default)
#' @param class character; get names for objects of this class only;
#'     "." for any class which is the default
#' @return a character vector if meta is FALSE or a data.frame if
#'     TRUE; or NULL if errors occurs
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, d2, vi, file = fi)
#' 
#' list_off(fi, meta = FALSE)
#' list_off(fi, class = "data.frame")
#' 
#' unlink(fi)
#' rm(df, d2, vi, fi)
#' @author Eddy Castellón
list_off <- function(file, meta = TRUE, class = "."){

    ANY  <- "."
    anyclas <- class == ANY

    ne <- new.env()
    cc <- try_load(file, ne)
    if (is.null(cc)) return(cc)
    ## !!!
    ## si nn = 0 no hay objetos en file?
    ## y si file no es un archivo R

    cc <- cc[order(cc)]
    nn <- length(cc)
    clases <- character(nn)
    metas <- character(nn)
    keep <- logical(nn)
  
    for (jj in seq_along(cc)){
        zz <- eval(as.name(cc[jj]), envir = ne)
        keep[jj] <- anyclas || inherits(zz, class)
        clases[jj] <- paste(class(zz), collapse = ",")
        metas[jj] <- ifelse(is.null(me <- attr(zz, "meta")),
                            NA_character_, me)
    }

    if (any(keep)) {
        if (meta) {
            xx <- data.frame(data = cc[keep],
                             meta = metas[keep],
                             stringsAsFactors = FALSE)
            if (anyclas) {
                return(cbind(xx, class = clases[keep],
                             stringsAsFactors = FALSE))
            } else {
                return(xx)
            }
        } else {
            if (anyclas) {
                return(data.frame(data = cc[keep],
                                  class = clases[keep],
                                  stringsAsFactors = FALSE))
            } else {
                return(cc[keep])
            }
        }
    } else {
        message("\n??? there is not objects of class ", class)
        return(NULL)
    }
}

#' data.frames in file
#' @description read the names of the data.frames in a file and
#'     produces a character vector or a data.frame with the names and
#'     the values in the attribute \code{meta}. It is an alias of the
#'     function list_off with the argument class set to "data.frame"
#' @param file character; path to the file
#' @param meta logical; read the value of the attribute \code{meta}
#'     also?; TRUE by default
#' @return character vector or data.frame
#' @seealso list_off, meta
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, d2, vi, file = fi)
#' 
#' list_dff(fi)
#' unlink(fi)
#' rm(df, d2, vi, fi)
#' @author Eddy Castellón
list_dff <- function(file, meta = TRUE) {
  list_off(file, meta, class = "data.frame")
}

#' load objects
#' @description Load objects saved in a file into an environment
#' @param ... object's names; if missing, load all the objects
#' @param file character; file's name
#' @param class character; the class of the objects to load; "."
#'     for any class which is the default
#' @param env environment; the parent.frame by default
#' @return character vector with the names of the loaded objects
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, d2, vi, file = fi)
#' 
#' rd <- read_off(file = fi, class = "data.frame")
#' exists(rd[1])
#' 
#' rd <- read_off(c("d2", "vi"), file = fi)
#' class(rd[2])
#' 
#' nwe <- new.env()
#' rd <- read_off(d2, file = fi, env = nwe)
#' exists("d2")
#' exists("d2", envir = nwe)
#'
#' unlink(fi)
#' rm(df, d2, vi, fi, nwe, rd)
#' @author eddy castellón
read_off <- function(..., file = character(), class = ".",
                     env = parent.frame()) {

    ANY <- "."
    if (!is.environment(env)) {
        message("!!! environment doesn't exists")
        return(NULL)
    }

    ne <- new.env()
    oo <- try_load(file, ne) #load to env could overwrite

    ## objects in file
    if (!is.null(oo)) {
        if (!missing(...)) {
            oo <- intersect(oo, dots_arg(...))
            if (!length(oo)) {#there is not objects in ...
                oo <- NULL
            }
        }
    }

    ## objects of class
    if (!is.null(oo)) {
        if (class == ANY) {
            class  <- "any"
        }
        
        ok <- logical(length(oo)) #track class
        for (kk in seq_along(oo)) {
            ob <- get(oo[kk], envir = ne, inherits = FALSE)
            if (ok[kk] <- inherits(ob, class)) {
                assign(oo[kk], ob, pos = env)
            }
        }
        
        oo <- oo[ok]
        if (!filled(oo)) {#not one of the class
            oo <- NULL
        }
    }
    return(oo)
}

#' load data.frame
#' @description Load one or more data frames saved in a file into an
#'     environment. It calls the function read_off with the parameter
#'     class set to "data.frame".
#' @param ... data frames' names; if missing load all data.frames
#' @param file character; file's path
#' @param env environment; the parent.frame by default
#' @return character vector with the names of the data frames loaded
#' @seealso read_off
#' @export
#' @author eddy castellón
read_dff <- function(..., file, env = parent.frame()){

    if (missing(...)) {
        oo <- read_off(file = file, env = env, class = "data.frame")
    } else {
        oo <- read_off(..., file = file, env = env,
                       class = "data.frame")
    }

    return(oo)
}

#' load-bind objects
#' @description Read one or more objects saved in a file to be binded
#'     to a variable
#' @param ... object's names; include all the objects if not specified
#' @param file character; file's path
#' @param class character; objects' class; "." for any class which is
#'     the default
#' @seealso read_off
#' @return object or list of objects or NULL; invisible return
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, d2, vi, file = fi)
#' 
#' rd <- get_off(file = fi)
#' sapply(rd, class)
#' 
#' rd <- get_off(file = fi, class = "data.frame")
#' length(rd)
#' names(rd[[1]])
#' 
#' rd <- get_off(d2, file = fi)
#' names(rd)
#' 
#' rd <- get_off(vi, file = fi, class = "integer")
#' length(rd)
#'
#' unlink(fi)
#' rm(df, d2, vi, rd, fi)
#' @export
#' @author Eddy Castellón
get_off <- function(..., file = character(), class = "."){
    ne <- new.env()
    if (missing(...)) {
        oo <- read_off(file = file, env = ne, class = class)
    } else {
        oo <- read_off(..., file = file, env = ne, class = class)
    }

    if (is.null(oo)) {
        return(oo)
    } else {
        ob <- as.list(ne)
        if (length(ob) == 1L) ob <- ob[[1]]
        invisible(ob)
    }
}

#' load-bind data.frame
#' @description read one or more data frames saved in a file for
#'     binding to a variable in the parent.frame. It is an alias of
#'     the function get_off with the parameter class set to
#'     "data.frame"
#' @param ... data frames' names or a character vector with the names;
#'     if it is not specified read all the data frames
#' @param file character; file's path
#' @return a data.frame, a list of data frames or NULL; return
#'     invisible
#' @seealso get_off
#' @export
#' @author eddy castellón
get_dff <- function(..., file){
    if (missing(...)){
        oo <- get_off(file = file, class = "data.frame")
    } else {
        oo <- get_off(..., file = file, class = "data.frame")
    }

    if (is.null(oo)) {
        return(oo)
    } else {
        invisible(oo)
    }
}

#' remove object
#' @description remove object from a file and save the remaining
#'     in another (or the same) file
#' @param x object's name
#' @param file character; file's path with the object
#' @param file2 character; file's path with the remaining objects; if
#'     it is missing, the remaining objects will be saved in the same
#'     file
#' @return a character vector with the name of the remaining objects,
#'     or NULL if the operation has fail
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, d2, vi, file = fi)
#' 
#' rd <- rm_off(vi, file = fi)
#' rd
#' 
#' unlink(fi)
#' rm(rd, fi, df, d2, vi)
#' @export
#' @author Eddy Castellón
rm_off <- function(x, file = character(), file2) {

    ne <- new.env()
    oo <- try_load(file, ne)

    if (!is.null(oo)) {
        if (missing(file2)) file2 <- file
        ob <- as.character(substitute(x))
        
        if (is.element(ob, oo)){
            warning("\n!!!! ... ", ob, " removed from ", file)
            rm(list = ob, envir = ne)
            oo <- ls(ne, all.names = TRUE)
            save(list = oo, file = file2, envir = ne)
        } else {
            message("... object ", x, " not found !!!")
        }
    }
    oo
}

#' add objects
#' @description Add objects to a file without removing the objects
#'     already saved in it. When one object has the same name than any
#'     one in the file, the object in the file is replaced with the
#'     new one
#' @param ... object's names or a character vector with the names of
#'     the objects
#' @param file character; file's path where the objects will be added;
#'     if the file does not exists the function creates it
#' @param env object environment where the objects live; the
#'     parent.frame by default
#' @return character vector with the names of the objects added or
#'     NULL if error
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, vi, file = fi)
#'
#' rd <- add_tof(d2, file = fi)
#' list_off(fi, meta = FALSE)
#'
#' unlink(fi)
#' rm(rd, fi, df, d2, vi)
add_tof <- function(..., file, env = parent.frame()){

    if (!is.environment(env)) {
        message("... environment does not exists !!!")
        return(NULL)
    }
    
    if (missing(...)) {
        message("... nothing to add !!!")
        return(NULL)
    }
    
    ## objects' names
    nm <- dots_arg(...)
    
    ## which objects in env
    jj <- vapply(nm, exists, FALSE, envir = env, inherits = FALSE,
                 USE.NAMES = FALSE)
    if (!all(jj)) {
        warning("... some objects doesn't exists in environment !!!")
        nm <- nm[jj]
    }
    
    if (ok <- filled(nm)) {
        ## exists load and process
        if (file.exists(file)) {
            ne <- new.env()
            oo <- try_load(file, ne)
            
            if (ok <- !is.null(oo)) {
                ## move the objects to the environment where
                ## the objects already in file are bounded
                ob <- mget(nm, env, ifnotfound = vector("list", 1),
                           inherits = FALSE)

                ## check for copies
                if (any(copi <- (nm %in% oo))) {
                    message(nm[copi],
                            "\n... to replace because the same name !!!")
                }
                
                for (jj in seq_along(nm)) {
                    assign(nm[jj], ob[[jj]], envir = ne, inherits = FALSE)
                }

                ok <- save_ok(list = ls(ne, all.names = TRUE),
                              file = file, envir = ne)
            }
        } else {#creates new file
            ok <- save_ok(nm, file = file, envir = env)
        }
    }

    if (!ok) {
        nm <- NULL
    }
    return(nm)
}

#' add objects
#' @description It is an alias of add_tof. Add objects to a file
#'     without removing the objects already saved in it, except when
#'     one object has the same name than any one in the file; in this
#'     case the object in the file is replaced with the new one
#' @param ... object's names or a character vector with the names of
#'     the objects
#' @param file character; file's path where the objects will be added;
#'     if the file does not exists the function creates it
#' @param env object environment where the objects live; the
#'     parent.frame by default
#' @return character vector with the names of the objects or NULL if
#'     error
#' @seealso add_tof
#' @export
save_add <- function(..., file, env = parent.frame()){
  add_tof(..., file, env)
}

#' generic
#' @description generic function to save a data.frame
#' @param x data.frame's name
#' @param ... other arguments
#' @export
save_df <- function(x, ...) UseMethod("save_df")

#' save data.frame
#' @description add a data.frame to a file with a new name or the same
#'     name, keeping the objects already saved in the file. Any object
#'     already saved with the same name than the data.frame being
#'     added, is replaced.
#' @param x name of the data frame
#' @param name character; the new data.frame's name; if missing or it
#'     is not a "valid" name, the data.frame's name won't change
#' @param file character; file's path
#' @param metadata character; a string that will be added as the
#'     data.frame's \code{meta} attribute. See \code{meta}.
#' @return the new name of the data.frame or NULL if the operation
#'     fails
#' @seealso \code{meta}, \code{add_tof}, \code{save_add}
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, vi, file = fi)
#'
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' save_df(d2, name = "first3", file = fi,
#' metadata = "(lower)uppercase first letters of the alphabet")
#' list_dff(file = fi)
#'
#' unlink(fi)
#' rm(df, vi, d2, fi)
#' @author eddy castellón
save_df.data.frame <- function(x, name = character(), file,
                               metadata = character()) {

    if (filled_char(metadata)) {
        meta(x) <- metadata
    }

    ## new name valid?
    ## !!! reserved words?
    ok <- filled_char(name) && nzchar(name) &&
        grepl("^[a-zA-z][[:alnum:]]*$", name)
    if (!ok) {
        message("\n... data.frame's name will not change")
        name <- deparse(substitute(x))
    }

    env <- new.env()
    if (file.exists(file)) {#file.access?
        load(file, envir = env)
    }
    assign(name, x, pos = env)
    
    if (save_ok(list = ls(name = env), file = file, envir = env,
                compress = TRUE)) {
        message("\n... d.f ", name, " added to ", file)
    } else {
        name <- NULL
    }
    
    return(name)
}
