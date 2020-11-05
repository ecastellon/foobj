# -*- coding: utf-8 -*-

#' load file
#' @description load a file catching errors
#' @param x character; file's name
#' @param env object environment where objects are loaded;
#' \code{parent.frame} by default
#' @return character; objects' names or NULL
#' @examples
#' try_load("xx.rda")
#' try_load("xx.rda", env = new.env())
#' @author eddy castellón
try_load <- function(x, env = parent.frame()) {
  if (missing(x) || !filled_char(x)){
      message("... file's name is missing !!!")
      return(NULL)
  }
  
  if (!is.environment(env)) {
    message("... argument is NOT an environment !!!")
    return(NULL)
  }

  tryCatch(load(x, envir = env),
           error = function(e){
               if (file.exists(x)) {
                   message("... read ERROR !!!")
               } else {
                   message("... file doesn't exists !!!")
               }
               return(NULL)},
           warning = function(e){
               if (file.exists(x)) {
                   message("... read ERROR!!!")
               } else {
                   message("... file doesn't exists !!!")
               }
               return(NULL)}
           )
}

## --- data.frames, objetos ---
## - agregar metadatos a d.f
## - agregar, leer, eliminar objetos y d.f de archivos

#' attribute \code{meta}
#' @description Add the attribute \code{meta} to the object for
#'     recording a short description of the data or the object; or
#'     instead the name of a text file where that description could be
#'     of any length. In this case, the name of the file must have at
#'     least a path separator (/). This is an alternative to the
#'     function \code{comment} of base R.
#' @param object that will hold the attribute
#' @export
#' @examples
#' meta(df) <- "some metadata"
#' meta(df) <- "./metadata.txt"
#' @author Eddy Castellón
`meta<-` <- function(x, value){
    attr(x, "meta") <- value
    invisible(x)
}

#' attribute \code{meta}
#' @description read the value of the attribute \code{meta} or the
#'     content of the file recorded in the attribute
#' @param x name of the object
#' @return string or \code{NA} if the object hasn't the attribute
#'     \code{meta}
#' @export
#' @examples
#' meta(name_df) -> attr(name_df, "meta")
#' @author Eddy Castellón
meta <- function(x){
    cc <- attr(x, "meta")
    if (is.null(cc)) {
        cc <- NA_character_
    } else {
        if (is_path(cc)) {
            if (file.exists(cc)) {
                cc <- readLines(cc, ok = FALSE)
            } else {
                message("\n ... file doesn't exists !!!")
            }
        }
    }
    cc
}

## --- objetos ---

#' objects
#' @description produces a vector (or data.frame) with the object's
#'     names and the values of the attribute \code{meta}, of the
#'     objects saved in a file
#' @param x character; file's name
#' @param meta logical; the value of the attribute \code{meta}
#'     returned as a column of a data.frame? (TRUE by default)
#' @param class character; return objects of specified class only; by
#'     default data.frame; "." for any class.
#' @return a character vector if meta is FALSE; a data.frame or NULL
#'     if errors otherwise
#' @export
#' @examples
#' list_off("file.rda") -> data.frame of data.frame names if any
#' list_off("file.rda", meta = FALSE) -> character vector
#' @author Eddy Castellón
list_off <- function(file, meta = TRUE, class = "data.frame"){

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
#' @seealso list_off
#' @export
#' @examples
#' list_dff("file_name.rda") -> data.frame
#' list_dff("file_name.rda", meta = FALSE) -> character vector
#' @author Eddy Castellón
list_dff <- function(file, meta = TRUE){
  list_off(file, meta, class = "data.frame")
}

#' load objects
#' @description Load objects of a specified class saved inf a file
#'     into an environment
#' @param ... object's names; if they are not specified, load all the
#'     objects
#' @param file character; file's name
#' @param class character; the class of the objects; write "." for any
#'     class; "data.frame" by default
#' @param env object environment; the parent.frame by default
#' @return character vector with the names of the loaded objects
#' @export
#' @examples
#' read_off(file = "file_name.rda", class = "data.frame") ->
#'     all data.frame loaded into the parent.frame
#' read_off(ob1, ob2, file = "file.rda", class = "data.frame") -> load
#'     into the parent.frame the objects ob1 and ob2 if they are of
#'     class data.frame
#' @author eddy castellón
read_off <- function(..., file = character(), class = "data.frame",
                     env = parent.frame()){

    ANY <- "."
    if (!is.environment(env)) {
        message("!!! environment doesn't exists")
        return(NULL)
    }

    oo <- try_load(file, env)
    if (!is.null(oo)) {
        if (missing(...)) {# todos?
            keep <- !logical(length(oo))
        } else {
            nm <- dots_arg(...)
            keep <- oo %in% nm
        }

        ## si hay objetos en el pedido, ahora por la clase
        if (any(keep) && class != ANY){
            for (jj in which(keep)) {
                ob <- get(oo[jj], envir = env, inherits = FALSE)
                keep[jj] <- inherits(ob, class)
            }
        }

        ## elimina los no pedidos
        if (!all(keep)) {
            rm(list = oo[!keep], envir = env)
            oo <- oo[keep]
        }
        
        if (!length(oo)) oo <- NULL
    }
    return(oo)
}

#' load data.frame
#' @description Load one or more data frames saved in a file into an
#'     environment. It is an alias of the function read_off with the
#'     parameter class set to "data.frame".
#' @param ... data frames' names; if not indicated load all the
#'     data.frames
#' @param file character; file's path
#' @param env environment object; the parent frame by default
#' @return character vector with the names of the data frames loaded
#' @seealso read_off
#' @examples
#' read_dff(c("aa", "bb"), file = "xx.rda")
#' read_dff(aa, bb, file = "xx.rda")
#' read_dff(file = "xx.rda")
#' nwe <- new.env(); read_dff(aa, file = "xx.rda", env = nwe)
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
#' @param class character; objects' class; "." for any class;
#'     "data.frame" by default
#' @seealso read_off
#' @return object or list of objects or NULL; invisible return
#' @examples
#' get_off(file = "ff.rda", class = "data.frame") -> return a list with
#' all data frames saved in the file
#' get_off(df1, df2, file = "ff.rda", class = "data.frame") -> a list
#' of 2 data frames if both df1 and df2 are of class data.frame
#' get_off(c("df1", "df2"), file = "ff.rda")
#' get_off(df1, file = "ff.rda") -> a data.frame
#' @export
#' @author Eddy Castellón
get_off <- function(..., file = character(), class = "data.frame"){
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
        if (length(ob) == 1) ob <- ob[[1]]
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
#' @examples
#' ww <- get_dff(aa, file = "xx.rda")
#' ww <- get_dff(aa, bb, file = "xx.rda")
#' ww <- get_dff(file = "xx.rda")
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
#'     objects in another or the same file
#' @param x object's name
#' @param file character; file's path with the object
#' @param file2 character; file's path with the remaining objects; if
#'     it is missing, the remaining objects will be saved in the same
#'     file
#' @return a character vector with the name of the remaining objects,
#'     or NULL if the operation is aborted
#' @examples
#' rm_off(xx, file = "file1.rda", file2 = "file2.rda") -> remove the
#' object from file and save the remanining objects in file2; file it
#' is not modified
#' rm_off(xx, file = "file1.rda") -> file is modified; the object is
#' lost
#' @export
#' @author Eddy Castellón
rm_off <- function(x, file = character(), file2) {

    ne <- new.env()
    oo <- try_load(file, ne)

    if (!is.null(oo)) {
        if (missing(file2)) file2 <- file
        ob <- as.character(substitute(x))
        
        if (is.element(ob, oo)){
            warning("\n!!!! ... ", ob, " ELIMINADO de ", file)
            rm(list = ob, envir = ne)
            oo <- ls(ne, all.names = TRUE)
            save(list = oo, file = file2, envir = ne)
        } else {
            message("... objeto ", x, " NO existe !!!")
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
#' add_tof(aa, bb, file = "xx.rda")
#' add_tof(c("aa", "bb"), file = "xx.rda")
#' add_tof(aa, file = "xx.rda", env = .GlobalEnv)
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

    if (!any(jj)) {
        message("\n... objects doesn't exists !!!")
        return(NULL)
    }

    ## exists and process
    nm <- nm[jj]
    if (file.exists(file)) {
        ne <- new.env()
        oo <- try_load(file, ne)
        if (!is.null(oo)) {
            ## move the objects to the environment where
            ## the objects already in file are bounded
            ob <- mget(nm, env, ifnotfound = vector("list", 1),
                       inherits = FALSE)

            ## check for copies
            if (any(copi <- (nm %in% oo))) {
                message(nm[copi],
                        "\n... to replace with the same name !!!")
            }
            
            for (jj in seq_along(nm)) {
                assign(nm[jj], ob[[jj]], envir = ne, inherits = FALSE)
            }

            save(list = ls(ne, all.names = TRUE), file = file,
                 envir = ne)
            message(gettextf("%i saved objects into %s\n",
                             nn, file))
        }
    } else {
        save(list = nm, file = file, envir = env)
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
#' @export
#' @examples
#' save_add(aa, bb, file = "xx.rda")
#' save_add(c("aa", "bb"), file = "xx.rda")
#' save_add(aa, file = "xx.rda", env = .GlobalEnv)
save_add <- function(..., file, env = parent.frame()){
  add_tof(..., file, env)
}

#' @export
save_df <- function(x, name, file) UseMethod("save_df")

#' save data.frame
#' @description Agrega data.frame a un archivo con un nuevo nombre si
#'     así fuera indicado. Los objetos (data.frame y otros)
#'     previamente almacenados en el archivo, son preservados.
#' @param x nombre del data.frame (sin comillas).
#' @param name nombre con que será almacenado el data.frame en el archivo.
#' @param file ruta/nombre del archivo
#' @export
#' @importFrom assertthat assert_that
save_df.data.frame <- function(x, name = character(), file,
                               metadata = character()) {
    stopifnot("file missing" = !missing(file))
    if (filled_char(metadata)) {
        meta(x) <- metadata
    }

    ## new name valid?
    ## !!! reserved words?
    ok <- filled_char(name) && nzchar(name) &&
        grepl("^[a-zA-z][[:alnum:]]*$", name)
    if (!ok) {
        warning("\n... data.frame's name not changed")
        name <- deparse(substitute(x))
    }

    ne <- new.env()
    assign(name, x, envir = ne)
    oo <- add_tof(name, file = file, env = ne)
    
    if (!is.null(oo)) {
        message("\n ... data.frame ", name, "added to ", file)
    }
    return(oo)
}
