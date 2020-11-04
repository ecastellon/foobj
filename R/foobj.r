# -*- coding: utf-8 -*-

#' load file
#' @description load a file catching errors
#' @param x file's name
#' @param env environment where objects are loaded;
#' \code{parent.frame} by default
#' @return objects' names or NULL
#' @examples
#' try_load("xx.rda")
#' try_load("xx.rda", env = new.env())
#' @author eddy castellón
try_load <- function(x, env = parent.frame()){
  if (!is.environment(env)) {
    message("... argument is NOT an environment !!!")
    return(NULL)
  }

  if (missing(x) || !filled_char(x)){
      message("... file's name is missing !!!")
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
#' @return a string or \code{NA} if the object hasn't the attribute
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
#' @return a character vector if meta is FALSE; a data.frame
#'     otherwise; or NULL if errors
#' @export
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

#' data.frames en archivo
#' @description Lista los data.frame almacenados en el archivo \code{file}
#' @param file ruta/nombre del archivo
#' @param meta incluir el atributo \code{meta}?; TRUE por omisión.
#' @return devuelve vector con los nombres de data.frame o un
#'     data.frame con el atributo \code{meta} asociado.
#' @export
#' @examples
#' list_dff("file_name")
#' @author Eddy Castellón
list_dff <- function(file, meta = TRUE){
  list_off(file, meta, clase = "data.frame")
}

#' Transferir objetos
#' @description Lleva un grupo de objetos almacenados en archivo a un
#'     \code{environment} específico; si este no se indica se da por
#'     supuesto que es el de donde se llama la función. Además del
#'     nombre de los objetos y del archivo, \code{clase} indica la
#'     clase de objetos que serán transferidos.
#' @param ... los objetos; no indicar el parámetro si quiere todos
#' @param file nombre del archivo
#' @param clase la clase (heredada) de los objetos; "." si
#'     todos; \code{data.frame} por omisión
#' @param env el \code{environment}
#' @return vector de caracteres con el nombre de los objetos
#'     transferidos
#' @export
#' @author eddy castellón
read_off <- function(..., file = character(), clase = "data.frame",
                     env = parent.frame()){

    ANY <- "."
    if (!is.environment(env)) {
        message("!!! No existe env")
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
        if (any(keep) && clase != ANY){
            for (jj in which(keep)) {
                ob <- get(oo[jj], envir = env, inherits = FALSE)
                keep[jj] <- inherits(ob, clase)
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

#' Transferencia data.frame
#' @description Lee uno o más data.frame almacenados en un archivo y
#'     los asigna a un \code{environment} con el mismo nombre que
#'     tienen en el archivo
#' @param ... nombres de los data.frame
#' @param file ruta/nombre del archivo
#' @param env \code{environment} destino; por omisión aquél desde
#'     donde se llama la función (parent.frame)
#' @return lista de los data.frame que fueron encontrados y devueltos
#' @examples
#' read_dff(c("aa", "bb"), file="xx.rda")
#' nwe <- new.env(); read_dff(aa, file = "xx.rda", env = nwe)
#' @export
#' @author eddy castellón
read_dff <- function(..., file, env = parent.frame()){

    if (missing(...)) {
        oo <- read_off(file = file, env = env, clase = "data.frame")
    } else {
        oo <- read_off(..., file = file, env = env,
                       clase = "data.frame")
    }

    return(oo)
}

#' Extraer objetos
#' @description Extrae (lee) objetos almacenados en archivo para
#'   asignarlos a una variable en el \code{environment} desde donde se
#'   llama la función
#' @param ... nombre de objeto; no indicar si todos
#' @param file nombre de archivo
#' @param clase la clase (heredada) de los objetos
#' @return objeto o lista de objetos si más de uno
#' @export
#' @author Eddy Castellón
get_off <- function(..., file = character(), clase = "data.frame"){
    ne <- new.env()
    if (missing(...)) {
        oo <- read_off(file = file, env = ne, clase = clase)
    } else {
        oo <- read_off(..., file = file, env = ne, clase = clase)
    }

    if (is.null(oo)) {
        return(oo)
    } else {
        ob <- as.list(ne)
        if (length(ob) == 1) ob <- ob[[1]]
        invisible(ob)
    }
}

#' Extraer data.frame
#' @description Lee data.frame almacenado en archivo para asignarlo a
#'     una variable en el \code{environment} desde donde se llama la
#'     función.
#' @param ... nombre(s) de los \code{data.frame}; no indicarlo si
#'     todos
#' @param file nombre del archivo
#' @return data.frame o lista de \code{data.frame} (invisible)
#' @examples
#' ww <- get_dff(aa, file="xx.rda")
#' @seealso get_dff_c
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
get_dff <- function(..., file){
    if (missing(...)){
        oo <- get_off(file = file, clase = "data.frame")
    } else {
        oo <- get_off(..., file = file, clase = "data.frame")
    }

    if (is.null(oo)) {
        return(oo)
    } else {
        invisible(oo)
    }
}

#' Remover objeto
#' @description Remueve objeto almacenado en archivo
#' @param x nombre de objeto
#' @param file nombre de archivo
#' @param file2 archivo actualizado; si no se indica, el archivo será
#'     el mismo de entrada
#' @return nombres de los objetos archivados; NULL si la operación no
#'     tiene éxito
#' @export
#' @author Eddy Castellón
rm_off <- function(x, file = character(), file2){

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

#' Agrega objetos a un archivo.
#' @description Agrega a un archivo uno o más objetos asociados a un
#'     \code{environment} específico, conservando los que ya se
#'     encontraban en él.
#' @param ... nombre de objetos (sin comillas o en un vector),
#'     asociados al \code{environment}.
#' @param file ruta/nombre del archivo
#' @param env el \code{environment} al que están asociados los
#'     objetos; por omisión, aquél desde donde se llama la función
#'     (parent.frame)
#' @return lista de objetos agregados o NULL si operación fracasa
#' @export
#' @examples
#' add_tof(aa, bb, file="xx.rda")
#' add_tof(c("aa", "bb"), file="xx.rda")
add_tof <- function(..., file, env = parent.frame()){

    if (!is.environment(env)) {
        message("... NO es \"environment\" !!!")
        return(NULL)
    }

    if (missing(...)) {
        message("... NADA que agregar !!!")
        return(NULL)
    }

    if (missing(file)){
        message("... FALTA nombre archivo !!!")
        return(NULL)
    }
    
    ne <- new.env()
    oo <- try_load(file, ne)
    if (!is.null(oo)) {
        ##!!! los obj. en ... asociados a env
        nm <- dots_arg(...)
        nn <- length(nm)
        ob <- mget(nm, env, ifnotfound = vector("list", nn),
                   inherits = FALSE)

        if (any(copi <- (nm %in% oo))) {
            message(nm[copi], " sust. a los que ya en archivo !!!")
        }
        
        for (jj in seq.int(nn)) {
            copi[jj] <- !is.null(ob[[jj]])
            if (copi[jj]) assign(nm[jj], ob[[jj]], envir = ne,
                                 inherits = FALSE)
        }

        if (any(copi)) {
            oo <- nm[copi]
            save(list = ls(ne, all.names = TRUE), file = file,
                 envir = ne)
            cat(gettextf("%i objetos transferidos a %s\n",
                        sum(copi), file))
        } else {
            oo <- NULL
        }

    }

    return(oo)
}

#' Agrega objetos a un archivo; alias de add_tof
#' @description Agrega a un archivo uno o más objetos asociados a un
#'     \code{environment} específico, conservando los que ya se
#'     encontraban en él.
#' @param ... nombre de objetos (sin comillas o en un vector),
#'     asociados al \code{environment}.
#' @param file ruta/nombre del archivo
#' @param env el \code{environment} al que están asociados los
#'     objetos; por omisión, aquél desde donde se llama la función
#'     (parent.frame)
#' @return lista de objetos agregados o NULL si operación fracasa
#' @export
#' @examples
#' add_tof(aa, bb, file="xx.rda")
#' add_tof(c("aa", "bb"), file="xx.rda")
save_add <- function(..., file, env = parent.frame()){
  add_tof(..., file, env)
}
