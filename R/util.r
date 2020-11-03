# -*- coding: utf-8 -*-

## --- funciones de utilidad general ---

#' Mismo tipo
#' @description vectores del mismo tipo?
#' @param x vector
#' @param y vector
#' @return TRUE si x e y son del mismo tipo
#' @author eddy castellón
eq_type <- function(x, y) {
    typeof(x) == typeof(y)
}

#' Mismo modo
#' @description vectores del mismo modo?
#' @param x vector
#' @param y vector
#' @return TRUE si x e y son del mismo modo
#' @author eddy castellón
eq_mode <- function(x, y) {
    ## mode y typeof son equivalentes
    mode(x) == mode(y)
}

#' Caracter
#' @description es vector de caracteres y tiene elementos
#' @param x vector
#' @return TRUE si es vector de tipo character y tiene elementos
#' @export
ok_chr <- function(x) {
    is.character(x) && length(x)
}

#' Numerico
#' @description es vector modo numérico y con elementos
#' @param x vector
#' @return TRUE si es vector numérico y tiene elementos
#' @export
ok_num <- function(x) {
    is.numeric(x) && length(x)
}

#' Entero
#' @description es vector tipo entero y con elementos
#' @param x vector
#' @return TRUE si es vector tipo entero y tiene elementos
#' @export
ok_int <- function(x) {
    is.integer(x) && length(x)
}

#' Enteros-letra
#' @description es vector cuyos elementos son dígitos-alfanuméricos
#' @param x vector
#' @return TRUE si los elementos de x son dígitos tipo caracter
#' @export
ok_int_chr <- function(x) {
    ok_chr(x) && !any(grepl("[^0-9]", x))
}

#' Desagregar palabras
#' @description Desgrega las "palabras" (token) separadas por coma o
#'     espacios que se encuentran en una ristra de caracteres, y las
#'     devuelve en un vector.
#' @param x palabras separadas por coma o espacios
#' @return vector de caracters con las palabras individuales
#' @export
#' @examples
#' tok_str("aa bb,cc") -> c("aa", "bb", "cc")
#' @author eddy castellón
tok_str <- function(x) {
    strsplit(x, split = "[[:space:],]+")[[1L]]
}

#' Poda espacios
#' @description quita los espacios antes y después de una ristra de caracteres
#' @param x ristra de caracteres
#' @export
podar_str <- function(x = character()) {
    if (ok_chr(x)) {
        regmatches(x, regexpr("\\b.*\\b", x, perl = TRUE))
    } else {
        message("... x NO es tipo character !!!")
        x
    }
}

#' Eliminar espacios
#' @description sustituye dos o más espacios por un espacio, que están enmedio
#' de una ristra de caracteres y poda los extremos
#' @param x ristra de caracteres
#' @return ristra de caracteres "normalizada"
#' @examples
#' rm_spacex("  abc   fg  ") -> "abc fg"
#' @export
rm_spacex <- function(x = character()) {
    if (ok_chr(x)) {
        podar_str(gsub("[[:space:]]+", " ", x))
    } else {
        message("... x NO es tipo character !!!")
        x
    }
}

#' match-alternativa
#' @description match con reporte de cuántos no hacen match; el índice
#'   de los sin pareja en atributo \code{sinpar}
#' @param x como en match
#' @param y como en match
#' @param msg con mensaje? TRUE por defecto
#' @return vector de enteros con atributo
#' @examples
#' pp <- parear(c("a", "b", "c"), c("c", "a"))
#' pp
#' [1] 2 NA 1
#' attr(pp, "sinpar")
#' [1] 2
#' @export
#' @author eddy castellón
parear <- function(x, y, msg = TRUE) {
    mm <- match(x, y, nomatch = NA, incomparables = NULL)

    if (any(ii <- is.na(mm))) {
      attr(mm, "sinpar") <- which(ii)
      if (msg) {
        warning("sin pareja: ", sum(ii), " de ", length(x))
      }
    }
    mm
}

#' No match
#' @description devuelve el atributo \code{sinpar} producido por
#'   \code{parear} o \code{match_2}
#' @param x vector producido por \code{parear}
#' @seealso \code{parear}, \code{match_2}
#' @return vector de enteros o NULL
#' @export
#' @author eddy castellón
sinpar <- function(x) attr(x, "sinpar")

#' parear - matrices
#' @description aplica la función \code{parear} a la interacción
#'      de las columnas de una matriz y la interacción de las columnas de
#'      otra matriz
#' @param x matriz
#' @param y matriz
#' @param msg reporte de número de parejas? TRUE por defecto
#' @return vector de enteros con atributo \code{sinpar}
#' @examples
#' @export
parear_m <- function(x, y) {

}

#' match-2
#' @description Generaliza la función \code{match} a dos
#' @param x1 primera variable
#' @param x2 segunda variable con mismo número de elementos que x1
#' @param y1 variable correspondiente a x1
#' @param y2 variable correspondiente a x2; con mismo número de
#'     elementos que y1
#' @return los índices como en \code{parear}; el atributo
#'     \code{sinpar} con el índice de los sin pareja. Si las x's o las
#'     y's son incompatibles en número de elementos, devuelve
#'     NA_integer_
#' @seealso \code{parear}, \code{sinpar}
#' @export
#' @author eddy castellón
match_2 <- function(x1, x2, y1, y2){
    if ((length(x1) == length(x2)) &&
        (length(y1) == length(y2))) {
        invisible(parear(interaction(x1, x2, drop = TRUE),
                         interaction(y1, y2, drop = TRUE)))
    } else {
        message("... vectores de tamaño distinto !!!")
        return(NA_integer_)
    }
}

#' Alias de operador in
#' @description remplazo de operador in
#' @param x vector busca
#' @param table vector donde buscar elementos de x
#' @return TRUE para cada elemento de x encontrado en table
#' @export
#' @author eddy castellón
en <- function(x, table){
    x %in% table
}

## --- llamado funciones ---

## !!! verificar que x sea c(.,.) o n:n o seq(.,.)
chk_vector_call <- function(x) TRUE

#' Argumento en dots
#' @description Produce vector de argumentos en ...
#' @param ...
#' @return character o integer o NULL
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
            message("\n!!! sin argumentos")
            NULL
        }
    }
}

#' Carga archivo
#' @description Intenta llevar a un \code{environment} los objetos
#'   almacenados en un archivo.
#' @param x nombre del archivo
#' @param env objeto \code{environment}; por omisión aquél desde donde
#'     se llama la función
#' @return nombres de los objetos o NULL si el proceso fracasa
#' @author eddy castellón
try_load <- function(x, env = parent.frame()){
  if (!is.environment(env)) {
    message("... argumento NO es environment !!!")
    return(NULL)
  }

  if (missing(x) || !ok_chr(x)){
      message("... falta nombre archivo !!!")
      return(NULL)
  }
  
  tryCatch(load(x, envir = env),
           error = function(e){
               if (file.exists(x)) {
                   message("... ERROR de lectura !!!")
               } else {
                   message("... NO existe archivo !!!")
               }
               return(NULL)},
           warning = function(e){
               if (file.exists(x)) {
                   message("... ERROR de lectura !!!")
               } else {
                   message("... NO existe archivo !!!")
               }
               return(NULL)}
           )
}

## --- data.frames, objetos ---
## - agregar metadatos a d.f
## - agregar, leer, eliminar objetos y d.f de archivos

#' Modifica el atributo \code{meta}
#' @description Para agregar alguna descripción significativa de los
#'     datos. Alternativa a la función \code{comment}.
#' @export
#' @examples
#' meta(df) <- "metadata"
#' @author Eddy Castellón
`meta<-` <- function(x, value){
    attr(x, "meta") <- value
    invisible(x)
}

#' Atributo \code{meta}
#' @param x nombre del data.frame u objeto (sin comillas)
#' @return Valor del atributo \code{meta} o \code{NA} si no ha sido asignado
#' @export
#' @importFrom assertthat assert_that
#' @examples
#' meta(name_df) -> attr(name_df, "meta")
#' @author Eddy Castellón
meta <- function(x){
    ifelse(is.null(cc <- attr(x, "meta")), NA_character_, cc)
}

## --- objetos ---

#' Lista de objetos
#' @description Lista de nombres y clases de objetos guardados en
#'     archivo
#' @param x nombre de archivo
#' @param meta devuelve atributo \code{meta} si lo tiene; TRUE por
#'     omisión
#' @param clase devuelve sólo lista de objetos de la clase indicada;
#'     \code{data.frame} por omisión; "." cualquier clase
#' @return vector con nombres; o data.frame con nombre, metadatos
#'     asociados y la clase de los objetos cuando clase = "."
#' @export
#' @author Eddy Castellón
list_off <- function(file, meta = TRUE, clase = "data.frame"){

    ANY  <- "."
    anyclas <- clase == ANY

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
        keep[jj] <- anyclas || inherits(zz, clase)
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
                return(cbind(xx, clase = clases[keep],
                             stringsAsFactors = FALSE))
            } else {
                return(xx)
            }
        } else {
            if (anyclas) {
                return(data.frame(data = cc[keep],
                                  clase = clases[keep],
                                  stringsAsFactors = FALSE))
            } else {
                return(cc[keep])
            }
        }
    } else {
        message("\n??? NO hay objetos de clase ", clase)
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
