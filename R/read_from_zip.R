#' Lire le contenu d'une archive
#'
#' @param zipfile texte. Chemin de l'archive
#' @param file_pattern texte. Tout ou partie du chemin du fichier à extraire
#' @param fun nom de la fonction à utiliser pour lire le fichier
#' @param ...  paramètres supplémentaires à passer à la fonction `fun`
#'
#' @export
#'
#' @importFrom archive archive_extract
read_from_zip <- function(zipfile, file_pattern, fun, ...) {
    
    temp <- tempfile()
    
    archive::archive_extract(zipfile, dir = temp)
    
    filepath <- list.files(
        path = temp,
        full.names = TRUE,
        recursive = TRUE
    ) %>%
        (function(x) {
            x[grepl(x = x, pattern = file_pattern)]
        })
    
    obj <- fun(filepath, ...)
    
    unlink(temp, recursive = TRUE)
    
    return(obj)
}
