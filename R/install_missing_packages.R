#' Installer des packages manquants sans les charger
#'
#' @param pkgs vecteur des noms de packages (caractères) à installer 
#'
#' @export
#'
#' @examples
#' @importFrom pacman p_isinstalled p_install
install_missing_packages <- function(pkgs) {

    not_installed <- pkgs[!pacman::p_isinstalled(pkgs)]
    
    if (length(not_installed) > 0)
        pacman::p_install(not_installed, character.only = TRUE)
}

