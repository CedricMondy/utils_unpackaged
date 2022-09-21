
#' Title
#'
#' @param x 
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom sf st_difference st_union st_combine
st_erase <- function(x, y) {
    sf::st_difference(
        x, 
        sf::st_union(sf::st_combine(y))
        )
}
