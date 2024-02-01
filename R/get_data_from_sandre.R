#' Title
#'
#' @param sandre_url 
#'
#' @return
#' @export
#'
#' @examples
get_sandre_layers <- function(sandre_url = "https://services.sandre.eaufrance.fr/geo/sandre") {
    client_sandre <- ows4R::WFSClient$new(
        sandre_url,
        serviceVersion = "2.0.0"
    )
    
    client_sandre$getFeatureTypes(pretty = TRUE)
}

#' Title
#'
#' @param layer 
#' @param crs 
#' @param sandre_url 
#'
#' @return
#' @export
#'
#' @examples
read_sandre_wfs <- function(layer, crs, sandre_url = "https://services.sandre.eaufrance.fr/geo/sandre") {
    sandre_url %>% 
        (function(x) {
            url <- httr::parse_url(x)
            url$query <- list(
                service = "wfs",
                request = "GetFeature",
                typename = layer,
                version = "2.0.0",
                srsName = paste0("EPSG:", crs)
            )
            url
        }) %>% 
        httr::build_url() %>% 
        sf::st_read()
}
