#' Get sites.
#'
#' @return
#' Returns a data frame (tibble) of valid sites where Mercado Libre is available.  Fields include default_currency_id, id, and name.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'

get_sites <- function() {
    res <- RETRY('GET', 'https://api.mercadolibre.com/sites')

    verify_result(res, "get_sites")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get site listing types.
#'
#' @param site_id Required. site_id of the country of interest.  See mercadolibreR::get_sites() or mercadolibreR::get_site_ids() for valid site_ids.
#' @return
#' Returns a data frame (tibble) of listing types for the given country's Mercado Libre site.  Fields include site_id, id, and name.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_site_listing_types <- function(site_id) {
    if (!site_id %in% get_site_ids()) {
        stop("STOP get_site_listing_types: invalid site_id.  See mercadolibreR::get_sites() or mercadolibreR::get_site_ids() for valid site_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/listing_types")
    res <- RETRY('GET', url)

    verify_result(res, "get_site_listing_types")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}
