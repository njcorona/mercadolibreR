#' Get site ids.
#'
#' @return
#' Returns a data frame (tibble) of site ids where Mercado Libre is available.  Fields include default_currency_id, id, and name.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_site_ids <- function() {
    res <- RETRY('GET', 'https://api.mercadolibre.com/sites')

    verify_result(res, "get_site_ids")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get site domains.
#'
#' @return
#' Returns a data frame (tibble) of site domains where Mercado Libre is available.  Fields include id, site_id, and country_id, locale, and tag.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_site_domains <- function() {
    res <- RETRY('GET', "https://api.mercadolibre.com/site_domains")

    verify_result(res, "get_site_domains")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get site domain information.  (Same information as mercadolibreR::get_site_domains() except for one domain.)
#'
#' @param site_domain_url Required. site_domain_url of the domain of interest.  See mercadolibreR::get_site_domains() for valid site_domain_url's.
#' @return
#' Returns a data frame (tibble) of information associated with a site domain.  Fields include id, site_id, and country_id, locale, and tag.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_site_domain_information <- function(site_domain_url) {
    if (!site_domain_url %in% get_site_domains()$id) {
        stop("STOP get_site_domain_information: invalid site_domain_url.  See mercadolibreR::get_site_domains() for valid site_domain_url's.")
    }

    url <- str_glue("https://api.mercadolibre.com/site_domains/", site_domain_url)

    res <- RETRY('GET', url)

    verify_result(res, "get_site_domain_information")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get site listing types.
#'
#' @param site_id Required. site_id of the country of interest.  See mercadolibreR::get_site_ids() for valid site_ids.
#' @return
#' Returns a data frame (tibble) of listing types for the given country's Mercado Libre site.  Fields include site_id, id, and name.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_site_listing_types <- function(site_id) {
    if (!site_id %in% get_site_ids()$id) {
        stop("STOP get_site_listing_types: invalid site_id.  See mercadolibreR::get_site_ids() for valid site_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/listing_types")
    res <- RETRY('GET', url)

    verify_result(res, "get_site_listing_types")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get site listing exposures.
#'
#' @param site_id Required. site_id of the country of interest.  See mercadolibreR::get_site_ids() for valid site_ids.
#' @return
#' Returns a data frame (tibble) of listing exposures for the given country's Mercado Libre site.  Fields include id, name, home_page, category_home_page, advertising_on_listing_page, and priority_in_search.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_site_listing_exposures <- function(site_id) {
    if (!site_id %in% get_site_ids()$id) {
        stop("STOP get_site_listing_exposures: invalid site_id.  See mercadolibreR::get_site_ids() for valid site_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/listing_exposures")
    res <- RETRY('GET', url)

    verify_result(res, "get_site_listing_exposures")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get site listing prices.
#'
#' @param site_id Required. site_id of the country of interest.  See mercadolibreR::get_site_ids() for valid site_ids.
#' @param price Required.  Maximum price for listing and selling.
#' @return
#' Returns a data frame (tibble) of prices for various listing types for the given country's Mercado Libre site.  Fields include listing_type_id, listing_type_name, listing_exposure, requires_picture, currency_id, listing_fee_amount, sale_fee_amount, free_relist, and stop_time.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_site_listing_prices <- function(site_id, price) {
    if (!site_id %in% get_site_ids()$id) {
        stop("STOP get_site_listing_prices: invalid site_id.  See mercadolibreR::get_site_ids() for valid site_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/listing_prices")

    params <- list(
        price = price
    )

    res <- RETRY('GET', url, query = params)

    verify_result(res, "get_site_listing_prices")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}
