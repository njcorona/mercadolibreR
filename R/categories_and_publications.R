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

#' Get site domain information.
#'
#' @param site_domain_url Required. site_domain_url of the domain of interest.  See mercadolibreR::get_site_domains() for valid site_domain_url's.
#' @return
#' Returns a data frame (tibble) of information associated with a single site domain.  Fields include id, site_id, and country_id, locale, and tag.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
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

#' Get listing types.
#'
#' @param site_id Required. site_id of the country of interest.  See mercadolibreR::get_site_ids() for valid site_ids.
#' @return
#' Returns a data frame (tibble) of listing types for the given country's Mercado Libre site.  Fields include site_id, id, and name.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_listing_types <- function(site_id) {
    if (!site_id %in% get_site_ids()$id) {
        stop("STOP get_listing_types: invalid site_id.  See mercadolibreR::get_site_ids() for valid site_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/listing_types")
    res <- RETRY('GET', url)

    verify_result(res, "get_listing_types")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get listing exposures.
#'
#' @param site_id Required. site_id of the country of interest.  See mercadolibreR::get_site_ids() for valid site_ids.
#' @return
#' Returns a data frame (tibble) of listing exposures for the given country's Mercado Libre site.  Fields include id, name, home_page, category_home_page, advertising_on_listing_page, and priority_in_search.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_listing_exposures <- function(site_id) {
    if (!site_id %in% get_site_ids()$id) {
        stop("STOP get_listing_exposures: invalid site_id.  See mercadolibreR::get_site_ids() for valid site_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/listing_exposures")
    res <- RETRY('GET', url)

    verify_result(res, "get_listing_exposures")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get listing prices.
#'
#' @param site_id Required. site_id of the country of interest.  See mercadolibreR::get_site_ids() for valid site_ids.
#' @param price Required.  Maximum price for listing and selling.
#' @return
#' Returns a data frame (tibble) of prices for various listing types for the given country's Mercado Libre site.  Fields include listing_type_id, listing_type_name, listing_exposure, requires_picture, currency_id, listing_fee_amount, sale_fee_amount, free_relist, and stop_time.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_listing_prices <- function(site_id, price) {
    if (!site_id %in% get_site_ids()$id) {
        stop("STOP get_listing_prices: invalid site_id.  See mercadolibreR::get_site_ids() for valid site_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/listing_prices")

    params <- list(
        price = price
    )

    res <- RETRY('GET', url, query = params)

    verify_result(res, "get_listing_prices")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get categories.
#'
#' @param site_id Required. site_id of the country of interest.  See mercadolibreR::get_site_ids() for valid site_ids.
#' @return
#' Returns a data frame (tibble) of the categories of products available on the given country's Mercado Libre site.  Fields include category id and name.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_categories <- function(site_id) {
    if (!site_id %in% get_site_ids()$id) {
        stop("STOP get_categories: invalid site_id.  See mercadolibreR::get_site_ids() for valid site_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/categories")

    res <- RETRY('GET', url)

    verify_result(res, "get_categories")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get category information.
#'
#' @param category_id Required. category_id of the category of interest.  See mercadolibreR::get_categories() for valid category ids.
#' @return
#' Returns a list() of the information associated with a given category of products available on Mercado Libre.  Information includes number of items, child categories, and settings.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_category_information <- function(category_id) {
    url <- str_glue("https://api.mercadolibre.com/categories/", category_id)

    res <- RETRY('GET', url)

    verify_result(res, "get_category_information")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(res)
}

#' Predict the category a product should fall under.
#'
#' @param site_id Required. site_id of the country the product will be listed in.  See mercadolibreR::get_site_ids() for valid site_ids.
#' @param title Required. Title of the product's listing.
#' @return
#' Returns a list() of the predicted categories with associated ids and probabilities.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
predict_category <- function(site_id, title) {
    if (!site_id %in% get_site_ids()$id) {
        stop("STOP predict_category: invalid site_id.  See mercadolibreR::get_site_ids() for valid site_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/category_predictor/predict")

    params <- list(
        title = title
    )

    res <- RETRY('GET', url, query = params, encode = 'json')

    verify_result(res, "predict_category")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(res)
}

#' Get a category's promotion packs.
#'
#' @param category_id Required. category_id of the category of interest.  See mercadolibreR::get_categories() for valid category ids.
#' @return
#' Returns a data frame (tibble) of the promotion packs associated with a given category of products.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_category_promotion_packs <- function(category_id) {
    # These three lines trigger a "Category not found" error if the category does not exist.
    # They are necessary because the API will return "Promotion packs not found" by default if the category does not exist.
    url <- str_glue("https://api.mercadolibre.com/categories/", category_id)
    res <- RETRY('GET', url)
    verify_result(res, "get_category_promotion_packs")

    url <- str_glue("https://api.mercadolibre.com/categories/", category_id, "/classifieds_promotion_packs")

    res <- RETRY('GET', url)

    verify_result(res, "get_category_promotion_packs")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get a listing type's information.
#'
#' @param site_id Required. site_id of the country the product will be listed in.  See mercadolibreR::get_site_ids() for valid site_ids.
#' @param listing_type_id Required. listing_type id of the listing of interest.  See mercadolibreR::get_listing_types() for valid listing_type_ids.
#' @return
#' Returns a list() of the information (settings) associated with a listing type.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
#' @export
#'
get_listing_type_information <- function(site_id, listing_type_id) {
    if (!site_id %in% get_site_ids()$id) {
        stop("STOP get_listing_type_information: invalid site_id.  See mercadolibreR::get_site_ids() for valid site_ids.")
    }

    if (!listing_type_id %in% get_listing_types(site_id)$id) {
        stop("STOP get_listing_type_information: invalid listing_type_id.  See mercadolibreR::get_listing_types() for valid listing_type_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/listing_types/", listing_type_id)

    res <- RETRY('GET', url)

    verify_result(res, "get_listing_type_information")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(res)
}
