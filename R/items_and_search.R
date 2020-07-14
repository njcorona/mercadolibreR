#' Get items listed under a given category.
#'
#' @param site_id Required. site_id of the country of interest.  See mercadolibreR::get_site_ids() for valid site_ids.
#' @param category_id Required. category_id of the category of interest.  See mercadolibreR::get_categories() for valid category_ids.
#' @param offset Optional.  Default is zero.  Maximum is 1000.  Starting point from which to return records.  For example, to receive the 51st to 100th records, offset = 50.
#' @param sort Optional.  Default is zero.  How to sort results.  0 = by relevance; 1 = by price, ascending; 2 = by price, descending.
#' @return
#' Returns a list() of information associated with fifty items listed under the given category.  Fields include sold_quantity, available_quantity, title, permalink, tags, seller, etc.  See the API \href{https://developers.mercadolibre.com.co/es_ar/items-y-busquedas}{documentation} for more information.
#' @export
#'
get_category_items <- function(site_id, category_id, offset = 0, sort = 0) {
    if (!is.numeric(sort)) {
        stop("STOP get_category_items: sort must be numeric.")
    }

    if (!is.numeric(offset)) {
        stop("STOP get_category_items: offset must be numeric.")
    }

    if (offset < 0 | offset > 1000) {
        stop("STOP get_category_items: offset out of bounds.  Must be >=0 and <= 1000.")
    }

    if (!site_id %in% get_site_ids()$id) {
        stop("STOP get_category_items: invalid site_id.  See mercadolibreR::get_site_ids() for valid site_ids.")
    }

    if (!category_id %in% get_categories(site_id)$id) {
        stop("STOP get_category_items: invalid category_id.  See mercadolibreR::get_categories() for valid category_ids.")
    }

    sortby <- "relevance"
    if (sort == 1) {
        sortby <- "price_asc"
    } else if (sort == 2) {
        sortby <- "price_desc"
    } else if (sort != 0) {
        if (!sort %in% c(0:2)) {
            stop("STOP get_category_items: invalid sort.  Must be 0, 1, or 2.")
        }
    }

    body <- list(offset = offset, sort = sortby)

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/search?category=", category_id)

    res <- RETRY(verb = 'GET', url = url, body = body)

    verify_result(res, "get_category_items")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(res)
}

#' Search Mercado Libre for item listings.
#'
#' @param site_id Required.  site_id of the country of interest.  See mercadolibreR::get_site_ids() for valid site_ids.
#' @param search Required.  What to search for.
#' @param additional_fields Optional.  A list() of named character fields that will be added as parameters for the search.  For example, giving list(buying_mode = "buy_it_now") will only search results whose buying_mode is "buy_it_now".
#' @param offset Optional.  Default is zero.  Maximum is 1000.  Starting point from which to return records.  For example, to receive the 51st to 100th records, offset = 50.
#' @param sort Optional.  Default is zero.  How to sort results.  0 = by relevance; 1 = by price, ascending; 2 = by price, descending.
#' @return
#' Returns a list() of information associated with fifty items listed under the given category.  Fields include sold_quantity, available_quantity, title, permalink, tags, seller, etc.  See the API \href{https://developers.mercadolibre.com.co/es_ar/items-y-busquedas}{documentation} for more information.
#' @export
#'
search <- function(site_id, search, additional_fields = list(), offset = 0, sort = 0) {
    if (!is.numeric(sort)) {
        stop("STOP search: sort must be numeric.")
    }

    if (!is.numeric(offset)) {
        stop("STOP search: offset must be numeric.")
    }

    if (offset < 0 | offset > 1000) {
        stop("STOP search: offset out of bounds.  Must be >=0 and <= 1000.")
    }

    if (!site_id %in% get_site_ids()$id) {
        stop("STOP search: invalid site_id.  See mercadolibreR::get_site_ids() for valid site_ids.")
    }

    sortby <- "relevance"
    if (sort == 1) {
        sortby <- "price_asc"
    } else if (sort == 2) {
        sortby <- "price_desc"
    } else if (sort != 0) {
        if (!sort %in% c(0:2)) {
            stop("STOP search: invalid sort.  Must be 0, 1, or 2.")
        }
    }

    body <- c(list(offset = offset, q = search, sort = sortby), additional_fields)

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/search")

    res <- RETRY(verb = 'GET', url = url, body = body)

    verify_result(res, "search")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(res)
}

#' Get items listed by a given seller.
#'
#' @param site_id Required. site_id of the country of interest.  See mercadolibreR::get_site_ids() for valid site_ids.
#' @param nickname Optional-ish. Must provide either nickname or seller_id argument.  Nickname of seller of interest.
#' @param seller_id Optional-ish. Must provide either nickname or seller_id argument.  seller_id of seller of interest.
#' @param category_id Optional. Will limit results to those in the category of interest.  See mercadolibreR::get_categories() for valid category_ids.
#' @param offset Optional.  Default is zero.  Maximum is 1000.  Starting point from which to return records.  For example, to receive the 51st to 100th records, offset = 50.
#' @param sort Optional.  Default is zero.  How to sort results.  0 = by relevance; 1 = by price, ascending; 2 = by price, descending.
#' @return
#' Returns a list() of information associated with fifty items listed under the given category.  Fields include sold_quantity, available_quantity, title, permalink, tags, seller, etc.  See the API \href{https://developers.mercadolibre.com.co/es_ar/items-y-busquedas}{documentation} for more information.
#' @export
#'
get_seller_items <- function(site_id, nickname = NULL, seller_id = NULL, category_id = NULL, offset = 0, sort = 0) {
    if (!is.numeric(sort)) {
        stop("STOP get_seller_items: sort must be numeric.")
    }

    if (!is.numeric(offset)) {
        stop("STOP get_seller_items: offset must be numeric.")
    }

    if (offset < 0 | offset > 1000) {
        stop("STOP get_seller_items: offset out of bounds.  Must be >=0 and <= 1000.")
    }

    if (!site_id %in% get_site_ids()$id) {
        stop("STOP get_seller_items: invalid site_id.  See mercadolibreR::get_site_ids() for valid site_ids.")
    }

    if (!category_id %in% get_categories(site_id)$id) {
        stop("STOP get_seller_items: invalid category_id.  See mercadolibreR::get_categories() for valid category_ids.")
    }

    if (!is.null(nickname) & !is.null(seller_id)) {
        stop("STOP get_seller_items: provide either nickname or seller_id, not both.")
    }

    if (is.null(nickname) & is.null(seller_id)) {
        stop("STOP get_seller_items: provide either nickname or seller_id.")
    }

    sortby <- "relevance"
    if (sort == 1) {
        sortby <- "price_asc"
    } else if (sort == 2) {
        sortby <- "price_desc"
    } else if (sort != 0) {
        if (!sort %in% c(0:2)) {
            stop("STOP get_seller_items: invalid sort.  Must be 0, 1, or 2.")
        }
    }

    if (is.null(seller_id)) {
        body <- list(offset = offset, sort = sortby, nickname = nickname)
    } else {
        body <- list(offset = offset, sort = sortby, seller_id = seller_id)
    }

    if (!is.null(category_id)) {
        body <- c(body, category = category_id)
    }

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/search")

    res <- RETRY(verb = 'GET', url = url, body = body)

    verify_result(res, "get_seller_items")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(res)
}

#' Returns a tibble() of what sold_quantity values correspond to in reality.  For example, '500' indicates there are 251 to 500 units sold.
#'
#' @return
#' Returns a tibble() of what sold_quantity values correspond to in reality.  See the API \href{https://developers.mercadolibre.com.co/es_ar/items-y-busquedas}{documentation} for more information.
#' @export
#'
sold_quantity <- function() {
    real <- c("1", "2", "3", "4", "5", "6to25", "51to100", "101to150", "151to200", "201to250", "251to500", "501to5000", "5001to50000", "50001to500000")
    standin <- c(1, 2, 3, 4, 5, 25, 50, 100, 150, 200, 250, 500, 5000, 50000)
    return(tibble(real, standin))
}

#' Returns a tibble() of what available_quantity values correspond to in reality.  For example, '500' indicates that 251 to 500 units are available.
#'
#' @return
#' Returns a tibble() of what available_quantity values correspond to in reality.  See the API \href{https://developers.mercadolibre.com.co/es_ar/items-y-busquedas}{documentation} for more information.
#' @export
#'
available_quantity <- function() {
    real <- c("0to50", "51to100", "101to150", "151to200", "201to250", "251to500", "501to5000", "5001to50000", "50001to99999")
    standin <- c(1, 50, 100, 150, 200, 250, 500, 5000, 50000)
    return(tibble(real, standin))
}

