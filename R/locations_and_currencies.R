#' Get country information.
#'
#' @return
#' Returns a data frame (tibble) of information associated with countries.  Fields include id, name, locale, and currency_id.  See the API \href{https://developers.mercadolibre.com.co/es_ar/ubicacion-y-monedas}{documentation} for more information.
#' @export
#'
get_countries <- function() {
    res <- RETRY('GET', "https://api.mercadolibre.com/classified_locations/countries")

    verify_result(res, "get_countries")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get country information for a specific country.
#'
#' @param country_id Required. country_id of the country of interest.  See mercadolibreR::get_countries() for valid country_ids.
#' @return
#' Returns a list() of information associated with a given country.  Fields may include geo_information, states, and time_zone.  See the API \href{https://developers.mercadolibre.com.co/es_ar/ubicacion-y-monedas}{documentation} for more information.
#' @export
#'
get_country_information <- function(country_id) {
    if (!country_id %in% get_countries()$id) {
        stop("STOP get_country_information: invalid country_id.  See mercadolibreR::get_countries() for valid country_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/classified_locations/countries/", country_id)

    res <- RETRY('GET', url)

    verify_result(res, "get_country_information")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(res)
}

#' Get states in a specific country.
#'
#' @param country_id Required. country_id of the country of interest.  See mercadolibreR::get_countries() for valid country_ids.
#' @return
#' Returns a data frame (tibble) of the states in a given country.  See the API \href{https://developers.mercadolibre.com.co/es_ar/ubicacion-y-monedas}{documentation} for more information.
#' @export
#'
get_states <- function(country_id) {
    if (!country_id %in% get_countries()$id) {
        stop("STOP get_states: invalid country_id.  See mercadolibreR::get_countries() for valid country_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/classified_locations/countries/", country_id)

    res <- RETRY('GET', url)

    verify_result(res, "get_states")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res$states))
}

#' Get state information for a specific state.
#'
#' @param country_id Required. country_id of the country of interest.  See mercadolibreR::get_countries() for valid country_ids.
#' @param state_id Required. state_id of the state of interest.  See mercadolibreR::get_states() for valid state_ids.
#' @return
#' Returns a list() of information associated with a given state in the given country.  Fields may include geo_information, states, and time_zone.  See the API \href{https://developers.mercadolibre.com.co/es_ar/ubicacion-y-monedas}{documentation} for more information.
#' @export
#'
get_state_information <- function(country_id, state_id) {
    if (!country_id %in% get_countries()$id) {
        stop("STOP get_state_information: invalid country_id.  See mercadolibreR::get_countries() for valid country_ids.")
    }

    if (!state_id %in% get_states(country_id)$id) {
        stop("STOP get_state_information: invalid state_id.  See mercadolibreR::get_states() for valid state_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/classified_locations/states/", state_id)

    res <- RETRY('GET', url)

    verify_result(res, "get_state_information")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(res)
}

#' Get cities in a specific state in a specific country.
#'
#' @param country_id Required. country_id of the country of interest.  See mercadolibreR::get_countries() for valid country_ids.
#' @param stte_id Required. state_id of the state of interest.  See mercadolibreR::get_states() for valid state_ids.
#' @return
#' Returns a data frame (tibble) of the cities in a given state.  See the API \href{https://developers.mercadolibre.com.co/es_ar/ubicacion-y-monedas}{documentation} for more information.
#' @export
#'
get_cities <- function(country_id, state_id) {
    if (!country_id %in% get_countries()$id) {
        stop("STOP get_cities: invalid country_id.  See mercadolibreR::get_countries() for valid country_ids.")
    }

    if (!state_id %in% get_states(country_id)$id) {
        stop("STOP get_cities: invalid state_id.  See mercadolibreR::get_states() for valid state_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/classified_locations/states/", state_id)

    res <- RETRY('GET', url)

    verify_result(res, "get_cities")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res$cities))
}

#' Get city information for a specific city.
#'
#' @param country_id Required. country_id of the country of interest.  See mercadolibreR::get_countries() for valid country_ids.
#' @param state_id Required. state_id of the state of interest.  See mercadolibreR::get_states() for valid state_ids.
#' @param city_id Required. city_id state of interest.  See mercadolibreR::get_cities() for valid city_ids.
#' @return
#' Returns a list() of information associated with a given city.  Fields may include geo_information and neighborhoods.  See the API \href{https://developers.mercadolibre.com.co/es_ar/ubicacion-y-monedas}{documentation} for more information.
#' @export
#'
get_city_information <- function(country_id, state_id, city_id) {
    if (!country_id %in% get_countries()$id) {
        stop("STOP get_city_information: invalid country_id.  See mercadolibreR::get_countries() for valid country_ids.")
    }

    if (!state_id %in% get_states(country_id)$id) {
        stop("STOP get_city_information: invalid state_id.  See mercadolibreR::get_states() for valid state_ids.")
    }

    if (!city_id %in% get_cities(country_id, state_id)$id) {
        stop("STOP get_city_information: invalid city_id.  See mercadolibreR::get_cities() for valid city_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/classified_locations/cities/", city_id)

    res <- RETRY('GET', url)

    verify_result(res, "get_city_information")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(res)
}
