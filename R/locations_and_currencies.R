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

#' Get currency information.
#'
#' @return
#' Returns a data frame (tibble) of data regarding Mercado Libre currencies.  Fields include id, symbol, description, and decimal_places.  See the API \href{https://developers.mercadolibre.com.co/es_ar/ubicacion-y-monedas}{documentation} for more information.
#' @export
#'
get_currencies <- function() {
    res <- RETRY('GET', "https://api.mercadolibre.com/currencies")

    verify_result(res, "get_currencies")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get currency information for a specific currency.
#'
#' @param currency_id Required. currency_id of the currency of interest.  See mercadolibreR::get_currencies() for valid currency_ids.
#' @return
#' Returns a data frame (tibble) of data regarding the given Mercado Libre currency.  Fields include id, symbol, description, and decimal_places.  See the API \href{https://developers.mercadolibre.com.co/es_ar/ubicacion-y-monedas}{documentation} for more information.
#' @export
#'
get_currency_information <- function(currency_id) {
    if (!currency_id %in% get_currencies()$id) {
        stop("STOP get_currency_information: invalid currency_id.  See mercadolibreR::get_currencies() for valid currency_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/currencies/", currency_id)

    res <- RETRY('GET', url)

    verify_result(res, "get_currencies")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get the conversion ratio Mercado Libre uses for two currencies.
#'
#' @param currency_id1 Required. currency_id of the first currency of interest.  See mercadolibreR::get_currencies() for valid currency_ids.
#' @param currency_id2 Required. currency_id of the second currency of interest.  See mercadolibreR::get_currencies() for valid currency_ids.
#' @return
#' Returns the conversion ratio from currency_id1 to currency_id2 as a list().  See the API \href{https://developers.mercadolibre.com.co/es_ar/ubicacion-y-monedas}{documentation} for more information.
#' @export
#'
currency_conversion_ratio <- function(currency_id1, currency_id2) {
    if (!currency_id1 %in% get_currencies()$id) {
        stop("STOP currency_conversion_ratio: invalid currency_id1.  See mercadolibreR::get_currencies() for valid currency_ids.")
    }

    if (!currency_id2 %in% get_currencies()$id) {
        stop("STOP currency_conversion_ratio: invalid currency_id2.  See mercadolibreR::get_currencies() for valid currency_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/currency_conversions/search?from=", currency_id1, "&to=", currency_id2)

    res <- RETRY('GET', url)

    verify_result(res, "currency_conversion_ratio")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(res)
}

#' Get zipcode information.
#'
#' @return
#' Returns a list() of information associated with a zipcode.  See the API \href{https://developers.mercadolibre.com.co/es_ar/ubicacion-y-monedas}{documentation} for more information.
#' @export
#'
get_zipcode_information <- function(country_id, zipcode) {
    if (!country_id %in% get_countries()$id) {
        stop("STOP get_zipcode_information: invalid country_id.  See mercadolibreR::get_countries() for valid country_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/countries/", country_id, "/zip_codes/", zipcode)

    res <- RETRY('GET', url)

    verify_result(res, "get_zipcode_information")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}

#' Get all of the zip code numbers between two zip codes for a country.
#'
#' @param country_id Required. country_id of the country of interest.  See mercadolibreR::get_countries() for valid country_ids.
#' @param zipcode_from Required. Lower end of range of zipcodes to be returned.
#' @param zipcode_to Required. Higher end of range of zipcodes to be returned.
#' @return
#' See the API \href{https://developers.mercadolibre.com.co/es_ar/ubicacion-y-monedas}{documentation} for more information.
#' @export
#'
get_zipcodes <- function(country_id, zipcode_from, zipcode_to) {
    stop("STOP get_zipcodes: This API method is not currently working.  @njcorona has submitted a support ticket to Mercado Libre check it out.")
    if (!country_id %in% get_countries()$id) {
        stop("STOP get_zipcodes: invalid country_id.  See mercadolibreR::get_countries() for valid country_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/country/", country_id, "/zip_codes/search_between?zip_code_from=", zipcode_from, "&zip_code_to=", zipcode_to)

    res <- RETRY('GET', url)

    verify_result(res, "get_zipcodes")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(as_tibble(res))
}
