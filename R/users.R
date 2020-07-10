#' Get user information.
#'
#' @param self TRUE if retrieving information about your user; FALSE otherwise.  Defaults to FALSE.
#' @param private_information Required if self is FALSE.  TRUE if retrieving private information from another user; FALSE otherwise.  Defaults to FALSE.
#' @param user_id Required if self is FALSE.  user_id of user to look up.  Use mercadolibreR::get_user_id() if you only have access to the user's nickname and site_id.
#' @param authorization Required if self is TRUE or if private_information is TRUE. If self is TRUE, should be your access token.  If private_information is TRUE, should be the access token for the user from which to retrieve information.  See the \href{https://developers.mercadolibre.com.co/es_ar/autenticacion-y-autorizacion/}{API authentication and authorization guide} for more details.
#' @return
#' Returns a list of information about the user. See the API \href{https://developers.mercadolibre.com.co/es_ar/producto-consulta-usuarios}{documentation} for more information.
#' @export
#'

get_user_information <- function(self = FALSE, private_information = TRUE, user_id = NULL, authorization = NULL) {
    if ((self | private_information) & is.null(authorization)) {
        stop("STOP get_user_information: missing authorization.")
    }

    if (!self & is.null(user_id)) {
        stop("STOP get_user_information: missing user_id.")
    }

    if (self) {
        url <- 'https://api.mercadolibre.com/users/me'

        params <- list(
            access_token = authorization
        )
    } else {
        url <- str_glue('https://api.mercadolibre.com/users/', user_id)
    }

    if (private_information) {
        params <- list(
            access_token = authorization
        )
    }

    res <- RETRY('GET', url, query = params, encode = 'json')

    verify_result(res = res, function_name = "get_user_information")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(res)
}

#' Get user_id from nickname and site_id.
#'
#' @param site_id Required. site_id of the country in which the user is registered.  See mercadolibreR::get_site_ids() for valid site_ids.
#' @param nickname Required. Nickname of the user.
#' @return
#' Returns numeric user_id, or NULL if user is not registered in the given country. See the API \href{https://developers.mercadolibre.com.co/es_ar/producto-consulta-usuarios}{documentation} for more information.
#' @export
#'

get_user_id <- function(site_id, nickname) {
    if (!site_id %in% get_site_ids()$id) {
        stop("STOP get_user_id: invalid site_id.  See mercadolibreR::get_site_ids() for valid site_ids.")
    }

    url <- str_glue("https://api.mercadolibre.com/sites/", site_id, "/search")

    params <- list(
        nickname = nickname
    )

    res <- RETRY('GET', url, query = params, encode = 'json')

    verify_result(res = res, function_name = "get_user_id")

    return(content(res)$seller$id)
}

#' Get user addresses.
#'
#' @param user_id user_id of user.  Use mercadolibreR::get_user_id() if you only have access to the user's nickname and site_id.
#' @param authorization A valid access token from developers.mercadolibre.com. See the \href{https://developers.mercadolibre.com.co/es_ar/autenticacion-y-autorizacion/}{API authentication and authorization guide} for more details.
#' @return
#' Returns a list of the user's addresses. See the API \href{https://developers.mercadolibre.com.co/es_ar/usuarios-y-aplicaciones}{documentation} for more information.
#' @export
#'

get_user_addresses <- function(user_id, authorization = Sys.getenv('MELI_ACCESS_TOKEN')) {

    url <- str_glue("https://api.mercadolibre.com/users/", user_id, "/addresses")

    params <- list(
        access_token = authorization
    )

    res <- RETRY('GET', url, query = params, encode = 'json')

    verify_result(res = res, function_name = "get_user_addresses")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(res)
}

#' Set user information.
#'
#' @param user_id Required.  user_id of user to update.  Use mercadolibreR::get_user_id() if you only have access to the user's nickname and site_id.
#' @param authorization Required.  A valid access token for the user from developers.mercadolibre.com. See the \href{https://developers.mercadolibre.com.co/es_ar/autenticacion-y-autorizacion/}{API authentication and authorization guide} for more details.
#' @param new_values a list() of updated information about the user.
#' @return
#' Returns an updated list of information about the user. See the API \href{https://developers.mercadolibre.com.co/es_ar/producto-consulta-usuarios}{documentation} for more information.
#' @export
#'

# I am unable to test this without test users, but I've created the maximum number of test users and I don't know how to
# get the user id's of the ones I made.
# TODO: fix this and write unit tests.
# I sent a support ticket 7/5/2020 asking about the id's of my test users so I can test this.
set_user_information <- function(user_id, authorization, new_values) {
    url <- str_glue("https://api.mercadolibre.com/users/", user_id, "?access_token=", authorization)

    res <- RETRY('PUT', url, content_type_json(), body = new_values, encode = 'json')

    verify_result(res = res, function_name = "set_user_information")

    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

    return(get_user_information(self = TRUE, user_id = user_id, authorization = authorization))
}
