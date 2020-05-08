#' Get Mercado Libre Access Token
#'
#' This function creates a Mercado Libre access token.
#' @param app_id Defaults to System Environment variable "MELI_APP_ID"
#' @param client_secret Defaults to System Environment variable "MELI_SECRET_KEY"
#' @keywords auth
#' @export
#' @examples
#' \dontrun{
#' get_mercadolibre_access_token()
#' }

get_mercadolibre_access_token <- function(app_id = Sys.getenv('MELI_APP_ID'), client_secret = Sys.getenv('MELI_SECRET_KEY')) {
    # In order to get an access token, the user appears to need to log in.
    # When I run the following line, the response URL redirects me to the log-in page.
    # p <- RETRY('POST', paste('https://auth.mercadolibre.com.ar/authorization?response_type=token&client_id=', app_id, sep = ""))
    # When I go to the response URL online, it takes me to the log-in page for Mercado Libre's developer platform.
    # When I log in, I then get my access token in the redirect URI.  I do not know how I can get the access
    # token directly; I think I am following the instructions correctly on the Mercado Libre site.
    # For the time being I will move ahead without implementing get_mercadolibre_access_token.

    # TODO: Review whether this function is necessary / can be implemented successfully without user log-in.

    stop("STOP get_mercadolibre_access_token: unimplemented")

    post <- RETRY('POST', 'https://auth.mercadolibre.com.ar/authorization',
                 accept_json(), authenticate(app_id, client_secret),
                 body = list(grant_type = 'code'),
                 encode = 'form', httr::config(http_version = 2)) %>% content

    p <- RETRY('POST', paste('https://auth.mercadolibre.com.ar/authorization?response_type=token&client_id=', app_id, sep = ""))
    # What I expect to get in return: https://secure.mlstatic.com/org-img/sdk/xd-1.0.4.html#access_token=APP_USR-2952745222604219-050707-c3e5fb777793327d07fccefcab3cb4cd-468488710&expires_in=21600&state=iframe&user_id=468488710&domains=secure.mlstatic.com,developers.mercadolibre.com.co
    # http://auth.mercadolibre.com/authorization?redirect_uri=https%3A%2F%2Fsecure.mlstatic.com%2Forg-img%2Fsdk%2Fxd-1.0.4.html&response_type=token&client_id=2952745222604219&state=iframe&display=popup&interactive=1
    if (!is.null(post$error)) {
        stop(str_glue('Could not authenticate with given Mercado Libre credentials:\n\t{post$error_description}'))
    }

    access_token <- post$access_token

    return(access_token)
}
#' Get Mercado Libre authorization Code
#'
#' This function creates a Mercado Libre access token.
#' @param app_id Defaults to System Environment variable "MELI_APP_ID"
#' @param client_secret Defaults to System Envioronment variable "MELI_SECRET_KEY"
#' @param scope Space delimited string of Mercado Libre scopes, found here: https://developers.mercadolibre.com.ar/es_ar/autenticacion-y-autorizacion#Obten-tu-access-token. All scopes are selected by default.
#' @keywords auth
#' @export
#' @examples
#' \dontrun{
#' get_mercadolibre_authorization_code()
#' }

get_mercadolibre_authorization_code <- function(app_id = Sys.getenv("MELI_APP_ID"), client_secret = Sys.getenv("MELI_SECRET_KEY"), scope = c('read', 'write', 'offline_access')) {
    # TODO:  How is this different from getting an access token?  I am not familiar enough with using Oauth to know.

    stop("STOP get_mercadolibre_authorization_code: unimplemented")

    endpoint <- oauth_endpoint(authorize = 'https://auth.mercadolibre.com.ar/authorization', access = 'https://developers.mercadolibre.com.co/')
    app <- oauth_app('mercadolibreR', app_id, client_secret)
    oauth2.0_token(endpoint = endpoint, app = app, scope = scope)
}
