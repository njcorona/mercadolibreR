#' Create a test user.
#'
#' @param site_id Required. The country in which you want to operate.
#' @param authorization Required. A valid access token from developers.mercadolibre.com. See the \href{https://developers.mercadolibre.com.co/es_ar/autenticacion-y-autorizacion/}{API authentication and authorization guide} for more details. Defaults to System Environment variable "MELI_ACCESS_TOKEN".
#' @return
#' Returns a list of results containing the new test user's user_id, nickname, password, and current status. See the API \href{https://developers.mercadolibre.com.co/es_ar/realiza-pruebas}{documentation} for more information.
#' @export
#'

create_test_user <- function(site_id, authorization = Sys.getenv('MELI_ACCESS_TOKEN')) {

  if (!site_id %in% site_ids()) {
    stop("STOP create_test_user: invalid site_id.  See mercadolibreR::sites() or mercadolibreR::site_ids() for valid site_ids.")
  }

  url <- 'https://api.mercadolibre.com/users/test_user'

  params <- list(
    access_token = authorization
  )

  res <- RETRY('POST', url, query = params, body = list(site_id = c(site_id)), encode = 'json')

  verify_result(res = res, function_name = "create_test_user")

  res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)

  return(res)
}
