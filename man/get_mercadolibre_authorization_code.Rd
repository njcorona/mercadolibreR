% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/authenticate.R
\name{get_mercadolibre_authorization_code}
\alias{get_mercadolibre_authorization_code}
\title{Get Mercado Libre authorization Code}
\usage{
get_mercadolibre_authorization_code(
  app_id = Sys.getenv("MELI_APP_ID"),
  client_secret = Sys.getenv("MELI_SECRET_KEY"),
  scope = scopes
)
}
\arguments{
\item{app_id}{Defaults to System Environment variable "MELI_APP_ID"}

\item{client_secret}{Defaults to System Envioronment variable "MELI_SECRET_KEY"}

\item{scope}{Space delimited string of Mercado Libre scopes, found here: https://developers.mercadolibre.com.ar/es_ar/autenticacion-y-autorizacion#Obten-tu-access-token. All scopes are selected by default.  Should match your app's enabled scopes.}
}
\description{
This function creates a Mercado Libre access token.
}
\examples{
\dontrun{
get_mercadolibre_authorization_code()
}
}
\keyword{auth}
