% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/locations_and_currencies.R
\name{get_currency_information}
\alias{get_currency_information}
\title{Get currency information for a specific currency.}
\usage{
get_currency_information(currency_id)
}
\arguments{
\item{currency_id}{Required. currency_id of the currency of interest.  See mercadolibreR::get_currencies() for valid currency_ids.}
}
\value{
Returns a data frame (tibble) of data regarding the given Mercado Libre currency.  Fields include id, symbol, description, and decimal_places.  See the API \href{https://developers.mercadolibre.com.co/es_ar/ubicacion-y-monedas}{documentation} for more information.
}
\description{
Get currency information for a specific currency.
}
