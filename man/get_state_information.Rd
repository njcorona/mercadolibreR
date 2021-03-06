% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/locations_and_currencies.R
\name{get_state_information}
\alias{get_state_information}
\title{Get state information for a specific state.}
\usage{
get_state_information(country_id, state_id)
}
\arguments{
\item{country_id}{Required. country_id of the country of interest.  See mercadolibreR::get_countries() for valid country_ids.}

\item{state_id}{Required. state_id of the state of interest.  See mercadolibreR::get_states() for valid state_ids.}
}
\value{
Returns a list() of information associated with a given state in the given country.  Fields may include geo_information, states, and time_zone.  See the API \href{https://developers.mercadolibre.com.co/es_ar/ubicacion-y-monedas}{documentation} for more information.
}
\description{
Get state information for a specific state.
}
