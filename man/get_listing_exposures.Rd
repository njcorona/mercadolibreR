% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/categories_and_publications.R
\name{get_listing_exposures}
\alias{get_listing_exposures}
\title{Get listing exposures.}
\usage{
get_listing_exposures(site_id)
}
\arguments{
\item{site_id}{Required. site_id of the country of interest.  See mercadolibreR::get_site_ids() for valid site_ids.}
}
\value{
Returns a data frame (tibble) of listing exposures for the given country's Mercado Libre site.  Fields include id, name, home_page, category_home_page, advertising_on_listing_page, and priority_in_search.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
}
\description{
Get listing exposures.
}
