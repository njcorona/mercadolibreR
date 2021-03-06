% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/categories_and_publications.R
\name{get_categories}
\alias{get_categories}
\title{Get categories.}
\usage{
get_categories(site_id)
}
\arguments{
\item{site_id}{Required. site_id of the country of interest.  See mercadolibreR::get_site_ids() for valid site_ids.}
}
\value{
Returns a data frame (tibble) of the categories of products available on the given country's Mercado Libre site.  Fields include category id and name.  See the API \href{https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones}{documentation} for more information.
}
\description{
Get categories.
}
