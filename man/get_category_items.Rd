% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/items_and_search.R
\name{get_category_items}
\alias{get_category_items}
\title{Get items listed under a given category.}
\usage{
get_category_items(site_id, category_id, offset = 0, sort = 0)
}
\arguments{
\item{site_id}{Required. site_id of the country of interest.  See mercadolibreR::get_site_ids() for valid site_ids.}

\item{category_id}{Required. category_id of the category of interest.  See mercadolibreR::get_categories() for valid category_ids.}

\item{offset}{Optional.  Default is zero.  Maximum is 1000.  Starting point from which to return records.  For example, to receive the 51st to 100th records, offset = 50.}

\item{sort}{Optional.  Default is zero.  How to sort results.  0 = by relevance; 1 = by price, ascending; 2 = by price, descending.}
}
\value{
Returns a list() of information associated with fifty items listed under the given category.  Fields include sold_quantity, available_quantity, title, permalink, tags, seller, etc.  See the API \href{https://developers.mercadolibre.com.co/es_ar/items-y-busquedas}{documentation} for more information.
}
\description{
Get items listed under a given category.
}
