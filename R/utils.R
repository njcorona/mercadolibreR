library(stringr)
library(httr)
library(tidyverse)
library(jsonlite)
authorization <- ""
# TODO: write unit tests.


#' Verify API result
#'
#' Check API result for error codes
#' @param res API result to check
#' @param function_name name of function calling verify_result
#' @export
verify_result <- function(res, function_name) {
    content <- content(res)
    if (!is.null(content$error)) {
        stop(str_glue('FUNCTION: {function_name}()\n  ERROR: {content$error} (HTTP {content$status})\n  MESSAGE: {content$message}'))
    }
    stop_for_status(res)
}

#' Valid scopes
#'
#' Vector of valid scopes for mercadolibreR::get_authorization_code()
#' @export
scopes <- c('read', 'write', 'offline_access')

#' Valid site_ids
#'
#' Vector of valid site_ids.
#' @export
get_site_ids <- function() {
    get_sites()$id
}

# TODO: delete this reference code
# Reference code for cleaning duplicate names
# dedupe_album_names <- function(df, album_name_col = 'album_name', album_release_year_col = 'album_release_year') {
#
#     album_dupe_regex <- '(deluxe|international|anniversary|version|edition|remaster|re-master|live|mono|stereo)'
#
#     base_album_names <- df %>%
#         mutate_('album_name_' = album_name_col,
#                 'album_release_year_' = album_release_year_col) %>%
#         dplyr::filter(!duplicated(tolower(album_name_))) %>%
#         mutate(base_album_name = gsub(str_glue(' \\(.*{album_dupe_regex}.*\\)'), '', tolower(album_name_)),
#                base_album_name = gsub(str_glue(' \\[.*{album_dupe_regex}.*\\]'), '', base_album_name),
#                base_album_name = gsub(str_glue(':.*{album_dupe_regex}.*'), '', base_album_name),
#                base_album_name = gsub(str_glue(' - .*{album_dupe_regex}.*'), '', base_album_name),
#                base_album = tolower(album_name_) == base_album_name) %>%
#         group_by(base_album_name) %>%
#         dplyr::filter((album_release_year_ == min(album_release_year_)) | base_album) %>%
#         mutate(num_albums = n(),
#                num_base_albums = sum(base_album)) %>%
#         ungroup() %>%
#         dplyr::filter((base_album == 1) |((num_base_albums == 0 | num_base_albums > 1) & row_number() == 1)) %>%
#         pull(album_name_)
#
#     df %>%
#         mutate_('album_name_' = album_name_col) %>%
#         filter(album_name_ %in% base_album_names) %>%
#         select(-album_name_)
# }


