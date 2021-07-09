#' Extract data from BOE tables
#'
#' @param geo_url The geography url; string
#' @param geo_name The geography name (e.g., "ed", "ad")
extract_geography_data <- function(geo_url, geo_name) {
  geo_req <- httr::GET(geo_url)
  if (httr::status_code(geo_req) %in% c(200, 201)) {
    geo_list <- httr::content(geo_req, "text", encoding = "UTF-8") %>%
      xml2::read_html() %>%
      rvest::html_nodes("table table table tr") %>%
      purrr::imap(~ {
        # DoE does some weird stuff with table spacing, they add blank columns
        data_cols <- rvest::html_nodes(.x, "td:nth-child(even)")
        # Header row
        if (.y == 1) {
          candidates <- data_cols %>%
            rvest::html_text() %>%
            .[-1L]
          tibble::lst(candidates)
          # candidates
        } else if (.y > 2) {
          geo <- rvest::html_nodes(.x, "td:nth-child(1)") %>%
            rvest::html_text() %>%
            stringr::str_squish()
          pct_reporting <- data_cols[1] %>%
            rvest::html_text()
          votes <- data_cols[-1] %>%
            rvest::html_text()
          tibble::lst(geo, pct_reporting, votes)
        }
      }) %>%
      purrr::compact()
    geo_table <- tibble::tibble(
      geo = purrr::map_chr(geo_list[-1], "geo"),
      pct_reporting = purrr::map_chr(geo_list[-1], "pct_reporting"),
      candidate = purrr::map(geo_list[1], "candidates"),
      votes = purrr::map(geo_list[-1], "votes") %>%
        purrr::map(readr::parse_number)
    ) %>%
      tidyr::unnest(c(candidate, votes))
    names(geo_table) <- names(geo_table) %>%
      stringr::str_replace("geo", geo_name)
    geo_table
  }
}
