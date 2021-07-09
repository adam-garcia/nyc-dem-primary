#' Extract links to nested geographies from BOE tables
#'
#' @param geo_url The geography geo_url; string
#' @param geo_name The geography name (e.g., "ed", "ad")
extract_geography_links <- function(geo_url, geo_name) {
  xml2::read_html(geo_url) %>%
    rvest::html_nodes(
      "table table table tr:nth-child(n + 3) td:nth-child(1)"
    ) %>%
    purrr::map_df(~ {
      table_links <- tibble::tibble(
        geo = rvest::html_text(.x) %>%
          stringr::str_squish(),
        geo_url = .x %>%
          rvest::html_node("a") %>%
          rvest::html_attr("href") %>%
          paste0("https://web.enrboenyc.us/", .)
      )
      names(table_links) <- names(table_links) %>%
        stringr::str_replace("geo", geo_name)
      table_links
  })
}
