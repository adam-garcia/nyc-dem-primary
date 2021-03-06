#   ____________________________________________________________________________
#   main.R                                                                  ####
#   Organization script

##  ............................................................................
#   Setup                                                                   ####
import::from(magrittr, "%>%")
# Boro-level results, table links to AD results which links to ED results
mayor_url <- "https://web.enrboenyc.us/CD24306ADI0.html"
# Load utilities
c(
  "src/scrape/extract_geography_links.R",
  "src/scrape/extract_geography_data.R",
  "src/output.R"
) %>%
  here::here() %>%
  purrr::walk(source)
##  ............................................................................
#   Scrape                                                                  ####
# Get borough urls from landing page
boro_urls <- extract_geography_links(mayor_url, "boro")
# Get assembly district urls from boro pages
ad_urls <- boro_urls %>%
  dplyr::mutate(
    ad_data = purrr::map(boro_url, extract_geography_links, "ad")
  ) %>%
  tidyr::unnest(ad_data)
# Get election district data from assembly district pages
ed_data <- ad_urls %>%
  dplyr::mutate(
    ed_data = purrr::map(ad_url, extract_geography_data, "ed")
  ) %>%
  tidyr::unnest(cols = c(ed_data)) %>%
  dplyr::mutate(
    elect_dist = sprintf(
      "%02d%03d",
      ad %>%
        stringr::str_extract("\\d+") %>%
        readr::parse_number(),
      ed %>%
        stringr::str_extract("\\d+") %>%
        readr::parse_number()
      )
  ) %>%
  dplyr::group_by(boro, ad, ed) %>%
  dplyr::mutate(
    votes_pct = votes / sum(votes, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    date_retrieved = lubridate::now()
  )

# Summarize up level-by-level for boro- and district-level data
# Assembly district
ad_data <- ed_data %>%
  dplyr::group_by(boro, ad, candidate) %>%
  dplyr::filter(boro != "Total", ad != "Total") %>%
  dplyr::summarize(
    votes = sum(votes, na.rm = TRUE),
    date_retrieved = head(date_retrieved, 1),
    .groups = "drop"
  ) %>%
  dplyr::group_by(boro, ad) %>%
  dplyr::mutate(
    votes_pct = votes / sum(votes, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()
# Borough
boro_data <- ad_data %>%
  dplyr::group_by(boro, candidate) %>%
  dplyr::summarize(
    votes = sum(votes, na.rm = TRUE),
    date_retrieved = head(date_retrieved, 1),
    .groups = "drop"
  ) %>%
  dplyr::group_by(boro) %>%
  dplyr::mutate(
    votes_pct = votes / sum(votes, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()
##  ............................................................................
#   Output                                                                  ####
tibble::lst(
  boro = boro_data,
  ad = ad_data,
  ed = ed_data
) %>%
  purrr::iwalk(~ {
    csv_out <- glue::glue("out/{.y}_round_01.csv") %>%
      here::here()
    readr::write_csv(.x, csv_out)
  })
