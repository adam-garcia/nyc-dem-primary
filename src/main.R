#   ____________________________________________________________________________
#   main.R                                                                  ####
#   Organization script

# Setup
import::from(magrittr, "%>%")
# Load utilities
c("src/scrape.R", "src/output.R") %>%
  here::here() %>%
  purrr::walk(source)
# Scrape

# Output
