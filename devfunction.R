# update documentation
devtools::document()

# update website
pkgdown::build_site()
pkgdown::preview_site()

# Package import and dependencies
usethis::use_package("tidyverse", type = "depends")

imports <- c("tools", "readxl")
purrr::walk(imports, usethis::use_package)
