#usethis::create_package(".")

use_packages <- list(
  Imports = c(
    "dplyr",
    "jsonlite",
    "processx",
    "purrr",
    "rlang",
    "stringr",
    "vctrs"
  ),
  Depends = c(
  ),
  Suggests = c(
  ),
  Enhances = c(
  ),
  LinkingTo = c(
  ),
  SystemRequirements = c(
    "awscli (>= 2)"
  )
)

purrr::walk2(names(use_packages), use_packages, function(type, packages) {
  purrr::walk(packages, usethis::use_package, type = type, min_version = TRUE)
})

usethis::use_mit_license()
usethis::use_testthat()
usethis::use_package_doc()

devtools::document()
#devtools::check(error_on = "error")
