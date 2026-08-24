fixture_name <- function(...) {
  file.path(...)
}

fixture_source_path <- function(...) {
  file.path(testthat::test_path("..", "..", "inst", "extdata"), ...)
}

fixture_installed_path <- function(...) {
  system.file("extdata", ..., package = "readAOFM")
}

fixture_path <- function(...) {
  source_path <- fixture_source_path(...)
  installed_path <- fixture_installed_path(...)

  if (file.exists(source_path)) {
    return(normalizePath(source_path, mustWork = TRUE))
  }

  if (nzchar(installed_path) && file.exists(installed_path)) {
    return(normalizePath(installed_path, mustWork = TRUE))
  }

  stop(
    sprintf(
      "Fixture '%s' was not found in source or installed extdata.",
      fixture_name(...)
    ),
    call. = FALSE
  )
}
