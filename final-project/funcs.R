View <- function(x, title) {
  is_rstudio <- requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()

  # Match original behaviour of setting title both in RStudio and in RGui
  if (missing(title)) {
    title <- paste(deparse(substitute(x))[1])
    if (!is_rstudio) title <- paste("Data:", title)
  }

  if (inherits(x, "sf")) x <- sf::st_set_geometry(x, NULL)

  if (is_rstudio) {
    .rs.viewHook(x = x, title = title)
  } else {
    utils::View(x, title = title)
  }

  invisible(NULL)
}