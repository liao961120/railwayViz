% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{pl_choropleth}
\alias{pl_choropleth}
\title{Plot Choropleth Map with County Level Data}
\usage{
pl_choropleth(.data, index, palette = c("RdBu", "Spectral"),
  theme = NULL, print = TRUE)
}
\description{
Plot Choropleth Map with County Level Data
}
\examples{
\dontrun{
compare_nye_normal \%>\%
  filter(year == 2018) \%>\%
  pl_choropleth(index = urban_idx, palette = 'Spectral')
}
}
