#' @importFrom rlang !!
#' @export
pl_choropleth <- function(.data, index, palette = c("RdBu", "Spectral"), theme = NULL, print = TRUE) {
  palette <- tmaptools::get_brewer_pal(palette[1], n = 4, plot = FALSE)

  index <- rlang::enquo(index)
  pl <- ggplot(data = .data) +
    geom_sf(aes(fill = !!index),
            colour = "#404040", size = 0.1) +
    scale_fill_gradientn(colours = rev(palette),
                             limits=c(-1, 1),
                             trans = sqrt4) +
    scale_x_continuous(limits = c(119.6, 122.3)) +
    scale_y_continuous(limits = c(21.65, 25.45)) + theme

  if (print) print(pl)
  return(pl)
}


#' @importFrom scales trans_new
#' @keywords internal
sqrt4 <- trans_new('sqrt4',
                   function(x) if_else(x > 0, x^(0.25), -(-x)^(0.25)),
                   function(y) if_else(y > 0, y^4, -(-y)^4))
