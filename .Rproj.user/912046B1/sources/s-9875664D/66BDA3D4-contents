#Functions and code for the first flower color palette

firstflower_colors <- c(
 `Carmine red` = "#ff4772",
 `Medium pink` = "#fa89a2",
 `Blush pink` = "#feaeca",
 `Pale pink` = "#eed3d6",
 `Honeydew` = "#e8f4e1",
 `Mint green` = "#b2edc5",
 `Turquoise green` = "#a5d1b4",
 `Sap green` = "#71a989",
 `Peacock green` = "#428a5f")



#' Title
#'
#' @param ... Character names of firstflower_colors
#'
#' @return
#' @export
#'
#' @examples
firstflower_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (firstflower_colors)

  firstflower_colors[cols]
}


#creating individual palettes
firstflower_palettes <- list(
  `full` = firstflower_colors,
  `main` = firstflower_cols("Carmine red", "Pale pink", "Honeydew", "Peacock green"),
  `warm` = firstflower_cols("Carmine red", "Medium pink", "Blush pink", "Pale pink"),
  `cool` = firstflower_cols("Honeydew", "Mint green", "Turquoise green", "Sap green", "Peacock green")
)




#' Title
#'
#' @param palette Character name of palette in firstflower_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @return
#' @export
#'
#' @examples
firstflower_pal <- function(palette = "full", reverse = FALSE, ...) {
  pal <- firstflower_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}





#' scale_color_firstflower
#'
#' @param palette Character name of palette in firstflower_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @return
#' @export
#'
#' @examples
scale_color_firstflower <- function(palette = "full", discrete = TRUE, reverse = FALSE, ...) {
  pal <- firstflower_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("firstflower_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}



#' scale_fill_firstflower
#'
#' @param palette Character name of palette in firstflower_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @return
#' @export
#'
#' @examples
scale_fill_firstflower <- function(palette = "full", discrete = TRUE, reverse = FALSE, ...) {
  pal <- firstflower_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("firstflower_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



