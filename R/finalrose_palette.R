#Functions and code for the final rose color palette

finalrose_colors <- c(
 `Dark red` = "#7a001d",
 `Crimson` = "#b0072c",
 `Poppy red` = "#eb4833",
 `Salmon` = "#ff8170",
 `Light peach` = "#e2b5a2",
 `Shamrock` = "#67a282",
 `Pine tree` = "#457359",
 `Forest green` = "#285339",
 `Deep green` = "#214540")


#' Title
#'
#' @param ... Character names of finalrose_colors
#'
#' @return
#' @export
#'
#' @examples
finalrose_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (finalrose_colors)

  finalrose_colors[cols]
}


#creating individual palettes
finalrose_palettes <- list(
  `full` = finalrose_colors,
  `main` = finalrose_cols("Dark red", "Salmon", "Shamrock", "Forest green"),
  `warm` = finalrose_cols("Dark red", "Crimson", "Poppy red", "Salmon", "Light peach"),
  `cool` = finalrose_cols("Shamrock", "Pine tree", "Forest green", "Deep green")
)




#' Title
#'
#' @param palette Character name of palette in finalrose_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @return
#' @export
#'
#' @examples
finalrose_pal <- function(palette = "full", reverse = FALSE, ...) {
  pal <- finalrose_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}





#' scale_color_finalrose
#'
#' @param palette Character name of palette in finalrose_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @return
#' @export
#'
#' @examples
scale_color_finalrose <- function(palette = "full", discrete = TRUE, reverse = FALSE, ...) {
  pal <- finalrose_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("finalrose_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}



#' scale_fill_finalrose
#'
#' @param palette Character name of palette in finalrose_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @return
#' @export
#'
#' @examples
scale_fill_finalrose <- function(palette = "full", discrete = TRUE, reverse = FALSE, ...) {
  pal <- finalrose_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("finalrose_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



