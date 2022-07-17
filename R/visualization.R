plot_placeholder <- function(text) {
  grid::grid.text(
    text, 0.5, 0.5, gp = grid::gpar(fontsize = 20))
}

plot_subpopulation_heatmap <- function(
  aggregated, show_gene_name = TRUE, ...) {
  stopifnot(is.matrix(aggregated))

  ComplexHeatmap::Heatmap(aggregated,
    row_names_side = "left",
    column_names_side = "top",
    show_row_names = show_gene_name,
    ...
  )
}

# Copy from InteractiveComplexHeatmap
get_pos_from_brush <- function(brush, ratio = 1) {
  coords <- brush$coords_css
  if (is.null(coords)) {
    return(NULL)
  }
  height <- (brush$range$bottom - brush$range$top) / brush$img_css_ratio$y
  pos1 <- grid::unit(c(coords$xmin, height - coords$ymin), "bigpts")
  pos2 <- grid::unit(c(coords$xmax, height - coords$ymax), "bigpts")
  pos1 <- pos1 / ratio
  pos2 <- pos2 / ratio
  list(pos1, pos2)
}

# Copy from InteractiveComplexHeatmap
get_pos_from_click <- function(click, ratio = 1) {
  if (identical(c("x", "y"), names(click))) {
    pos1 <- grid::unit(c(click$x, click$y), "bigpts")
  } else {
    coords <- click$coords_css
    if (is.null(coords)) {
      return(NULL)
    }
    height <- (click$range$bottom - click$range$top) / click$img_css_ratio$y
    pos1 <- grid::unit(c(coords$x, height - coords$y), "bigpts")
  }
  pos1[1] <- pos1[1] / ratio
  pos1[2] <- pos1[2] / ratio
  pos1
}