better_smooth <- function(data, mapping, ptcol, ptalpha=1, ptsize=1, linecol, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(color=ptcol, alpha=ptalpha, size=ptsize) +
    geom_smooth(color=linecol, ...)
  p
}