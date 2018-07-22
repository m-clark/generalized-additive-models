better_corr = function (data, mapping, alignPercent = 0.6, method = "pearson",
                        use = "complete.obs", corAlignPercent = NULL, corMethod = NULL,
                        corUse = NULL, ...)
{
  # for now this is just ggally_cor but gets rid of the ridiculous gridlines and "Corr:"
  require(GGally)
  if (!is.null(corAlignPercent)) {
    stop("'corAlignPercent' is deprecated.  Please use argument 'alignPercent'")
  }
  if (!is.null(corMethod)) {
    stop("'corMethod' is deprecated.  Please use argument 'method'")
  }
  if (!is.null(corUse)) {
    stop("'corUse' is deprecated.  Please use argument 'use'")
  }
  useOptions <- c("all.obs", "complete.obs", "pairwise.complete.obs",
                  "everything", "na.or.complete")
  use <- pmatch(use, useOptions)
  if (is.na(use)) {
    warning("correlation 'use' not found.  Using default value of 'all.obs'")
    use <- useOptions[1]
  }
  else {
    use <- useOptions[use]
  }
  cor_fn <- function(x, y) {
    cor(x, y, method = method, use = use)
  }
  xCol <- deparse(mapping$x)
  yCol <- deparse(mapping$y)
  if (GGally:::is_date(data[[xCol]]) || GGally:::is_date(data[[yCol]])) {
    if (!identical(class(data), "data.frame")) {
      data <- fix_data(data)
    }
    for (col in c(xCol, yCol)) {
      if (GGally:::is_date(data[[col]])) {
        data[[col]] <- as.numeric(data[[col]])
      }
    }
  }
  if (is.numeric(GGally:::eval_data_col(data, mapping$colour))) {
    stop("ggally_cor: mapping color column must be categorical, not numeric")
  }
  colorCol <- deparse(mapping$colour)
  singleColorCol <- ifelse(is.null(colorCol), NULL, paste(colorCol, collapse = ""))
  if (use %in% c("complete.obs", "pairwise.complete.obs", "na.or.complete")) {
    if (length(colorCol) > 0) {
      if (singleColorCol %in% colnames(data)) {
        rows <- complete.cases(data[c(xCol, yCol, colorCol)])
      }
      else {
        rows <- complete.cases(data[c(xCol, yCol)])
      }
    }
    else {
      rows <- complete.cases(data[c(xCol, yCol)])
    }
    if (any(!rows)) {
      total <- sum(!rows)
      if (total > 1) {
        warning("Removed ", total, " rows containing missing values")
      }
      else if (total == 1) {
        warning("Removing 1 row that contained a missing value")
      }
    }
    data <- data[rows, ]
  }
  xVal <- data[[xCol]]
  yVal <- data[[yCol]]
  if (length(names(mapping)) > 0) {
    for (i in length(names(mapping)):1) {
      tmp_map_val <- deparse(mapping[names(mapping)[i]][[1]])
      if (tmp_map_val[length(tmp_map_val)] %in% colnames(data))
        mapping[[names(mapping)[i]]] <- NULL
      if (length(names(mapping)) < 1) {
        mapping <- NULL
        break
      }
    }
  }
  if (length(colorCol) < 1) {
    colorCol <- "ggally_NO_EXIST"
  }
  if ((singleColorCol != "ggally_NO_EXIST") && (singleColorCol %in%
                                                colnames(data))) {
    cord <- ddply(data, c(colorCol), function(x) {
      cor_fn(x[[xCol]], x[[yCol]])
    })
    colnames(cord)[2] <- "ggally_cor"
    cord$ggally_cor <- signif(as.numeric(cord$ggally_cor),
                              3)
    lev <- levels(data[[colorCol]])
    ord <- rep(-1, nrow(cord))
    for (i in 1:nrow(cord)) {
      for (j in seq_along(lev)) {
        if (identical(as.character(cord[i, colorCol]),
                      as.character(lev[j]))) {
          ord[i] <- j
        }
      }
    }
    cord <- cord[order(ord[ord >= 0]), ]
    cord$label <- str_c(cord[[colorCol]], ": ", cord$ggally_cor)
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 *
                  (xmax - xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 *
                  (ymax - ymin))
    p <- ggally_text(label = str_c("Cor : ", signif(cor_fn(xVal,
                                                           yVal), 3)), mapping = mapping, xP = 0.5, yP = 0.9,
                     xrange = xrange, yrange = yrange, color = "black",
                     ...) + theme(legend.position = "none")
    xPos <- rep(alignPercent, nrow(cord)) * diff(xrange) +
      min(xrange, na.rm = TRUE)
    yPos <- seq(from = 0.9, to = 0.2, length.out = nrow(cord) +
                  1)
    yPos <- yPos * diff(yrange) + min(yrange, na.rm = TRUE)
    yPos <- yPos[-1]
    cordf <- data.frame(xPos = xPos, yPos = yPos, labelp = cord$label)
    cordf$labelp <- factor(cordf$labelp, levels = cordf$labelp)
    p <- p +
      geom_text(data = cordf, aes(x = xPos, y = yPos, label = labelp, color = labelp), hjust = 1, ...) +
      theme(legend.position = "none",
            panel.grid.major = element_blank())

    p
  }
  else {
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 * (xmax - xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 * (ymax - ymin))
    cval = cor_fn(xVal, yVal)
    p <- ggally_text(label = paste(signif(cval, 3), sep = "", collapse = ""),
                     mapping,
                     xP = 0.5,
                     color=I(ifelse(cval>0,
                                    scales::alpha(scales::muted("blue"), alpha=abs(cval)),
                                    scales::alpha(scales::muted("red"), alpha=abs(cval)))),
                     yP = 0.5,
                     xrange = xrange,
                     yrange = yrange, ...) +
      theme(legend.position = "none",
            panel.grid.major = element_blank())
    p
  }
}