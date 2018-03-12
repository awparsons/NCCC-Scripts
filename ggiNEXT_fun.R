ggiNEXT.iNEXT<-function (x, type = 1, se = TRUE, facet.var = "none", color.var = "site", 
                         grey = FALSE) 
{
  TYPE <- c(1, 2, 3)
  SPLIT <- c("none", "order", "site", "both")
  if (is.na(pmatch(type, TYPE)) | pmatch(type, TYPE) == -1) 
    stop("invalid plot type")
  if (is.na(pmatch(facet.var, SPLIT)) | pmatch(facet.var, SPLIT) == 
      -1) 
    stop("invalid facet variable")
  if (is.na(pmatch(color.var, SPLIT)) | pmatch(color.var, SPLIT) == 
      -1) 
    stop("invalid color variable")
  type <- pmatch(type, 1:3)
  facet.var <- match.arg(facet.var, SPLIT)
  color.var <- match.arg(color.var, SPLIT)
  if (facet.var == "order") 
    color.var <- "site"
  if (facet.var == "site") 
    color.var <- "order"
  options(warn = -1)
  z <- fortify(x, type = type)
  options(warn = 0)
  if (ncol(z) == 7) {
    se <- FALSE
  }
  datatype <- unique(z$datatype)
  if (color.var == "none") {
    if (levels(factor(z$order)) > 1 & "site" %in% names(z)) {
      warning("invalid color.var setting, the iNEXT object consists multiple sites and orders, change setting as both")
      color.var <- "both"
      z$col <- paste(z$site, z$order, sep = "-")
    }
    else if ("site" %in% names(z)) {
      warning("invalid color.var setting, the iNEXT object consists multiple orders, change setting as order")
      color.var <- "site"
      z$col <- z$site
    }
    else if (levels(factor(z$order)) > 1) {
      warning("invalid color.var setting, the iNEXT object consists multiple sites, change setting as site")
      color.var <- "order"
      z$col <- factor(z$order)
    }
    else {
      z$col <- rep(1, nrow(z))
    }
  }
  else if (color.var == "order") {
    z$col <- factor(z$order)
  }
  else if (color.var == "site") {
    if (!"site" %in% names(z)) {
      warning("invalid color.var setting, the iNEXT object do not consist multiple sites, change setting as order")
      z$col <- factor(z$order)
    }
    z$col <- z$site
  }
  else if (color.var == "both") {
    if (!"site" %in% names(z)) {
      warning("invalid color.var setting, the iNEXT object do not consist multiple sites, change setting as order")
      z$col <- factor(z$order)
    }
    z$col <- paste(z$site, z$order, sep = "-")
  }
  z$lty <- factor(z$method, levels=unique(c("interpolated", "observed", "extrapolated"), 
                                          c("interpolation", "interpolation", "extrapolation")))
  z$col <- factor(z$col)
  data.sub <- z[which(z$method == "observed"), ]
  g <- ggplot(z, aes_string(x = "x", y = "y", colour = "col"))
  g <- g + geom_line(lwd = 1.5) + 
    guides(linetype = guide_legend(title = "Method"), colour = guide_legend(title = "Guides"), 
           fill = guide_legend(title = "Guides")) + 
    theme(legend.position = "bottom", legend.title = element_blank(), 
          text = element_text(size = 18))
  if (type == 2L) {
    g <- g + labs(x = "Number of sampling units", y = "Sample coverage")
    if (datatype == "abundance") 
      g <- g + labs(x = "Number of individuals", y = "Sample coverage")
  }
  else if (type == 3L) {
    g <- g + labs(x = "Sample coverage", y = "Species diversity")
  }
  else {
    g <- g + labs(x = "Number of sampling units", y = "Species diversity")
    if (datatype == "abundance") 
      g <- g + labs(x = "Number of individuals", y = "Species diversity")
  }
  if (se) 
    g <- g + geom_ribbon(aes_string(ymin = "y.lwr", ymax = "y.upr", 
                                    fill = "factor(col)", colour = "NULL"), alpha = 0.2)
  if (facet.var == "order") {
    if (length(levels(factor(z$order))) == 1 & type != 2) {
      warning("invalid facet.var setting, the iNEXT object do not consist multiple orders.")
    }
    else {
      g <- g + facet_wrap(~order, nrow = 1)
      if (color.var == "both") {
        g <- g + guides(colour = guide_legend(title = "Guides", 
                                              ncol = length(levels(factor(z$order))), byrow = TRUE), 
                        fill = guide_legend(title = "Guides"))
      }
    }
  }
  if (facet.var == "site") {
    if (!"site" %in% names(z)) {
      warning("invalid facet.var setting, the iNEXT object do not consist multiple sites.")
    }
    else {
      g <- g + facet_wrap(~site, nrow = 1)
      if (color.var == "both") {
        g <- g + guides(colour = guide_legend(title = "Guides", 
                                              nrow = length(levels(factor(z$order)))), fill = guide_legend(title = "Guides"))
      }
    }
  }
  if (facet.var == "both") {
    if (length(levels(factor(z$order))) == 1 | !"site" %in% 
        names(z)) {
      warning("invalid facet.var setting, the iNEXT object do not consist multiple sites or orders.")
    }
    else {
      g <- g + facet_wrap(site ~ order)
      if (color.var == "both") {
        g <- g + guides(colour = guide_legend(title = "Guides", 
                                              nrow = length(levels(factor(z$site))), byrow = TRUE), 
                        fill = guide_legend(title = "Guides"))
      }
    }
  }
  if (grey) {
    g <- g + theme_bw(base_size = 18) + scale_fill_grey(start = 0, 
                                                        end = 0.4) + scale_colour_grey(start = 0.2, end = 0.2) + 
      guides(linetype = guide_legend(title = "Method"), 
             colour = guide_legend(title = "Guides"), fill = guide_legend(title = "Guides")) +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  g <- g + theme(legend.box = "vertical")
  return(g)
}

