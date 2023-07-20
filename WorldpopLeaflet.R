# WorldPop Age-Sex Leaflet Mapping
# Andrew Zimmer
# Jul 18 2023

# load packages ####
library(tidyverse)
library(leaflet)
library(sf)

# load data ####

#ghs-ucdb polygons
ucdb_poly <- st_read("/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Population Pyramid/Urban Polygons/UCDB/ghs_ucdb.shp")

#demographic data
change_data <- read.csv("/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Paper 1 - Dem Var/Exported Datasets/worldpop_migration.csv")
change_data <- change_data %>% mutate(across(where(is.numeric), round, 2))
change_data$net_migration <- trunc(change_data$net_migration)

# leaflet legend function ####
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins   
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}

# setup leaflet colors ####
pal1 <- colorNumeric(
  palette = "viridis",
  domain = change_data$young_dependency_ratio,
  na.color = NA)

pal2 <- colorNumeric(
  palette = "viridis",
  domain = change_data$old_dependency_ratio,
  na.color = NA)

pal3 <- colorNumeric(
  palette = "viridis",
  domain = change_data$dependency_ratio,
  na.color = NA)

pal4 <- colorNumeric(
  palette = "viridis",
  domain = change_data$dependency_ratio_delta,
  na.color = NA)

pal5 <- colorNumeric(
  palette = "viridis",
  domain = change_data$working_sex_ratio,
  na.color = NA)

pal6 <- colorNumeric(
  palette = "viridis",
  domain = change_data$fertility_rate,
  na.color = NA)

pal7 <- colorNumeric(
  palette = "viridis",
  domain = change_data$working_sex_ratio_delta,
  na.color = NA)

pal8 <- colorNumeric(
  palette = "viridis",
  domain = change_data$fertility_rate_delta,
  na.color = NA)

pal9 <- colorNumeric(
  palette = "viridis",
  domain = change_data$net_migration,
  na.color = NA)

palHeat <- colorNumeric(
  palette = "YlOrRd", 
  values(wbgt_30_trend),
  na.color = NA)

palFlood <- colorNumeric(
  palette = "Blues", 
  values(jrc_data),
  na.color = NA)

palLUC <- colorNumeric(
  palette = "Spectral",
  values(lcluc_glad_sv),
  na.color = NA)

# run leaflet map ####
leaflet(change_data) %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  
  addPolygons(data=ucdb_poly, weight = 2, col = "white", fillOpacity = 0.2, fillColor = "grey50", group = "Urban Polygons") %>%
  
  # addRasterImage(wbgt_30_trend, opacity = 0.9, colors = palHeat, group = "Heat Data") %>%
  # addRasterImage(jrc_data, opacity = 0.9, colors = palFlood, group = "Flood Data") %>%
  # addRasterImage(lcluc_glad_sv, opacity = 0.9, colors = palLUC, group = "LCLUC Data") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 Young Dependency Ratio: </strong>", prettyNum(`young_dependency_ratio`, big.mark = ","), "<br>"),
                   color = ~pal1(`young_dependency_ratio`),
                   radius = ~2,
                   group = "2020 Young Dependency Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 Old Dependency Ratio: </strong>", prettyNum(`old_dependency_ratio`, big.mark = ","), "<br>"),
                   color = ~pal2(`old_dependency_ratio`),
                   radius = ~2,
                   group = "2020 Old Dependency Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 Total Dependency Ratio: </strong>", prettyNum(`dependency_ratio`, big.mark = ","), "<br>"),
                   color = ~pal3(`dependency_ratio`),
                   radius = ~2,
                   group = "2020 Total Dependency Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> Change in Dependency Ratio (2000-2020): </strong>", prettyNum(`dependency_ratio_delta`, big.mark = ","), "<br>"),
                   color = ~pal4(`dependency_ratio_delta`),
                   radius = ~2,
                   group = "Change in Dependency Ratio (2000-2020)") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 Working-Age Sex-Ratio: </strong>", prettyNum(`working_sex_ratio`, big.mark = ","), "<br>"),
                   color = ~pal5(`working_sex_ratio`),
                   radius = ~2,
                   group = "2020 Working-Age Sex-Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 Fertility Rate: </strong>", prettyNum(`fertility_rate`, big.mark = ","), "<br>"),
                   color = ~pal6(`fertility_rate`),
                   radius = ~2,
                   group = "2020 Fertility Rate") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> Change in Working-Age Sex-Ratio (2000-2020): </strong>", prettyNum(`working_sex_ratio_delta`, big.mark = ","), "<br>"),
                   color = ~pal7(`working_sex_ratio_delta`),
                   radius = ~2,
                   group = "Change in Working-Age Sex-Ratio (2000-2020)") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> Change in Fertility Rate (2000-2020): </strong>", prettyNum(`fertility_rate_delta`, big.mark = ","), "<br>"),
                   color = ~pal8(`fertility_rate_delta`),
                   radius = ~2,
                   group = "Change in Fertility Rate (2000-2020)") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> Net Migration of Working-Age People (2010-2020): </strong>", prettyNum(`net_migration`, big.mark = ","), "<br>"),
                   color = ~pal9(`net_migration`),
                   radius = ~2,
                   group = "Net Migration of Working-Age People (2010-2020)") %>%
  
  
  addLegend_decreasing(group = "2020 Young Dependency Ratio", position = "bottomleft", labels = "2020 Young Dependency Ratio", pal = pal1, values = ~`young_dependency_ratio`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2020 Old Dependency Ratio", position = "bottomleft", labels = "2020 Old Dependency Ratio", pal= pal2, values = ~`old_dependency_ratio`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2020 Total Dependency Ratio", position = "bottomleft", labels = "2020 Total Dependency Ratio", pal= pal3, values = ~dependency_ratio, decreasing = TRUE) %>%
  addLegend_decreasing(group = "Change in Dependency Ratio (2000-2020)", position = "bottomleft", labels = "Change in Dependency Ratio (2000-2020)", pal= pal4, values = ~dependency_ratio_delta, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2020 Working-Age Sex-Ratio", position = "bottomleft", labels = "2020 Working-Age Sex-Ratio", pal= pal5, values = ~working_sex_ratio, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2020 Fertility Rate", position = "bottomleft", labels = "2020 Fertility Rate", pal= pal6, values = ~fertility_rate, decreasing = TRUE) %>%
  addLegend_decreasing(group = "Change in Working-Age Sex-Ratio (2000-2020)", position = "bottomleft", labels = "Change in Working-Age Sex-Ratio (2000-2020)", pal= pal7, values = ~working_sex_ratio_delta, decreasing = TRUE) %>%
  addLegend_decreasing(group = "Change in Fertility Rate (2000-2020)", position = "bottomleft", labels = "Change in Fertility Rate (2000-2020)", pal= pal8, values = ~fertility_rate_delta, decreasing = TRUE) %>%
  addLegend_decreasing(group = "Net Migration of Working-Age People (2010-2020)", position = "bottomleft", labels = "Net Migration of Working-Age People (2010-2020)", pal= pal9, values = ~net_migration, decreasing = TRUE) %>%
  #  addLegend_decreasing(group = "Heat Data", pal = palHeat, values = values(wbgt_30_trend), title = "WBGT 30C TREND (extra days)", decreasing = TRUE, position = "bottomleft") %>%
  #  addLegend_decreasing(group = "Flood Data", pal = palFlood, values = values(jrc_data), title = "20 YR FLOOD RISK (m)", decreasing = TRUE, position = "bottomleft") %>%
  #  addLegend_decreasing(group = "LCLUC Data", pal = palLUC, values = values(lcluc_glad_sv), title = "SV % Change", decreasing = TRUE, position = "bottomleft") %>%
  
  
  addLayersControl(overlayGroups = c("Urban Polygons",
                                     "2020 Young Dependency Ratio", "2020 Old Dependency Ratio", "2020 Total Dependency Ratio",
                                     "Change in Dependency Ratio (2000-2020)", "2020 Working-Age Sex-Ratio", "2020 Fertility Rate",
                                     "Change in Working-Age Sex-Ratio (2000-2020)", "Change in Fertility Rate (2000-2020)", "Net Migration of Working-Age People (2010-2020)"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  hideGroup("2020 Young Dependency Ratio") %>%
  hideGroup("2020 Old Dependency Ratio") %>%
  hideGroup("Change in Dependency Ratio (2000-2020)") %>%
  hideGroup("2020 Working-Age Sex-Ratio") %>%
  hideGroup("2020 Fertility Rate") %>%
  hideGroup("Change in Working-Age Sex-Ratio (2000-2020)") %>%
  hideGroup("Change in Fertility Rate (2000-2020)") %>%
  hideGroup("Net Migration of Working-Age People (2010-2020)")
