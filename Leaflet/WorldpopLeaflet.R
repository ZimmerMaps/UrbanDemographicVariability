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
leaflet_data <- read.csv("/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Population Pyramid/Version 2 - From November Onwards/Data/Merged CSVs/sedacData/worldpop_agesex_leaflet_dataset.csv")
leaflet_data <- leaflet_data %>% mutate(across(where(is.numeric), round, 2))

leaflet_data_migration_count <- leaflet_data %>%
  filter(!is.na(total_migration) & total_migration > 0)

leaflet_data_migration_perc <- leaflet_data %>%
  filter(migration_perc > 0 & migration_perc < 100 & total_migration > 0)


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
  domain = leaflet_data$dependency_ratio,
  na.color = NA)

pal2 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$young_dependency_ratio,
  na.color = NA)

pal3 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$old_dependency_ratio,
  na.color = NA)

pal4 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$total_sex_ratio,
  na.color = NA)

pal5 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$young_sex_ratio,
  na.color = NA)

pal6 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$working_sex_ratio,
  na.color = NA)

pal7 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$old_sex_ratio,
  na.color = NA)

pal8 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$birth_rate,
  na.color = NA)

pal9 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$fertility_rate,
  na.color = NA)

pal10 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data_migration_count$total_migration,
  na.color = NA)

pal10 <- colorBin(
  palette = "viridis",
  bins = round(seq(0, sqrt(18000000), length.out = 8)^2, 1), 
  domain = leaflet_data_migration_count$total_migration)

pal11 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data_migration_perc$migration_perc,
  na.color = NA)

pal12 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$dependency_ratio_delta,
  na.color = NA)

pal13 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$young_dependency_ratio_delta,
  na.color = NA)

pal14 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$old_dependency_ratio_delta,
  na.color = NA)

pal15 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$total_sex_ratio_delta,
  na.color = NA)

pal16 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$young_sex_ratio_delta,
  na.color = NA)

pal17 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$working_sex_ratio_delta,
  na.color = NA)

pal18 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$old_sex_ratio_delta,
  na.color = NA)

pal19 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$birth_rate_delta,
  na.color = NA)

pal20 <- colorNumeric(
  palette = "viridis",
  domain = leaflet_data$fertility_rate_delta,
  na.color = NA)


# run leaflet map ####
leaflet(leaflet_data) %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  
  addPolygons(data=ucdb_poly, weight = 2, col = "black", fillOpacity = 0.1, fillColor = "grey95", group = "Urban Polygons") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 Total Dependency Ratio: </strong>", prettyNum(`dependency_ratio`, big.mark = ","), "<br>"),
                   color = ~pal1(`dependency_ratio`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2020 Total Dependency Ratio") %>%
  
    addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 Young Dependency Ratio: </strong>", prettyNum(`young_dependency_ratio`, big.mark = ","), "<br>"),
                   color = ~pal2(`young_dependency_ratio`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2020 Young Dependency Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 Old Dependency Ratio: </strong>", prettyNum(`old_dependency_ratio`, big.mark = ","), "<br>"),
                   color = ~pal3(`old_dependency_ratio`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2020 Old Dependency Ratio") %>%
  
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 Total Sex Ratio: </strong>", prettyNum(`total_sex_ratio`, big.mark = ","), "<br>"),
                   color = ~pal4(`total_sex_ratio`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2020 Total Sex Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 Young Sex Ratio: </strong>", prettyNum(`young_sex_ratio`, big.mark = ","), "<br>"),
                   color = ~pal5(`young_sex_ratio`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2020 Young Sex Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 Working Sex Ratio: </strong>", prettyNum(`working_sex_ratio`, big.mark = ","), "<br>"),
                   color = ~pal6(`working_sex_ratio`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2020 Working Sex Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 Old Sex Ratio: </strong>", prettyNum(`old_sex_ratio`, big.mark = ","), "<br>"),
                   color = ~pal7(`old_sex_ratio`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2020 Old Sex Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 Birth Rate: </strong>", prettyNum(`birth_rate`, big.mark = ","), "<br>"),
                   color = ~pal8(`birth_rate`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2020 Birth Rate") %>%
  
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2020 General Fertility Rate: </strong>", prettyNum(`fertility_rate`, big.mark = ","), "<br>"),
                   color = ~pal9(`fertility_rate`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2020 General Fertility Rate") %>%
  
  addCircleMarkers(data = leaflet_data_migration_count, lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2000-2020 Total In-Migration: </strong>", prettyNum(`total_migration`, big.mark = ","), "<br>"),
                   color = ~pal10(`total_migration`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2000-2020 Total In-Migration") %>%
  
  addCircleMarkers(data = leaflet_data_migration_perc, lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2000-2020 Migration (% of pop.): </strong>", prettyNum(`migration_perc`, big.mark = ","), "<br>"),
                   color = ~pal11(`migration_perc`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2000-2020 Migration (% of pop.)") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2000-2020 Δ Total D-Ratio: </strong>", prettyNum(`dependency_ratio_delta`, big.mark = ","), "<br>"),
                   color = ~pal12(`dependency_ratio_delta`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2000-2020 Δ Total D-Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2000-2020 Δ Young D-Ratio: </strong>", prettyNum(`young_dependency_ratio_delta`, big.mark = ","), "<br>"),
                   color = ~pal13(`young_dependency_ratio_delta`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2000-2020 Δ Young D-Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2000-2020 Δ Old D-Ratio: </strong>", prettyNum(`old_dependency_ratio_delta`, big.mark = ","), "<br>"),
                   color = ~pal14(`old_dependency_ratio_delta`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2000-2020 Δ Old D-Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2000-2020 Δ Total Sex Ratio: </strong>", prettyNum(`total_sex_ratio_delta`, big.mark = ","), "<br>"),
                   color = ~pal15(`total_sex_ratio_delta`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2000-2020 Δ Total Sex Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2000-2020 Δ Young Sex Ratio: </strong>", prettyNum(`young_sex_ratio_delta`, big.mark = ","), "<br>"),
                   color = ~pal16(`young_sex_ratio_delta`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2000-2020 Δ Young Sex Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2000-2020 Δ Working Sex Ratio: </strong>", prettyNum(`working_sex_ratio_delta`, big.mark = ","), "<br>"),
                   color = ~pal17(`working_sex_ratio_delta`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2000-2020 Δ Working Sex Ratio") %>%

  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2000-2020 Δ Old Sex Ratio: </strong>", prettyNum(`old_sex_ratio_delta`, big.mark = ","), "<br>"),
                   color = ~pal18(`old_sex_ratio_delta`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2000-2020 Δ Old Sex Ratio") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2000-2020 Δ Birth Rate: </strong>", prettyNum(`birth_rate_delta`, big.mark = ","), "<br>"),
                   color = ~pal19(`birth_rate_delta`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2000-2020 Δ Birth Rate") %>%
  
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   popup = ~paste("<strong> City Name: </strong>", city_name, "<br>",
                                  "<strong> 2020 Total Population: </strong>", prettyNum(total_pop, big.mark = ","), "<br>",
                                  "<strong> 2000-2020 Δ Gen. Fertility Rate: </strong>", prettyNum(`fertility_rate_delta`, big.mark = ","), "<br>"),
                   color = ~pal20(`fertility_rate_delta`),
                   radius = ~6,
                   stroke = TRUE,
                   fillOpacity = 0.8,
                   group = "2000-2020 Δ Gen. Fertility Rate") %>%
  
  
  addLegend_decreasing(group = "2020 Total Dependency Ratio", position = "bottomleft", labels = "2020 Total Dependency Ratio", pal = pal1, values = ~`dependency_ratio`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2020 Young Dependency Ratio", position = "bottomleft", labels = "2020 Young Dependency Ratio", pal = pal2, values = ~`young_dependency_ratio`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2020 Old Dependency Ratio", position = "bottomleft", labels = "2020 Old Dependency Ratio", pal = pal3, values = ~`old_dependency_ratio`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2020 Total Sex Ratio", position = "bottomleft", labels = "2020 Total Sex Ratio", pal = pal4, values = ~`total_sex_ratio`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2020 Young Sex Ratio", position = "bottomleft", labels = "2020 Young Sex Ratio", pal = pal5, values = ~`young_sex_ratio`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2020 Working Sex Ratio", position = "bottomleft", labels = "2020 Working Sex Ratio", pal = pal6, values = ~`working_sex_ratio`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2020 Old Sex Ratio", position = "bottomleft", labels = "2020 Old Sex Ratio", pal = pal7, values = ~`old_sex_ratio`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2020 Birth Rate", position = "bottomleft", labels = "2020 Birth Rate", pal = pal8, values = ~`birth_rate`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2020 General Fertility Rate", position = "bottomleft", labels = "2020 General Fertility Rate", pal = pal9, values = ~`fertility_rate`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2000-2020 Total In-Migration", position = "bottomleft", labels = "2000-2020 Total In-Migration", pal = pal10, values = ~`total_migration`, decreasing = TRUE, data = leaflet_data_migration_count) %>%
  addLegend_decreasing(group = "2000-2020 Migration (% of pop.)", position = "bottomleft", labels = "2000-2020 Migration (% of pop.)", pal = pal11, values = ~`migration_perc`, decreasing = TRUE, data = leaflet_data_migration_perc) %>%
  addLegend_decreasing(group = "2000-2020 Δ Total D-Ratio", position = "bottomleft", labels = "2000-2020 Δ Total D-Ratio", pal = pal12, values = ~`dependency_ratio_delta`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2000-2020 Δ Young D-Ratio", position = "bottomleft", labels = "2000-2020 Δ Young D-Ratio", pal = pal13, values = ~`young_dependency_ratio_delta`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2000-2020 Δ Old D-Ratio", position = "bottomleft", labels = "2000-2020 Δ Old D-Ratio", pal = pal14, values = ~`old_dependency_ratio_delta`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2000-2020 Δ Total Sex Ratio", position = "bottomleft", labels = "2000-2020 Δ Total Sex Ratio", pal = pal15, values = ~`total_sex_ratio_delta`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2000-2020 Δ Young Sex Ratio", position = "bottomleft", labels = "2000-2020 Δ Young Sex Ratio", pal = pal16, values = ~`young_sex_ratio_delta`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2000-2020 Δ Working Sex Ratio", position = "bottomleft", labels = "2000-2020 Δ Working Sex Ratio", pal = pal17, values = ~`working_sex_ratio_delta`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2000-2020 Δ Old Sex Ratio", position = "bottomleft", labels = "2000-2020 Δ Old Sex Ratio", pal = pal18, values = ~`old_sex_ratio_delta`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2000-2020 Δ Birth Rate", position = "bottomleft", labels = "2000-2020 Δ Birth Rate", pal = pal19, values = ~`birth_rate_delta`, decreasing = TRUE) %>%
  addLegend_decreasing(group = "2000-2020 Δ Gen. Fertility Rate", position = "bottomleft", labels = "2000-2020 Δ Gen. Fertility Rate", pal = pal20, values = ~`fertility_rate_delta`, decreasing = TRUE) %>%
  
  addLayersControl(overlayGroups = c("Urban Polygons",
                                     "2020 Total Dependency Ratio", 
                                     "2020 Young Dependency Ratio", 
                                     "2020 Old Dependency Ratio",
                                     "2020 Total Sex Ratio",
                                     "2020 Young Sex Ratio",
                                     "2020 Working Sex Ratio",
                                     "2020 Old Sex Ratio",
                                     "2020 Birth Rate",
                                     "2020 General Fertility Rate",
                                     "2000-2020 Total In-Migration",
                                     "2000-2020 Migration (% of pop.)",
                                     "2000-2020 Δ Total D-Ratio",
                                     "2000-2020 Δ Young D-Ratio",
                                     "2000-2020 Δ Old D-Ratio",
                                     "2000-2020 Δ Total Sex Ratio",
                                     "2000-2020 Δ Young Sex Ratio",
                                     "2000-2020 Δ Working Sex Ratio",
                                     "2000-2020 Δ Old Sex Ratio",
                                     "2000-2020 Δ Birth Rate",
                                     "2000-2020 Δ Gen. Fertility Rate"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  hideGroup("Urban Polygons") %>%
  hideGroup("2020 Young Dependency Ratio") %>%
  hideGroup("2020 Old Dependency Ratio") %>%
  hideGroup("2020 Total Sex Ratio") %>%
  hideGroup("2020 Young Sex Ratio") %>%
  hideGroup("2020 Working Sex Ratio") %>%
  hideGroup("2020 Old Sex Ratio") %>%
  hideGroup("2020 Birth Rate") %>%
  hideGroup("2020 General Fertility Rate") %>%
  hideGroup("2000-2020 Total In-Migration") %>%
  hideGroup("2000-2020 Migration (% of pop.)") %>%
  hideGroup("2000-2020 Δ Total D-Ratio") %>%
  hideGroup("2000-2020 Δ Young D-Ratio") %>%
  hideGroup("2000-2020 Δ Old D-Ratio") %>%
  hideGroup("2000-2020 Δ Total Sex Ratio") %>%
  hideGroup("2000-2020 Δ Young Sex Ratio") %>%
  hideGroup("2000-2020 Δ Working Sex Ratio") %>%
  hideGroup("2000-2020 Δ Old Sex Ratio") %>%
  hideGroup("2000-2020 Δ Birth Rate") %>%
  hideGroup("2000-2020 Δ Gen. Fertility Rate")




