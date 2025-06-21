library(sf)
library(tidyverse)
library(terra)
library(ggplot2)
library(ggpattern)
library(ggspatial)
library(rnaturalearth)
library(exactextractr)

# 1. Load and process temperature rasters
library(terra)

# Set working directory where temp files are located
setwd("temperature_data")

# Load all yearly rasters from 2007 to 2017
years <- 2007:2017
temp_files <- paste0("1kmtmp", years, ".tif")

# Load and stack all files
temp_stack <- rast(temp_files)

# Check layer count
if (nlyr(temp_stack) != length(years)) {
  stop("Mismatch between raster layers and years. Check file names.")
}

# Compute year-on-year warming (difference between consecutive years)
warming_rasters <- list()
for (i in 1:(nlyr(temp_stack) - 1)) {
  warming_rasters[[i]] <- temp_stack[[i + 1]] - temp_stack[[i]]
}

# Stack all warming rasters and compute mean warming
warming_stack <- rast(warming_rasters)
mean_warming <- app(warming_stack, fun = mean, na.rm = TRUE)

# Reproject mean warming raster to Albers Equal Area




# 3. Load karst lines and process them
karst_lines <- st_read("/mnt/d/main/download3.geojson", quiet = TRUE) %>% 
  st_make_valid()

karst_polygons <- karst_lines %>%
  group_by(grp = ceiling(row_number()/10)) %>%  # Group nearby lines
  summarise() %>%
  st_cast("POLYGON") %>%
  st_make_valid() %>%
  st_buffer(dist = 1000)  # Close gaps to form polygons

# 4. Load China provinces and isolate karst provinces
china <- ne_states(country = "China", returnclass = "sf") %>%
  st_make_valid()
provinces_with_karst <- china %>%
  st_filter(karst_polygons, .predicate = st_intersects)

# 5. Reproject everything to Albers Equal Area CRS
target_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=30 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
provinces_proj <- st_transform(provinces_with_karst, target_crs)
karst_proj <- st_transform(karst_polygons, target_crs)

# 6. Reproject raster, crop and mask it
warming_proj <- project(mean_warming, target_crs)

warming_crop <- crop(warming_proj, vect(provinces_proj))
warming_mask <- mask(warming_crop, vect(provinces_proj))
warming_df <- as.data.frame(warming_mask, xy = TRUE, na.rm = TRUE) %>%
  rename(MeanWarming = mean)

# 8. Create final map
custom_palette <- c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7")

final_map <- ggplot() +
  geom_tile(data = warming_df, aes(x = x, y = y, fill = MeanWarming)) +
  scale_fill_gradientn(name = "Mean Annual Warming (°C/year)\n(2007–2017)",
                       colors = custom_palette,
                       na.value = NA) +
  geom_sf(data = provinces_proj, fill = NA, color = "gray30", linewidth = 0.5) +
  ggpattern::geom_sf_pattern(
    data = karst_proj,
    pattern = 'stripe',
    pattern_fill = "#ef233c",
    pattern_color = "#ef233c",
    pattern_spacing = 0.008,
    pattern_angle = 45,
    pattern_density = 0.25,
    pattern_size = 0.2,
    fill = NA,
    color = NA
  ) +
  geom_sf(data = karst_proj, fill = NA, color = alpha("#ef233c", 0.15), linewidth = 0.1) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_minimal()) +
  coord_sf(crs = target_crs) +
  ggpattern::geom_rect_pattern(
    data = data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1, type = "Karst Forest"),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, pattern = type),
    inherit.aes = FALSE,
    fill = "white",
    color = NA,
    pattern_fill = "#ef233c",
    pattern_color = "#ef233c",
    pattern_spacing = 0.008,
    pattern_angle = 45,
    pattern_density = 0.25,
    pattern_size = 0.2
  ) +
  scale_pattern_manual(
    name = NULL,
    values = c("Karst Forest" = "stripe"),
    guide = guide_legend(
      title = NULL,
      override.aes = list(
        pattern = "stripe",
        pattern_spacing = 0.008,
        pattern_angle = 45,
        pattern_size = 0.2,
        pattern_density = 0.25,
        pattern_color = "#ef233c",
        pattern_fill = "#ef233c",
        size = 4
      )
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.spacing.y = unit(0.5, "cm")
  ) +
  guides(
    fill = guide_colorbar(order = 1),
    pattern = guide_legend(order = 2)
  )

# 9. Save outputs
ggsave("mean_warming_map.tiff", final_map, width = 10, height = 8, dpi = 600, device = "tiff", compression = "lzw")
ggsave("mean_warming_map.pdf", final_map, width = 10, height = 8, device = "pdf")

print(final_map)














# soil warming
library(sf)
library(tidyverse)
library(terra)
library(ggplot2)
library(ggpattern)
library(ggspatial)
library(rnaturalearth)
library(lubridate)
library(exactextractr)

# 1. Load NetCDF file and extract monthly soil temperature
soil_rast <- rast("data_stream-moda.nc")

# Extract layer names and decode timestamps
layer_names <- names(soil_rast)
valid_times <- as.numeric(gsub("stl1_valid_time=", "", layer_names))
dates <- as_datetime(valid_times, origin = "1970-01-01")
years <- year(dates)

# Only keep data between 2007 and 2017
target_years <- 2007:2017
idx_keep <- which(years %in% target_years)
soil_rast_filtered <- soil_rast[[idx_keep]]
dates_filtered <- dates[idx_keep]
years_filtered <- years[idx_keep]

# Aggregate to annual mean soil temperature
soil_annual <- lapply(target_years, function(y) {
  idx <- which(years_filtered == y)
  app(soil_rast_filtered[[idx]], mean, na.rm = TRUE)
})
soil_stack <- rast(soil_annual)
names(soil_stack) <- as.character(target_years)

# Compute year-on-year warming in °C (difference between consecutive years)
warming_rasters <- list()
for (i in 1:(length(target_years) - 1)) {
  warming_rasters[[i]] <- soil_stack[[i + 1]] - soil_stack[[i]]
}
warming_stack <- rast(warming_rasters)

# Compute mean annual soil warming (°C/year)
mean_warming <- app(warming_stack, mean, na.rm = TRUE)

# 2. Load and process karst polygons
karst_lines <- st_read("/mnt/d/main/download3.geojson", quiet = TRUE) %>% 
  st_make_valid()

karst_polygons <- karst_lines %>%
  group_by(grp = ceiling(row_number() / 10)) %>%
  summarise() %>%
  st_cast("POLYGON") %>%
  st_make_valid() %>%
  st_buffer(dist = 1000)

# 3. Load China provinces and isolate karst areas
china <- ne_states(country = "China", returnclass = "sf") %>%
  st_make_valid()
provinces_with_karst <- st_filter(china, karst_polygons, .predicate = st_intersects)

# 4. Reproject all spatial data to Albers Equal Area
target_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=30 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
provinces_proj <- st_transform(provinces_with_karst, target_crs)
karst_proj <- st_transform(karst_polygons, target_crs)
warming_proj <- project(mean_warming, target_crs)

# 5. Crop, mask, and convert raster to dataframe
warming_crop <- crop(warming_proj, vect(provinces_proj))
warming_mask <- mask(warming_crop, vect(provinces_proj))
warming_df <- as.data.frame(warming_mask, xy = TRUE, na.rm = TRUE) %>%
  rename(MeanWarming = mean)

# 6. Plot the final soil warming map
custom_palette <- c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7")

final_map <- ggplot() +
  geom_tile(data = warming_df, aes(x = x, y = y, fill = MeanWarming)) +
  scale_fill_gradientn(name = "Mean Annual Soil Warming (°C/year)\n(2007–2017)",
                       colors = custom_palette,
                       na.value = NA) +
  geom_sf(data = provinces_proj, fill = NA, color = "gray30", linewidth = 0.5) +
  ggpattern::geom_sf_pattern(
    data = karst_proj,
    pattern = 'stripe',
    pattern_fill = "#ef233c",
    pattern_color = "#ef233c",
    pattern_spacing = 0.008,
    pattern_angle = 45,
    pattern_density = 0.25,
    pattern_size = 0.2,
    fill = NA,
    color = NA
  ) +
  geom_sf(data = karst_proj, fill = NA, color = alpha("#ef233c", 0.15), linewidth = 0.1) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_minimal()) +
  coord_sf(crs = target_crs) +
  ggpattern::geom_rect_pattern(
    data = data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1, type = "Karst Forest"),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, pattern = type),
    inherit.aes = FALSE,
    fill = "white",
    color = NA,
    pattern_fill = "#ef233c",
    pattern_color = "#ef233c",
    pattern_spacing = 0.008,
    pattern_angle = 45,
    pattern_density = 0.25,
    pattern_size = 0.2
  ) +
  scale_pattern_manual(
    name = NULL,
    values = c("Karst Forest" = "stripe"),
    guide = guide_legend(
      title = NULL,
      override.aes = list(
        pattern = "stripe",
        pattern_spacing = 0.008,
        pattern_angle = 45,
        pattern_size = 0.2,
        pattern_density = 0.25,
        pattern_color = "#ef233c",
        pattern_fill = "#ef233c",
        size = 4
      )
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.spacing.y = unit(0.5, "cm")
  ) +
  guides(
    fill = guide_colorbar(order = 1),
    pattern = guide_legend(order = 2)
  )

# 7. Save output maps
ggsave("mean_soil_warming_map.tiff", final_map, width = 10, height = 8, dpi = 600, device = "tiff", compression = "lzw")
ggsave("mean_soil_warming_map.pdf", final_map, width = 10, height = 8, device = "pdf")

print(final_map)



















































final_map <- ggplot() +
  geom_tile(data = warming_df, aes(x = x, y = y, fill = MeanWarming)) +
  scale_fill_gradientn(name = "Mean Annual Soil Warming\n(2007–2017)",
                       colors = custom_palette,
                       na.value = NA,
                       breaks = c(0),
                       labels = c("0")
                       ) +
  geom_sf(data = provinces_proj, fill = NA, color = "gray30", linewidth = 0.5) +
  ggpattern::geom_sf_pattern(
    data = karst_proj,
    pattern = 'stripe',
    pattern_fill = "#ef233c",
    pattern_color = "#ef233c",
    pattern_spacing = 0.008,
    pattern_angle = 45,
    pattern_density = 0.25,
    pattern_size = 0.2,
    fill = NA,
    color = NA
  ) +
  geom_sf(data = karst_proj, fill = NA, color = alpha("#ef233c", 0.15), linewidth = 0.1) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_minimal()) +
  coord_sf(crs = target_crs) +
  ggpattern::geom_rect_pattern(
    data = data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1, type = "Karst Forest"),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, pattern = type),
    inherit.aes = FALSE,
    fill = "white",
    color = NA,
    pattern_fill = "#ef233c",
    pattern_color = "#ef233c",
    pattern_spacing = 0.008,
    pattern_angle = 45,
    pattern_density = 0.25,
    pattern_size = 0.2
  ) +
  scale_pattern_manual(
    name = NULL,
    values = c("Karst Forest" = "stripe"),
    guide = guide_legend(
      title = NULL,
      override.aes = list(
        pattern = "stripe",
        pattern_spacing = 0.008,
        pattern_angle = 45,
        pattern_size = 0.2,
        pattern_density = 0.25,
        pattern_color = "#ef233c",
        pattern_fill = "#ef233c",
        size = 4
      )
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.spacing.y = unit(0.5, "cm")
  ) +
  guides(
    fill = guide_colorbar(order = 1),
    pattern = guide_legend(order = 2)
  )




















library(sf)
library(tidyverse)
library(terra)
library(ggplot2)
library(ggpattern)
library(ggspatial)
library(rnaturalearth)
library(lubridate)
library(exactextractr)

# 1. Load NetCDF file and extract monthly soil temperature
soil_rast <- rast("/mnt/d/main/data_stream-moda.nc")

# Extract layer names and decode timestamps
layer_names <- names(soil_rast)
valid_times <- as.numeric(gsub("stl1_valid_time=", "", layer_names))
dates <- as_datetime(valid_times, origin = "1970-01-01")
years <- year(dates)

# Only keep data between 2007 and 2017
target_years <- 2007:2017
idx_keep <- which(years %in% target_years)
soil_rast_filtered <- soil_rast[[idx_keep]]
dates_filtered <- dates[idx_keep]
years_filtered <- years[idx_keep]

# Aggregate to annual mean soil temperature
soil_annual <- lapply(target_years, function(y) {
  idx <- which(years_filtered == y)
  app(soil_rast_filtered[[idx]], mean, na.rm = TRUE)
})
soil_stack <- rast(soil_annual)
names(soil_stack) <- as.character(target_years)

# Compute year-on-year warming in °C (difference between consecutive years)
warming_rasters <- list()
for (i in 1:(length(target_years) - 1)) {
  warming_rasters[[i]] <- soil_stack[[i + 1]] - soil_stack[[i]]
}
warming_stack <- rast(warming_rasters)

# Compute mean annual soil warming (°C/year)
mean_warming <- app(warming_stack, mean, na.rm = TRUE)

# 2. Load and process karst polygons
karst_lines <- st_read("/mnt/d/main/download3.geojson", quiet = TRUE) %>% 
  st_make_valid()

karst_polygons <- karst_lines %>%
  group_by(grp = ceiling(row_number() / 10)) %>%
  summarise() %>%
  st_cast("POLYGON") %>%
  st_make_valid() %>%
  st_buffer(dist = 1000)

# 3. Load China provinces and isolate karst areas
china <- ne_states(country = "China", returnclass = "sf") %>%
  st_make_valid()
provinces_with_karst <- st_filter(china, karst_polygons, .predicate = st_intersects)

# 4. Reproject all spatial data to Albers Equal Area
target_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=30 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
provinces_proj <- st_transform(provinces_with_karst, target_crs)
karst_proj <- st_transform(karst_polygons, target_crs)
warming_proj <- project(mean_warming, target_crs)

# 5. Crop, mask, and convert raster to dataframe
warming_crop <- crop(warming_proj, vect(provinces_proj))
warming_mask <- mask(warming_crop, vect(provinces_proj))
warming_df <- as.data.frame(warming_mask, xy = TRUE, na.rm = TRUE) %>%
  rename(MeanWarming = mean)

# 6. Plot the final soil warming map
custom_palette <- c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7")

final_map <- ggplot() +
  geom_tile(data = warming_df, aes(x = x, y = y, fill = MeanWarming)) +
  scale_fill_gradientn(name = "Mean Annual Soil Warming\n(2007–2017)",
                       colors = custom_palette,
                       na.value = NA,
                       breaks = c(0),
                       labels = c("0")
                       ) +
  geom_sf(data = provinces_proj, fill = NA, color = "gray30", linewidth = 0.5) +
  ggpattern::geom_sf_pattern(
    data = karst_proj,
    pattern = 'stripe',
    pattern_fill = "#ef233c",
    pattern_color = "#ef233c",
    pattern_spacing = 0.008,
    pattern_angle = 45,
    pattern_density = 0.25,
    pattern_size = 0.2,
    fill = NA,
    color = NA
  ) +
  geom_sf(data = karst_proj, fill = NA, color = alpha("#ef233c", 0.15), linewidth = 0.1) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_minimal()) +
  coord_sf(crs = target_crs) +
  ggpattern::geom_rect_pattern(
    data = data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1, type = "Karst Forest"),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, pattern = type),
    inherit.aes = FALSE,
    fill = "white",
    color = NA,
    pattern_fill = "#ef233c",
    pattern_color = "#ef233c",
    pattern_spacing = 0.008,
    pattern_angle = 45,
    pattern_density = 0.25,
    pattern_size = 0.2
  ) +
  scale_pattern_manual(
    name = NULL,
    values = c("Karst Forest" = "stripe"),
    guide = guide_legend(
      title = NULL,
      override.aes = list(
        pattern = "stripe",
        pattern_spacing = 0.008,
        pattern_angle = 45,
        pattern_size = 0.2,
        pattern_density = 0.25,
        pattern_color = "#ef233c",
        pattern_fill = "#ef233c",
        size = 4
      )
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.spacing.y = unit(0.5, "cm")
  ) +
  guides(
    fill = guide_colorbar(order = 1),
    pattern = guide_legend(order = 2)
  )


ggsave("mean_soil_warming_map1.tiff", final_map, width = 10, height = 8, dpi = 600, device = "tiff", compression = "lzw")
ggsave("mean_soil_warming_map1.pdf", final_map, width = 10, height = 8, device = "pdf")

print(final_map)

























library(sf)
library(tidyverse)
library(terra)
library(ggplot2)
library(ggpattern)
library(ggspatial)
library(rnaturalearth)
library(exactextractr)

# 1. Load and process temperature rasters
library(terra)

# Set working directory where temp files are located
setwd("temperature_data")

# Load all yearly rasters from 2007 to 2017
years <- 2007:2017
temp_files <- paste0("1kmtmp", years, ".tif")

# Load and stack all files
temp_stack <- rast(temp_files)

# Check layer count
if (nlyr(temp_stack) != length(years)) {
  stop("Mismatch between raster layers and years. Check file names.")
}

# Compute year-on-year warming (difference between consecutive years)
warming_rasters <- list()
for (i in 1:(nlyr(temp_stack) - 1)) {
  warming_rasters[[i]] <- temp_stack[[i + 1]] - temp_stack[[i]]
}

# Stack all warming rasters and compute mean warming
warming_stack <- rast(warming_rasters)
mean_warming <- app(warming_stack, fun = mean, na.rm = TRUE)

# Reproject mean warming raster to Albers Equal Area




# 3. Load karst lines and process them
karst_lines <- st_read("/mnt/d/main/download3.geojson", quiet = TRUE) %>% 
  st_make_valid()

karst_polygons <- karst_lines %>%
  group_by(grp = ceiling(row_number()/10)) %>%  # Group nearby lines
  summarise() %>%
  st_cast("POLYGON") %>%
  st_make_valid() %>%
  st_buffer(dist = 1000)  # Close gaps to form polygons

# 4. Load China provinces and isolate karst provinces
china <- ne_states(country = "China", returnclass = "sf") %>%
  st_make_valid()
provinces_with_karst <- china %>%
  st_filter(karst_polygons, .predicate = st_intersects)

# 5. Reproject everything to Albers Equal Area CRS
target_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=30 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
provinces_proj <- st_transform(provinces_with_karst, target_crs)
karst_proj <- st_transform(karst_polygons, target_crs)

# 6. Reproject raster, crop and mask it
warming_proj <- project(mean_warming, target_crs)

warming_crop <- crop(warming_proj, vect(provinces_proj))
warming_mask <- mask(warming_crop, vect(provinces_proj))
warming_df <- as.data.frame(warming_mask, xy = TRUE, na.rm = TRUE) %>%
  rename(MeanWarming = mean)

# 8. Create final map
custom_palette <- c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7")

final_map <- ggplot() +
  geom_tile(data = warming_df, aes(x = x, y = y, fill = MeanWarming)) +
  scale_fill_gradientn(name = "Mean Annual CW\n(2007–2017)",
                       colors = custom_palette,
                       na.value = NA,
                       breaks = c(0),
                       labels = c("0")
                       ) +
  geom_sf(data = provinces_proj, fill = NA, color = "gray30", linewidth = 0.5) +
  ggpattern::geom_sf_pattern(
    data = karst_proj,
    pattern = 'stripe',
    pattern_fill = "#ef233c",
    pattern_color = "#ef233c",
    pattern_spacing = 0.008,
    pattern_angle = 45,
    pattern_density = 0.25,
    pattern_size = 0.2,
    fill = NA,
    color = NA
  ) +
  geom_sf(data = karst_proj, fill = NA, color = alpha("#ef233c", 0.15), linewidth = 0.1) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_minimal()) +
  coord_sf(crs = target_crs) +
  ggpattern::geom_rect_pattern(
    data = data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1, type = "Karst Forest"),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, pattern = type),
    inherit.aes = FALSE,
    fill = "white",
    color = NA,
    pattern_fill = "#ef233c",
    pattern_color = "#ef233c",
    pattern_spacing = 0.008,
    pattern_angle = 45,
    pattern_density = 0.25,
    pattern_size = 0.2
  ) +
  scale_pattern_manual(
    name = NULL,
    values = c("Karst Forest" = "stripe"),
    guide = guide_legend(
      title = NULL,
      override.aes = list(
        pattern = "stripe",
        pattern_spacing = 0.008,
        pattern_angle = 45,
        pattern_size = 0.2,
        pattern_density = 0.25,
        pattern_color = "#ef233c",
        pattern_fill = "#ef233c",
        size = 4
      )
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.spacing.y = unit(0.5, "cm")
  ) +
  guides(
    fill = guide_colorbar(order = 1),
    pattern = guide_legend(order = 2)
  )

# 9. Save outputs
ggsave("mean_warming_map1.tiff", final_map, width = 10, height = 8, dpi = 600, device = "tiff", compression = "lzw")
ggsave("mean_warming_map1.pdf", final_map, width = 10, height = 8, device = "pdf")

print(final_map)