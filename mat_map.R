library(sf)
library(tidyverse)
library(terra)
library(ggplot2)
library(ggpattern)
library(ggspatial)
library(rnaturalearth)
library(exactextractr)

# 1. Load and process data ---------------------------------------------------
setwd("temperature_data")

# Temperature rasters
years <- 2007:2017
temp_stack <- rast(lapply(paste0("1kmtmp", years, ".tif"), rast))
mean_temp <- app(temp_stack, fun = mean, na.rm = TRUE)

# Karst line strings
karst_lines <- st_read("/mnt/d/main/download3.geojson", quiet = TRUE) %>% 
  st_make_valid() %>%
  st_buffer(1000) %>%  # Create narrow polygons
  st_union() %>%      # Combine all features
  st_as_sf()

# China provinces
china <- ne_states(country = "China", returnclass = "sf") %>%
  st_make_valid()

# Point data
point_data <- read_csv("/mnt/d/main/regression_matrix.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  group_by(geometry) %>%
  summarize(MAT = mean(MAT, na.rm = TRUE)) %>%
  st_make_valid()

# 2. Project data ------------------------------------------------------------
target_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=30 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Find provinces with karst
provinces_with_karst <- china %>%
  st_filter(karst_lines, .predicate = st_intersects) %>%
  st_transform(target_crs)

karst_proj <- st_transform(karst_lines, target_crs)
points_proj <- st_transform(point_data, target_crs)

# Process temperature data
temp_proj <- project(mean_temp, target_crs)
temp_crop <- crop(temp_proj, vect(provinces_with_karst))
temp_df <- as.data.frame(temp_crop, xy = TRUE, na.rm = TRUE) %>%
  rename(Temperature = mean)

# 3. Create the map ---------------------------------------------------------
custom_palette <- c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7")

final_map <- ggplot() +
  # Temperature background
  geom_tile(data = temp_df, aes(x = x, y = y, fill = Temperature), alpha = 0.9) +
  
  # Point data (semi-transparent)
  geom_sf(data = points_proj, aes(color = MAT), 
          size = 2, alpha = 0.6, shape = 16) +
  
  # Karst areas with subtle pattern
  geom_sf_pattern(
    data = karst_proj,
    pattern = 'stripe',
    pattern_fill = "#003049",
    pattern_color = alpha("#003049", 0.3),  # More transparent
    pattern_spacing = 0.015,
    pattern_angle = 45,
    pattern_density = 0.15,  # More sparse
    fill = alpha("#003049", 0.05),  # Very light fill
    color = alpha("#003049", 0.3),  # Light boundary
    linewidth = 0.2
  ) +
  
  # Province boundaries
  geom_sf(data = provinces_with_karst, fill = NA, color = "gray30", linewidth = 0.5) +
  
  # Color scales
  scale_fill_gradientn(
    name = "Mean Temperature (°C)\n(2007-2017)",
    colors = custom_palette,
    na.value = NA
  ) +
  #scale_color_gradientn(
    #name = "Station MAT (°C)",
    #colors = custom_palette,
    #guide = guide_colorbar(order = 1)
  #) +
  
  # Map elements
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tr", 
    which_north = "true",
    style = north_arrow_minimal()
  ) +
  
  coord_sf(crs = target_crs) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# 4. Save outputs -----------------------------------------------------------
ggsave("temperature_map.tiff", final_map, 
       width = 10, height = 8, dpi = 600, device = "tiff", compression = "lzw")
ggsave("temperature_map.pdf", final_map, 
       width = 10, height = 8, device = "pdf")

print(final_map)











library(sf)
library(tidyverse)
library(terra)
library(ggplot2)
library(ggpattern)
library(ggspatial)
library(rnaturalearth)
library(exactextractr)
library(lwgeom) # For advanced geometry operations

# 1. Load and process data ---------------------------------------------------
setwd("temperature_data")

# Temperature rasters
years <- 2007:2017
temp_stack <- rast(lapply(paste0("1kmtmp", years, ".tif"), rast))
mean_temp <- app(temp_stack, fun = mean, na.rm = TRUE)

# Karst line strings - convert to valid polygons
karst_lines <- st_read("/mnt/d/main/download3.geojson", quiet = TRUE) %>%
  st_make_valid() %>%
  # Convert lines to polygons by creating buffers
  st_buffer(1000) %>%
  # Clean geometries
  st_make_valid() %>%
  # Combine overlapping polygons
  st_union() %>%
  # Convert back to sf object
  st_as_sf() %>%
  # Fix any remaining geometry issues
  st_make_valid() %>%
  # Simplify if needed
  st_simplify(preserveTopology = TRUE, dTolerance = 100)

# China provinces with geometry repair
china <- ne_states(country = "China", returnclass = "sf") %>%
  st_make_valid() %>%
  # Clean geometries more thoroughly
  st_buffer(0) %>%
  st_make_valid()

# Point data
point_data <- read_csv("/mnt/d/main/regression_matrix.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  group_by(geometry) %>%
  summarize(MAT = mean(MAT, na.rm = TRUE)) %>%
  st_make_valid()

# 2. Project data with robust error handling ---------------------------------
target_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=30 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Find provinces with karst (with geometry validation)
provinces_with_karst <- china %>%
  st_make_valid() %>%
  st_filter(karst_lines %>% st_make_valid(), .predicate = st_intersects) %>%
  # Additional geometry cleaning
  st_buffer(0) %>%
  st_make_valid() %>%
  st_transform(target_crs)

# Transform other layers
karst_proj <- karst_lines %>%
  st_make_valid() %>%
  st_transform(target_crs)

points_proj <- point_data %>%
  st_make_valid() %>%
  st_transform(target_crs)

# Process temperature data
temp_proj <- project(mean_temp, target_crs)
temp_crop <- crop(temp_proj, vect(provinces_with_karst))
temp_mask <- mask(temp_crop, vect(provinces_with_karst))
temp_df <- as.data.frame(temp_mask, xy = TRUE, na.rm = TRUE) %>%
  rename(Temperature = mean)

# 3. Create the map with proper layering ------------------------------------
custom_palette <- c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7")

final_map <- ggplot() +
  # Base temperature layer
  geom_tile(data = temp_df, aes(x = x, y = y, fill = Temperature), alpha = 0.85) +
  
  # Point data layer
  geom_sf(data = points_proj, aes(color = MAT), 
          size = 2.5, alpha = 0.7, shape = 19) +
  
  # Very subtle karst pattern layer
  geom_sf_pattern(
    data = karst_proj,
    pattern = 'stripe',
    pattern_fill = alpha("#003049", 0.2),
    pattern_color = alpha("#003049", 0.2),
    pattern_spacing = 0.02,
    pattern_angle = 45,
    pattern_density = 0.1,
    fill = alpha("#003049", 0.03),
    color = alpha("#003049", 0.15),
    linewidth = 0.15
  ) +
  
  # Province boundaries
  geom_sf(data = provinces_with_karst, fill = NA, color = "gray30", linewidth = 0.4) +
  
  # Color scales
  scale_fill_gradientn(
    name = "Mean Temperature (°C)\n2007-2017",
    colors = custom_palette,
    na.value = NA
  ) +
  scale_color_gradientn(
    name = "Station MAT (°C)",
    colors = custom_palette,
    guide = guide_colorbar(order = 1)
  ) +
  
  # Map elements
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(
    location = "tr", 
    which_north = "true",
    style = north_arrow_minimal()
  ) +
  
  coord_sf(crs = target_crs) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.8, "cm"),
    legend.title = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.margin = margin(0, 10, 0, 0)
  )

# 4. Save outputs -----------------------------------------------------------
ggsave("temperature_map.tiff", final_map, 
       width = 10, height = 8, dpi = 600, device = "tiff", compression = "lzw")
ggsave("temperature_map.pdf", final_map, 
       width = 10, height = 8, device = "pdf")

print(final_map)















library(sf)
library(tidyverse)
library(terra)
library(ggplot2)
library(ggpattern)
library(ggspatial)
library(rnaturalearth)
library(lwgeom)

# 1. Load and process data ---------------------------------------------------
setwd("temperature_data")

# Temperature rasters
years <- 2007:2017
temp_stack <- rast(lapply(paste0("1kmtmp", years, ".tif"), rast))
mean_temp <- app(temp_stack, fun = mean, na.rm = TRUE)

# Load and convert karst LineStrings to proper Polygons
karst_lines <- st_read("/mnt/d/main/download3.geojson", quiet = TRUE) %>%
  st_make_valid() %>%
  # Convert lines to polygons by creating buffers
  st_buffer(1000) %>%
  # Clean geometries
  st_make_valid() %>%
  # Combine overlapping polygons
  st_union() %>%
  # Convert back to sf object
  st_as_sf() %>%
  # Fix any remaining geometry issues
  st_make_valid() %>%
  # Simplify if needed
  st_simplify(preserveTopology = TRUE, dTolerance = 100)

# China provinces
china <- ne_states(country = "China", returnclass = "sf") %>%
  st_make_valid()

# Point data (no legend needed)
point_data <- read_csv("/mnt/d/main/regression_matrix.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  group_by(geometry) %>%
  summarize(MAT = mean(MAT, na.rm = TRUE)) %>%
  st_make_valid()

# 2. Project data ------------------------------------------------------------
target_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=30 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Find provinces with karst
provinces_with_karst <- china %>%
  st_filter(karst_lines, .predicate = st_intersects) %>%
  st_transform(target_crs)

karst_proj <- st_transform(karst_lines, target_crs)
points_proj <- st_transform(point_data, target_crs)

# Process temperature data
temp_proj <- project(mean_temp, target_crs)
temp_crop <- crop(temp_proj, vect(provinces_with_karst))
temp_df <- as.data.frame(temp_crop, xy = TRUE, na.rm = TRUE) %>%
  rename(Temperature = mean)

# 3. Create the map with proper layering ------------------------------------
custom_palette <- c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7")

final_map <- ggplot() +
  # Temperature background
  geom_tile(data = temp_df, aes(x = x, y = y, fill = Temperature), alpha = 0.9) +
  
  # Point data (no legend)
  geom_sf(data = points_proj, aes(color = MAT), 
          size = 2, alpha = 0.7, shape = 16, show.legend = FALSE) +
  scale_color_gradientn(colors = custom_palette) +
  
  # Karst polygons with visible fill and thin slanted lines
  geom_sf_pattern(
    data = karst_proj,
    pattern = 'stripe',
    pattern_fill = "#003049",
    pattern_color = "#003049",
    pattern_spacing = 0.01,
    pattern_angle = 45,
    pattern_density = 0.3,
    fill = alpha("#003049", 0.15),  # Noticeable but transparent fill
    color = NA,  # No border
    linewidth = 0.05  # Very thin lines
  ) +
  
  # Province boundaries
  geom_sf(data = provinces_with_karst, fill = NA, color = "gray30", linewidth = 0.4) +
  
  # Color scale for temperature only
  scale_fill_gradientn(
    name = "Mean Temperature (°C)\n2007-2017",
    colors = custom_palette,
    na.value = NA
  ) +
  
  # Map elements
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(
    location = "tr", 
    which_north = "true",
    style = north_arrow_minimal()
  ) +
  
  coord_sf(crs = target_crs) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.8, "cm"),
    legend.title = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# 4. Save outputs -----------------------------------------------------------
ggsave("temperature_map.tiff", final_map, 
       width = 10, height = 8, dpi = 600, device = "tiff", compression = "lzw")
ggsave("temperature_map.pdf", final_map, 
       width = 10, height = 8, device = "pdf")

print(final_map)


















#The one I'm using
library(sf)
library(tidyverse)
library(terra)
library(ggplot2)
library(ggpattern)
library(ggspatial)
library(rnaturalearth)
library(exactextractr)
library(lwgeom) # For advanced geometry operations

# 1. Load and process data ---------------------------------------------------
setwd("temperature_data")

# Temperature rasters
years <- 2007:2017
temp_stack <- rast(lapply(paste0("1kmtmp", years, ".tif"), rast))
mean_temp <- app(temp_stack, fun = mean, na.rm = TRUE)

# Karst line strings - convert to valid polygons
karst_lines <- st_read("/mnt/d/main/download3.geojson", quiet = TRUE) %>%
  st_make_valid() %>%
  # Convert lines to polygons by creating buffers
  st_buffer(1000) %>%
  # Clean geometries
  st_make_valid() %>%
  # Combine overlapping polygons
  st_union() %>%
  # Convert back to sf object
  st_as_sf() %>%
  # Fix any remaining geometry issues
  st_make_valid() %>%
  # Simplify if needed
  st_simplify(preserveTopology = TRUE, dTolerance = 100)

# China provinces with geometry repair
china <- ne_states(country = "China", returnclass = "sf") %>%
  st_make_valid() %>%
  # Clean geometries more thoroughly
  st_buffer(0) %>%
  st_make_valid()

# Point data
point_data <- read_csv("/mnt/d/main/regression_matrix.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  group_by(geometry) %>%
  summarize(MAT = mean(MAT, na.rm = TRUE)) %>%
  st_make_valid()

# 2. Project data with robust error handling ---------------------------------
target_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=30 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Find provinces with karst (with geometry validation)
provinces_with_karst <- china %>%
  st_make_valid() %>%
  st_filter(karst_lines %>% st_make_valid(), .predicate = st_intersects) %>%
  # Additional geometry cleaning
  st_buffer(0) %>%
  st_make_valid() %>%
  st_transform(target_crs)

# Transform other layers
karst_proj <- karst_lines %>%
  st_make_valid() %>%
  st_transform(target_crs)

points_proj <- point_data %>%
  st_make_valid() %>%
  st_transform(target_crs)

# Process temperature data
temp_proj <- project(mean_temp, target_crs)
temp_crop <- crop(temp_proj, vect(provinces_with_karst))
temp_mask <- mask(temp_crop, vect(provinces_with_karst))
temp_df <- as.data.frame(temp_mask, xy = TRUE, na.rm = TRUE) %>%
  rename(Temperature = mean)

# 3. Create the map with proper layering ------------------------------------
custom_palette <- c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7")

final_map <- ggplot() +
  # Base temperature layer
  geom_tile(data = temp_df, aes(x = x, y = y, fill = Temperature), alpha = 0.85) +
  
  # Point data layer
  geom_sf(data = points_proj, aes(color = MAT), 
          size = 2.5, alpha = 0.7, shape = 19) +
  
  # Very subtle karst pattern layer
  geom_sf_pattern(
    data = karst_proj,
    pattern = 'stripe',
    pattern_fill = alpha("#003049", 0.2),
    pattern_color = alpha("#003049", 0.2),
    pattern_spacing = 0.02,
    pattern_angle = 45,
    pattern_density = 0.1,
    fill = alpha("#003049", 0.15),
    color = alpha("#003049", 0.15),
    linewidth = 0.15
  ) +
  
  # Province boundaries
  geom_sf(data = provinces_with_karst, fill = NA, color = "gray30", linewidth = 0.15) +
  
  # Color scales
  scale_fill_gradientn(
    name = "Mean Temperature (°C)\n2007-2017",
    colors = custom_palette,
    na.value = NA
  ) +
  scale_color_gradientn(
    name = "Station MAT (°C)",
    colors = custom_palette,
    guide = guide_colorbar(order = 1)
  ) +
  
  # Map elements
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(
    location = "tr", 
    which_north = "true",
    style = north_arrow_minimal()
  ) +
  
  coord_sf(crs = target_crs) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.8, "cm"),
    legend.title = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.margin = margin(0, 10, 0, 0)
  )

# 4. Save outputs -----------------------------------------------------------
ggsave("temperature_map.tiff", final_map, 
       width = 10, height = 8, dpi = 600, device = "tiff", compression = "lzw")
ggsave("temperature_map.pdf", final_map, 
       width = 10, height = 8, device = "pdf")

print(final_map)














# Last one I used
library(sf)
library(tidyverse)
library(terra)
library(ggplot2)
library(ggpattern)
library(ggspatial)
library(rnaturalearth)
library(exactextractr)

# 1. Load and process temperature data
setwd("temperature_data")
years <- 2007:2017
temp_stack <- rast(lapply(paste0("1kmtmp", years, ".tif"), rast))
mean_temp <- app(temp_stack, fun = mean, na.rm = TRUE)

# 2. Load and process karst line strings
karst_lines <- st_read("/mnt/d/main/download3.geojson", quiet = TRUE) %>% 
  st_make_valid()

# Convert LineStrings to Polygons by closing them
karst_polygons <- karst_lines %>%
  group_by(grp = ceiling(row_number()/10)) %>%  # Group nearby lines
  summarise() %>%
  st_cast("POLYGON") %>%
  st_make_valid() %>%
  st_buffer(dist = 1000)  # Small buffer to ensure proper polygons

# 3. Load China provinces and isolate only those with karst
china <- ne_states(country = "China", returnclass = "sf") %>%
  st_make_valid()

# Find provinces intersecting karst areas
provinces_with_karst <- china %>%
  st_filter(karst_polygons, .predicate = st_intersects)

# 4. Project all data to Albers Equal Area for China
target_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=30 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

provinces_proj <- st_transform(provinces_with_karst, target_crs)
karst_proj <- st_transform(karst_polygons, target_crs)

# 5. Process temperature data for the target area
temp_proj <- project(mean_temp, target_crs)
temp_crop <- crop(temp_proj, vect(provinces_proj))
temp_mask <- mask(temp_crop, vect(provinces_proj))

# Convert to dataframe for plotting
temp_df <- as.data.frame(temp_mask, xy = TRUE, na.rm = TRUE) %>%
  rename(Temperature = mean)

# 6. Create the final map with proper styling
custom_palette <- c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7")

final_map <- ggplot() +
  # 1. Temperature background (primary fill scale)
  geom_tile(data = temp_df, aes(x = x, y = y, fill = Temperature)) +
  scale_fill_gradientn(name = "Mean Annual Temperature (°C)\n(2007-2017)",
                       colors = custom_palette,
                       na.value = NA) +

  # 2. Province boundaries
  geom_sf(data = provinces_proj, fill = NA, color = "gray30", linewidth = 0.5) +

  # 3. Karst areas with pattern
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

  # 4. Karst boundaries (subtle)
  geom_sf(data = karst_proj, fill = NA, color = alpha("#ef233c", 0.15), linewidth = 0.1) +

  # 5. Map elements
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_minimal()) +

  coord_sf(crs = target_crs) +

  # 6. Dummy layer for Karst Forest pattern legend
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
        size = 4  # controls key box size, relative to text
      )
    )
  ) +

  # 7. Theme and legend adjustments
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

  # 8. Legend order
  guides(
    fill = guide_colorbar(order = 1),
    pattern = guide_legend(order = 2)
  )

# Save outputs
ggsave("temperature_map_final.tiff", final_map, width = 10, height = 8, dpi = 600, 
       device = "tiff", compression = "lzw")
ggsave("temperature_map_final.pdf", final_map, width = 10, height = 8, device = "pdf")

print(final_map)







