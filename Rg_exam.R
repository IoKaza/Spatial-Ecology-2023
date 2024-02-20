# Install and load required packages
install.packages(c("readxl", "ggplot2", "maps", "mapdata", "rnaturalearth", "patchwork"))

# Load libraries
library(readxl)
library(ggplot2)
library(maps)
library(mapdata)
library(rnaturalearth)
library(patchwork)
library(gridExtra)
# Read the Excel file
My_data <- read_xlsx("data/useme.xlsx")

# Create a dataframe with only the info needed
Rg <- My_data[, c('gbifID', 'decimalLatitude', 'decimalLongitude', 'year')]

# Keep only rows without missing values in specific columns
Rg <- Rg[complete.cases(Rg[, c('gbifID', 'decimalLatitude', 'decimalLongitude', 'year')]), ]

# Remove duplicate rows
Rg <- unique(Rg)

# Check for missing values in specific columns
missing_values <- any(is.na(Rg[, c('gbifID', 'decimalLatitude', 'decimalLongitude', 'year')]))

if (missing_values) {
  cat("There are still missing values in the specified columns.\n")
} else {
  cat("No missing values in the specified columns.\n")
}

# Display summary statistics of the columns we need
summary(Rg[c('decimalLatitude', 'decimalLongitude', 'year')])

# Identify outliers in 'decimalLatitude'
latitude_outliers <- Rg$decimalLatitude < -90 | Rg$decimalLatitude > 90

# Identify outliers in 'decimalLongitude'
longitude_outliers <- Rg$decimalLongitude < -180 | Rg$decimalLongitude > 180

# Combine the outliers for both latitude and longitude
all_outliers <- latitude_outliers | longitude_outliers

# Print rows with outliers
outliers_data <- Rg[all_outliers, ]
print(outliers_data)

# Plot the world map with distribution points
world_map <- map_data("world")
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, map_id = region),
           fill = "white", color = "black", size = 0.2) +
  geom_point(data = Rg, aes(x = decimalLongitude, y = decimalLatitude), color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Distribution Data on World Map")

# Fetch land polygons
land <- ne_download(scale = 110, category = "physical", type = "land", returnclass = "sf")

# Zoom-in box where most of distribution is visible
bbox <- list(xmin = -50, xmax = 40, ymin = 40, ymax = 90)

# Distribution in the zoomed-in box
ggplot() +
  geom_sf(data = land, fill = "lightgray", color = "black") +
  geom_point(data = Rg, aes(x = decimalLongitude, y = decimalLatitude), color = "red", size = 1) +
  coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax)) +
  theme_minimal() +
  labs(title = "Zoomed-in Distribution Data on World Map")

# Create two time periods and categorize data
period_2000_2009 <- Rg[Rg$year >= 2000 & Rg$year <= 2009, ]
period_2010_2019 <- Rg[Rg$year >= 2010 & Rg$year <= 2019, ]

# Plot distribution data for each time period
plot_2000_2009 <- ggplot() +
  geom_sf(data = land, fill = "lightgray", color = "black") +
  geom_point(data = period_2000_2009, aes(x = decimalLongitude, y = decimalLatitude), color = "blue", size = 1) +
  coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax)) +
  theme_minimal() +
  labs(title = "Distribution Data (2000-2009)")

plot_2010_2019 <- ggplot() +
  geom_sf(data = land, fill = "lightgray", color = "black") +
  geom_point(data = period_2010_2019, aes(x = decimalLongitude, y = decimalLatitude), color = "green", size = 1) +
  coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax)) +
  theme_minimal() +
  labs(title = "Distribution Data (2010-2019)")

# Display the two plots side by side
grid.arrange(plot_2000_2009, plot_2010_2019, ncol = 2)

##Latitudinal distribution during the two periods

# Density plot for the latitudinal distribution in 2000-2009
lat1 <- ggplot(period_2000_2009, aes(x = decimalLatitude)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Latitudinal Distribution (2000-2009)")

# Density plot for the latitudinal distribution in 2010-2019
lat2 <- ggplot(period_2010_2019, aes(x = decimalLatitude)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Latitudinal Distribution (2010-2019)")

# Display the two density plots side by side
grid.arrange(lat1, lat2, ncol = 2)

# Overlay the density plots for latitude
combined_plot_latitude <- lat1 + 
  geom_density(data = period_2010_2019, aes(x = decimalLatitude), fill = "green", alpha = 0.5) +
  labs(title = "Latitudinal Distribution Comparison")

# Display the combined plot
print(combined_plot_latitude)

##Longitudinal distribution during the two periods

# Density plot for the longitudinal distribution in 2000-2009
long1 <- ggplot(period_2000_2009, aes(x = decimalLongitude)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Longitudinal Distribution (2000-2009)")

# Density plot for the longitudinal distribution in 2010-2019
long2 <- ggplot(period_2010_2019, aes(x = decimalLongitude)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Longitudinal Distribution (2010-2019)")

# Display the two density plots side by side
grid.arrange(long1, long2, ncol = 2)

# Overlay the density plots for longitude
combined_plot_longitude <- long1 + 
  geom_density(data = period_2010_2019, aes(x = decimalLongitude), fill = "green", alpha = 0.5) +
  labs(title = "Longitudinal Distribution Comparison")

# Display the combined plot
print(combined_plot_longitude)

# 2D density plot to show the shift between the two distributions
# Create 2D density plot for 2000-2009
plot_2000_2009_density <- ggplot(period_2000_2009, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_density_2d(aes(color = "blue"), size = 0.5) +
  labs(title = "2D Density Plot (2000-2009)")

# Create 2D density plot for 2010-2019
plot_2010_2019_density <- ggplot(period_2010_2019, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_density_2d(aes(color = "green"), size = 0.5) +
  labs(title = "2D Density Plot (2010-2019)")

# Combine the 2D density plots
combined_density_plot <- plot_2000_2009_density + plot_2010_2019_density +
  plot_layout(nrow = 2, byrow = TRUE)

# Display the combined plot
print(combined_density_plot) 
