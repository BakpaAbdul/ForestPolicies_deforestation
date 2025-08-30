# importing manually 
    #load packages 
library(sf)
library(tidyverse)      
library(countrycode) 
library(readxl)


    #path of downloaded wdpa folder 
gdb_path <- '/Users/abdulbaari/Dropbox/Migration Africa Forest Protection/data/WDPA_Jul2025_Public/WDPA_Jul2025_Public.gdb'

    # Get the list of African ISO3 codes 
african_iso3s <- countrycode::codelist |>
  filter(continent == "Africa") |>
  pull(iso3c)

    # Remove any NA values from the list of African ISO3 codes
african_iso3s <- african_iso3s[!is.na(african_iso3s)] 

    # Construct the SQL WHERE clause for filtering by ISO3
country_filter_string <- paste0("'", african_iso3s, "'", collapse = ", ")

    #  select desired columns from the database tables
desired_columns <- c("NAME","ORIG_NAME", "WDPAID", "DESIG_ENG", "IUCN_CAT", "STATUS", "ISO3", "GOV_TYPE","MARINE","REP_AREA")

    #  column names for SQL
columns_to_select_string <- paste0(desired_columns, collapse = ", ")

    # Construct the SQL WHERE clause for filtering by ISO3 for polygons
sql_query_polygons <- paste0("SELECT ", columns_to_select_string , " FROM WDPA_poly_Jul2025 WHERE ISO3 IN (", country_filter_string, ")")

    # Construct the SQL WHERE clause for filtering by ISO3 for points
sql_query_points <- paste0("SELECT ", columns_to_select_string , " FROM WDPA_point_Jul2025 WHERE ISO3 IN (", country_filter_string, ")")


    # Import ONLY the African polygon data using the SQL filter
wdpa_africa_polygons <- read_sf(dsn = gdb_path,
                                layer = "WDPA_poly_Jul2025",
                                query = sql_query_polygons)


   # Import ONLY the African point data using the SQL filter 
wdpa_africa_points <- read_sf(dsn = gdb_path,
                              layer = "WDPA_point_Jul2025",
                              query = sql_query_points)


#  Save  data 
        #define output directory 
output_dir <- '/Users/abdulbaari/Dropbox/Migration Africa Forest Protection/data/filtered_wdpa' 

        # create the output directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


        # Write the filtered data to GeoPackage files
write_sf(wdpa_africa_polygons, file.path(output_dir, "wdpa_africa_polygons_jul2025.gpkg"), delete_layer = TRUE)
write_sf(wdpa_africa_points, file.path(output_dir, "wdpa_africa_points_jul2025.gpkg"), delete_layer = TRUE)


  

# joing the colloborative management data with the wdpa

    # import database of co-manage area 
coll_data <- read_xlsx('/Users/abdulbaari/Dropbox/Migration Africa Forest Protection/data/filtered_wdpa/PPP_database_20240214.xlsx')
   
 # extract the polygons of the  coloborative mang forest from the wdpa polygons
    # join the polygons based on WDPAID and DESIG_ENG from the coll_data
coll_data_shp <- coll_data %>%
  left_join(wdpa_africa_polygons, by = c("WDPAID" = "WDPAID", "DESIG_ENG" = "DESIG_ENG", "ISO3"= "ISO3"))
    #drop forest with NA in the geometry column(shape)
coll_data_shp <- coll_data_shp %>%
  filter(!st_is_empty(SHAPE))

    # convert the coll_data_shp to an sf object
coll_data_shp <- st_as_sf(coll_data_shp, sf_column_name = "SHAPE")

    # save the filtered polygons to a new GeoPackage file
write_sf(coll_data_shp, file.path(output_dir, "coll_data_shp.gpkg"), delete.layer=T)


#ploting 
    # load the map of Africa with adim2 level boundaries
map_of_africa <- read_sf('/Users/abdulbaari/Dropbox/Migration Africa Forest Protection/data/admin2_africa_no_island.gpkg')
  #create a ISO3 column in the map_of_africa
map_of_africa$ISO3 <- countrycode(map_of_africa$ADMIN, "country.name", "iso3c", nomatch = NA)

    # set all shape files to the same CRS
map_of_africa <- st_transform(map_of_africa, crs = st_crs(wdpa_africa_polygons))


# --- Define the target countries with collaborative management projects (CMP) ---
all_target_ppp_countries <- c(
  "AGO", "BEN", "CAF", "CMR", "COD", "COG", "ETH", "KEN", "MOZ", "MWI", "NER",
  "NGA", "RWA", "SEN", "SSD", "TCD", "TZA", "UGA", "ZMB", "ZWE"
)

    # --- Define  color palette for ppp_type 
ppp_type_colors <- c(
  "Delegated" = "#8B4513", # Saddle brown, adjust to match your map's brown
  "Financial / Technical support" = "#FFD700", # Gold
  "Co-management (bilateral)" = "#6A5ACD",    # SlateBlue
  "Co-management (integrated)" = "#20B2AA"    # LightSeaGreen
)

# --- Loop through each country ---
for (current_iso3 in all_target_ppp_countries) {
  
  # Convert ISO3 code to full country name for the title
  current_country_name_full <- countrycode(current_iso3, "iso3c", "country.name", warn = FALSE)
  if (is.na(current_country_name_full)) {
    message(paste("Skipping", current_iso3, "as full country name could not be determined."))
    next # Skip to the next iteration if name is NA
  }
  

  
  # --- Create the plot for the current country ---
  p <- ggplot() +
    # Plot the base map of the country (using map_of_africa and filtering by ISO3)
    geom_sf(data = map_of_africa %>% filter(ISO3 == current_iso3), fill = NA, color = "black") + # <--- CHANGED HERE
    # Plot the polygons and points of protected areas in the country
    geom_sf(data = wdpa_africa_polygons %>% filter(ISO3 == current_iso3), fill = "darkgreen", color = NA, alpha = 0.7) +
    geom_sf(data = wdpa_africa_points %>% filter(ISO3 == current_iso3), color = "darkblue", size = 0.5, shape = 1) +
    # Plot the collaborative management areas
    geom_sf(data = coll_data_shp %>% filter(ISO3 == current_iso3), aes(fill = ppp_type), color = NA, alpha = 0.7) +
    # Manually set fill colors for ppp_type
    scale_fill_manual(values = ppp_type_colors) +
    # Add labels and theme (dynamic title)
    labs(title = paste("Protected Areas in", current_country_name_full),
         subtitle = "Including Collaborative Management Areas",
         caption = "Source: WDPA Jul2025") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # --- Save the plot ---
  figures_dir <- '/Users/abdulbaari/Dropbox/Migration Africa Forest Protection/data/filtered_wdpa/figures'
  all_countries <- file.path(figures_dir, paste0("protected_areas_", tolower(current_iso3), "_jul2025.png"))
  
  ggsave(
    filename = all_countries,
    plot = p,
    width = 10,
    height = 15,
    dpi = 300
  )
}


#Ploting for the whole of Africa

  # --- Dissolve Admin 1 boundaries to Admin 0 (Country) boundaries for the ENTIRE continent ---
map_of_africa_admin0 <- map_of_africa %>%
  group_by(ISO3) %>% # Group by the country's ISO3 code
  summarise(geometry = st_union(geom)) %>% # Union all polygons within each ISO3 group
  ungroup()
  # --- Save the dissolved Admin 0 boundaries for the entire continent ---
write_sf(map_of_africa_admin0, file.path(output_dir, "map_of_africa_admin0.gpkg"), delete_layer = TRUE)


  # --- Define a bounding box for Africa ---
bbox_africa <- st_bbox(c(xmin = -20, ymin = -30, xmax = 45, ymax = 20),
                       crs = st_crs(map_of_africa))%>%
                st_as_sfc()


# --- Crop map_of_africa to a defined bounding box -----------------------------
cropped_map_of_africa <- st_crop(map_of_africa_admin0, bbox_africa)


# ----filter the WDPA polygons and points for only the target CMP countries---
filtered_wdpa_polygons <- wdpa_africa_polygons %>% filter(ISO3 %in% all_target_ppp_countries)
filtered_wdpa_points <- wdpa_africa_points %>% filter(ISO3 %in% all_target_ppp_countries)


  #--plot for the whole of Africa with the target countries only
p_africa_combined <- ggplot() +
  geom_sf(data = cropped_map_of_africa, fill = NA, color = "black") + # Black country outlines
  # Plot the protected area polygons  only for  countries with cmp
  geom_sf(data = filtered_wdpa_polygons, fill = "darkgreen", color = NA, alpha = 0.7) +
  # Plot the protected area points (blue dots) - for cmp countries only
  geom_sf(data = filtered_wdpa_points, color = "darkblue", size = 0.5, shape = 1) +
  # Plot the collaborative management areas, colored by ppp_type - ONLY FOR TARGET COUNTRIES
  geom_sf(data = coll_data_shp, aes(fill = ppp_type), color = NA, alpha = 0.7) +
  # Manually set fill colors for ppp_type
  scale_fill_manual(values = ppp_type_colors, name = "PPP Management Type") +
  # Add labels for cmp names 
  geom_sf_text(data = coll_data_shp, 
              aes(label = NAME.y), 
              size = 3,          
              color = "black",   
              check_overlap = TRUE, ) + 
  
  labs(title = "Protected Areas and Collaborative Management Projects",
       caption = "Source: WDPA Jul2025") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.box.background = element_rect(fill = "white", color = "black"),
    legend.box.margin = margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 1, size = 9)
  )

  #define output directory for figure
wdpa_with_cmp_pdf <- file.path(figures_dir, "protected_areas_africa_combined_jul2025.pdf")
wdpa_with_cmp_png <- file.path(figures_dir, "protected_areas_africa_combined_large_jul2025.png")

ggsave(
  filename = wdpa_with_cmp_pdf,
  plot = p_africa_combined,
  width = 24, 
  height = 30, 
  device = "pdf" )






# --- Plotting the entire African continent with protected areas colored by GOV_TYPE ---


    # Define a color palette for GOV_TYPE
gov_type_colors <- c(
  "Collaborative governance"          = "#A6CEE3", # Light Blue
  "Federal or national ministry or agency" = "#1F78B4", # Dark Blue
  "For-profit organisations"          = "#E31A1C", # Red
  "Government-delegated management"   = "#FB9A99", # Light Red/Pink
  "Indigenous peoples"                = "#33A02C", # Green
  "Individual landowners"             = "#B2DF8A", # Light Green
  "Joint governance"                  = "#FDBF6F", # Orange
  "Local communities"                 = "#FF7F00", # Dark Orange
  "Non-profit organisations"          = "#CAB2D6", # Light Purple
  "Not Reported"                      = "#6A3D9A", # Dark Purple
  "Sub-national ministry or agency"   = "#FFFF99", # Yellow
  "Transboundary governance"          = "#8D4585"  # Plum/Brown
)

    # Create a plot for the entire African continent with protected areas colored by GOV_TYPE

p_africa_combined_govtype <- ggplot() +
  geom_sf(data = map_of_africa_admin0, fill = NA, color = "black") +
  #Plot the protected area polygons, mapped by GOV_TYPE
  geom_sf(data = wdpa_africa_polygons, aes(fill = GOV_TYPE), color = NA, alpha = 0.7) +
  geom_sf(data = wdpa_africa_points, color ="red" , size = 0.5, shape = 1) +
  scale_fill_manual(values = gov_type_colors, name = "Governance Type (Polygons)") +
  labs(
    title = "Protected Areas in Africa by Governance Type",
    subtitle = "Distribution of Protected Areas across the African Continent",
    caption = "Source: WDPA Jul2025"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 1, size = 9)
  )
wdpa_with_govt_pdf <- file.path(figures_dir, "protected_areas_gov_type_jul2025.pdf")

ggsave(
  filename = wdpa_with_govt_pdf,
  plot = p_africa_combined_govtype,
  width = 24, 
  height = 30, 
  device = "pdf" )





### plot for each country 

   #create a directory to for the plots of each country 
plots_dir <- '/Users/abdulbaari/Dropbox/Migration Africa Forest Protection/data/filtered_wdpa/wdpa_for_each_country'
if (!dir.exists(plots_dir)) {
      dir.create(plots_dir, recursive = TRUE)
}


# --- Start of the loop to plot WDPA for each country ---
for (current_iso3 in african_iso3s) {
  
  # Convert ISO3 code to full country name for the title
  current_country_name_full <- countrycode(current_iso3, "iso3c", "country.name", warn = FALSE)
  if (is.na(current_country_name_full)) {
    message(paste("Skipping", current_iso3, "as full country name could not be determined."))
    next # Skip to the next iteration if name is NA
  }
  
  # Filter the Admin 2 map data for the current country using your map_of_africa
  country_admin2_sf <- map_of_africa %>% filter(ISO3 == current_iso3)
  
  # Filter the polygons and points for the current country
  current_polygons <- wdpa_africa_polygons %>% filter(ISO3 == current_iso3)
  current_points <- wdpa_africa_points %>% filter(ISO3 == current_iso3)
  
  # Check if there's any data for the country before plotting
  if (nrow(country_admin2_sf) == 0) {
    message(paste("Skipping", current_iso3, ": No Admin 2 map data found for this country in 'map_of_africa'."))
    next
  }
  if (nrow(current_polygons) == 0 && nrow(current_points) == 0) {
    message(paste("Skipping", current_iso3, ": No Protected Area data found for this country."))
    next
  }
  
  # Create the plot for the current country
  wdpa_country <- ggplot() +
    # 1. Plot Admin 2 boundaries for the selected country from your map_of_africa
    geom_sf(data = country_admin2_sf, fill = NA, color = "grey50", linewidth = 0.3) +
    
    # 2. Plot the protected area polygons, colored by GOV_TYPE
    geom_sf(data = current_polygons, aes(fill = GOV_TYPE), color = NA, alpha = 0.7) +
    
    # 3. Plot the protected area points, colored by GOV_TYPE
    geom_sf(data = current_points, aes(color = GOV_TYPE), size = 1.5, shape = 16, stroke = 0.5) +
    
    # 4. Apply the custom color palette for polygons (fill) and points (color)
    scale_fill_manual(values = gov_type_colors, name = "Governance Type") +
    scale_color_manual(values = gov_type_colors, name = "Governance Type") +
    
    labs(
      title = paste("Protected Areas in", current_country_name_full, "by Governance Type"),
      subtitle = "Showing Admin 2 Divisions",
      caption = "Source: WDPA Jul2025"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      plot.caption = element_text(hjust = 1, size = 8)
    )
  
  # Save the plot
  current_plots_dir <- file.path(plots_dir, paste0("protected_areas_", tolower(current_iso3), "_admin2_jul2025.png"))
  
  ggsave(
    filename =   current_plots_dir,
    plot = wdpa_country,
    width = 10,
    height = 12,
    dpi = 300
  )
 
}