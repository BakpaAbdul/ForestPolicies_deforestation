# This script match the forest policies with ipums_id. 
# It uses spatial intersection to identify which forest policies overlap with IPUMS admin2 units and assigns the corresponding ipums_id to each policy.
# It also filters out small overlaps where the intersected area is less than 20% of the original policy area. this is to ensure that only significant overlaps are considered. 
# Small overlaps may be  cause my small changes in admisnistrative units counts or minor inaccuracies in the spatial data. 

# Load necessary libraries
library(sf)
library(tidyverse)
library(openxlsx)
library(readxl)
library(haven)
library(here)


# Define the list of sample countries. these are the countries with with ipums data
sample_countries <- c(
  "Democratic Republic of Congo", "Côte D'Ivoire",
  "Botswana", "Guinea", "Lesotho", "Malawi", "Mauritius", "Sudan", "Uganda",
  "Tanzania", "Cameroon", "Benin", "Ghana", "Kenya", "Mali", "Rwanda", "Senegal",
  "Sierra Leone", "South Africa", "Zimbabwe", "Togo", "Burkina Faso", "Zambia",
  "Mozambique")




# import forest polices. this is an excel file with information on forest protection policies and their locations at admin2 level 
forest_policies <- read.xlsx( here ( "data", "raw", 'forest_protection_policies.xlsx'))

#clean the forest policies data
forest_policies <- forest_policies |>
  #select relevant columns
  select(Name, Intervention.type, Specific.type, Project.start, Project.end, Country,  NAME_1, GID_1, NAME_2, GID_2) |> 
  #remove rows with missing missing admin2 units (GID_2) values
  filter(!is.na(GID_2)) |> 
  #trim whitespace from character columns
  mutate(across(where(is.character), str_trim)) |> 
  #filter to keep only the sample countries with ipums data
  filter(Country %in% sample_countries)

  

#import admin2 level polygons. this is a geopackage file with admin2 level polygons for african countries
#this is used to join with forest policies to get the geometry for each policy based on its admin2 unit (GID_2)
admin2 <- st_read(here("data","raw", "maps", "admin2_africa_no_island.gpkg")) 


#join forest policies with admin2 polygons to get the geometry for each policy based on its admin2 unit (GID_2)
forest_policies_admin2 <- forest_policies |> 
  left_join(admin2, by =  "GID_2") |> 
  st_as_sf() |>
  st_make_valid()


#Calculate the area of the original polygons for later use in filtering small overlaps
forest_policies_admin2 <- forest_policies_admin2 |>
  mutate(original_area = as.numeric(st_area(geom)))

#import ipums admin2 level polygons. 
ipums <-st_read(here("data", "raw", "maps", "SampleUnits.shp") )|> 
  #one of the country names is misspelled, so we correct it here
  mutate(
    country = ifelse(
      country == "Democratic Republic of congo",
      "Democratic Republic of Congo",
      country)) |> 
  st_make_valid()

#make sure both datasets use the same coordinate reference system (CRS)
forest_policies_admin2 <- st_transform(forest_policies_admin2, st_crs(ipums))



#intersect forest policies ploygons with ipums polygons to get ipums ID where forest policies are located

# Get the unique vector of countries in the forest policies dataset
countries <- unique(forest_policies_admin2$Country)

#Create an empty list to store the results
results_list <- list()

# Loop through each country
for (country_name in countries) {
# Filter the data for the current country
policies_subset <- forest_policies_admin2 |> filter(Country == country_name)
ipums_subset <- ipums |> filter(country == country_name)
message(paste("Processing country:", country_name))
# Perform the intersection for this subset
intersected_subset <- st_intersection(policies_subset, ipums_subset)

# Store the result in the list
results_list[[country_name]] <- intersected_subset
}

# Combine the results back into a single sf object
policy_with_ipums_id <- do.call(rbind, results_list)



#calculate the intersected area and area ratio to filter small overlaps. 
policy_with_ipums_id <- policy_with_ipums_id |>
  st_make_valid() |> 
  mutate(intersected_area = as.numeric(st_area(geom))) |>
  mutate(area_ratio = intersected_area / original_area) |> 
  filter(area_ratio >= 0.2) |> 
  select(
    ipums_id, NAME_2.y, GID_2, Name, Intervention.type, Specific.type, ISO_A3, country, Project.start, Project.end, 
    NAME_1.y, GID_1.y,   original_area, geom
  )




# Save the result to a new shapefile
st_write(policy_with_ipums_id, here ("data","built","ForestPolicy_ipums_id.gpkg"), delete_dsn = TRUE)

