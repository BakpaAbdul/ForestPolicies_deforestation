#MATCHING PAs TO IPUMS DATA

# Load  libraries
library(sf)
library(tidyverse)
library(here)
library(wdpar)
library(haven)

# Load the shapefile of protected areas

# Define the list of countries using their ISO3 codes
sample_iso3 <- c("BEN", "BWA", "BFA", "CMR", "CIV", "COD", "GHA", "GIN", 
  "KEN", "LSO", "MWI", "MLI", "MUS", "MOZ", "RWA", "SEN", 
  "SLE", "ZAF", "SDN", "TZA", "TGO", "UGA", "ZMB", "ZWE")

# define the layer  for polygons to me imorted
polygon_layer_name <- "WDPA_poly_Aug2025"

# desire columns fromt the protected area data
desire_columns <- c("WDPAID", "NAME", "STATUS_YR", "DESIG_ENG", "REP_AREA", "ISO3", "IUCN_CAT", "GOV_TYPE", "MARINE")

# Create a SQL query to filter both rows (countries) and desire columns
sql_query_poly <- paste0(
  "SELECT ", paste(desire_columns, collapse = ", "),
  " FROM ", polygon_layer_name,
  " WHERE ISO3 IN ('", paste(sample_iso3, collapse = "', '"), "')")

# Read the data
PAs_poly <- st_read( here( "data","raw","WDPA_Aug2025_Public", "WDPA_Aug2025_Public.gdb"),
  query = sql_query_poly)

#clean it 
PAs_poly <- PAs_poly |>
  filter(MARINE == 0) |>  # Keep only terrestrial protected areas
  select(- MARINE) |>  # Remove the MARINE column if not needed
  filter(REP_AREA > 25 ) # Filter out  protected areas smaller than 25km². chosen based on  Desbureaux et al. (2024) https://orcid.org/0000-0001-5489-4917


#save the polygon data as gpkg file
st_write(PAs_poly, 
  here("data","built","PAs_poly.gpkg"), 
  layer = "PAs_poly", 
  delete_dsn = TRUE)




#import IPUMS data 
ipums_data <- st_read( here("data", "raw", "maps","SampleUnits.shp")) |> 
  st_make_valid()

# Clean and standardize country names in the IPUMS data, i am doing this because i want to match with the PA data using a loop. intersecting all of at once was taking too long
# Create a lookup table for country names and ISO3 codes
country_iso_lookup <- tribble(
  ~country_name_clean, ~ISO3,
  "democratic republic of congo", "COD",
  "côte d'ivoire", "CIV",
  "botswana", "BWA",
  "guinea", "GIN",
  "lesotho", "LSO",
  "malawi", "MWI",
  "mauritius", "MUS",
  "sudan", "SDN",
  "uganda", "UGA",
  "tanzania", "TZA",
  "cameroon", "CMR",
  "benin", "BEN",
  "ghana", "GHA",
  "kenya", "KEN",
  "mali", "MLI",
  "rwanda", "RWA",
  "senegal", "SEN",
  "sierra leone", "SLE",
  "south africa", "ZAF",
  "zimbabwe", "ZWE",
  "togo", "TGO",
  "burkina faso", "BFA",
  "zambia", "ZMB",
  "mozambique", "MOZ"
)

ipums_data <- ipums_data |>
  mutate(
    country_clean = str_to_lower(country), #make it consistent with the lookup table
    country_clean = str_trim(country_clean),
    # 3. Replace known abbreviations or misspellings
    country_clean = str_replace(country_clean, "democratic republic of congo", "democratic republic of congo"),
    country_clean = str_replace(country_clean, "côte d'ivoire", "côte d'ivoire"),
    country_clean = str_replace(country_clean, "serra leone", "sierra leone"), # Handle a potential misspelling
  ) |>
  # Join with the lookup table
  left_join(country_iso_lookup, by = c("country_clean" = "country_name_clean")) 



# Ensure both protected area datasets have the same CRS as the IPUMS data

PAs_poly <- st_transform(PAs_poly, st_crs(ipums_data)) |> 
  st_repair_geometry() # Repair geometries.





#intersefcting polygons with ipums for all countries one by one using a loop
# Loop through each country in the ISO3 list
sample_iso3 <- unique(ipums_data$ISO3)

#  lists to store the results for each country
all_poly_intersections_list <- list()
# Loop through each country's ISO3 code
for (iso3 in sample_iso3) {
  message(paste("Processing data for:", iso3))
  # Filter the IPUMS data for the current country
  ipums_country <- ipums_data |> filter(ISO3 == iso3)
  
  # Filter the protected area polygons for the current country
  pa_poly_country <- PAs_poly |> filter(ISO3 == iso3)
  
  # Perform intersection
  intersection_result <- st_intersection(pa_poly_country, ipums_country)
  
  # Store the result in the list
  all_poly_intersections_list[[iso3]] <- intersection_result
}
# Combine all intersection results into a single data frame
ipums_PA_poly <- do.call(rbind, all_poly_intersections_list)


#drop ISO3.1 
ipums_PA_poly <- ipums_PA_poly |>
  select(-ISO3.1)

# save the intersection results as a GeoPackage file
st_write(ipums_PA_poly, 
  here("data","built","ipums_PA_poly.gpkg"), 
  delete_dsn = TRUE)

#drop the geometry column from the point  and polygon data
ipums_PA <- ipums_PA_poly |>
  st_drop_geometry() |> 
  filter(STATUS_YR != 0 ) |> #remove PAs with no establishment year date
  filter(STATUS_YR <= 2020 ) |>  #only consider PAs established on or before 2020
  select(-c(ISO3.1, country,  DESIG_ENG, REP_AREA, IUCN_CAT, GOV_TYPE, country_clean, ISO3)) 


#save the intersection results as a dta file
ipums_PA <- write_dta(ipums_PA, 
  here("data","built","ipums_PA.dta"))



