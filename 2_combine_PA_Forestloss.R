#load libraries
library(tidyverse)
library(haven)
library(here)

ipums_PA <-read_dta(here( "data", "built" ,"ipums_PA.dta")) 

#1. base panel  for every ipums_id and year
ipums_PA_panel <- ipums_PA %>%
  distinct(ipums_id, WDPAID, NAME, STATUS_YR, admin_name ) %>% 
  crossing(year = 2000:2020) 

# 2. Create the indicator to check if a PA is active in a given year in a particular IPUMS ID.
temp_panel_data <- ipums_PA_panel %>%
  mutate(
    # Create a temporary binary variable for PA presence in a given row.
    # We check if STATUS_YR is a valid year AND it is less than or equal to the current year.
    pa_active_dummy = case_when(
      STATUS_YR <= year ~ 1,              # PA is active in the current year
      TRUE ~ 0    ))                        # PA exists but is not yet active in this year 
  


# 3. unique ipums_id-year panel
final_panel <- temp_panel_data %>%
  group_by(ipums_id, year) %>%
  summarise(
    # Sum the pa_active_dummy to get the PA_count for the year.This will be 0 if PAs exist but aren't active yet, and > 0 if they are active.
    PA_count = sum(pa_active_dummy, na.rm = TRUE),

    # Create a binary indicator for PA presence in the year.
    PA_dummy = ifelse(PA_count > 0, 1, 0),
    .groups = "drop" ) 
  


#import forest loss data and merge with final_panel
forest_loss <- read_dta(here("data","raw","forestloss_cuts_long_with_row.dta") )|> 
  mutate(year = as.integer(year)) |> 
  #drop the observations named Rest of the World with ipums_id 1
  filter(ipums_id != 1) 
  
  

# Merge the final panel with forest loss data
PA_forestloss <- forest_loss |>
  left_join(final_panel, by = c("ipums_id", "year")) |> 
  select(ipums_id, year, everything()) |> 
  mutate(across(c(PA_dummy, PA_count), ~ replace_na(.x, 0L)))

#New variable : shares of forest  in ipums units
PA_forestloss <- PA_forestloss |> 
    mutate( Share_forest_30 = treecover_new_30 / areaha,
          Share_forest_20 = treecover_new_20 / areaha,
          Share_forest_10 = treecover_new_10 / areaha,
          Share_forest_40 = treecover_new_40/ areaha,
          Share_forest_90 = treecover_new_90 / areaha)
    
        


# Save the final panel with forest loss data
write_dta(PA_forestloss, 
          here( "data","built", "PA_forestloss.dta"))
