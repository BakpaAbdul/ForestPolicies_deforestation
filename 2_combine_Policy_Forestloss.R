# This script builds a project-year database from policies. By crossing project data with a sequence of years, and using start date of projects to create a an indicator of active projects per year.  
# It then creates a panel database that counts active projects in a particular ipsum_id per year. 
# And also counts active projects by type per year.
# Finally, it merges the panel database with forest loss data.


#load libraries
library(tidyverse)
library(lubridate) 
library(sf)
library(stringr)
library(here)



#Create a Project-Year Database
policies <- st_read(here ("data","built","ForestPolicy_ipums_id.gpkg")) |> 
  st_drop_geometry() |> 
  select(ipums_id, `Name` , Intervention.type, everything()) |> 
  mutate(Intervention.type = str_to_lower(Intervention.type))


# Generate a sequence of years
years <- 2000:2020

# Create a database where each observation is a project x year
policy_year_db <- policies |>
  distinct(ipums_id, Name, Project.start, Project.end, Intervention.type) |>
  crossing(year = years) |>
  mutate(
    indicator = ifelse(
      # Condition 1: If Project.start is NA, the indicator is NA
      is.na(Project.start),
      NA,
      # Condition 2: If Project.end is NA, check if year is >= Project.start
      ifelse(
        is.na(Project.end),
        ifelse(year >= Project.start, 1, 0),
        # Condition 3: If Project.end is not NA, check if year is within the range
        ifelse(
          year >= Project.start & year <= Project.end,
          1,
          0
        )
      )
    )
  )




# Create a panel database that counts active projects in a particular ipsum_id per year
ipums_year_db_by_type <- policy_year_db |>
  group_by(ipums_id, year) |>
  summarise(
    #Count total active projects
    total_active_projects = sum(indicator, na.rm = TRUE),
    
    # Count each specific project type
    dfm_policy = sum(indicator[Intervention.type == "dfm"], na.rm = TRUE),
    pes_policy = sum(indicator[Intervention.type == "pes"], na.rm = TRUE),
    icdp_active_policy = sum(indicator[Intervention.type == "icdp"], na.rm = TRUE),
    sustainable_use_policy = sum(indicator[Intervention.type == "sustainable use"], na.rm = TRUE),
    certification_projects = sum(indicator[Intervention.type == "certification"], na.rm = TRUE),
    ipl_projects = sum(indicator[Intervention.type == "ipl"], na.rm = TRUE),
    governance_projects = sum(indicator[Intervention.type == "governance"], na.rm = TRUE),
    land_titl_reform_policy = sum(indicator[Intervention.type == "land titling and reform"], na.rm = TRUE),
    
    .groups = "drop"
  )

# Save the database
write_dta(ipums_year_db_by_type, here ("data", "built", "ipums_year_db_by_type.dta"))

#import forest loss data and merge with ipums_year_db
library(haven)
forest_loss <- read_dta(here("data", "raw", "forestloss_cuts_long_with_row.dta")) |> 
  filter(ipums_id != 1)




#join the ipums_year_db_by_type database with the forest loss data
policy_forestloss <- left_join(forest_loss, ipums_year_db_by_type, by = c("ipums_id", "year")) |> 
  select(ipums_id, year, total_active_projects, everything())


#save the final database
write_dta(policy_forestloss,  here ("data", "built", "policy_forestloss.dta"))


