#This script creates new variables for analysis.
#Although some variables have been created presviously as part of joing the forest policies and ipums data.
#These are just additional variables that may be useful for analysis.

# Load libraries
library(tidyverse)
library(haven)
library(fixest)
library(stargazer)
library(here)


# Load the  dataset with forest loss and policies
policies_forestloss <- read_dta(here("data", "built", "policy_forestloss.dta"))


#creating for total number of active projects
policies_forestloss <- policies_forestloss |> 
  #  In the dataset, ipums without policies have NA values for total_active_projects.
  #  We replace these NA values with 0 to indicate that there are no active projects in those ipums-years. 
  mutate(total_active_projects0 = ifelse(is.na(total_active_projects), 0, total_active_projects)) |> 
  # Create the policy dummy variable
  mutate(totalpolicies_dummy = ifelse(total_active_projects0 > 0, 1, 0)) |> 
  # Create the policy type dummy variables
  mutate(dfm_dummy = ifelse(dfm_policy > 0, 1, 0),
         icdp_dummy = ifelse(icdp_active_policy > 0, 1, 0),
         pes_dummy = ifelse(pes_policy > 0, 1, 0),
         ipl_dummy = ifelse(ipl_projects > 0, 1, 0),
         govt_dummy = ifelse(governance_projects > 0, 1, 0),
         land_titl_dummy = ifelse(land_titl_reform_policy > 0, 1, 0),
         sust_dummy = ifelse(sustainable_use_policy > 0, 1, 0),
         cert_dummy = ifelse(certification_projects > 0, 1, 0)
         ) |> 
  # Create a new variables for the policy type by replacing NA values in policy type dummy variables with 0
  mutate(dfm_dummy0 = ifelse(is.na(dfm_dummy), 0, dfm_dummy),
         icdp_dummy0 = ifelse(is.na(icdp_dummy), 0, icdp_dummy),
         pes_dummy0 = ifelse(is.na(pes_dummy), 0, pes_dummy),
         ipl_dummy0 = ifelse(is.na(ipl_dummy), 0, ipl_dummy),
         govt_dummy0 = ifelse(is.na(govt_dummy), 0, govt_dummy),
         land_titl_dummy0 = ifelse(is.na(land_titl_dummy), 0, land_titl_dummy),
         sust_dummy0 = ifelse(is.na(sust_dummy), 0, sust_dummy),
         cert_dummy0 = ifelse(is.na(cert_dummy), 0, cert_dummy)
         )
   
  

# Create forest share variables for different forest cover thresholds

policies_forestloss <- policies_forestloss |> 
  mutate(
    forest_share_10 =  treecover_new_10 / areaha,
    forest_share_20 = treecover_new_20 / areaha,
    forest_share_30 = treecover_new_30 / areaha,
    forest_share_40 = treecover_new_40 / areaha,
    forest_share_50 = treecover_new_50 / areaha
  )
# Save the updated dataset with new variables
write_dta(policies_forestloss, here("data", "built", "policies_forestloss_newvars.dta"))

# End of script