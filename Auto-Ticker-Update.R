##################################################
## Project: Automotive Automation
## Script purpose: Automate with new database layout
## Date: Tue Aug 27 14:32:28 2019
## Author: Brad Hill
##################################################


# Setup -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidytext, lubridate, odbc, devtools, rgeos, janitor, readxl, zipcode, janitor)
creds <- tryCatch(read_lines('C:/ELT/Production/Modules/psm_r/credentials'), error = function(e){read_lines("~/FW/credentials")})
github_auth <- creds[which(creds == 'github PAT:') + 1]
devtools::install_github('bradisbrad/FW', subdir = 'fwfun', auth_token = github_auth); library(fwfun)
devtools::install_github('bradisbrad/olfatbones')
sql_connect <- function(database, user = 'fwetl'){
  db <- case_when(tolower(database) %in% c('s', 'staging', 'stage') ~ "Staging",
                  tolower(database) %in% c('w', 'warehouse', 'ware', 'wh') ~ "Warehouse",
                  tolower(database) %in% c('f','factset','fs') ~ "Factset",
                  tolower(database) %in% c('segment', 'seg', 'user') ~ "Segment")
  pass <- case_when(user == "fwetl" ~ "d]h4g2G`'<4c&sZ_",
                    user == "brad_hill" ~ "XXn#v69f3pDRhvNS")
  if(is.na(db)) {
    return('You must choose either Staging, Warehouse, Segment or Factset.')
    break
  } else {
    return(odbc::dbConnect(odbc(),
                           Driver = "SQL Server",
                           Server = "freightwaves.ctaqnedkuefm.us-east-2.rds.amazonaws.com",
                           Database = db,
                           UID = user,
                           PWD = pass,
                           Port = 1433))
  }
}
scon <- sql_connect('s')
wcon <- sql_connect('w')
data(zipcode)


# Load in staging data ----------------------------------------------------

stg_auto <- tbl(scon, "indx_index_data") %>% 
  filter(index_id %in% c(134)) %>% 
  select(data_timestamp, data_value, index_id, granularity_item_id) %>% 
  collect() %>% 
  mutate(data_timestamp = ymd(data_timestamp))
motor_match <- tbl(scon, 'indx_granularity_item') %>% 
  filter(granularity_level_id %in% c(19, 39)) %>% 
  select(granularity_item_id = id, granularity_level_id, granularity1, Description) %>% 
  collect()


# Load in LMC Warehouse Tables --------------------------------------------

whtbl_auto <- tbl(wcon, 'lmc_auto') %>% 
  collect() %>% 
  clean_names() %>% 
  mutate(asofdate = ymd(asofdate)) %>% 
  filter(asofdate == max(asofdate))
whtbl_cap <- tbl(wcon, 'lmc_cap') %>% 
  collect() %>% 
  clean_names() %>% 
  mutate(asofdate = ymd(asofdate)) %>% 
  filter(asofdate == max(asofdate))
# file_name <- "~/proj/work/local/LMCA NA LVPF - Database - August 2019.xlsm"
# whtbl_auto <- read_xlsx(file_name,sheet = 2)
# whtbl_cap <- read_xlsx(file_name, sheet = 4) %>% 
#   janitor::clean_names()


# Join LMC Tables ---------------------------------------------------------

whtbl <- whtbl_auto %>% 
  inner_join(whtbl_cap, by = c('plant' = 'plant_name', 'manufacturer', 'country', 'year'))


# Match plant to mkt --------------------------------------------------

plant_loc <- whtbl_cap %>%
  filter(country == 'USA') %>%
  select(plant_name, city,location) %>%
  distinct %>% 
  left_join(data.frame(state.abb,state.name),by = c('location' = 'state.name')) %>%
  left_join(zipcode,by = c('city','state.abb' = 'state')) %>%
  mutate(zip3 = str_sub(zip,1,3)) %>%
  left_join(collect(tbl(scon, 'KMA')), by = c('zip3' = 'ZIP3')) %>%
  select(plant_name, city,location,KMA_CODE) %>%
  distinct  %>% 
  mutate(xma_code = paste0(KMA_CODE,'+'),
         xma_code = ifelse(city == 'Claycomo','MO_KAN+',xma_code)) %>% 
  left_join(collect(tbl(scon, 'indx_granularity_item')), by = c('xma_code'='granularity2')) %>% 
  select(id,plant = plant_name,city,location,mkt = granularity1)


# Attach markets to whtbl and clean -----------------------------------

clean_auto <- whtbl %>% 
  filter(country == 'USA') %>% 
  mutate(last_actual = myd(str_c(last_actual, '-01')),
         production_date = myd(str_c(str_sub(month, -3, -1), '-', year, '-01'))) %>% 
  left_join(plant_loc, by = c('city', 'location', 'plant')) %>% 
  select(manufacturer, regional_make, global_production_model, plant, city, 
         location, country, sop_month, sop_year, eop_month, eop_year, last_actual, 
         month, year, mkt, id, production_volume) %>% 
  mutate(start_of_prod = myd(str_c(str_sub(sop_month, -3, -1), sop_year, '01', sep = "-")),
         end_of_prod = myd(str_c(str_sub(eop_month, -3, -1), eop_year, '01', sep = "-")),
         prod_date = myd(str_c(str_sub(month, -3, -1), year, '01', sep = "-"))) %>% 
  select(-c(sop_month, sop_year, eop_month, eop_year, month, year)) %>% 
  mutate(pt_type = case_when(prod_date > last_actual ~ "forecasted",
                             T ~ "actual"))


# Manufacturer Aggregation ------------------------------------------------

man_agg <- clean_auto %>% 
  mutate(Description = case_when(manufacturer == 'Daimler Group' ~ 'Daimler AG',
                                 manufacturer == 'Ford Group' ~ 'Ford Motor Co',
                                 manufacturer == 'General Motors Group' ~ 'General Motors Co',
                                 manufacturer == 'Navistar' ~ 'Navistar International Corp',
                                 manufacturer == 'Tesla Motors' ~ 'Tesla',
                                 manufacturer == 'Volvo' ~ 'Volvo AB',
                                 manufacturer == 'Workhorse' ~ 'Workhorse Group',
                                 T ~ manufacturer)) %>% 
  left_join(motor_match) %>% 
  group_by(granularity_item_id, prod_date) %>% 
  summarize(prod = as.numeric(sum(production_volume))) %>% 
  mutate(prod_date = rollback(prod_date %m+% months(1))) %>% 
  filter(prod_date <= rollback(Sys.Date())) %>% 
  rename(data_timestamp = prod_date,
         data_value = prod) %>% 
  mutate(index_id = 134)


# Find updates ------------------------------------------------------------------

update_tbl <- clean_auto %>% 
  group_by(prod_date, mkt, id) %>% 
  summarize(production_volume = sum(production_volume)) %>% 
  ungroup() %>% 
  mutate(production_volume = as.numeric(production_volume),
         prod_date = rollback(prod_date %m+% months(1)),
         index_id = 134) %>% 
  arrange(desc(prod_date)) %>% 
  filter(prod_date <= rollback(Sys.Date()),
         ! mkt %in% c('BMI', 'SBN')) %>% 
  select(data_timestamp = prod_date, data_value = production_volume, index_id, granularity_item_id = id) %>% 
  bind_rows(man_agg) %>% 
  anti_join(stg_auto)


# Add Total USA Auto Production - AUTO.USA -----------------------------------

update_tbl <- clean_auto %>%
  filter(prod_date <= rollback(Sys.Date()),
         ! mkt %in% c('BMI', 'SBN')) %>% 
  group_by(prod_date) %>% 
  summarize(production_volume = sum(production_volume)) %>% 
  ungroup() %>% 
  mutate(production_volume = as.numeric(production_volume),
         prod_date = rollback(prod_date %m+% months(1)),
         index_id = 134,
         id = 1) %>%
  select(data_timestamp = prod_date, data_value = production_volume, index_id, granularity_item_id = id) %>% 
  bind_rows(update_tbl) %>% 
  anti_join(stg_auto)


# Create delete statement -------------------------------------------------

delete_params <- update_tbl %>% 
  mutate(del_st = sprintf("(index_id = %s AND granularity_item_id = %s AND data_timestamp = '%s')", index_id, granularity_item_id, data_timestamp)) %>% 
  pull(del_st) %>% 
  str_c(collapse = ' OR ')
delete_string <- str_c("DELETE FROM indx_index_data WHERE ", delete_params)


# Delete from and append to staging.dbo.indx_index_data -----------------------------------

if(length(delete_params) > 0){
  dbSendStatement(scon, delete_string)
  dbDisconnect(scon)
  gc()
  scon <- sql_connect('s')
  dbWriteTable(scon, 'indx_index_data', update_tbl, append = T)
}
