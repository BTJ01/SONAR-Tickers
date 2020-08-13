#wcon <- sql_connect('w')
# con <- dbConnect(odbc(),
#                      Driver = "",
#                      Server = ""
#                      Database = "Factset",
#                      port = ,
#                      UID = ,
#                      PWD = ,
# )

mxdt <- dbGetQuery(scon, 'SELECT TOP 1 data_timestamp FROM staging.dbo.indx_index_data WHERE index_id = 4884 ORDER BY data_timestamp desc')
#Don't need staging, just need last date of data in staging ^^^
#staging_index <- 
#  tbl(scon, "indx_index_data") %>% 
#  filter(index_id == 4884) %>% # pull only currency exchange data
#  select(index_id, granularity_item_id, data_timestamp, data_value) %>% 
#  collect() %>% 
#  mutate(data_timestamp = ymd(data_timestamp))
## --------------- Check for updates --------------------------------
# don't need to anti_join staging since we're going on mxdt now
#fx_new <- 
#  anti_join(fx_index, 
#            staging_index, 
#            by = c('granularity_item_id', 'data_timestamp'))
#
#



as_tibble(dbGetQuery(fcon,
                    "SELECT fx.iso_currency, [date], exch_rate_usd, exch_rate_per_usd
                     FROM Factset.ref_v2.fx_rates_usd AS fx
					           INNER JOIN Factset.ref_v2.iso_currency_map AS cm
					           ON cm.iso_currency = fx.iso_currency
                     WHERE cm.active = 1
					           AND fx.iso_currency NOT IN ('BOV', 'CLF', 'COU', 'CUC', 'GBX', 'ILX', 'UYW', 'UYI', 'ZWL', '999');
                    ")
          )

as_tibble(dbGetQuery(scon,
                     "SELECT [index_id], [granularity_item_id], [data_timestamp], [data_value]
                       FROM [Staging].[dbo].[indx_index_data]
                       WHERE [index_id] = 4789
                      ")  # index_id = 4789 
)

as_tibble(dbGetQuery(scon,
                     "SELECT [id] AS granularity_item_id, granularity1, [Description]
                       FROM Staging.dbo.indx_granularity_item
                       WHERE granularity_level_id = 58
                      ")
)

# don't need it, everything is already in granularity_index
currency_tbl <- 
  tbl(fcon, dbplyr::in_schema('ref_v2', 'iso_currency_map')) %>% 
  #inner_join("fx_rates_usd", by = '') %>% 
  filter(active %in% '1', 
         !iso_currency %in% c('BOV', 'CLF', 'COU', 'CUC', 'GBX', 
                              'ILX', 'UYW', 'UYI', 'ZWL', '999')) %>% 
  select(iso_currency, currency_desc) %>% 
  collect()  




dplyr::union(DF1,DF2) #combines and dedups
DF4 <- bind_rows(DF1, DF2, .id = '1,2') #combines and keeps all
DF5 <- merge(DF1, DF2, by = c("date", "thing")) # makes a new column for unique data from each DF

# WINNER!!
DF6 <- subset(DF2, !(DF2$date %in% DF1$date)) #creates a subset of data from DF2 that is NOT IN DF1
DF1 <- bind_rows(DF1,DF6) # bind new data from DF2 (stored as DF6) onto DF1


# Combines and dedups, NOT necessary as data should not change 
#staging_index <- dplyr::union(staging_index, forex_index) 

# I have no idea what I'm doing...
fx_tmp <- 
  tbl(fcon, "iso_currency") %>% 
  #inner_join("fx_rates_usd", by = '') %>% 
  filter(active %in% '1', 
         !iso_currency %in% c('BOV', 'CLF', 'COU', 'CUC', 'GBX', 
                              'ILX', 'UYW', 'UYI', 'ZWL', '999')) %>% 
  select(iso_currency, currency_desc) %>% 
  collect()  


# original query to do most of the work before data came to R
# decided to pull into R first, then do the work
forex_index <- dbGetQuery(fcon,
                         "SELECT 4789 as index_id, gi.[id] as granularity_item_id, fx.[date] as data_timestamp, exch_rate_usd as data_value 
                          FROM Factset.ref_v2.iso_currency_map AS cm
                          	INNER JOIN Staging.dbo.indx_granularity_item AS gi
                          		ON cm.currency_desc = gi.[Description] COLLATE SQL_Latin1_General_CP1_CI_AS
                          	INNER JOIN Factset.ref_v2.fx_rates_usd AS fx
                          		ON cm.iso_currency = fx.iso_currency
                          WHERE gi.granularity_level_id = 58 
                          AND cm.[iso_currency] NOT IN ('BOV', 'CLF', 'COU', 'CUC', 'GBX', 'ILX', 'UYW', 'UYI', 'ZWL', '999')
                          AND cm.[active] = '1' 
                          ORDER BY data_timestamp DESC
                         ")  # only pull active currencies and ignore 'unit of account', crazy unstable Zimbabwe currency, and cent currencies
