require(dplyr)
require(readr)
require(caret)
require(pROC)
require(lubridate)






df_agg = df_subset %>%
  mutate(month = month(date)) %>% 
  group_by(nation,state,county,location_combined,fips,month, population) %>% 
  summarise(#mean_pop = mean(population, na.rm=TRUE), 
            #med_pop = median(population, na.rm=TRUE),
            sum_cases = sum(cases, na.rm=TRUE),
            mean_cases = mean(cases, na.rm=TRUE),
            median_cases = median(cases, na.rm=TRUE),
            sum_deaths = sum(deaths, na.rm=TRUE),
            mean_deaths = mean(deaths, na.rm=TRUE),
            median_deaths = median(deaths, na.rm=TRUE),
            mean_retail = mean(mobility_retail_recreation_change, na.rm=TRUE),
            median_retail = median(mobility_retail_recreation_change, na.rm=TRUE),
            mean_grocery = mean(mobility_grocery_pharmacy_change, na.rm=TRUE),
            median_grocery = median(mobility_grocery_pharmacy_change, na.rm=TRUE),
            mean_parks = mean(mobility_parks_change, na.rm=TRUE),
            median_parks = median(mobility_parks_change, na.rm=TRUE),
            mean_transit = mean(mobility_transit_stations_change, na.rm=TRUE),
            median_transit = median(mobility_transit_stations_change, na.rm=TRUE),
            mean_work = mean(mobility_workplaces_change, na.rm=TRUE),
            median_work = median(mobility_workplaces_change, na.rm=TRUE),
            mean_res = mean(mobility_residential_change, na.rm=TRUE),
            median_res = median(mobility_residential_change, na.rm=TRUE)
            ) %>%
  pivot_wider(id_cols=c("nation","state","county","location_combined","fips", "population"),
              names_from=c("month"),
              values_from=c(#"mean_pop", "med_pop", 
                            "sum_cases", "mean_cases", "median_cases", "sum_deaths", "mean_deaths", "median_deaths",
                            "mean_retail", "median_retail", "mean_grocery", "median_grocery", "mean_parks", "median_parks", "mean_transit",
                            "median_transit", "mean_work", "median_work", "mean_res", "median_res"))






