df_agg_OOT = df_subset_OOT %>%
  mutate(month = month(date)) %>% 
  group_by(nation,state,county,location_combined,fips,month, population) %>% 
  summarise(#mean_pop = mean(population, na.rm=TRUE), 
    #med_pop = median(population, na.rm=TRUE),
    end_cases = last(cases,order_by=date),
    mean_cases = mean(cases, na.rm=TRUE),
    median_cases = median(cases, na.rm=TRUE),
    end_deaths = last(deaths,order_by=date),
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
  # mutate(end_cases = ifelse(is.na(end_cases),0,end_cases),
  #        mean_cases = ifelse(is.na(mean_cases),0,mean_cases),
  #        median_cases = ifelse(is.na(median_cases),0,median_cases),
  #        end_deaths = ifelse(is.na(end_deaths),0,end_deaths),
  #        mean_deaths = ifelse(is.na(mean_deaths),0,mean_deaths),
  #        median_deaths = ifelse(is.na(median_deaths),0,median_deaths),
  #        mean_retail = ifelse(is.na(mean_retail),0,mean_retail),
  #        median_retail = ifelse(is.na(median_retail),0,median_retail),
  #        mean_grocery = ifelse(is.na(mean_grocery),0,mean_grocery),
  #        median_grocery = ifelse(is.na(median_grocery),0,median_grocery),
  #        mean_parks = ifelse(is.na(mean_parks),0,mean_parks),
#        median_parks = ifelse(is.na(median_parks),0,median_parks),
#        mean_transit = ifelse(is.na(mean_transit),0,mean_transit),
#        median_transit = ifelse(is.na(median_transit),0,median_transit),
#        mean_work = ifelse(is.na(mean_work),0,mean_work),
#        median_work = ifelse(is.na(median_work),0,median_work),
#        mean_res = ifelse(is.na(mean_res),0,mean_res),
#        median_res = ifelse(is.na(median_res),0,median_res)
#        ) %>%
pivot_wider(id_cols=c("nation","state","county","location_combined","fips", "population"),
            names_from=c("month"),
            values_from=c(#"mean_pop", "med_pop", 
              "end_cases", "mean_cases", "median_cases", "end_deaths", "mean_deaths", "median_deaths",
              "mean_retail", "median_retail", "mean_grocery", "median_grocery", "mean_parks", "median_parks", "mean_transit",
              "median_transit", "mean_work", "median_work", "mean_res", "median_res")) %>%
  na.omit() # drops count from 2766 to 2689. something to look into?





#should have named this something else, maybe, but renaming incorrectly just so predict function works easily.

df_agg_OOT$case_diff_7_8 = df_agg$end_cases_9/df_agg$end_cases_8 - 1
df_agg_OOT$case_diff_7_8[is.na(df_agg$case_diff_7_8)] = df_agg$end_cases_9[is.na(df_agg$case_diff_7_8)] - 1

# Hardcoded county median as response variable cutoff but could be an arbitrary value
cutoff = 0.3409769
df_agg_OOT$high_growth_8 = factor(ifelse(df_agg$case_diff_7_8 > cutoff,'high_growth','low_growth'))
df_agg_OOT$high_growth_8 = relevel(df_agg$high_growth_8,ref='low_growth')

df_agg_OOT$retail_diff_7_8 = (df_agg$median_retail_9 - df_agg$median_retail_8)
df_agg_OOT$grocery_diff_7_8 = (df_agg$median_grocery_9 - df_agg$median_grocery_8)
df_agg_OOT$parks_diff_7_8 = (df_agg$median_parks_9 - df_agg$median_parks_8)
df_agg_OOT$transit_diff_7_8 = (df_agg$median_transit_9 - df_agg$median_transit_8)
df_agg_OOT$work_diff_7_8 = (df_agg$median_work_9 - df_agg$median_work_8)
df_agg_OOT$res_diff_7_8 = (df_agg$median_res_9 - df_agg$median_res_8)
# df_agg$ind_retail = as.factor(df_agg$median_retail_8 > median(df_agg$median_retail_8))
# df_agg$ind_grocery = as.factor(df_agg$median_grocery_8 > median(df_agg$median_grocery_8))
# df_agg$ind_parks = as.factor(df_agg$median_parks_8 > median(df_agg$median_parks_8))
# df_agg$ind_transit = as.factor(df_agg$median_transit_8 > median(df_agg$median_transit_8))
# df_agg$ind_work = as.factor(df_agg$median_work_8 > median(df_agg$median_work_8))
# df_agg$ind_res = as.factor(df_agg$median_res_8 > median(df_agg$median_res_8))

anyNA(df_agg_OOT)


















predict(experimental, newdata=df_agg_OOT)

