require(dplyr)
require(readr)
require(caret)
require(pROC)
require(lubridate)
require(leaps)




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
  mutate(sum_cases = ifelse(is.na(sum_cases),0,sum_cases),
         mean_cases = ifelse(is.na(mean_cases),0,mean_cases),
         median_cases = ifelse(is.na(median_cases),0,median_cases),
         sum_deaths = ifelse(is.na(sum_deaths),0,sum_deaths),
         mean_deaths = ifelse(is.na(mean_deaths),0,mean_deaths),
         median_deaths = ifelse(is.na(median_deaths),0,median_deaths),
         mean_retail = ifelse(is.na(mean_retail),0,mean_retail),
         median_retail = ifelse(is.na(median_retail),0,median_retail),
         mean_grocery = ifelse(is.na(mean_grocery),0,mean_grocery),
         median_grocery = ifelse(is.na(median_grocery),0,median_grocery),
         mean_parks = ifelse(is.na(mean_parks),0,mean_parks),
         median_parks = ifelse(is.na(median_parks),0,median_parks),
         mean_transit = ifelse(is.na(mean_transit),0,mean_transit),
         median_transit = ifelse(is.na(median_transit),0,median_transit),
         mean_work = ifelse(is.na(mean_work),0,mean_work),
         median_work = ifelse(is.na(median_work),0,median_work),
         mean_res = ifelse(is.na(mean_res),0,mean_res),
         median_res = ifelse(is.na(median_res),0,median_res)

         ) %>%
  pivot_wider(id_cols=c("nation","state","county","location_combined","fips", "population"),
              names_from=c("month"),
              values_from=c(#"mean_pop", "med_pop", 
                            "sum_cases", "mean_cases", "median_cases", "sum_deaths", "mean_deaths", "median_deaths",
                            "mean_retail", "median_retail", "mean_grocery", "median_grocery", "mean_parks", "median_parks", "mean_transit",
                            "median_transit", "mean_work", "median_work", "mean_res", "median_res")) %>%
  na.omit() # drops count from 2766 to 2689. something to look into?



df_agg$case_diff_7_8 = df_agg$sum_cases_8/df_agg$sum_cases_7-1
df_agg$case_diff_7_8[is.na(df_agg$case_diff_7_8)] = 1

anyNA(df_agg)


# Bad dumb model 1

lm1 = lm(case_diff_7_8~population+mean_retail_7+mean_grocery_7+mean_parks_7+mean_transit_7+mean_work_7+mean_res_7,df_agg)

summary(lm1)
plot(lm1)



# Simple forward/backward/stepwise setup

set.seed(1)

train.control = trainControl(method="cv",number=10)

step.model <- train(case_diff_7_8 ~ population+mean_retail_7+mean_grocery_7+mean_parks_7+mean_transit_7+mean_work_7+mean_res_7, 
                    data = df_agg,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, id=4)





