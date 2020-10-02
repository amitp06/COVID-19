require(dplyr)
require(readr)
require(caret)
require(pROC)
require(PRROC)
require(lubridate)
require(leaps)

df_agg = df_subset %>%
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

df_agg$case_diff_7_8 = df_agg$end_cases_8/df_agg$end_cases_7 - 1
df_agg$case_diff_7_8[is.na(df_agg$case_diff_7_8)] = df_agg$end_cases_8[is.na(df_agg$case_diff_7_8)] - 1

# Hardcoded county median as response variable cutoff but could be an arbitrary value
cutoff = 0.3409769
df_agg$high_growth_8 = factor(ifelse(df_agg$case_diff_7_8 > cutoff,'high_growth','low_growth'))
df_agg$high_growth_8 = relevel(df_agg$high_growth_8,ref='low_growth')

df_agg$retail_diff_7_8 = (df_agg$median_retail_8 - df_agg$median_retail_7)
df_agg$grocery_diff_7_8 = (df_agg$median_grocery_8 - df_agg$median_grocery_7)
df_agg$parks_diff_7_8 = (df_agg$median_parks_8 - df_agg$median_parks_7)
df_agg$transit_diff_7_8 = (df_agg$median_transit_8 - df_agg$median_transit_7)
df_agg$work_diff_7_8 = (df_agg$median_work_8 - df_agg$median_work_7)
df_agg$res_diff_7_8 = (df_agg$median_res_8 - df_agg$median_res_7)
# df_agg$ind_retail = as.factor(df_agg$median_retail_8 > median(df_agg$median_retail_8))
# df_agg$ind_grocery = as.factor(df_agg$median_grocery_8 > median(df_agg$median_grocery_8))
# df_agg$ind_parks = as.factor(df_agg$median_parks_8 > median(df_agg$median_parks_8))
# df_agg$ind_transit = as.factor(df_agg$median_transit_8 > median(df_agg$median_transit_8))
# df_agg$ind_work = as.factor(df_agg$median_work_8 > median(df_agg$median_work_8))
# df_agg$ind_res = as.factor(df_agg$median_res_8 > median(df_agg$median_res_8))

anyNA(df_agg)

# Bad dumb model 1

lm1 = lm(case_diff_7_8~population+mean_retail_7+mean_grocery_7+mean_parks_7+mean_transit_7+mean_work_7+mean_res_7,df_agg)
summary(lm1)
plot(lm1)

# Simple forward/backward/stepwise setup

set.seed(1)

train.control = trainControl(method="cv",number=10)

step.model <- train(case_diff_7_8 ~ population + retail_diff_7_8 + grocery_diff_7_8 + parks_diff_7_8
                    + transit_diff_7_8 + work_diff_7_8 + res_diff_7_8 + mean_retail_8 + mean_grocery_8
                    + mean_parks_8 + mean_transit_8 + mean_work_8 + mean_res_8, 
                    data = df_agg,
                    method = "leapBackward", 
                    #tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, id=nrow(summary(step.model)$which))

# Save some stepwise objects for quick reference

stepwise = as.data.frame(summary(step.model)$which)
stepwise_names = tail(names(stepwise[,stepwise[nrow(stepwise),]==TRUE]),-1)
stepwise_formula = as.formula(paste('case_diff_7_8~',paste(stepwise_names,collapse="+"),sep=""))
stepwise_lm = lm(stepwise_formula,data=df_agg)
summary(stepwise_lm)

# Train experimental classifiers and check performance

set.seed(1)
train_experimental = trainControl(method="cv",number=10,classProbs=TRUE,savePredictions=TRUE)
experimental = train(high_growth_8 ~ population + retail_diff_7_8 + grocery_diff_7_8 + parks_diff_7_8
                     + transit_diff_7_8 + work_diff_7_8 + res_diff_7_8 + mean_retail_8 + mean_grocery_8
                     + mean_parks_8 + mean_transit_8 + mean_work_8 + mean_res_8, 
                    data = df_agg,
                    method = 'glmStepAIC',
                    trControl = train_experimental
)
summary(experimental)

# Get confusion matrix stats
confusionMatrix(experimental)
precision = confusionMatrix(experimental)$table[2,2]/sum(confusionMatrix(experimental)$table[2,])
recall = confusionMatrix(experimental)$table[2,2]/sum(confusionMatrix(experimental)$table[,2])
precision;recall

# Get ROC stats

roc_curve = roc(experimental$pred$obs,experimental$pred$high_growth)
plot(roc_curve)
roc_curve$auc

# Get calibration stats
calibration = calibration(experimental$pred$obs~experimental$pred$high_growth,cuts=5,class='high_growth')
xyplot(calibration)

# Save model candidate (only use when you want to keep a model for faster loading in a future script)
# saveRDS(experimental,'logistic_selected.rds')