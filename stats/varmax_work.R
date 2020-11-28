if(!require(MTS)){install.packages('MTS');require(MTS)}
if(!require(caret)){install.packages('caret');require(caret)}


# Use VARX function?

# should be able to work with something similar to df_agg


#data("mts-examples",package="MTS")
#gdp=log(qgdp[,3:5])
#zt=diffM(gdp)
#m1=VAR(zt,p=2)


df_agg2 = df_subset %>%
  group_by(fips) %>%
  arrange(fips,date) %>%
  mutate(cases_inc = cases - lag(cases,default=first(cases))) %>%
  mutate(deaths_inc = deaths - lag(deaths,default=first(deaths)))

#spot check that grouping worked print(df_agg2[df_agg2$fips == '10001' | df_agg2$fips == '10003',][c('fips','cases_inc')],n=500)

df_agg3 = df_subset %>% 
  group_by(fips) %>% 
  summarise(
    meanpop = mean(population, na.rm=TRUE),
    mean_cases = mean(cases, na.rm=TRUE)) %>%
  arrange(meanpop)

dim(df_agg3)[1]

keep_fips = df_agg3$fips[2500:dim(df_agg3)[1]]


df_agg_zt = df_agg2[c('fips','date', 'cases_inc')]

df_agg_zt_filt = df_agg_zt[df_agg_zt$fips %in% keep_fips,]

df_agg_xt = df_agg2[c('fips','date', 'mobility_retail_recreation_change','mobility_grocery_pharmacy_change','mobility_parks_change',
                      'mobility_transit_stations_change','mobility_workplaces_change','mobility_residential_change')]

df_agg_xt_filt = df_agg_xt[df_agg_xt$fips %in% keep_fips,]


#zt_df = df_agg_zt %>%
#  pivot_wider(id_cols=c("date"),
#              names_from=c("fips"),
#              values_from=c("cases_inc")) 


zt_df_filt = df_agg_zt_filt %>%
  pivot_wider(id_cols=c("date"),
              names_from=c("fips"),
              values_from=c("cases_inc")) 

xt_df_filt = df_agg_xt_filt %>%
  pivot_wider(id_cols=c("date"),
              names_from=c("fips"),
              values_from=c('mobility_retail_recreation_change','mobility_grocery_pharmacy_change','mobility_parks_change',
                            'mobility_transit_stations_change','mobility_workplaces_change','mobility_residential_change')) 

# some dimensionality problems still, I think

#zt_df_nonull = zt_df %>% 
#  select_if(~ !any(is.na(.)))

zt_df_nonull_filt = zt_df_filt %>% 
  select_if(~ !any(is.na(.)))

xt_df_nonull_filt = xt_df_filt %>% 
  select_if(~ !any(is.na(.)))


anyNA(xt_df_nonull_filt)


# A simpler case failed before, I think due to dimensionality, so this might too. Maybe need to agg to at least state level?

zt = zt_df_nonull[-1]

zt_filt = zt_df_nonull_filt[-1]
#zt_nonull = zt_df_nonull[-1]

xt_filt = xt_df_nonull_filt[-1]

remove = findCorrelation(
  zt,
  cutoff = 1,
  verbose = FALSE,
  names = FALSE
)

remove_filt = findCorrelation(
  zt_filt,
  cutoff = 1,
  verbose = FALSE,
  names = FALSE
)

remove_xt_filt = findCorrelation(
  xt_filt,
  cutoff = 1,
  verbose = FALSE,
  names = FALSE
)


zt_nocorr = zt[,-remove]
zt_filt_nocorr = zt_filt[,-remove_filt]
xt_filt_nocorr = xt_filt[,-remove_xt_filt]


varmatest = VARMA(zt_filt_nocorr)
vtest = VARX(zt_filt_nocorr,p=1,xt=xt_filt_nocorr)

#vtest2 = VARX(zt_nonull,1)





