# https://cran.rstudio.com/web/packages/sweep/vignettes/SW01_Forecasting_Time_Series_Groups.html
# Going to use this link to get started

# ugh, this link is only partially helpful, need to figure out how to bring other predictors into it

if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(tidyquant)){install.packages('tidyquant');require(tidyquant)}
if(!require(timetk)){install.packages('timetk');require(timetk)}
if(!require(sweep)){install.packages('sweep');require(sweep)}
if(!require(forecast)){install.packages('forecast');require(forecast)}
if(!require(zoo)){install.packages('zoo');require(zoo)}
if(!require(vctrs)){install.packages('vctrs');require(vctrs)}
if(!require(purrr)){install.packages('purrr');require(purrr)}

# just do a sort on state and fips, may need to eventually drop fips

organized <- df_subset %>%
  group_by(state, fips, date)

# Nest into tables

organized_nest <- organized  %>%
  group_by(state, fips) %>%
  nest()

# mutate and map

organized_nest_ts <- organized_nest %>%
  mutate(data.ts = map(.x       = data, 
                       .f       = tk_ts, 
                       select   = -date, 
                       start    = 1,
                       freq     = 7)) %>%
  select(-data)



organized_unnest_ts <- organized_nest_ts %>%
  pmap_dfr(data.frame)





