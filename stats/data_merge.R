if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(reshape2)){install.packages('reshape2');require(reshape2)}

df_Mobility <- read.csv("./stats/2020_US_Region_Mobility_Report.csv")

df_COVID_conf_US = read.csv("./csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

df_COVID_death_US = read.csv("./csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

# Following stuff is just goofing around

head(df_Mobility)

summary(df_Mobility)

table(df_Mobility[df_Mobility$sub_region_1=='Louisiana',]$sub_region_2)

head(df_COVID_conf_US)

head(df_COVID_death_US)

table(df_COVID_conf_US$FIPS)

# Curious about what Louisiana Parishes are listed as

table(df_COVID_conf_US[df_COVID_conf_US$Province_State=='Louisiana',]$Admin2)

#end Goofs


#stripping off the word "Parish" and "County". I am 70% sure there will be other things to fix here when joining.

df_Mobility$County_trim = word(df_Mobility$sub_region_2,1,-2)

#Need to do some date transposing before joining. 


df_COVID_conf_US_resh = melt(df_COVID_conf_US,id.vars=c("UID","iso2","iso3","code3","FIPS","Admin2","Province_State","Country_Region","Lat","Long_","Combined_Key"),na.rm=FALSE)

df_COVID_death_US_resh = melt(df_COVID_death_US,id.vars=c("UID","iso2","iso3","code3","FIPS","Admin2","Province_State","Country_Region","Lat","Long_","Combined_Key"),na.rm=FALSE)

# Next step is to parse some dates in both tables for the join





