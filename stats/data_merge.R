if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(reshape2)){install.packages('reshape2');require(reshape2)}

# The file paths below assume your working directory is 'stats' (where this file is located)

df_Mobility = read.csv("../stats/2020_US_Region_Mobility_Report.csv")

df_COVID_conf_US = read.csv("../csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

df_COVID_death_US = read.csv("../csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

# Following stuff is just goofing around

# head(df_Mobility)
# 
# summary(df_Mobility)
# 
# table(df_Mobility[df_Mobility$sub_region_1=='Louisiana',]$sub_region_2)
# 
# head(df_COVID_conf_US)
# 
# head(df_COVID_death_US)
# 
# table(df_COVID_conf_US$FIPS)
# 
# # Curious about what Louisiana Parishes are listed as
# 
# table(df_COVID_conf_US[df_COVID_conf_US$Province_State=='Louisiana',]$Admin2)

#end Goofs


#stripping off the word "Parish" and "County". I am 70% sure there will be other things to fix here when joining.

#df_Mobility$County_trim = word(df_Mobility$sub_region_2,1,-2)

#Need to do some date transposing before joining. 

df_COVID_conf_US_resh = melt(df_COVID_conf_US,id.vars=c("UID","iso2","iso3","code3","FIPS","Admin2","Province_State","Country_Region","Lat","Long_","Combined_Key"),na.rm=FALSE)

df_COVID_death_US_resh = melt(df_COVID_death_US,id.vars=c("UID","iso2","iso3","code3","FIPS","Admin2","Province_State","Country_Region","Lat","Long_","Combined_Key","Population"),na.rm=FALSE)

# Next step is to parse some dates in both tables for the join
# Parsing all dates as date_parsed, dropping raw date, renaming associated value

df_Mobility$date_parsed = as.Date(df_Mobility$date,format='%Y-%m-%d')

df_COVID_conf_US_resh$date_parsed = str_sub(str_replace_all(as.character(df_COVID_conf_US_resh$variable),'\\.','/'),start=2)
df_COVID_conf_US_resh$date_parsed = as.Date(df_COVID_conf_US_resh$date_parsed,format='%m/%d/%y')
df_COVID_conf_US_resh = subset(df_COVID_conf_US_resh,select=-variable)
names(df_COVID_conf_US_resh)[names(df_COVID_conf_US_resh)=="value"] = "cases"

df_COVID_death_US_resh$date_parsed = str_sub(str_replace_all(as.character(df_COVID_death_US_resh$variable),'\\.','/'),start=2)
df_COVID_death_US_resh$date_parsed = as.Date(df_COVID_death_US_resh$date_parsed,format='%m/%d/%y')
df_COVID_death_US_resh = subset(df_COVID_death_US_resh,select=-variable)
names(df_COVID_death_US_resh)[names(df_COVID_death_US_resh)=="value"] = "deaths"

# Joining all three tables using the 'conf' table as the base
# First join is between the time series DFs on all variables available since it's the same source
# The first inner join causes a loss of 2.3% of rows (small county diffs)
# Second join is between the new time series DF and the mobility DF on FIPS and Date 
# The second inner join causes a loss of 17.6% (small county and time period diffs)

df_intermediate = inner_join(df_COVID_conf_US_resh,df_COVID_death_US_resh)
df_intermediate$FIPS = as.character(df_intermediate$FIPS)
df_Mobility$census_fips_code = as.character(df_Mobility$census_fips_code)
df_merged = inner_join(df_intermediate,df_Mobility,by=c('date_parsed'='date_parsed',"FIPS"='census_fips_code'))

# I'll select and rename columns for all further analysis while leaving the joined table alone

df_subset = subset(df_merged,
                   select=c(Country_Region,Province_State,Admin2,Combined_Key,FIPS,date_parsed,Population,cases,deaths,
                            retail_and_recreation_percent_change_from_baseline,grocery_and_pharmacy_percent_change_from_baseline,
                            parks_percent_change_from_baseline,transit_stations_percent_change_from_baseline,
                            workplaces_percent_change_from_baseline,residential_percent_change_from_baseline))

names(df_subset) = c('nation','state','county','location_combined','fips','date','population','cases','deaths',
                     'mobility_retail_recreation_change','mobility_grocery_pharmacy_change','mobility_parks_change',
                     'mobility_transit_stations_change','mobility_workplaces_change','mobility_residential_change')
