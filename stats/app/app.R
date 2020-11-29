if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(reshape2)){install.packages('reshape2');require(reshape2)}
require(dplyr)
require(readr)
require(caret)
require(pROC)
require(PRROC)
require(lubridate)
require(leaps)
library(shiny)

df_Mobility = read.csv("2020_US_Region_Mobility_Report.csv")
df_COVID_conf_US = read.csv("time_series_covid19_confirmed_US.csv")
df_COVID_death_US = read.csv("time_series_covid19_deaths_US.csv")

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

df_intermediate = inner_join(df_COVID_conf_US_resh,df_COVID_death_US_resh,na_matches='never')
df_intermediate$FIPS = as.character(df_intermediate$FIPS)
df_Mobility$census_fips_code = as.character(df_Mobility$census_fips_code)
df_merged = inner_join(df_intermediate,df_Mobility,by=c('date_parsed'='date_parsed',"FIPS"='census_fips_code'),na_matches='never')

# I'll select and rename columns for all further analysis while leaving the joined table alone

df_subset = subset(df_merged,
                   select=c(Country_Region,Province_State,Admin2,Combined_Key,FIPS,date_parsed,Population,cases,deaths,
                            retail_and_recreation_percent_change_from_baseline,grocery_and_pharmacy_percent_change_from_baseline,
                            parks_percent_change_from_baseline,transit_stations_percent_change_from_baseline,
                            workplaces_percent_change_from_baseline,residential_percent_change_from_baseline))

names(df_subset) = c('nation','state','county','location_combined','fips','date','population','cases','deaths',
                     'mobility_retail_recreation_change','mobility_grocery_pharmacy_change','mobility_parks_change',
                     'mobility_transit_stations_change','mobility_workplaces_change','mobility_residential_change')


df_subset_OOT = df_subset[df_subset['date'] > '2020-07-31',]

df_subset = df_subset[df_subset['date'] <= '2020-08-31',]

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

choices = c('Retail and Recreation Mobility', 'Grocery and Pharmacy Mobility', 'Parks Mobility',
            'Transit Stations Mobility', 'Workplace Mobility', 'Residential Mobility')

df_agg$case_diff_7_8 = df_agg$end_cases_8/df_agg$end_cases_7 - 1
df_agg$case_diff_7_8[is.na(df_agg$case_diff_7_8)] = df_agg$end_cases_8[is.na(df_agg$case_diff_7_8)] - 1

df_agg2 = df_agg[,c('mean_retail_7', 'mean_grocery_7', 'mean_parks_7', 'mean_transit_7','mean_work_7', 'mean_res_7','case_diff_7_8')]

choicelookup = as.data.frame(cbind(choices, c('mean_retail_7', 'mean_grocery_7', 'mean_parks_7', 'mean_transit_7','mean_work_7', 'mean_res_7')))

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId='Category',
                  label = 'Mobilty Category',
                  choices = choices,
                  selected=choices[1])
    ),
    mainPanel(
      plotOutput("Category")
    )
  )
)

server <- function(input, output) {
  output$Category <- renderPlot({
    plot(data.frame(df_agg2[,choicelookup[choicelookup$choices==input$Category,]$V2],df_agg2[,"case_diff_7_8"]*100),
         xlab="Mobility Change for Selected Category (July 2020 Mean)", ylab = "Percent Change in Cases (July-August 2020)",main='COVID Case Growth by Mobility Category')
  })
  
}

shinyApp(ui = ui, server = server)