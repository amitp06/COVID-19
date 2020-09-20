require(dplyr)
require(readr)
require(caret)
require(pROC)


#treat all data as training for now

Xtrain = select(df_subset, -c('cases','deaths'))
Ytrain = select(df_subset, c('cases','deaths'))


#explore structure

head(Xtrain)
str(Xtrain) 
#Note: we can put the number of unique values with the data structure:
rbind(sapply(Xtrain,function(x){ length(unique(x))}),
      sapply(Xtrain,class))
str(Ytrain)
rbind(sapply(Ytrain,function(x){ length(unique(x))}),
      sapply(Ytrain,class))


# Check to see if there are any missing values in the training data
anyNA(Xtrain)

# Note - NAs appear to be related to Google mobility data
# Google says treat the "gaps" as true unknowns and don't assume it means places weren't busy 
# https://support.google.com/covid19-mobility/answer/9825414?hl=en&ref_topic=9822927
# Changes are measured in percent changes from baseline
# Consider an imputation based on that day of the week from surrounding weeks?
# or treat the NAs separately?

# view Missing for fun

ggplot_missing <- function(x){
  if(!require(reshape2)){warning('you need to install reshape2')}
  require(reshape2)
  require(ggplot2)
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
ggplot_missing(Xtrain)


# Interesting, lots of missing FIPs, the rest happens to be google data which is more expected.

print(Xtrain[is.na(Xtrain$fips) & Xtrain$state != 'Massachusetts' & Xtrain$state != 'Michigan' & Xtrain$state != 'Missouri' & Xtrain$state != 'Utah',])


#skip imputation for now

#Do we want state and county dummies at all? Will just do state for now.

XtrainFact = select(Xtrain, c('state'),
                       ) %>% 
  mutate_all(factor)

dummyModel = dummyVars(~ ., data = XtrainFact, fullRank = TRUE)

XtrainQualDummy = predict(dummyModel, XtrainFact)


XtrainQuan = select(Xtrain,date,population,mobility_retail_recreation_change,mobility_grocery_pharmacy_change,mobility_parks_change,
                    mobility_transit_stations_change,mobility_workplaces_change,mobility_residential_change)

XtrainFull = cbind(XtrainQualDummy, XtrainQuan)

XtrainQuan2 = select(Xtrain,population,mobility_retail_recreation_change,mobility_grocery_pharmacy_change,mobility_parks_change,
mobility_transit_stations_change,mobility_workplaces_change,mobility_residential_change)

XtrainFull2= cbind(XtrainQualDummy, XtrainQuan2)

XtrainFull3 = cbind(XtrainFact,XtrainQuan)


#corrplot

require(corrplot)
corrplot(cor(XtrainFull2), tl.cex = 0.5)





