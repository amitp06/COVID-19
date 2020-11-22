if(!require(MTS)){install.packages('MTS');require(MTS)}


# Use VARX function?

# should be able to work with something similar to df_agg


data("mts-examples",package="MTS")
gdp=log(qgdp[,3:5])
zt=diffM(gdp)
m1=VAR(zt,p=2)

head(df_agg)


df_agg_zt = df_agg[c('fips','end_cases_2','end_cases_3','end_cases_4','end_cases_5','end_cases_6','end_cases_7','end_cases_8')]

df_agg_zt['end_cases_3_2'] = df_agg_zt['end_cases_3'] - df_agg_zt['end_cases_2'] 
df_agg_zt['end_cases_4_3'] = df_agg_zt['end_cases_4'] - df_agg_zt['end_cases_3'] 
df_agg_zt['end_cases_5_4'] = df_agg_zt['end_cases_5'] - df_agg_zt['end_cases_4'] 
df_agg_zt['end_cases_6_5'] = df_agg_zt['end_cases_6'] - df_agg_zt['end_cases_5'] 
df_agg_zt['end_cases_7_6'] = df_agg_zt['end_cases_7'] - df_agg_zt['end_cases_6'] 
df_agg_zt['end_cases_8_7'] = df_agg_zt['end_cases_8'] - df_agg_zt['end_cases_7'] 

df_agg_zt = df_agg_zt[-c(2:8)]

#pivot_longer(df_agg_zt,names_to='fips')
#melt(df_agg_zt,id.vars='fips')

df_agg_ztdf = as.data.frame(df_agg_zt)

row.names(df_agg_ztdf) = df_agg_ztdf$fips


df_agg_ztdf = df_agg_ztdf[-1]
df_agg_ztmat = as.matrix(df_agg_ztdf)
zt = t(df_agg_ztmat)



vtest = VARX(zt,1)


