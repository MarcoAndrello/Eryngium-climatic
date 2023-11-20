# Calculate climatic variables

rm(list=ls())

library(tidyverse)
library(readxl)
library(caret)
library(corrgram)

# Explanatory variables: ibutton-derived variables
# Read iButton data
v.site.ib <- c("DES","BER","BOU","PRA","PRB","PRC","PRD")

# Functions to calculate variables of interest
calc.ddays <- function(x) {
    x[x<1] <- 0
    sum(x)
}
calc.num_stress_periods <- function(x) {
    rle.year <- rle(x) 
    length(which(rle.year$length[which(rle.year$values==1)]>3))
}
calc.num_stress_days <- function(x) {
    rle.year <- rle(x) 
    # Number of days in each stressful period
    num_days_stressful_periods <- rle.year$length[which(rle.year$values==1)]
    # Number of days in each stressful period that is longer than 3 days
    num_days_stressful_period_above_thr_length <- num_days_stressful_periods[which(num_days_stressful_periods>3)]
    #Total number of days in stressful periods
    sum(num_days_stressful_period_above_thr_length)
}


xvar <- list()
snowmelt_date <- list()
i.site <- 1
for (i.site in 1 : 7) {
    data.ib <- read_xlsx(paste0(getwd(),"/Julie Chaumont 2022/",v.site.ib[i.site],".xlsx"))
    
    data.ib %>%
        # Concatenate date and heure column and convert it into lubridate::datetime
        mutate(dati = as_datetime(paste(data.ib$date, data.ib$heure), tz="Europe/Paris")) %>%
        # Keep only after 2013
        filter(dati >"2014-01-01 00:00:00") %>%
        # Delete unnecessary columns
        select(-c(date, heure, V2, source)) -> 
        data.ib
    
    data.ib %>%
        # Group by day
        group_by(DATE = as_date(dati)) %>%
        # Calculate minimal, mean and maximal daily temperature
        summarise(iTmin = min(temp), iTmean=mean(temp), iTmax=max(temp)) %>%
        # Add month and year columns
        mutate(month=month(DATE), year=year(DATE)) ->
        idata # i-Button data
    
    
    # First occurrence of two snow-free days after march 15th
    snowmelt_date[[i.site]] <- vector()
    i.year <- 1
    for (i.year in 1 : 8) {
        march_year <- filter(idata, year == 2013+i.year, month %in% c(3:12))
        if (nrow(march_year) == 0) next
        for (i.day in  1 : 120) {
            if(march_year$iTmean[i.day] + march_year$iTmean[i.day+1] > 2) {
                snowmelt_date[[i.site]][i.year] <- as.character(march_year$DATE[i.day])
                break
            }
            
        }
        cat("Site",v.site.ib[i.site],"Year",2013+i.year,"snow melts at day",i.day,"\n")
        # print(march_year, n = 60)
    }
    snowmelt_date[[i.site]] %>% as_date -> snowmelt_date[[i.site]]
    
    ivar_annual_temp <- list()
    i.year <- 1
    for (i.year in 1:8){
            end_date <- snowmelt_date[[i.site]][i.year] + 120
        filter(idata, year == 2013+i.year, DATE >= snowmelt_date[[i.site]][i.year] & DATE <= end_date) %>%
            mutate(s_Tmin_10 = ifelse(iTmin > 10,1,0),
                   s_Tmin_15 = ifelse(iTmin > 15,1,0),
                   s_Tmax_25 = ifelse(iTmax > 25,1,0),
                   s_Tmax_30 = ifelse(iTmax > 30,1,0)) -> idata_annual
        if (nrow(idata_annual) == 0) next
        idata_annual %>%
            summarise(Year = 2013+i.year,
                      ddays = calc.ddays(iTmean),
                      num_sp_Tmin_10 = calc.num_stress_periods(s_Tmin_10),
                      num_sp_Tmin_15 = calc.num_stress_periods(s_Tmin_15),
                      num_sp_Tmax_25 = calc.num_stress_periods(s_Tmax_25),
                      num_sp_Tmax_30 = calc.num_stress_periods(s_Tmax_30),
                      num_sd_Tmin_10 = calc.num_stress_days(s_Tmin_10),
                      num_sd_Tmin_15 = calc.num_stress_days(s_Tmin_15),
                      num_sd_Tmax_25 = calc.num_stress_days(s_Tmax_25),
                      num_sd_Tmax_30 = calc.num_stress_days(s_Tmax_30)) -> ivar_annual_temp[[i.year]]
    }
    bind_rows(ivar_annual_temp) %>% mutate(Site = v.site.ib[i.site]) -> xvar[[i.site]]
}
rm(data.ib,idata)
# SI POTREBBE AGGIUNGERE ALTRE SOGLIE (5 GIORNI INVECE DI 3 ETC) 

xvar <- bind_rows(xvar)
xvar$Year <- factor(xvar$Year)
xvar$Site <- factor(xvar$Site)
save(xvar,file="xvar.RData") # per provare il BRT

# remove correlated variables
xvar %>% select(-c(Year,Site)) %>%
    cor(method="spearman",use = "na.or.complete") %>%
    findCorrelation(names=T,cutoff=0.5) -> var_names_toRemove
var_names_toRemove
xvar %>% select(!all_of(var_names_toRemove)) -> xvar_1
# Plot correlations
corrgram(xvar_1 %>% select(-c(Year, Site)),cor.method="spearman",upper.panel=panel.cor, lower.panel=panel.pts)
# tolgo anchge tmax30 e tmin10. per avere solo 2 variabili.
# poi vedremo di calcolare meglio una variabile di stress termico/drough, magari integrando anche i dati di piogge di safran
xvar_1 %>% select(-c(num_s_Tmin_10, num_s_Tmax_30)) -> xvar_1
summary(xvar_1)
xvar_1$Year <- factor(xvar_1$Year)
xvar_1$Site <- factor(xvar_1$Site)
save(xvar_1,file="xvar_1.RData")


### MI FERMO QUI





# # OLD VARIABLES
# # Create dataframe of ddays for each month, per year
# idata %>% group_by(year,month) %>% summarise(ddays = calc.ddays(iTmean)) %>% pivot_wider(names_from=month, values_from=ddays) -> ddays
# # Create dataframe of mean Temperature for each month, per year
# idata %>% group_by(year,month) %>% summarise(meanT = mean(iTmean)) %>% pivot_wider(names_from=month, values_from=meanT) -> meanT
# # Create dataframe of number of stress period per year (number of periods with more than 3 "hot" consecutive days; "hot" defined in 4 ways)
# idata %>% filter(month %in% c(6,7,8)) %>% mutate(s_Tmin_10 = ifelse(iTmin > 10,1,0),
#                                                  s_Tmin_15 = ifelse(iTmin > 15,1,0),
#                                                  s_Tmax_25 = ifelse(iTmax > 25,1,0),
#                                                  s_Tmax_30 = ifelse(iTmax > 30,1,0)) %>% 
#     group_by(year) %>% summarise(num_s_Tmin_10 = calc.num_stress_periods(s_Tmin_10),
#                                  num_s_Tmin_15 = calc.num_stress_periods(s_Tmin_15),
#                                  num_s_Tmax_25 = calc.num_stress_periods(s_Tmax_25),
#                                  num_s_Tmax_30 = calc.num_stress_periods(s_Tmax_30)) -> num_s
# # Join the three dataframes
# ddays %>% left_join(meanT,by="year",suffix=c(".ddays",".meanT")) %>% left_join(num_s,by="year") %>% mutate(site=v.site.ib[i.site]) -> xvar[[i.site]]
# xvar %>% bind_rows %>% ungroup -> xvar
# # remove winter months
# xvar %>% select(-c("1.ddays","2.ddays","3.ddays","9.ddays","10.ddays","11.ddays","12.ddays",
#                    "1.meanT","2.meanT","3.meanT","9.meanT","10.meanT","11.meanT","12.meanT")) -> xvar
# # remove highly correlated variables
# xvar %>% select(-c(year,site)) %>%
#     cor(method="spearman",use = "na.or.complete") %>%
#     findCorrelation(names=T,cutoff=0.5) -> var_names_toRemove
# var_names_toRemove
# xvar %>% select(!all_of(var_names_toRemove)) -> xvar_1
# # Plot correlations
# corrgram(xvar_1 %>% select(-year),cor.method="spearman",upper.panel=panel.cor)
# save(xvar_1,file="xvar_1.RData")
# 
