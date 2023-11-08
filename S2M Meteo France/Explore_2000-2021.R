# Explore SAFRAN and CROCUS 2001-2021

rm(list=ls())

library(tidyverse)
library(lubridate)
library(here)
library(ade4)
library(corrgram)

# Set language to French for graphs
Sys.setlocale("LC_ALL", "French")

load("SAFRAN_Variables.RData")
load("CrocusVariables.RData")

v.site <- c("DES","BER","BOU","PraloA","PraloB","PraloC","PraloD")
i.site <- 1

SafranVariables %>%
    # Convert to Celsius
    mutate(Tmin = Tmin - 273.15, Tmean = Tmean - 273.15, Tmax = Tmax - 273.15) %>%
    # Convert to lubridate::date
    mutate(DATE = as_date(DATE)) %>%
    # Keep only one site between 2000 and 2021
    filter(idplot == v.site[i.site], DATE > "1999-12-31" & DATE < "2021-12-31") %>%
    # Keep only interesting variables
    select(DATE, Tmin, Tmean, Tmax, Rainf, Snowf) %>%
    # Make tibble
    as_tibble() ->
    sdata # Safran data

CrocusVariables %>%
    # Convert to Celsius
    mutate(TG1 = TG1 - 273.15, TG4 = TG4 - 273.15) %>%
    # Convert to lubridate::date
    mutate(DATE = as_date(X)) %>%
    # Keep only one site after 2013
    filter(idplot == v.site[i.site], DATE > "1999-12-31" & DATE < "2021-12-31") %>%
    # Keep only interesting variables
    select(DATE, TG1, TG4, DSN_T_ISBA) %>%
    # Make tibble
    as_tibble() ->
    crdata # Crocus data


# Combine the two datasets
left_join(sdata,crdata,by="DATE") %>%
    # Add Year and Month column
    mutate(Year=factor(year(DATE)), Month=factor(month(DATE))) -> 
    data
rm(sdata, crdata)

# Plot from March to July to June to identify Snowmelt
theme_set(theme_gray())
ggplot(filter(data, Month %in% c(3:5), Year %in% c(2000:2021)), aes(x=DATE)) +
    geom_line(aes(y=DSN_T_ISBA),col="red") +
    # geom_line(aes(y=Rainf),col="darkgreen") +
    # geom_line(aes(y=Snowf),col="darkblue") +
    facet_wrap(vars(Year), scales="free_x") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# First occurrence of two snow-free days after march 15th
snowmelt_date <- Date(length = 19)
for (i.year in 1 : 19) {
    march_year <- filter(data, Year == 1999+i.year, Month %in% c(3:12))
    for (i.day in  1 : 90) {
        if(march_year$DSN_T_ISBA[i.day] + march_year$DSN_T_ISBA[i.day+1] == 0) {
            snowmelt_date[i.year] <- march_year$DATE[i.day]
            break
        }
    }
}
rm(march_year)

# Soil temperature
ggplot(filter(data, Month %in% c(3:5), Year %in% c(2000:2021)), aes(x=DATE)) +
    geom_line(aes(y=TG4),col="red") +
    geom_line(aes(y=DSN_T_ISBA),col="black") +
    facet_wrap(vars(Year), scales="free_x") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# # Estimate for 2019 as mean of the other years
# as.Date(mean(yday(snowmelt_date)),origin="2018-12-31")

snowmelt_data_frame <- data.frame(Year=c(2000:2018),snowmelt_date)

# Calculate variables
calc.ddays <- function(x) {
    x[x<0] <- 0
    sum(x)
}

# Taking different periods:
 # - one month (30 days) after snowmelt
 # - two months (60 days) after snowmelt
 # - three months (90 days) after snowmelt
 # - four months (120 days) after snowmelt
 # - five months (150 days) after snowmelt
 # - July 31st as end of period
data_annual_temp <- list()
for (i.enddate in 1:6){
    data_annual_temp[[i.enddate]] <- list()
    for (i.year in 1:19){
        if (i.enddate <=5) {
            end_date <- snowmelt_date[i.year] + 30*i.enddate
        } else {
            end_date <- paste0(1999+i.year,"-07-31")
        }
        filter(data, Year == 1999+i.year, DATE >= snowmelt_date[i.year] & DATE <= end_date) %>%
            summarise(season = if (i.enddate <=5) paste0(i.enddate," months") else "July 31st",
                      Year = 1999+i.year,
                      Tmean = mean(TG4),
                      ddays = calc.ddays(TG4),
                      Rainf = sum(Rainf),
                      Snowf = sum(Snowf)) -> data_annual_temp[[i.enddate]][[i.year]]
    }
}

# Unlisting and storing in a dataframe
data_annual <- unlist(data_annual_temp,recursive=F)[[1]]
for (i.list in 2: length(unlist(data_annual_temp,recursive=F))) {
    data_annual <- rbind(data_annual,
                         unlist(data_annual_temp,recursive=F)[[i.list]])
}
rm(data_annual_temp)
data_annual <- left_join(data_annual,snowmelt_data_frame)

####

# Plot with different periods
ggplot(data_annual,aes(x=Year)) + 
  geom_col(aes(y=ddays)) +
  facet_wrap(facets=vars(season))
ggplot(data_annual,aes(x=Year)) + 
  geom_col(aes(y=Rainf)) +
  facet_wrap(facets=vars(season))

# Look at correlations between variables
# Degree days
data_annual %>% select(Year,season,ddays) %>%
  pivot_wider(names_from = season, values_from = ddays) ->
  table_ddays
corrgram(table_ddays[,c(2:7)],
         lower.panel=panel.pts,
         upper.panel=panel.cor)
# Rainfall
data_annual %>% select(Year,season,Rainf) %>%
  pivot_wider(names_from = season, values_from = Rainf) ->
  table_Rainf
corrgram(table_Rainf[,c(2:7)],
         lower.panel=panel.pts,
         upper.panel=panel.cor)



