# Imputation of iButton data from SAFRAN and CROCUS

rm(list=ls())

library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(MuMIn)

v.site <- c("DES","BER","BOU","PraloA","PraloB","PraloC","PraloD")
v.site.ib <- c("DES","BER","BOU","PRA","PRB","PRC","PRD")

load("SAFRAN_Variables.RData")
load("CrocusVariables.RData")

i.site <- 1

SafranVariables %>%
    # Convert to Celsius
    mutate(Tmin = Tmin - 273.15, Tmean = Tmean - 273.15, Tmax = Tmax - 273.15) %>%
    # Convert to lubridate::date
    mutate(DATE = as_date(DATE)) %>%
    # Keep only DES after 1999
    filter(idplot == v.site[i.site], DATE > "1999-12-31") %>%
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
    # Keep only DES after 2013
    filter(idplot == v.site[i.site], DATE > "1999-12-31") %>%
    # Keep only interesting variables
    select(DATE, TG1, TG4, DSN_T_ISBA) %>%
    # Make tibble
    as_tibble() ->
    crdata # Crocus data
    
# Read iButton data
data.ib <- read_xlsx(here("..","Julie Chaumont 2022",paste0(v.site.ib[i.site],".xlsx")))

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
    summarise(iTmin = min(temp), iTmean=mean(temp), iTmax=max(temp)) ->
    idata # i-Button data

# Combine the three datasets
left_join(sdata,crdata,by="DATE") %>%
    left_join(idata,by="DATE") %>%
    # Add Year and Month column
    mutate(Year=factor(year(DATE)), Month=factor(month(DATE))) %>%
  filter(Month %in% c(5:7)) -> data
#summary(data)

# Prepare data for imputation
data.imp <- na.omit(data)

# Fit model to 2013-2018 dataset
mfull <- lm(iTmean ~ Tmin + Tmean + Tmax + Rainf + Snowf + TG1 + TG4, data.imp, na.action=na.fail)
summary(mfull)

# Model selection
d <- dredge(mfull)
d

## Taking the best model
mbest <- get.models(d,1)[[1]]
summary(mbest)
plot(data.imp$iTmean,predict(mbest))
abline(a=0,b=1)

# ## With model avergaing
# mavg <- model.avg(get.models(dredge(mfull),subset=T))
# summary(mavg)
# plot(data.imp$iTmean,predict(mavg))
# abline(a=0,b=1)

# Create newdata
data -> newdata

# Predict newdata
pTmean <- predict(mbest, newdata, se.fit=T) 
newdata$pTmean <- pTmean$fit
newdata$pTmean_SE <- pTmean$se.fit

# Calculate variables of interest
calc.ddays <- function(x) {
  x[x<0] <- 0
  sum(x)
}

newdata %>% select(DATE,Year,Month,iTmean,pTmean,pTmean_SE) -> data_imputed
# Remove rows where pTmean is NA
data_imputed <- data_imputed[-which(is.na(data_imputed$pTmean)),] # BEWARE OF REMOVING ONLY SOME DAYS--> DDAYS ARE BIASED?

# Calculate Tmean and ddays on the years with iButton
data_imputed %>%
    group_by(Year) %>%
    summarise(Tmean = mean(iTmean),
              ddays = calc.ddays(iTmean)) -> data_annual_iButton


# Then do several time the drawing from a normal with mean = pTmean and sigma = pTmean_SE...
data_annual_temp <- list()
for (i.draw in 1:100) {
    # Draw from Gaussian
    data_imputed$draw_pTmean <- rnorm(nrow(data_imputed), data_imputed$pTmean, data_imputed$pTmean_SE)
    # Calculate mean temperature and growing degree days per year
    data_imputed %>% 
        group_by(Year) %>%
        summarise(Tmean = mean(draw_pTmean),
                  ddays = calc.ddays(draw_pTmean)) -> data_annual_temp[[i.draw]]
}

# Unlisting
data_annual <- data_annual_temp[[1]]
for (i.list in 2: length(data_annual_temp)) {
  data_annual <- rbind(data_annual,
                       data_annual_temp[[i.list]])
}

# Summarising
data_annual %>% group_by(Year) %>%
    summarise(Tmean_min = min(Tmean),
              Tmean_mean = mean(Tmean),
              Tmean_max = max(Tmean),
              ddays_min = min(ddays),
              ddays_mean = mean(ddays),
              ddays_max = max(ddays)) %>%
    left_join(data_annual_iButton) %>%
    mutate(Tmean_iButton = Tmean,
           ddays_iButton = ddays) %>%
    select(-Tmean, -ddays) -> data_annual_stat

# Degree days May-July
# ddays_plot <- 
ggplot(data_annual_stat, aes(x=Year)) +
    geom_col(aes(y=ddays_mean)) +
  geom_errorbar(aes(y=ddays_mean, ymin = ddays_min, ymax = ddays_max))
ggsave(ddays_plot)

ggplot(data_annual_stat, aes(x=Year)) +
    geom_col(aes(y=ddays_iButton))
ggplot(data_annual_stat, aes(x=ddays_iButton)) + 
    geom_errorbar(aes(y=ddays_mean, ymin = ddays_min, ymax = ddays_max))



