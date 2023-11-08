# Compare SAFRAN and CROCUS with iButton

rm(list=ls())

library(tidyverse)
library(lubridate)
library(here)
library(readxl)

# Set language to French for graphs
Sys.setlocale("LC_ALL", "French")

v.site <- c("DES","BER","BOU","PraloA","PraloB","PraloC","PraloD")
v.site.ib <- c("DES","BER","BOU","PRA","PRB","PRC","PRD")

load("SAFRAN_Variables.RData")
load("CrocusVariables.RData")

for(i.site in 1 : 7) {
    
    SafranVariables %>%
        # Convert to Celsius
        mutate(Tmin = Tmin - 273.15, Tmean = Tmean - 273.15, Tmax = Tmax - 273.15) %>%
        # Convert to lubridate::date
        mutate(DATE = as_date(DATE)) %>%
        # Keep only DES after 2013
        filter(idplot == v.site[i.site], DATE > "2013-12-31") %>%
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
        filter(idplot == v.site[i.site], DATE > "2013-12-31") %>%
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
        mutate(Year=factor(year(DATE)), Month=factor(month(DATE))) -> 
        cdata
    
    
    theme_set(theme_gray())
    
    # Plot snow
    
    snowplot <- ggplot(filter(cdata, Year %in% c(2014:2018), Month %in% c(3:5)), aes(x=DATE)) +
        geom_line(aes(y=TG4,col="Temp 8 cm deep")) +
        geom_line(aes(y=DSN_T_ISBA*10,col="Snow height")) +
        geom_line(aes(y=iTmean, col="Temp (ibutton)")) +
        # Set axis and secondary axis
        scale_y_continuous(name="Temp",breaks=c(0,10,20,30), minor_breaks=NULL, limits=c(0,25),
                           sec.axis = sec_axis(~./10, name="Snow height")) +
        scale_colour_manual(name="",
                            breaks=c("Snow height","Temp 8 cm deep","Temp (ibutton)"),
                            values=c("Snow height"="orange",
                                     "Temp 8 cm deep"="cyan",
                                     "Temp (ibutton)" = "blue"))+
        # Panels by year
        facet_grid(cols=vars(Year), scales="free_x") +
        # Add title
        ggtitle(v.site[i.site]) +
        # Adjust axis and legend text
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.text = element_text(size=12),
              legend.title = element_text(size=12),
              legend.position="none",
              plot.title = element_text(hjust=0.5))
    ggsave(filename=paste0("Snow_",v.site[i.site],".png"),plot=snowplot,width=20,height=5,units="cm",dpi=300)  
}

###

# Plot Temperature 

tempplot <- ggplot(filter(cdata, Month==c(6,7)), aes(x=DATE)) +
    # Three time-series:
    geom_line(aes(y=iTmean, col="ibutton")) +      # ibutton
    geom_line(aes(y=TG4,col="Crocus 8 cm deep")) + # Crocus
    # Set axis scale and line colurs
    scale_y_continuous(breaks=c(0,10,20,30), minor_breaks=NULL, limits=c(0,25)) +
    scale_colour_manual(name="Mean daily temperature",
                        values=c("ibutton" = "blue",
                                 "Crocus 8 cm deep"="darkgreen")) +
    # Panels by year
    facet_wrap(vars(Year), scales="free_x") +
    # Adjust axis and legend text
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12))
tempplot

tempplot + 
    geom_line(aes(y=Tmean,col="Safran 1.5 m")) +   # Safran
    scale_colour_manual(name="Mean daily temperature",
                        breaks=c("ibutton","Crocus 8 cm deep","Safran 1.5 m"),
                        values=c("ibutton" = "blue",
                                 "Crocus 8 cm deep"="darkgreen",
                                 "Safran 1.5 m"="purple"))


# Plot correlation in July
ggplot(filter(cdata, Month==7)) +
    geom_point(aes(x=Tmean,y=iTmean,col=Year))
cor.test(~ Tmean + iTmean, data = filter(cdata, Month==7))
cor.test(~ Tmean + iTmean, data = filter(cdata, Year == 2014, Month==7))
cor.test(~ Tmean + iTmean, data = filter(cdata, Year == 2015, Month==7))
cor.test(~ Tmean + iTmean, data = filter(cdata, Year == 2016, Month==7))
cor.test(~ Tmean + iTmean, data = filter(cdata, Year == 2017, Month==7))
cor.test(~ Tmean + iTmean, data = filter(cdata, Year == 2018, Month==7))
cor.test(~ Tmean + iTmean, data = filter(cdata, Year == 2019, Month==7))


# Plot temporal trend
ggplot(cdata, aes(x=DATE)) +
    geom_line(aes(y=Tmean)) +
    geom_line(aes(y=iTmean),col="blue")

# Plot temporal trend
ggplot(filter(cdata, DATE > "2014-05-31" & DATE < "2014-11-01"), aes(x=DATE)) +
    geom_line(aes(y=Tmean)) +
    geom_line(aes(y=iTmean),col="blue")



# Plot temporal trend
ggplot(crdata, aes(x=crDATE)) +
    geom_line(aes(y=DSN_T_ISBA)) +
    geom_line(aes(y=TG1),col="green") +
    geom_line(aes(y=TG4),col="blue")

# Concatenate with the other data
cdata <- as_tibble(cbind(cdata,crdata))

ggplot(cdata, aes(x=crDATE)) +
    geom_line(aes(y=DSN_T_ISBA)) +
    geom_line(aes(y=iTmean),col="blue") + # ibutton
    geom_line(aes(y=TG4),col="darkblue")  # Crocus


