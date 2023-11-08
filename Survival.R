rm(list=ls())

library(tidyverse)
library(readxl)
library(here)

site.name <- c("Boujurian","Deslioures","Bernards","PraloA","PraloB","PraloC","PraloD")

i.site <- 3
for (i.site in 1 : 7) {
  # Read data sheet
  a <- read_xlsx("C:/Users/marco/Il mio Drive/Collab/Eryngium/Eryngium_alpinum_data/Data_brut.xlsx",site.name[i.site], na=c("","NA","0"),
                 col_types = "text")
  
  # Remove unnecessary columns
  a <- select(a, !2:6) # Select out 2000 to 2004 ID codes
  id.repro.1 <- grep("nb",colnames(a))[1] # First column of reproductive parameters
  a <- select(a, !id.repro.1 : ncol(a)) # Select out reproductive parameters
  rm(id.repro.1)
  
  # Populate ID numbers for those not having it
  which(is.na(a$num_2005))
  a$num_2005[which(is.na(a$num_2005))] <- paste0("nn",1 : length(which(is.na(a$num_2005))))
  # a$num_2005
  
  # Define unique ID: sitequad-num_2005
  a$ID <- paste0(a$quadrat,"-",a$num_2005)
  
  
  # Sometimes non-flowering adults (4) were incorrectly scored as juveniles (3) in the field, and vice-versa
  # The criterion is: juvenile has never flowered; non-flowering has flowered at least once.
  # Here we correct 3 --> 4 on the basis of individual flowering histories
  for (i in 1 : nrow(a)) {
    ind <- a[i,3:(ncol(a)-1)] # Individual history
    yJ <- which(ind==3) # Years as juveniles (not real years as 2001, 2002 etc, but just position in the vector)
    if(length(yJ) == 0) next
    firstF <- match("5",ind) # Year of first flowering (not real years as 2001, 2002 etc, but just position in the vector)
    if(is.na(firstF)) next
    if(any(yJ > firstF)) { # Is any year as juvenile after the year of first flowering
      a[i,yJ[which(yJ > firstF)]+2] <- "4" # plus 2 because the first two columns of a contains Quadrat and Individual number
      cat("Ind",a$ID[i],"Excel row",(i+1),"year",(yJ[which(yJ > firstF)]+1999),"\n")
    }
  }
  # correcting 4 --> 3 is harder bcs possible only for individuals with known birth 
  
  # Wide to long pivot with year on column
  num_years <- max(as.numeric(colnames(a)),na.rm=T) - 2000
  b <- pivot_longer(a,cols=c(3:(num_years + 3)),names_to="Year",values_to="State",names_transform=list(Year=as.numeric))
  # print(b,n=100)
  # summary(b)
  rm(num_years)
  
  # Left join fate
  b$Year_fate = b$Year + 1
  bb <- left_join(b,b,by=c("ID", "Year_fate"="Year"))
  bb %>% transmute(Site = site.name[i.site],
                   Quad = quadrat.x,
                   num_2005 = num_2005.x,
                   ID = ID,
                   Year = Year,
                   State = State.x,
                   Fate = State.y) -> bb1
  
  # filter out 6,NA on state (keep only 3,4 and 5)
  bb1 %>% filter(State %in% c(3,4,5)) -> bb2
  
  # survival: fate 3,4,5 = 1 and 6 = 0
  bb2 %>% mutate(fateSurv = case_when(Fate %in% c(3,4,5) ~ 1,
                                      Fate == 6 ~ 0)) -> bb3
  
  # Concatenate dataframes
  if (i.site == 1) data.surv <- bb3 else data.surv <- rbind(data.surv,bb3)
}
rm(ind,yJ,firstF,b,bb,bb1,bb2,bb3)

## remove NA and define factors
data.surv <- na.omit(data.surv)
data.surv$Site <- factor(data.surv$Site)
data.surv$Quad <- factor(data.surv$Quad)
data.surv$ID <- factor(data.surv$ID)
data.surv$Year <- factor(data.surv$Year)
data.surv$State <- factor(data.surv$State)
data.surv$fateSurv <- factor(data.surv$fateSurv)
summary(data.surv)

# Explanatory variables: ibutton-derived variables
# Read iButton data
v.site.ib <- c("DES","BER","BOU","PRA","PRB","PRC","PRD")
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
        summarise(iTmin = min(temp), iTmean=mean(temp), iTmax=max(temp)) ->
        idata # i-Button data
}

#### MI FERMO QUI. ANDARE AVANTI, CALCOLARE LE VARIABILI PER ANNO (DDAYS, TEMP MEDIE MARZO, APRILE, MAGGIO, GIUGNO ETC, NUMERO GIORNI DI FILA CON TEMP ALTE, TEMP BASSE ETC
### VEDERE SCRIPT IMPUTATION PER CODICE


# # analysis Bernoulli model, explanatory var: site * state * year
# m1 <- glm(fateSurv ~ Site * State * Year, data=data.surv, family=binomial())
# summary(m1)
# # Simplify model
# m2 <- step(m1)
# summary(m2)
# 
# # Predicted values
# newdata <- expand.grid(Site=factor(levels(data.surv$Site)),
#                        Year=factor(levels(data.surv$Year)),
#                        State=factor(levels(data.surv$State)))
# predSurv <- predict(m2,newdata,type="response",se.fit=T)
# newdata$surv <- predSurv$fit
# newdata$surv_low <- predSurv$fit - predSurv$se.fit
# newdata$surv_high <- predSurv$fit + predSurv$se.fit
# newdata$labels <- c("J","NF","F")[newdata$State]
# newdata$labels <- factor(newdata$labels,levels=c("J","NF","F"))
# # Plot
# ggplot(newdata,aes(y=surv,x=Year,ymin=surv_low,ymax=surv_high)) + 
#     geom_errorbar() +
#     geom_point() +
#     facet_grid(rows=vars(Site), cols=vars(labels), scales="fixed")
# 
# 
# 
# mydata <- newdata$surv
# # Ward Hierarchical Clustering
# d <- dist(mydata, method = "euclidean") # distance matrix
# fit <- hclust(d, method="ward.D2")
# plot(fit) # display dendogram
# groups <- cutree(fit, k=3) # cut tree into 5 clusters
# # draw dendogram with red borders around the 5 clusters
# rect.hclust(fit, k=3, border="red")     
# 
# newdata$groups <- factor(groups)
# ggplot(newdata,aes(y=surv,groups=groups)) +
#          geom_boxplot()
