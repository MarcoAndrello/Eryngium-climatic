# Create demographic dataset

rm(list=ls())

library(tidyverse)
library(readxl)

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
  # which(is.na(a$num_2005))
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
  
  # survival: fate 3,4,5 = 1; fate 6 = 0
  bb2 %>% mutate(fateSurv = case_when(Fate %in% c(3,4,5) ~ 1,
                                      Fate == 6 ~ 0)) -> bb_surv
  
  # flowering: fate 5 = 1; fate 3,4 = 0; fate 6 = 0 (effectively NA)
  bb2 %>% mutate(fateFlow = case_when(Fate == 5 ~ 1,
                                      Fate == 6 ~ NA,
                                      Fate %in% c(3,4,6) ~ 0)) -> bb_flow
  
  # Concatenate dataframes
  if (i.site == 1) data.surv <- bb_surv else data.surv <- rbind(data.surv,bb_surv)
  if (i.site == 1) data.flow <- bb_flow else data.flow <- rbind(data.flow,bb_flow)
}
rm(a,ind,yJ,firstF,b,bb,bb1,bb2,bb_surv, bb_flow)

## remove NA and define factors
data.surv <- na.omit(data.surv)
data.surv$Site <- factor(data.surv$Site)
data.surv$Quad <- factor(data.surv$Quad)
data.surv$ID <- factor(data.surv$ID)
data.surv$Year <- factor(data.surv$Year)
data.surv$State <- factor(data.surv$State)
data.surv$fateSurv <- factor(data.surv$fateSurv)
summary(data.surv)
data.surv$Site <- plyr::mapvalues(data.surv$Site,levels(data.surv$Site),c("BER","BOU","DES","PRA","PRB","PRC","PRD"))
save(data.surv,file="data.surv.RData")

## remove NA and define factors
data.flow <- na.omit(data.flow)
data.flow$Site <- factor(data.flow$Site)
data.flow$Quad <- factor(data.flow$Quad)
data.flow$ID <- factor(data.flow$ID)
data.flow$Year <- factor(data.flow$Year)
data.flow$State <- factor(data.flow$State)
data.flow$fateFlow <- factor(data.flow$fateFlow)
summary(data.flow)
data.flow$Site <- plyr::mapvalues(data.flow$Site,levels(data.flow$Site),c("BER","BOU","DES","PRA","PRB","PRC","PRD"))
save(data.flow,file="data.flow.RData")

