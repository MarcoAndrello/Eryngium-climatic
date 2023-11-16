# Analysis with GLMM

rm(list=ls())

library(tidyverse)
library(lme4)
library(sjPlot)

# Join demographic and environmental dataset
load("data.surv.RData")
load("xvar_1.RData")
left_join(data.surv, xvar_1, by=c("Site", "Year")) %>% filter(Year %in% c(2014:2021)) -> data
data <- na.omit(data)
data

# Statistical model: GLMM
# Scale predictors
data[,c("ddays", "num_s_Tmin_15")] <- scale(data[,c("ddays", "num_s_Tmin_15")])
summary(data)
# Preliminary analysis without random terms
m0 <- glm(fateSurv ~ State*Site*(ddays+num_s_Tmin_15), data=data, family=binomial)
m1 <- glmer(fateSurv ~ State*Site*(ddays+num_s_Tmin_15) + (1 | ID), data=data, family=binomial, nAGQ=0)
AIC(m0,m1)
m2 <- glm(fateSurv ~ State*Site + State*(ddays+num_s_Tmin_15), data=data, family=binomial)
m3 <- glm(fateSurv ~ State*Site + (ddays+num_s_Tmin_15), data=data, family=binomial)
m4 <- glm(fateSurv ~ State+Site + (ddays+num_s_Tmin_15), data=data, family=binomial)
AIC(m0,m1,m2,m3,m4)
summary(m3)
plot_model(m3,type="pred",terms=c("ddays"))
plot_model(m3,type="pred",terms=c("num_s_Tmin_15"))
plot_model(m3,type="pred",terms=c("State","Site"))

### rifare mettendo sito in fisso e state in fisso e la loro interazione!
# e poi in interazione con i predittori climatici! e sceglierne di migliori!


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
