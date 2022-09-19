# Improting Libraries

library(margins)
library(esquisse)
library(tidyverse)
library(moments)
library(dplyr)
library(ggplot2)
library(corrplot)
library(ISLR)
library(car)
library(foreign)
library(ggthemes)
library(colorRamps)
library(foreign)
library(multcomp)
library(survey)
library(lmtest)
library(car)
library(estimatr)
library(gridExtra)
library(gplots)
library(plm)
library(estimatr)
library(lmtest)
source("https://www.r-statistics.com/wp-content/uploads/2010/07/coefplot.r.txt")
library(broom)
library(knitr)
library(sjPlot)
library(stargazer)
install.packages("jtools")
install.packages("devtools")
library(jtools)
library(devtools)
install.packages("huxtable")
library(huxtable)

# Improting the data set
?`estimatr-package`
guns_data<- read.dta("C:\\Users\\nnavn\\OneDrive\\Desktop\\Course Work\\2nd Sem\\Econometrics\\Project\\guns.dta")
View(guns_data)

guns_data$all_crime<-guns_data$mur + guns_data$rob + guns_data$vio
guns_data$robmur<-guns_data$mur + guns_data$rob
view(guns_data)

hist(guns_data$vio)
hist(guns_data$mur)
hist(guns_data$rob)
hist(guns_data$incarc_rate)
hist(guns_data$density)
hist(guns_data$avginc)
hist(guns_data$pop)
hist(guns_data$pb1064)
hist(guns_data$pw1064)
hist(guns_data$pm1029)
hist(guns_data$all_crime)
hist(guns_data$robmur)

library(ggplot2)
guns_data %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density() 

guns_data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

trans_guns = guns_data 
view(trans_guns)


guns_data$log_vio<- log(guns_data$vio)
guns_data$log_all_crime<- log(guns_data$all_crime)
guns_data$log_density<- log(guns_data$density)
guns_data$log_incarc_rate<- log(guns_data$incarc_rate)
guns_data$log_mur<- log(guns_data$mur)
guns_data$log_pop<- log(guns_data$pop)
guns_data$log_rob<- log(guns_data$rob)
guns_data$log_robmur<- log(guns_data$robmur)
guns_data$log_pb1064<- log(guns_data$pb1064)

guns_data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

guns_data %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()

#Creating Time dummies

guns_data$y77<- ifelse(guns_data$year==77,1,0)
guns_data$y78<- ifelse(guns_data$year==78,1,0)
guns_data$y79<- ifelse(guns_data$year==79,1,0)

guns_data$y80<- ifelse(guns_data$year==80,1,0)
guns_data$y81<- ifelse(guns_data$year==81,1,0)
guns_data$y82<- ifelse(guns_data$year==82,1,0)
guns_data$y83<- ifelse(guns_data$year==83,1,0)
guns_data$y84<- ifelse(guns_data$year==84,1,0)
guns_data$y85<- ifelse(guns_data$year==85,1,0)
guns_data$y86<- ifelse(guns_data$year==86,1,0)
guns_data$y87<- ifelse(guns_data$year==87,1,0)
guns_data$y88<- ifelse(guns_data$year==88,1,0)
guns_data$y89<- ifelse(guns_data$year==89,1,0)

guns_data$y90<- ifelse(guns_data$year==90,1,0)
guns_data$y91<- ifelse(guns_data$year==91,1,0)
guns_data$y92<- ifelse(guns_data$year==92,1,0)
guns_data$y93<- ifelse(guns_data$year==93,1,0)
guns_data$y94<- ifelse(guns_data$year==94,1,0)
guns_data$y95<- ifelse(guns_data$year==95,1,0)
guns_data$y96<- ifelse(guns_data$year==96,1,0)
guns_data$y97<- ifelse(guns_data$year==97,1,0)
guns_data$y98<- ifelse(guns_data$year==98,1,0)
guns_data$y99<- ifelse(guns_data$year==99,1,0)



plotmeans(guns_data$all_crime~guns_data$year,xlab = "Year",ylab = "Overall Crime Rate(all_crime)")

plotmeans(guns_data$robmur~guns_data$year,xlab = "Year",ylab = "Overall Crime Rate(all_crime)")

plotmeans(guns_data$rob~guns_data$year,xlab = "Year",ylab = "Overall Crime Rate(all_crime)")

plotmeans(guns_data$mur~guns_data$year,xlab = "Year",ylab = "Overall Crime Rate(all_crime)")

plotmeans(guns_data$vio~guns_data$year,xlab = "Year",ylab = "Overall Crime Rate(all_crime)")

lin_model1<-lm(guns_data$all_crime~guns_data$year)

summary(lin_model1)
m<-resid(lin_model1)
plot(guns_data$year,m)

guns_data$factor_shall<- as.factor(guns_data$shall)

guns_data$factor_year<-as.factor(guns_data$year)


ols_model1<-lm(all_crime~incarc_rate+pb1064+pw1064+pm1029+pop+avginc+density+shall+y77+y78+y79+y80+y81+y82+y83+y84+y85+y86+y87+y88+y89+y90+y91+y92+y93+y94+y95+y96+y97+y98+y99,data = guns_data)
summary(ols_model1)

ols_model2<-lm(all_crime~incarc_rate+pb1064+pw1064+pm1029+pop+avginc+density+shall+year,data = guns_data)
summary(ols_model2)

ols_model3<-lm(robmur~incarc_rate+pb1064+pw1064+pm1029+pop+avginc+density+shall+year,data = guns_data)
summary(ols_model3)

ols_model4<-lm(vio~incarc_rate+pb1064+pw1064+pm1029+pop+avginc+density+shall+year,data = guns_data)
summary(ols_model4)

ols_model5<-lm(rob~incarc_rate+pb1064+pw1064+pm1029+pop+avginc+density+shall+year,data = guns_data)
summary(ols_model5)

ols_model6<-lm(mur~incarc_rate+pb1064+pw1064+pm1029+pop+avginc+density+shall+year,data = guns_data)
summary(ols_model6)


#Models giving different results : Model 2, model 3 and model 4. Model 5 and 6 is similar to model 3.Hence now we run similar models with log transforms. 

ols_model7<-lm(log_all_crime~log_incarc_rate+log_pb1064+pw1064+pm1029+log_pop+avginc+log_density+shall,data = guns_data)
summary(ols_model7)

ols_model8<-lm(log_robmur~log_incarc_rate+log_pb1064+pw1064+pm1029+log_pop+avginc+log_density+shall,data = guns_data)
summary(ols_model8)

ols_model9<-lm(log_vio~log_incarc_rate+log_pb1064+pw1064+pm1029+log_pop+avginc+log_density+shall,data = guns_data)
summary(ols_model9)

# Now we try to run these models with robust random errors to fix the SE

p_dat<-pdata.frame(guns_data,index = c("stateid","year"))
library(plm)

ols_model10<-plm(log_all_crime~log_incarc_rate+log_pb1064+pw1064+pm1029+log_pop+avginc+log_density+shall,data = p_dat, model = "pooling")
summary(ols_model10)

# Random Robust Errors for the above models 7, 8 , 9 
x1<-coeftest(ols_model7, vcov=vcovHC(ols_model7, cluster="group"))
x1

x2<-coeftest(ols_model8, vcov=vcovHC(ols_model8, cluster="group"))
x2

x3<-coeftest(ols_model9, vcov=vcovHC(ols_model9, cluster="group"))
x3


#Heteroskedasticity

a<-resid(ols_model7)
b<-fitted(ols_model7)

# Residual PLOT PROOF FOR HETEROSKEDASTICITY INFORMAL WAY
ggplot(mapping=aes(x=b,y=a))+geom_point(color="navyBlue")+geom_abline(slope = 0,intercept = 0,color="red",size=1)+xlab("Fitted Values of ln_allcrime (y-hat)")+ylab("Residuals")+theme_igray()+ggtitle("Informal Residual Plot To Check Heteroskedasticity")

#ggplot(mapping=aes(x=guns_data$year,y=a))+geom_point(color="black")+geom_abline(slope = 0,intercept = 0,color="red",size=1)+xlab("Fitted Values of y (y-hat)")+ylab("Residuals")+theme_igray()+ggtitle("Informal Residual Plot To Check Heteroskedasticity")

#ggplot(mapping=aes(x=guns_data$stateid,y=a))+geom_point(color="darkgreen")+geom_abline(slope = 0,intercept = 0,color="red",size=1)+xlab("State-ID")+ylab("Residuals")+theme_igray()+ggtitle("Informal Residual Plot across entities To Check Heteroskedasticity across entities")

c<-resid(ols_model8)
d<-fitted(ols_model8)

ggplot(mapping=aes(x=d,y=c))+geom_point(color="navyBlue")+geom_abline(slope = 0,intercept = 0,color="red",size=1)+xlab("Fitted Values of ln_robmur (y-hat)")+ylab("Residuals")+theme_igray()+ggtitle("Informal Residual Plot To Check Heteroskedasticity")


e<-resid(ols_model9)
f<-fitted(ols_model9)

ggplot(mapping=aes(x=f,y=e))+geom_point(color="navyBlue")+geom_abline(slope = 0,intercept = 0,color="red",size=1)+xlab("Fitted Values of ln_vio (y-hat)")+ylab("Residuals")+theme_igray()+ggtitle("Informal Residual Plot To Check Heteroskedasticity")


# White Test

library("sandwich")

kable(tidy(bptest(ols_model7,~log_incarc_rate+log_pb1064+pw1064+pm1029+log_pop+avginc+log_density+shall,data = guns_data)))

kable(tidy(bptest(ols_model8,~log_incarc_rate+log_pb1064+pw1064+pm1029+log_pop+avginc+log_density+shall,data = guns_data)))

kable(tidy(bptest(ols_model9,~log_incarc_rate+log_pb1064+pw1064+pm1029+log_pop+avginc+log_density+shall,data = guns_data)))


p_dat<-pdata.frame(guns_data,index = c("stateid","year"))

# Fixed Time Entity Models

fixed_time_entity_model1<-plm(log_all_crime~log_incarc_rate+log_pb1064+pw1064+pm1029+log_pop+avginc+log_density+factor_shall+factor_year,data = p_dat,model = "within")
summary(fixed_time_entity_model1)
coefplot(fixed_time_entity_model1)

residual_fixed<- resid(fixed_time_entity_model1)
fitted_fixed<- fitted(fixed_time_entity_model1)

ggplot(mapping=aes(x=fitted_fixed,y=residual_fixed))+geom_point(color="black")+geom_abline(slope = 0,intercept = 0,color="red",size=1)+xlab("Fitted Values of ln_allcrime (y-hat)")+ylab("Residuals")+theme_igray()+ggtitle("Informal Residual Plot To Check Heteroskedasticity")


fixed_time_entity_model2<-plm(log_robmur~log_incarc_rate+log_pb1064+pw1064+pm1029+log_pop+avginc+log_density+factor_shall+factor_year,data = p_dat,model = "within")
summary(fixed_time_entity_model2)
coefplot(fixed_time_entity_model2)

residual_fixed<- resid(fixed_time_entity_model2)
fitted_fixed<- fitted(fixed_time_entity_model2)

ggplot(mapping=aes(x=fitted_fixed,y=residual_fixed))+geom_point(color="black")+geom_abline(slope = 0,intercept = 0,color="red",size=1)+xlab("Fitted Values of ln_robmur (y-hat)")+ylab("Residuals")+theme_igray()+ggtitle("Informal Residual Plot To Check Heteroskedasticity")

fixed_time_entity_model3<-plm(log_vio~log_incarc_rate+log_pb1064+pw1064+pm1029+log_pop+avginc+log_density+factor_shall+factor_year,data = p_dat,model = "within")
summary(fixed_time_entity_model3)
coefplot(fixed_time_entity_model3)

residual_fixed<- resid(fixed_time_entity_model3)
fitted_fixed<- fitted(fixed_time_entity_model3)

ggplot(mapping=aes(x=fitted_fixed,y=residual_fixed))+geom_point(color="black")+geom_abline(slope = 0,intercept = 0,color="red",size=1)+xlab("Fitted Values of ln_vio (y-hat)")+ylab("Residuals")+theme_igray()+ggtitle("Informal Residual Plot To Check Heteroskedasticity")



#Fixed Entity Models

fixed_entity_model1<-plm(log_all_crime~log_incarc_rate+log_pb1064+pw1064+pm1029+log_pop+avginc+log_density+factor_shall,data = p_dat,model = "within")
summary(fixed_entity_model1)
coefplot(fixed_entity_model1)

entity_fixed_resid<- resid(fixed_entity_model1)
entity_fixed_fitted<- fitted(fixed_entity_model1)

ggplot(mapping=aes(x=entity_fixed_fitted,y=entity_fixed_resid))+geom_point(color="maroon")+geom_abline(slope = 0,intercept = 0,color="navyblue",size=1)+xlab("Fitted Values of ln_allcrime (y-hat)")+ylab("Residuals")+theme_igray()+ggtitle("Informal Residual Plot To Check Heteroskedasticity")

fixed_entity_model2<-plm(log_robmur~log_incarc_rate+log_pb1064+pw1064+pm1029+log_pop+avginc+log_density+factor_shall,data = p_dat,model = "within")
summary(fixed_entity_model2)
coefplot(fixed_entity_model2)

entity_fixed_resid<- resid(fixed_entity_model2)
entity_fixed_fitted<- fitted(fixed_entity_model2)

ggplot(mapping=aes(x=entity_fixed_fitted,y=entity_fixed_resid))+geom_point(color="maroon")+geom_abline(slope = 0,intercept = 0,color="navyblue",size=1)+xlab("Fitted Values of ln_robmur (y-hat)")+ylab("Residuals")+theme_igray()+ggtitle("Informal Residual Plot To Check Heteroskedasticity")

fixed_entity_model3<-plm(log_vio~log_incarc_rate+log_pb1064+pw1064+pm1029+log_pop+avginc+log_density+factor_shall,data = p_dat,model = "within")
summary(fixed_entity_model3)
coefplot(fixed_entity_model3)

entity_fixed_resid<- resid(fixed_entity_model3)
entity_fixed_fitted<- fitted(fixed_entity_model3)

ggplot(mapping=aes(x=entity_fixed_fitted,y=entity_fixed_resid))+geom_point(color="maroon")+geom_abline(slope = 0,intercept = 0,color="navyblue",size=1)+xlab("Fitted Values of ln_vio (y-hat)")+ylab("Residuals")+theme_igray()+ggtitle("Informal Residual Plot To Check Heteroskedasticity")


# ENTITY-FIXED VS TIME & ENTITY FIXED
null2<-c("factor_year78=0","factor_year79=0","factor_year80=0","factor_year81=0","factor_year82=0","factor_year83=0","factor_year84=0","factor_year85=0","factor_year86=0","factor_year87=0","factor_year88=0","factor_year89=0","factor_year90=0","factor_year91=0","factor_year92=0","factor_year93=0","factor_year94=0","factor_year95=0","factor_year96=0","factor_year97=0","factor_year98=0","factor_year99=0")

linearHypothesis(fixed_time_entity_model1,null2)
kable(linearHypothesis(fixed_time_entity_model1,null2))
pFtest(fixed_time_entity_model1,fixed_entity_model1)

linearHypothesis(fixed_time_entity_model2,null2)
kable(linearHypothesis(fixed_time_entity_model2,null2))
pFtest(fixed_time_entity_model2,fixed_entity_model2)
       
linearHypothesis(fixed_time_entity_model3,null2)
kable(linearHypothesis(fixed_time_entity_model3,null2))
pFtest(fixed_time_entity_model3,fixed_entity_model3)

# Time Fixed Effects are needed (factor year is needed)


# Choosing between entity and time Fixed Effect and OLS Pooling Model

#Null Hypothesis: Ols Model Better than entity and timeFixed effect Model(B1,1=B1,2=B1,3=....=B1,90)
#Alternative Hypothesis: entity and time Fixed effect Model is better than Ols Model(NOT ALL THE INTERCEPTS ARE EQUAL)


pFtest(fixed_time_entity_model1,ols_model7)

pFtest(fixed_time_entity_model2,ols_model8)

pFtest(fixed_time_entity_model3,ols_model9)
