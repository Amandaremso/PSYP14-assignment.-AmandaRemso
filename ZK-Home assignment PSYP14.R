data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
view(data_sample_1)
data<-data_sample_1
#Part 1-Hierarchical regression model to predict postoperative pain after wisdom tooth surgery. 
#Predictions (x): Age, sex, anxiety (STAI_trait), pain catastrophizing (pain_cat), mindfulness (mindfulness) and serum cortisol (cortisol_serum, cortisol_saliva.
#Outcome (y): Postoperative pain (pain) on a 0-10 scale, 0=no pain, 10=worst pain you can imagine. 
#Will adding the the psychological and hormonal variables work as better predictors of pain, compared to only using demographic variables? (age,sex)
#1:Model 1: Age and sex
#2:Model 2: Age, sex, STAI_trait,pain_cat,mindfulness, cortisol_serum, and cortisol_saliva.
#3:Model comparison between model 1 and model 2.

#Data screening:outliers, coding errors
#Check all variables, predictors and outcomes
#Check all assumptions 
#Report possible corrections and/or exclusions

str(data)
head(data)
summary(data)
view(dat)

#Outliers
data %>% ggplot()+aes(x=age,y=pain)+geom_point()
data %>% ggplot()+aes(x=STAI_trait,y=pain)+geom_point()
data %>% ggplot()+aes(x=pain_cat,y=pain)+geom_point()
data %>% ggplot()+aes(x=mindfulness,y=pain)+geom_point()
data %>% ggplot()+aes(x=cortisol_serum,y=pain)+geom_point()

data %>% ggplot()+aes(x=age,y=pain)+geom_point()+geom_smooth(method="lm")
data %>% ggplot()+aes(x=STAI_trait,y=pain)+geom_point()+geom_smooth(method="lm")
data %>% ggplot()+aes(x=pain_cat,y=pain)+geom_point()+geom_smooth(method="lm")
data %>% ggplot()+aes(x=mindfulness,y=pain)+geom_point()+geom_smooth(method="lm")
data %>% ggplot()+aes(x=cortisol_serum,y=pain)+geom_point()+geom_smooth(method="lm")

mtest<-mahalanobis(data[,c(2,4,5,6,7,8,9)],colMeans(data[,c(2,4,5,6,7,8,9)]),cov(data[,c(2,4,5,6,7,8,9)]))
summary(mtest)
cutoffmtest<-qchisq(.999,ncol(data[,c(2,4,5,6,7,8,9)]))
cutoffmtest
summary(mtest<cutoffmtest)
data_new<-data[mtest<cutoffmtest,]
data %>% ggplot()+aes(x=pain)+geom_boxplot()
out<-boxplot.stats(data$pain)$out
out<-which(data$pain%in%c(out))
out
data %>% ggplot()+aes(x=STAI_trait)+geom_boxplot()
out<-boxplot.stats(data$STAI_trait)$out
out2<-which(data$STAI_trait%in%c(out))
out2
cooks.distance(outputmodel1)

view(data_new)
#Recode sex to a dummy variable
data_new<-data_new %>% mutate(sex_recode=recode(sex,"male"="1","female"="0"))
view(data_new)

data_new %>% summarise(meanpain=mean(pain))
data_new %>% summarise(sdpain=sd(pain))

#Second model
outputmodel1<-lm(pain~sex+age+STAI_trait+pain_cat+cortisol_serum+mindfulness,data=data_new)
summary(outputmodel1)
lm.beta(outputmodel1)

#Correlations, multicollinearity
correlation1<-data_new%>% select(sex_recode,age,pain,STAI_trait,cortisol_serum,cortisol_saliva, mindfulness)
rcorrelation=rcorr(as.matrix(correlation1))
rcorrelation
vif(outputmodel1)

#normality
outputmodel1<-lm(pain~sex_recode+age+STAI_trait+pain_cat+cortisol_serum+mindfulness,data=data_new)
standardized=rstudent(outputmodel1)
fitted=scale(outputmodel1$fitted.values)
hist(standardized)
summary(outputmodel1)
describe(residuals(outputmodel1)) #skew: -0.18, k:0.02
outputmodel1 %>% plot()

#linearity
qqnorm(standardized)
abline(0,1)
residualPlots(outputmodel1)

#homogeneity and homoscedasticity
plot(fitted,standardized)
abline(0,0)
abline(v=0)
outputmodel1 %>% plot()
bptest(outputmodel1)
ncvTest(outputmodel1)

#Regression with only age and sex
outputageandsex<-lm(pain~sex_recode+age,data = data_new)
summary(outputageandsex)
AIC(outputageandsex)
#beta
lm.beta(outputageandsex)

#Second model
outputmodel1<-lm(pain~sex+age+STAI_trait+pain_cat+cortisol_serum+mindfulness,data=data_new)
summary(outputmodel1)
summary(outputageandsex)
lm.beta(outputmodel1)

#Anova comparison between the two models
anova(outputageandsex,outputmodel1)
AIC(outputageandsex)
AIC(outputmodel1)

confint(outputageandsex)
lm.beta(outputageandsex)
confint(outputmodel1)
lm.beta(outputmodel1)
coef_table(outputmodel1)
coef_table(outputageandsex)

#Part 2-Backward regression
#Predictors: age, sex, STAI, pain catastrophizing, mindfulness, serum cortisol,weight, IQ, household income. 
#Backward regression model
#Theory based model (part 1)
#Compare the models, AIC and ANOVA
#these regression equations should be applied on the new data (data file 2), to predict pain.) Compare the predicted values with the actual pain ratings. Which model was able to predict the actual pain ratings in data file 2 better?

#Full model, all of the variables 
outputbackward<-lm(pain~sex_recode+age+STAI_trait+pain_cat+cortisol_serum+mindfulness+weight+IQ+household_income,data=data_new)
summary(outputbackward)
AIC(outputbackward)
coef_table(outputbackward)

#Outliers
data_new %>% ggplot()+aes(x=weight,y=pain)+geom_point()
data_new %>% ggplot()+aes(x=IQ,y=pain)+geom_point()
data_new %>% ggplot()+aes(x=household_income,y=pain)+geom_point()
mmtest<-mahalanobis(data_new[,c(2,4,5,6,7,9,10,11,12)],colMeans(data_new[,c(2,4,5,6,7,9,10,11,12)]),cov(data_new[,c(2,4,5,6,7,9,10,11,12)]))
summary(mmtest)
cutoffmmtest<-qchisq(.999,ncol(data_new[,c(2,4,5,6,7,9,10,11,12)]))
cutoffmmtest
summary(mmtest<cutoffmtest)

#Correaltions
correlationtwo<-data_new %>% select(age,pain,pain_cat,STAI_trait,cortisol_serum,mindfulness,IQ,weight,household_income,sex_recode)
secondrcorrelation=rcorr(as.matrix(correlationtwo))
secondrcorrelation
vif(outputbackward)

#Normality
standardizedw=rstudent(outputbackward)
fittedw=scale(outputbackward$fitted.values)
hist(standardizedw)
summary(outputbackward)
describe(residuals(outputbackward)) #skew: -0.17,k: -0.08
qqplot(outputbackward)

#Linearity
qqnorm(standardizedw)
abline(0,1)
residualPlots(outputbackward)

#homogeneity and homoscedasticity
plot(fittedw,standardizedw)
abline(0,0)
abline(v=0)
bptest(outputbackward)
ncvTest(outputbackward)

#Backwardmodel
back<-step(outputbackward,direction="backward")
back.model<-lm(pain~age+pain_cat+cortisol_serum+mindfulness,data=data_new)
summary(back.model)
AIC(back.model)

#Model from Part 1
outputmodel1<-lm(pain~sex_recode+age+STAI_trait+pain_cat+cortisol_serum+mindfulness,data=data_new)
summary(outputmodel1)

#Compare between full model and backward
anova(outputbackward,back.model)
AIC(outputbackward)
AIC(back.model)

lm.beta(outputbackward)
confint(outputbackward)
coef_table(outputbackward)
lm.beta(back.model)
confint(back.model)
coef_table(back.model)

#compare between backward and theory
anova(outputmodel1,back.model)
AIC(outputmodel1)
AIC(back.model)

#Predict pain on the new data set using theory and backward model. 
#just use the regression equations that you derived based on data file 1. These regression equations should be applied on the new data (data file 2), to predict pain.) Compare the predicted values with the actual pain ratings. Which model was able to predict the actual pain ratings in data file 2 better?
data_sample_2=read.csv("https://tinyurl.com/87v6emky")
view(data_sample_2)

pred.theorymodel <- predict(outputmodel1, data_sample_2)
pred.backmodels<-predict(back.model, data_sample_2)
RSS_test_theory=sum((data_sample_2[,"pain"]-pred.theorymodel)^2)
RSS_test_backed=sum((data_sample_2[,"pain"]-pred.backmodels)^2)
RSS_test_theory
RSS_test_backed

#Part 3
data_sample_3=read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_3.csv")
view(data_sample_3)
data_sample_4=read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_4.csv")
view(data_sample_4)

stdCoef.merMod <-function(object)
  sdy <- sd(getME(object, "y"))
sdx <- apply(getME(object, "X"), 2, sd)
sc <- fixef(object) * sdx/sdyse.fixef <- coef(summary(object))[, "Std. Error"]
se <- se.fixef * sdx/sdyreturn(data.frame(stdcoef = sc, stdse = se))

str(data_sample_3)
head(data_sample_3)

table(data_sample_3$sex)
data_sample_3<-data_sample_3 %>% mutate(sex=recode(sex,"male"="male","female"="female","woman"="female"))
view(data_sample_3)

#Linear random intercept regression, on data file 3. Random intercept: hospital id, fixed effects from part 1.
part3_randommodel<-lmer(pain~sex+age+STAI_trait+pain_cat+cortisol_serum+mindfulness+(1|hospital), data=data_sample_3)
summary(part3_randommodel)

#Outliers
outobservation = influence(part3_randommodel, obs = T,allow.new.levels=TRUE)$alt.fixed 
outgroup = influence(part3_randommodel, group = "hospital")$alt.fixed
influence_observation = influence(part3_randommodel, obs = T)$alt.fixed 
influence_group = influence(part3_randommodel, group = "hospital")$alt.fixed
influence_group
influence_observation
infobs = as_tibble(influence_group) %>%gather(colnames(influence_group), value = coefficient, key = predictor)
infobs %>%ggplot() + aes(x = 1, y = coefficient, group = predictor) +geom_boxplot() + geom_jitter(width = 0.2) + facet_wrap(~predictor,scales = "free")
info=influence(part3_randommodel,obs=T)
plot(info,which="cook")
infoo=influence(part3_randommodel,group="hospital")
plot(infoo)
plot(infoo,which="cook")
CD1<-cooks.distance(info)
CD2<-cooks.distance(infoo)
plot(infoo)

#Assumptions

#multicollinearity
pairs.panels(data_sample_3[, c("age", "STAI_trait","mindfulness","cortisol_serum", "pain_cat", "sex")], col = "blue", lm = T)

#normality
qqmath(part3_randommodel)
qqmath(ranef(part3_randommodel))
data_sample_3 %>%ggplot()+ aes(sample = resid) + stat_qq() + stat_qq_line()+facet_wrap(~hospital, scales = "free")
describe(random_effects$intercept) #skew 0.05, kurtosis -1.12, normality of random effect

#linearity
qqnorm(resid(part3_randommodel))
plot(part3_randommodel, arg = "pearson")
data_sample_3 %>%ggplot() + aes(x = hospital, y = resid) + geom_point()

# homoscedasticity
plot(part3_randommodel,arg="pearson")
plot(ranef(part3_randommodel))
linearmodel = lm(resid^2 ~ hospital, data = data_sample_3)
summary(linearmodel)

#Linear random intercept regression, on data file 3. Random intercept: hospital id, fixed effects from part 1.
part3_randommodel<-lmer(pain~sex+age+STAI_trait+pain_cat+cortisol_serum+mindfulness+(1|hospital), data=data_sample_3)
summary(part3_randommodel)

#Model coefficients and the confidence intervals of the coefficients for all fixed predictors, compare coefficients from part 1.
cAIC(part3_randommodel)$caic
stdCoef.merMod(part3_randommodel)
confint(part3_randommodel)
anova(outputmodel1,part3_randommodel)

#Variance explained by the fixed effect predictor, marginal r2
#variance explained by the fixed and random effect terms combined using conditional r2.
r.squaredGLMM(part3_randommodel)
r2beta(part3_randommodel,method="nsj")

data_sample_4<-data_sample_4 %>% mutate(sex=recode(sex,"male"="1","female"="0"))
view(data_sample_4)

#Predict pain on dataset 4 with part3_randommodel
#Predict  r2 with the model on dataset 4, compare with the values from dataset 3
pred3<-predict(part3_randommodel,newdata=data_sample_4,allow.new.levels=TRUE)
pred3
sum_squared_error4<-sse(data_sample_4$pain,pred3)
sum_squared_error4
mean_squared_error4<-mse(data_sample_4$pain,pred3)
mean_squared_error4
y_test_4<-data_sample_4$pain
error_4<-y_test_4-pred3
adjr2<-1-(mean_squared_error4/var(y_test_4))
adjr2
mod_4 <- lmer(pain ~ 1+(1|hospital), data = data_sample_4)
tss4=sum((data_sample_4$pain-predict(mod_4))^2)
tss4
R2=1-305.1739/484.5061
R2 

#new regression random intercept and slope, most influentual predictors only
slopes<-lmer(pain~cortisol_serum+pain_cat+(1+cortisol_serum|hospital)+(1+pain_cat|hospital),data=data_sample_3)
summary(slopes)
r.squaredGLMM(slopes)
r2beta(slopes,method="nsj")
cAIC(slopes)$caic
cAIC(part3_randommodel)$caic
anova(part3_randommodel,slopes)

r2beta(slopes,method="nsj")
r2beta(part3_randommodel,method="nsj")

sum(residuals(slopes)^2)
sum(residuals(part3_randommodel)^2)

#Visualize the fitted regresion lines for each hospital separatly. 
 data_sample_3 %>%ggplot()+ aes(y = pain,x=cortisol_serum+pain_cat,color=hospital)+geom_point(size = 2) + geom_smooth(method = "lm", se = F,fullrange = TRUE) + xlim(-1, 50) + geom_hline(yintercept = 0) +geom_vline(xintercept = 0)
 data_sample_3 %>%ggplot()+ aes(y = pain,x=pain_cat,color=hospital)+geom_point(size = 2) + geom_smooth(method = "lm", se = F,fullrange = TRUE) + xlim(-1, 50) + geom_hline(yintercept = 0) +geom_vline(xintercept = 0)
 data_sample_3 %>% ggplot()+aes(y=pain,)
 
#Tables
tab_model(outputageandsex, show.se = TRUE, show.std = TRUE, show.stat = TRUE,file="new.doc")
tab_model(outputmodel1, show.se = TRUE, show.std = TRUE, show.stat = TRUE,file="newe.doc")
tab_model(back.model, show.se = TRUE, show.std = TRUE, show.stat = TRUE,file="newes.doc")
tab_model(part3_randommodel, show.se = TRUE, show.std = TRUE, show.stat = TRUE,file="newest.doc")
tab_model(part3_randommodel,outputmodel1, show.se = TRUE, show.std = TRUE, show.stat = TRUE,file="heloollo.doc")
tab_model(slopes, show.se = TRUE, show.std = TRUE, show.stat = TRUE,file="monday.doc")