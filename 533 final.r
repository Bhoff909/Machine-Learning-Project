data = read.csv("C:/Users/jbrin/OneDrive/Documents/Fall 2023/Math 533/final/Allstatesinsurvey/all_jails.csv",header=T)
#data = read.csv("C:/Users/jon/OneDrive/Documents/Fall 2023/Math 533/final/Allstatesinsurvey/all_jails.csv",header=T)
#replace NA values with zeroes
data = replace(data,is.na(data),0)
attach(data)

#total deaths by state, 2008
hist(d2008[state == "California"])
hist(d2008)

boxplot(d2008~state, sub = "Distributions of deaths by state in 2008.")#all states
boxplot(d2011~state, sub = "Distributions of deaths by state in 2011.")
boxplot(d2014~state, sub = "Distributions of deaths by state in 2014.")
boxplot(d2008[state == "California"])#Only California
boxplot(d2008~(state == "California"))#California compared to other states
length(d2008[state == "California"])#38 jails

relevant.data = data[,9:104]
#NAs in relevant data
sum(is.na(relevant.data))/(sum(is.na(relevant.data) + !is.na(relevant.data)))
sum(is.na(relevant.data))/(523*96)


#there are no NA values in CA data
#train.deaths.ca = relevant.data[35:72,]
#train.deaths.other = relevant.data[-(35:72),]

deaths.ca = data[35:72,9:19]
deaths.other = data[-(35:72),9:19]

#vector of total yearly deaths in CA
yearly.deaths.ca = numeric()
#yearly.deaths.ca = colSums(train.deaths.ca)
for(i in 1:dim(deaths.ca)[2]){
  yearly.deaths.ca[i] = sum(deaths.ca[,i])
}
barplot(yearly.deaths.ca,names.arg = c(2008:2018),sub = "Total deaths is CA by year.")
#There doesn't appear to be a trend in yearly deaths

#vector of total yearly deaths in US except CA
yearly.deaths.other = numeric()
for(i in 1:dim(train.deaths.other)[2]){
  yearly.deaths.other[i] = sum(deaths.other[,i])
}
barplot(yearly.deaths.other,names.arg = c(2008:2018),sub = "Total deaths in other states by year.") 
#There is a clear upward trend in deaths. Better data collection? higher population?

#vector of total deaths in CA by jail
jail.deaths.ca = numeric()
for(i in 1:dim(train.deaths.ca)[1]){
  jail.deaths.ca[i] = sum(train.deaths.ca[i,])
}
barplot(jail.deaths.ca,names.arg = c(35:72))

#contingency tables
#columns are years, rows types of deaths
#leave out 2019 as testing data

data2 = read.csv("C:/Users/jbrin/OneDrive/Documents/Fall 2023/Math 533/final/CA.csv",header=T)

#california
train.ca = data2[-12,]
test.ca = data2[12,]

#All deaths by year in CA
barplot(t(train.ca[,2]),beside=T,names.arg = c(2008:2018),sub = "Total deaths in CA by year.",legend = colnames(train.ca[,2]))

#deaths by type
colors = c("red","blue","yellow","green","orange","black")
barplot(t(train.ca[,-c(1,2,9)]),beside=T,col=colors,names.arg = c(2008:2018),sub = "Deaths in CA by type.",)
legend("topright",fill=colors,legend = colnames(train.ca[,-c(1,2,9)]),cex=0.6,ncol=2)

data2 = read.csv("C:/Users/jbrin/OneDrive/Documents/Fall 2023/Math 533/final/TX.csv",header=T)

#texas
train.tx = data2[-12,]
test.tx = data2[12,]

#All deaths by year in TX
barplot(t(train.tx[,2]),beside=T,names.arg = c(2008:2018),sub = "Total deaths in TX by year.",legend = colnames(train.tx[,2]))

#deaths by type in TX
barplot(t(train.tx[,-c(1,2,9)]),beside=T,col=colors,names.arg = c(2008:2018),sub = "Deaths in TX by type.")
legend("topright",fill=colors,legend = colnames(train.tx[,-c(1,2,9)]),cex=0.6,ncol=2)

data2 = read.csv("C:/Users/jbrin/OneDrive/Documents/Fall 2023/Math 533/final/MS.csv",header=T)

#mississippi
train.ms = data2[-12,]
test.ms = data2[12,]

data2 = read.csv("C:/Users/jbrin/OneDrive/Documents/Fall 2023/Math 533/final/PA.csv",header=T)

#All deaths by year in MS
barplot(t(train.ms[,2]),beside=T,names.arg = c(2008:2018),sub = "Total deaths in MS by year.",legend = colnames(train.ms[,2]))

#deaths by type
barplot(t(train.ms[,-c(1,2,9)]),beside=T,col=colors,names.arg = c(2008:2018),sub = "Deaths by type in MS.")
legend("topright",fill=colors,legend = colnames(train.tx[,-c(1,2,9)]),cex=0.6,ncol=1)

#pennsylvania
train.pa = data2[-12,]
test.pa = data2[12,]

#All deaths by year in PA
barplot(t(train.pa[,2]),beside=T,names.arg = c(2008:2018),sub = "Total deaths in PA by year.")

#deaths by type
barplot(t(train.pa[,-c(1,2,9)]),beside=T,col=colors,names.arg = c(2008:2018),sub = "Deaths in PA by type.")
legend("topright",fill=colors,legend = colnames(train.tx[,-c(1,2,9)]),cex=0.6,ncol=1)

#number of incarcerated by state
adp = cbind(train.ca[,9],train.tx[,9],train.ms[,9],train.pa[,9])
colors2 = c("red","yellow","blue","green")
barplot(t(adp), beside=T, col=colors2, names.arg = c(2008:2018),sub = "Average daily inmate population by state.")
legend("topright",fill=colors2,legend = c("CA","TX","MS","PA"),cex=0.5,ncol=4)

#Daily death rates?
rates = cbind((train.ca[,2]/365)/train.ca[,9], (train.tx[,2]/365)/train.tx[,9], (train.ms[,2]/365)/train.ms[,9], (train.pa[,]/365)/train.pa[,9])
barplot(t(rates),beside=T,col=colors2, names.arg = c(2008:2018),sub = "Daily death rates by state.")
legend("topleft",fill=colors2,legend = c("CA","TX","MS","PA"),cex=0.5,ncol=4)

#linear regression
train.ca = cbind(rep("CA",11),train.ca)
train.tx = cbind(rep("TX",11),train.tx)
ca_tx = data.frame(train.ca,train.tx)

ca.model = glm(train.ca$state~train.ca$deaths+train.ca$suicides+train.ca$drug.alc+train.ca$illness+train.ca$other+train.ca$homicide+train.ca$accident)
#illness, other, and accident are significant and negative. Deaths is significant and positive

ca.pred = predict(ca.model,newdata = data.frame(t(test.ca[,-c(1,9)])))

tx.model = lm(train.tx$year~train.tx$deaths+train.tx$suicides+train.tx$drug.alc+train.tx$illness+train.tx$other+train.tx$homicide+train.tx$accident)
summary(tx.model)
#no significant predictors

ms.model = lm(train.ms$year~train.ms$deaths+train.ms$suicides+train.ms$drug.alc+train.ms$illness+train.ms$other+train.ms$homicide+train.ms$accident)
summary(ms.model)

pa.model = lm(train.pa$year~train.pa$deaths+train.pa$suicides+train.pa$drug.alc+train.pa$illness+train.pa$other+train.pa$homicide+train.pa$accident)
summary(pa.model)

#logistic regression
#compare california and texas
ca = cbind(state = rep(0,11),train.ca)
tx = cbind(state = rep(1,11),train.tx)
ca.tx = rbind(ca,tx)
#ca.tx = ca.tx[,-10]
txca.model = glm(state~.,data=ca.tx, family=binomial)
txca.model
summary(txca.model)

#convert to proportions
train.ca = train.ca/train.ca[,9]
#treat deaths as response, types of deaths as predictor?
model1 = lm(year~suicides+drug.alc+illness+other+homicide+accident,data=train.tx)
summary(model1)
plot(train.tx$year,train.tx$illness)

#linear trend in total deaths by year#############################
model = lm(deaths~year,data=train.tx)
summary(model)
predict(model,newdata = data.frame(year=2019))

model = lm(deaths~year,data=train.ca)
summary(model)

model = lm(deaths~year,data=train.ms)
summary(model)

model = lm(deaths~year,data=train.pa)
summary(model)

#there is no linear trend in any of the selected states

#check for trends in type of death by year in CA
for(i in 3:8){
  model = lm(train.ca[,i]~year,data=train.ca)
  print(i)
  print(summary(model))
}
#significant negative linear relationship between illness related deaths and years
#possibly significant positive relationship between accident deaths over time

#check for trends in type of death by year in TX
for(i in 3:8){
  model = lm(train.tx[,i]~year,data=train.tx)
  print(i)
  print(summary(model))
}
#there are no apparent linear trends in deaths by type in texas

#check for trends in type of death by year in MS
for(i in 3:8){
  model = lm(train.ms[,i]~year,data=train.ms)
  print(i)
  print(summary(model))
}
#there are no apparent linear trends in deaths by type in mississippi

#check for trends in type of death by year in pa
for(i in 3:8){
  model = lm(train.pa[,i]~year,data=train.pa)
  print(i)
  print(summary(model))
}
#significant increase in suicides and drug/alcohol related deaths in pa jails

#poisson regression?
out = glm(deaths~suicides+drug.alc+illness+other+homicide+accident,data=train.ca,family=poisson(link="log"))
summary(out)


###############################################compare total deaths across states#########################################################
deaths = read.csv("C:/Users/jbrin/OneDrive/Documents/Fall 2023/Math 533/final/totaldeaths.csv",header=T)

model.pois = glm(deaths~state+year, data=deaths, family = poisson(link="log"))

summary(model.pois)

test.deaths = read.csv("C:/Users/jbrin/OneDrive/Documents/Fall 2023/Math 533/final/totaldeaths.test.csv",header=T)
#exponentiate?
pred.pois = exp(predict(model.pois,newdata=test.deaths))

mse.pois = mean((test.deaths$deaths - pred.pois)^2)

#linear regression
model.lin = lm(deaths~state+year, data=deaths)
summary(model.lin)
pred.lin = predict(model.lin,newdata=test.deaths)
mse.lin = mean((test.deaths$deaths - pred.lin)^2)

##############################elastic net
library(glmnet)
#p. 444
x <- model.matrix(deaths~state+year - 1, data = deaths)
y = deaths$deaths

cv3 = cv.glmnet(x, y, alpha = .5)
best.lam.elasticnet = cv3$lambda.min
model.elastic = glmnet(x,y,alpha = 0.5,lambda=best.lam.elasticnet)
pred.el = predict(model.elastic,newx = model.matrix(deaths~state+year - 1, data=test.deaths))
mse.elastic = mean((test.deaths$deaths - pred.el)^2)

#lasso
cv1 = cv.glmnet(x, y, alpha = 1)
best.lam.lasso = cv1$lambda.min
model.lasso = glmnet(x,y,alpha = 1,lambda=best.lam.lasso)
pred.la = predict(model.lasso,newx = model.matrix(deaths~state+year - 1, data=test.deaths))
mse.lasso = mean((test.deaths$deaths - pred.la)^2)

#ridge
cv2 = cv.glmnet(x, y, alpha = 0)
best.lam.ridge = cv2$lambda.min
model.ridge = glmnet(x,y,alpha = 0,lambda=best.lam.ridge)
pred.ri = predict(model.ridge,newx = model.matrix(deaths~state+year - 1, data=test.deaths))
mse.ridge = mean((test.deaths$deaths - pred.ri)^2)

###############################neural network
library(tidyverse)
library(neuralnet)
#states are encoded. ca = 1, tx = 2, ms = 3, pa = 4
# state.code = numeric()
# for(i in 1:dim(deaths)[1]){
#   if(deaths[i,]$state == "ca"){
#     state.code[i] = 1
#   }
#   if(deaths[i,]$state == "tx"){
#     state.code[i] = 2
#   }
#   if(deaths[i,]$state == "ms"){
#     state.code[i] = 3
#   }
#   if(deaths[i,]$state == "pa"){
#     state.code[i] = 4
#   }
# }
# deaths$state.code = state.code

#norm.deaths = read.csv("C:/Users/jbrin/OneDrive/Documents/Fall 2023/Math 533/final/norm.death.csv",header=T)

library(tidyverse)
library(neuralnet)
library(mltools)
library(data.table)

deaths$state = as.factor(deaths$state)
newdata <- one_hot(as.data.table(deaths)) #dummy code states
newdata$deaths = scale(newdata$deaths) #normalize deaths?
newdata$year = scale(newdata$year) #normalize year

model.neural = neuralnet(deaths~state_ca+state_ms+state_pa+state_tx+year,data=newdata,hidden=c(5,5))

test.deaths$state = as.factor(test.deaths$state)
new.test = one_hot(as.data.table(test.deaths))
new.test$deaths = scale(new.test$deaths)
new.test$year = 0

pred = predict(model.neural,new.test)
unscaled.pred = (pred*sd(deaths$deaths))+mean(deaths$deaths)

neural.mse = mean((unscaled.pred - test.deaths$deaths)^2)

table = data.frame(model = c("linear","poisson","elasticnet","ridge","lasso","neuralnet"),
                   mse = c(mse.lin,mse.pois,mse.elastic,mse.ridge,mse.lasso,neural.mse))

library(lemon)
knit_print.data.frame <- lemon_print
print(table)
##############################################compare death ratios, suicide ratios, illness ratios across states###############################################
deaths = read.csv("C:/Users/jbrin/OneDrive/Documents/Fall 2023/Math 533/final/totaldeaths.csv",header=T)
test.deaths = read.csv("C:/Users/jbrin/OneDrive/Documents/Fall 2023/Math 533/final/totaldeaths.test.csv",header=T)

deaths$death.ratio = deaths$deaths/deaths$avgpop
deaths$suicide.ratio = deaths$suicide/deaths$avgpop
deaths$illness.ratio = deaths$illness/deaths$avgpop

test.deaths$death.ratio = test.deaths$deaths/test.deaths$avgpop
test.deaths$suicide.ratio = test.deaths$suicide/test.deaths$avgpop
test.deaths$illness.ratio = test.deaths$illness/test.deaths$avgpop

lin.model.dratio = lm(death.ratio~state+year,data=deaths)
lin.model.sratio = lm(suicide.ratio~state+year,data=deaths)
lin.model.illratio = lm(illness.ratio~state+year,data=deaths)

summary(lin.model.dratio)
summary(lin.model.sratio)
summary(lin.model.illratio)

dratio.pred = predict(model.dratio, newdata = test.deaths)
mse.dratio = mean((test.deaths$death.ratio - dratio.pred)^2)

#poisson again
pois.model.dratio = glm(death.ratio~state+year, data=deaths, family = poisson(link="log"))
pois.model.sratio = glm(suicide.ratio~state+year, data=deaths, family = poisson(link="log"))
pois.model.iratio = glm(illness.ratio~state+year, data=deaths, family = poisson(link="log"))

summary(pois.model.dratio)
summary(pois.model.sratio)
summary(pois.model.iratio)

pois.pred2 = exp(predict(model.pois2,newdata=test.deaths))

pois.mse2 = mean((test.deaths$death.ratio - pois.pred2)^2)

#elasticnet, ridge, lasso
library(glmnet)
#p. 444
x <- model.matrix(death.ratio~state+year - 1, data = deaths)
y = deaths$death.ratio

cv3 = cv.glmnet(x, y, alpha = .5)
best.lam.elasticnet = cv3$lambda.min

model.elastic = glmnet(x,y,alpha = 0.5,lambda=best.lam.elasticnet)
model.elastic

#new.data = model.matrix(test.deaths$deaths~test.deaths$year+test.deaths$state)[,-1]
pred.el = predict(model.elastic,newx = model.matrix(death.ratio~state+year - 1, data=test.deaths))

mse.elastic = mean((test.deaths$deaths - pred.el)^2)

#lasso
cv1 = cv.glmnet(x, y, alpha = 1)
best.lam.lasso = cv1$lambda.min

model.lasso = glmnet(x,y,alpha = 1,lambda=best.lam.lasso)
pred.la = predict(model.lasso,newx = model.matrix(death.ratio~state+year - 1, data=test.deaths))
mse.lasso = mean((test.deaths$deaths - pred.la)^2)

#ridge
cv2 = cv.glmnet(x, y, alpha = 0)
best.lam.ridge = cv2$lambda.min

model.ridge = glmnet(x,y,alpha = 0,lambda=best.lam.ridge)
pred.ri = predict(model.ridge,newx = model.matrix(death.ratio~state+year - 1, data=test.deaths))
mse.ridge = mean((test.deaths$deaths - pred.ri)^2)
















#suicides for 2008, ca and ms
suicide = read.csv("C:/Users/jbrin/OneDrive/Documents/Fall 2023/Math 533/final/suicide2008.csv",header=T)
bin.model = glm(suicide~state+year,data=suicide,family=binomial)
summary(bin.model)
log.pred = predict(bin.model, newdata = data.frame(year = 2009, state = c("ca","tx","ms","pa")))
log.pred
