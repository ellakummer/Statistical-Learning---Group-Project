my_data <- heart_datas
names(my_data)
attach(my_data)

# ----------- correlation matrix  -----------

cor(my_data)

# ----------- LINEAR REGRESSION WITH VALIDATION SET APPROACH -----------

set.seed(1)

# training set (we take half of our datas) : 
train=sample(303,101)

# we apply a linear regression :
lm.fit=lm(target~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal,data=my_data,subset=train)

# test (test error MSE) :
mean((target-predict(lm.fit,my_data))[-train]^2)
# WE FIND MSE = 0.1540146
# I get a different one: 0.1727088 (made sure to set seed = 1)

summary(lm.fit)


# ----------- LOGISTIC REGRESSION WITH CROSS VALIDATION -----------

# GENERAL FORM : 
glm.fit=glm(target~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal,data=my_data,subset=train)
# informations about the regression : 
coef(glm.fit)
summary(glm.fit)

# test error MSE :
mean((target-predict(glm.fit,my_data, type="response"))[-train]^2)

# CROSS VALIDATION : 
glm.fit2=glm(target~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal,data=my_data)
set.seed(17)

# K = 10 
library(boot)
cv.error.10=cv.glm(my_data,glm.fit2,K=10)$delta[1]
cv.error.10

# leave-one-out cross validation : 
cv.error=cv.glm(my_data,glm.fit2)$delta[1]
cv.error

# ----------- TREE BASED METHOD  -----------

#install.packages("tree")

# CLASSIFICATION  tree : 
library(tree)
disease=ifelse(target<=0.5,"No","Yes")
my_data_2=data.frame(my_data,disease)
tree.datas=tree(disease~.-target,my_data_2)
summary(tree.datas)
plot(tree.datas)
text(tree.datas,pretty=0)
tree.datas

# error test classification tree : 
set.seed(2)
train=sample(1:nrow(my_data_2), 150)
my_data_2.test=my_data_2[-train,]
disease.test=disease[-train]
tree.datas=tree(disease~.-target,my_data_2,subset=train)
tree.pred=predict(tree.datas,my_data_2.test,type="class")
table(tree.pred,disease.test)
# (52+64)/(49+22+18+64) = 116/153 = 0.7582

# PRUNNING TREE 
set.seed(3)
cv.prune.datas=cv.tree(tree.datas,FUN=prune.misclass)
cv.prune.datas
par(mfrow=c(1,2))
prune.datas=prune.misclass(tree.datas,best=9)
plot(prune.datas)
text(prune.datas,pretty=0)
# error test prunning tree : 
tree.pred2=predict(prune.datas,my_data_2.test,type="class")
table(tree.pred2,disease.test)
# (49+69)/153 = 118/153 = 0.7712 

# ---------------------------------------------------------------
# ------------------ BEST SUBSET SELECTION  ---------------------
# ---------------------------------------------------------------

# with the result : try non-linear method (with lm.())

install.packages("leaps")
library(leaps)

regfit.full=regsubsets(target~.,data=my_data,nvmax=13)
# to predict the salary, we found these predictors in the Hitters components set:
reg.summary=summary (regfit.full) 
reg.summary

# models:
names(reg.summary)

# USELESS ?? 
# we want according to adjusted R2, BIC, Cp
reg.summary$adjr2
reg.summary$bic
reg.summary$cp
reg.summary$rsq

par(mfrow=c(2,2))

# USELESS ?? 
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")

# bests models according to : 

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(10,reg.summary$adjr2[10], col="red",cex=2,pch=20)
coef(regfit.full ,10)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(9,reg.summary$cp[9],col="red",cex=2,pch=20)
coef(regfit.full ,9)

plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(7,reg.summary$bic[7],col="red",cex=2,pch=20)
coef(regfit.full ,7)

# Show some plots to provide evidence : 
# USELESS ??  don't like how it's plot 
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

# Choosing Among Models : MSE

set.seed(1)
train2=sample(c(TRUE,FALSE), nrow(my_data),rep=TRUE)
test2=(!train2)
regfit.best=regsubsets(target~.,data=my_data[train2,],nvmax=13)
test.mat=model.matrix(target~.,data=my_data[test2,])
val.errors=rep(NA,13)
for(i in 1:13){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((my_data$target[test2]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,8)

# Choosing Among Models : CROSS VALIDATION 

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

k=10
set.seed(1)
folds=sample(1:k,nrow(my_data),replace=TRUE)
cv.errors=matrix(NA,k,13, dimnames=list(NULL, paste(1:13)))

for(j in 1:k){
  best.fit=regsubsets(target~.,data=my_data[folds!=j,],nvmax=13)
  for(i in 1:13){
    pred=predict(best.fit,my_data[folds==j,],id=i)
    cv.errors[j,i]=mean( (my_data$target[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
which.min(mean.cv.errors)
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(target~.,data=my_data, nvmax=13)
coef(reg.best,11)
coef(reg.best,7)


# ----------- LOGISTIC REGRESSION WITH CROSS VALIDATION (TAKING BEST SUBSET) -----------

# GENERAL FORM : 
glm.fit3=glm(target~sex+cp+trestbps+chol+restecg+thalach+exang+oldpeak+slope+ca+thal,data=my_data,subset=train)
# informations about the regression : 

# test error MSE :
mean((target-predict(glm.fit3,my_data, type="response"))[-train]^2)

# CROSS VALIDATION : 
glm.fit4=glm(target~sex+cp+trestbps+chol+restecg+thalach+exang+oldpeak+slope+ca+thal,data=my_data)
set.seed(17)

# K = 10 
library(boot)
cv.error.10=cv.glm(my_data,glm.fit4,K=10)$delta[1]
cv.error.10

# leave-one-out cross validation : 
cv.error=cv.glm(my_data,glm.fit4)$delta[1]
cv.error


# ---------------------------------------------------------------
# ---------------------NON LINEAR MODELING ----------------------
# ---------------------------------------------------------------

# ----------- POLYNOMIAL REGRESSION + SPLINE -----------

# exemples : 
#fit.1=lm(wage~education+age,data=Wage)
#fit.2=lm(wage~education+poly(age,2),data=Wage)
#fit.3=lm(wage~education+poly(age,3),data=Wage)
#anova(fit.1,fit.2,fit.3)

# but not sure usefull if we're doing GAM : 

# ----------- GAM -----------

# we gonna try : 7 best : cubique, 11-7 rest : square
# ! predictors with less than 4 values can't be smoothing variables 
gam.test1=gam(target~sex+s(cp, 3)+s(trestbps, 2)+s(chol, 2)+restecg+s(thalach, 3)+exang+s(oldpeak, 3)+slope+ca+thal,data=my_data)
par(mfrow =c(3,4))
plot(gam.test, se=TRUE ,col ="blue ")
# after having a look :
# linear function for : all except chol, oldpeak
gam.test2=gam(target~sex+cp+trestbps+s(chol, 3)+restecg+thalach+exang+s(oldpeak, 3)+slope+ca+thal,data=my_data)
gam.test3=gam(target~sex+cp+trestbps+s(chol, 2)+restecg+thalach+exang+s(oldpeak, 3)+slope+ca+thal,data=my_data)
gam.test4=gam(target~sex+cp+trestbps+s(chol, 2)+restecg+thalach+exang+s(oldpeak, 2)+slope+ca+thal,data=my_data)
gam.test5=gam(target~cp+trestbps+s(chol, 2)+restecg+thalach+exang+s(oldpeak, 2)+slope+ca+thal,data=my_data)
# test, which one is the best 
anova(gam.test5, gam.test4 ,gam.test3, gam.test2, gam.test1, test="F")

plot(gam.test2, se=TRUE ,col ="blue ")

# MSE : 
mean((target-predict(gam.test2,my_data))[-train]^2)


