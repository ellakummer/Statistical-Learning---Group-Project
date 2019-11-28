my_data <- heart_datas
names(my_data)
attach(my_data)

# ----------- LINEAR REGRESSION WITH VALIDATION SET APPROACH -----------

set.seed(1)

# training set (we take half of our datas) : 
train=sample(303,101)

# we apply a linear regression :
lm.fit=lm(target~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal,data=my_data,subset=train)

# test (test error MSE) :
mean((target-predict(lm.fit,my_data))[-train]^2)
# WE FIND MSE = 0.1540146

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

set.seed(2)
train=sample(1:nrow(my_data_2), 150)
my_data_2.test=my_data_2[-train,]
disease.test=disease[-train]
tree.datas=tree(disease~.-target,my_data_2,subset=train)
tree.pred=predict(tree.datas,my_data_2,type="class")
table(tree.pred,disease.test)
(104+50)/200

# PRUNNING TREE 
set.seed(3)
cv.prune.datas=cv.tree(tree.datas,FUN=prune.misclass)
cv.prune.datas
par(mfrow=c(1,2))
prune.datas=prune.misclass(tree.datas,best=4)
plot(prune.datas)
text(prune.datas,pretty=0)
# check error 
tree.pred=predict(prune.datas,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200


# ---------------------------------------------------------------
# ------------------ BEST SUBSET SELECTION  ---------------------
# ---------------------------------------------------------------

# with the result : try non-linear method (with lm.())



# ---------------------------------------------------------------
# ---------------------NON LINEAR MODELING ----------------------
# ---------------------------------------------------------------

# ----------- POLYNOMIAL REGRESSION + SPLINE -----------

# see tuto5 !! :) 





