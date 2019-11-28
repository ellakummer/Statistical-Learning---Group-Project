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

# test (test error MSE) :
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


# ----------- POLYNOMIAL REGRESSION + SPLINE -----------



# ----------- TREE BASED METHOD  -----------


# ----------- BEST SUBSET SELECTION  -----------
# with the result : try non-linear method (with lm.())

