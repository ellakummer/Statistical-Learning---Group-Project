my_data <- test2
names(my_data)

# ----------- LINEAR REGRESSION WITH VALIDATION SET APPROACH -----------

attach(my_data)
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
# let's take leave-one-out cross validation -> far less bias, and we have time to implement it 

glm.fit=glm(target~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal,data=my_data,subset=train)
coef(glm.fit)

