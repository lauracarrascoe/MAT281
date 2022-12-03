library(ggplot2)
library(caret) # Accuracy
library(e1071)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(caTools)
library(descr)
library(gmodels)
library(class)
library(splitstackshape)


data<-read.csv2("C:/Users/Biblioteca Usm/Desktop/bank/bank-full.csv")
d2<-data
test<-read.csv2("C:/Users/Biblioteca Usm/Desktop/bank/bank.csv")
t2<-test

dim(data)
str(data)
summary(data)
table(data$y)



############################################################
# Transformamos a numeros las variables binarias
############################################################

d2$default<-as.numeric(ifelse(d2$default=="yes",1,0))
d2$housing<-as.numeric(ifelse(d2$housing=="yes",1,0))
d2$loan<-as.numeric(ifelse(d2$loan=="yes",1,0))
d2$y<-as.factor(ifelse(d2$y=="yes",1,0))

##################################################
# Transformamos a numeros las variables categoricas
##################################################

job1=c("unknown","unemployed","housemaid","student","retired")
job2=c("entrepreneur","self-employed","services","blue-collar","technician")
job3=c("admin.","management")
for(i in 1:45211){
  if(d2$job[i] %in% job1){
    d2$job[i]=0
  }
  if(d2$job[i] %in% job2){
    d2$job[i]=1
  }
  if(d2$job[i] %in% job3){
    d2$job[i]=2
  }
}

d2$job<-as.numeric(d2$job)

for(i in 1:45211){
  if(d2$marital[i]=="single"){
    d2$marital[i]=0
  }
  if(d2$marital[i]=="divorced"){
    d2$marital[i]=1
  }
  if(d2$marital[i]=="married"){
    d2$marital[i]=2
  }
}

d2$marital<-as.numeric(d2$marital)


for(i in 1:45211){
  if(d2$education[i]=="primary"|d2$education[i]=="unknown"){
    d2$education[i]=0
  }
  if(d2$education[i]=="secondary"){
    d2$education[i]=1
  }
  if(d2$education[i]=="tertiary"){
    d2$education[i]=2
  }
}

d2$education<-as.numeric(d2$education)


for(i in 1:45211){
  if(d2$contact[i]=="telephone"){
    d2$contact[i]=0
  }
  if(d2$contact[i]=="cellular"){
    d2$contact[i]=1
  }
  if(d2$contact[i]=="unknown"){
    d2$contact[i]=2
  }
}

d2$contact<-as.numeric(d2$contact)



for(i in 1:45211){
  if(d2$month[i]=="jan"){
    d2$month[i]=0
  }
  if(d2$month[i]=="feb"){
    d2$month[i]=1
  }
  if(d2$month[i]=="mar"){
    d2$month[i]=2
  }
  if(d2$month[i]=="apr"){
    d2$month[i]=3
  }
  if(d2$month[i]=="may"){
    d2$month[i]=4
  }
  if(d2$month[i]=="jun"){
    d2$month[i]=5
  }
  if(d2$month[i]=="jul"){
    d2$month[i]=6
  }
  if(d2$month[i]=="aug"){
    d2$month[i]=7
  }
  if(d2$month[i]=="sep"){
    d2$month[i]=8
  }
  if(d2$month[i]=="oct"){
    d2$month[i]=9
  }
  if(d2$month[i]=="nov"){
    d2$month[i]=10
  }
  if(d2$month[i]=="dec"){
    d2$month[i]=11
  }
}

d2$month<-as.numeric(d2$month)

for(i in 1:45211){
  if(d2$poutcome[i]== "failure"){
    d2$poutcome[i]=0
  }
  if(d2$poutcome[i]== "success"){
    d2$poutcome[i]=1
  }
  if(d2$poutcome[i]== "unknown"|d2$poutcome[i]=="other"){
    d2$poutcome[i]=2
  }
}

d2$poutcome<-as.numeric(d2$poutcome)

#########################################################
# Las variables numericas son:
#  age
#  balance
#  day
#  duration
#  campaign
#  pdays
#  previous
#########################################################

test$y <- as.factor(ifelse(test$y == 'yes', 1, 0))
test$default<-as.numeric(ifelse(test$default=="yes",1,0))
test$housing<-as.numeric(ifelse(test$housing=="yes",1,0))
test$loan<-as.numeric(ifelse(test$loan=="yes",1,0))

job1=c("unknown","unemployed","housemaid","student","retired")
job2=c("entrepreneur","self-employed","services","blue-collar","technician")
job3=c("admin.","management")
for(i in 1:4521){
  if(test$job[i] %in% job1){
    test$job[i]=0
  }
  if(test$job[i] %in% job2){
    test$job[i]=1
  }
  if(test$job[i] %in% job3){
    test$job[i]=2
  }
}
test$job<-as.numeric(test$job)

for(i in 1:4521){
  if(test$marital[i]=="single"){
    test$marital[i]=0
  }
  if(test$marital[i]=="divorced"){
    test$marital[i]=1
  }
  if(test$marital[i]=="married"){
    test$marital[i]=2
  }
}
test$marital<-as.numeric(test$marital)


for(i in 1:4521){
  if(test$education[i]=="primary"|test$education[i]=="unknown"){
    test$education[i]=0
  }
  if(test$education[i]=="secondary"){
    test$education[i]=1
  }
  if(test$education[i]=="tertiary"){
    test$education[i]=2
  }
}
test$education<-as.numeric(test$education)


for(i in 1:4521){
  if(test$contact[i]=="telephone"){
    test$contact[i]=0
  }
  if(test$contact[i]=="cellular"){
    test$contact[i]=1
  }
  if(test$contact[i]=="unknown"){
    test$contact[i]=2
  }
}
test$contact<-as.numeric(test$contact)



for(i in 1:4521){
  if(test$month[i]=="jan"){
    test$month[i]=0
  }
  if(test$month[i]=="feb"){
    test$month[i]=1
  }
  if(test$month[i]=="mar"){
    test$month[i]=2
  }
  if(test$month[i]=="apr"){
    test$month[i]=3
  }
  if(test$month[i]=="may"){
    test$month[i]=4
  }
  if(test$month[i]=="jun"){
    test$month[i]=5
  }
  if(test$month[i]=="jul"){
    test$month[i]=6
  }
  if(test$month[i]=="aug"){
    test$month[i]=7
  }
  if(test$month[i]=="sep"){
    test$month[i]=8
  }
  if(test$month[i]=="oct"){
    test$month[i]=9
  }
  if(test$month[i]=="nov"){
    test$month[i]=10
  }
  if(test$month[i]=="dec"){
    test$month[i]=11
  }
}
test$month<-as.numeric(test$month)

for(i in 1:4521){
  if(test$poutcome[i]== "failure"){
    test$poutcome[i]=0
  }
  if(test$poutcome[i]== "success"){
    test$poutcome[i]=1
  }
  if(test$poutcome[i]== "unknown"|test$poutcome[i]=="other"){
    test$poutcome[i]=2
  }
}
test$poutcome<-as.numeric(test$poutcome)

#BALANCEAR
bal<-stratified(d2,c("y"),size=5289)
bal
str(bal)
summary(bal)



##########################################
#    REGRESION LOGISTICA
attach(d2)

regre_model <- glm(y ~ .,data=d2, family=binomial(link="logit"))
summary(regre_model)

#Eliminamos month
d2$month=NULL
test$month=NULL

regre_model <- glm(y ~ .,data=d2, family=binomial(link="logit"))
summary(regre_model)

#Eliminamos default
d2$default=NULL
test$default=NULL


regre_model <- glm(y ~ .,data=d2, family=binomial(link="logit"))
summary(regre_model)

#Eliminamos age
d2$age=NULL
test$age=NULL

regre_model <- glm(y ~ .,data=d2, family=binomial(link="logit"))
summary(regre_model)

#Eliminamos day
d2$day=NULL
test$day=NULL

regre_model <- glm(y ~ .,data=d2, family=binomial(link="logit"))
summary(regre_model)

#Eliminamos job
d2$job=NULL
test$job=NULL

regre_model <- glm(y ~ .,data=d2, family=binomial(link="logit"))
summary(regre_model)

p2 <- predict(regre_model,test,type="response")
pred2<-p2
pred3<-p2
pred4<-p2
pred5<-p2
pred6<-p2
for(i in 1:45211){
  if(pred2[i]>0.5){
    pred2[i]=1
  }
  else{
    pred2[i]=0
  }
}
pred2<-as.vector(pred2)
pred2<-as.factor(pred2)

for(i in 1:45211){
  if(pred3[i]>0.1){
    pred3[i]=1
  }
  else{
    pred3[i]=0
  }
}
pred3<-as.vector(pred3)
pred3<-as.factor(pred3)

for(i in 1:45211){
  if(pred4[i]>0.4){
    pred4[i]=1
  }
  else{
    pred4[i]=0
  }
}
pred4<-as.vector(pred4)
pred4<-as.factor(pred4)


for(i in 1:45211){
  if(pred5[i]>0.3){
    pred5[i]=1
  }
  else{
    pred5[i]=0
  }
}
pred5<-as.vector(pred5)
pred5<-as.factor(pred5)

for(i in 1:45211){
  if(pred6[i]>0.2){
    pred6[i]=1
  }
  else{
    pred6[i]=0
  }
}
pred6<-as.vector(pred6)
pred6<-as.factor(pred6)

##########################################################################
# Matriz de Confusion
##########################################################################

confusionMatrix(pred2,test$y,positive = "1")
confusionMatrix(pred3,test$y,positive = "1")
confusionMatrix(pred4,test$y,positive = "1")
confusionMatrix(pred5,test$y,positive = "1")
confusionMatrix(pred6,test$y,positive = "1")

# REGRESION BALANCEADA
regre_model_bal <- glm(y ~ .,data=bal, family=binomial(link="logit"))
summary(regre_model_bal)

p2_bal <- predict(regre_model_bal,test,type="response")
pred2_bal<-p2_bal
pred3_bal<-p2_bal
pred4_bal<-p2_bal


for(i in 1:45211){
  if(pred2_bal[i]>0.5){
    pred2_bal[i]=1
  }
  else{
    pred2_bal[i]=0
  }
}
pred2_bal<-as.vector(pred2_bal)
pred2_bal<-as.factor(pred2_bal)

for(i in 1:45211){
  if(pred3_bal[i]>0.4){
    pred3_bal[i]=1
  }
  else{
    pred3_bal[i]=0
  }
}
pred3_bal<-as.vector(pred3_bal)
pred3_bal<-as.factor(pred3_bal)

for(i in 1:45211){
  if(pred4_bal[i]>0.3){
    pred4_bal[i]=1
  }
  else{
    pred4_bal[i]=0
  }
}
pred4_bal<-as.vector(pred4_bal)
pred4_bal<-as.factor(pred4_bal)

# MATRIZ DE CONFUSION
confusionMatrix(pred2_bal,test$y,positive = "1")
confusionMatrix(pred3_bal,test$y,positive = "1")
confusionMatrix(pred4_bal,test$y,positive = "1")



##########################
# ARBOL DE DESICION

tree_model = rpart(y ~ ., data = d2, method="class", minbucket=15)
rpart.plot(tree_model)

#con datos balanceados
tree_model_bal = rpart(y ~ ., data = bal, method="class", minbucket=15)
rpart.plot(tree_model_bal)

p<-predict(tree_model,test,type="prob")
pred2<-factor(ifelse(p[,2]>0.1,1,0))
pred<-factor(ifelse(p[,2]>0.5,1,0))
pred3<-factor(ifelse(p[,2]>0.2,1,0))
pred4<-factor(ifelse(p[,2]>0.3,1,0))
pred5<-factor(ifelse(p[,2]>0.4,1,0))

#problema balanceado
p_b<-predict(tree_model,test,type="prob")
pred2_b<-factor(ifelse(p_b[,2]>0.1,1,0))
pred_b<-factor(ifelse(p_b[,2]>0.5,1,0))
pred3_b<-factor(ifelse(p_b[,2]>0.2,1,0))
pred4_b<-factor(ifelse(p_b[,2]>0.3,1,0))
pred5_b<-factor(ifelse(p_b[,2]>0.4,1,0))


#############################################################
# Matriz de Confusion
#############################################################

confusionMatrix(pred,test$y,positive = "1")
confusionMatrix(pred2,test$y,positive = "1")
confusionMatrix(pred3,test$y,positive = "1")
confusionMatrix(pred4,test$y,positive = "1")
confusionMatrix(pred5,test$y,positive = "1")

#problema balanceado
confusionMatrix(pred_b,test$y,positive = "1")
confusionMatrix(pred2_b,test$y,positive = "1")
confusionMatrix(pred3_b,test$y,positive = "1")
confusionMatrix(pred4_b,test$y,positive = "1")
confusionMatrix(pred5_b,test$y,positive = "1")


###########################
#      KNN

pred_knn=knn(train = d2, test = test, d2$y, k=20, prob=T)
pred_knn2=knn(train = d2, test = test, d2$y, k=15, prob=T)
pred_knn3=knn(train = d2, test = test, d2$y, k=10, prob=T)
pred_knn4=knn(train = d2, test = test, d2$y, k=5, prob=T)

#BALANCEADO

pred_knn_bal=knn(train = bal, test = test, bal$y, k=20, prob=T)
pred_knn2_bal=knn(train = bal, test = test, bal$y, k=15, prob=T)
pred_knn3_bal=knn(train = d2, test = test, d2$y, k=10, prob=T)
pred_knn4_bal=knn(train = d2, test = test, d2$y, k=5, prob=T)

##########################################################
# Matriz de confusion
#########################################################

confusionMatrix(pred_knn,test$y,positive = "1")
confusionMatrix(pred_knn2,test$y,positive = "1")
confusionMatrix(pred_knn3,test$y,positive = "1")
confusionMatrix(pred_knn4,test$y,positive = "1")
#BALANCEADO

confusionMatrix(pred_knn_bal,test$y,positive = "1")
confusionMatrix(pred_knn2_bal,test$y,positive = "1")
confusionMatrix(pred_knn3_bal,test$y,positive = "1")
confusionMatrix(pred_knn4_bal,test$y,positive = "1")
