# seta o diretorio que vamos trabalhar
setwd("D:/users/F65391A/Downloads")

#bibliotecas
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#importa o arquivo
train <- read.csv("D:/users/F65391A/Downloads/train.csv")
test <- read.csv("D:/users/F65391A/Downloads/test.csv")

#repete 0 na coluna survived e cria o arquivi

test$Survived <- rep(0, 418)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

#sumariza coluna survived por sexo

prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived),1)

#zera coluna surveved e colca 1 onde o sexo for feminino
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

#zera coluna criança e colca 1 para quem for maior de 18
train$Child <- 0
train$Child[train$Age < 18] <- 1


#agrega coluna surveved por sexo e indicador de criança
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)

#cria faixa de preço do ticket
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

#agrega coluna survived por sexo, e indicador de criança
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#cria o arquivo 
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)


#usa a funçao rpart de arvore de decisao e plota na tela
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")

fancyRpartPlot(fit)

#usa o predict para prever quem sobreviveu utlizando a arvore de decisao
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#usa a arvore de decisão mas aumentando a quantidade de niveis e complexidade

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#colcoa n/a onde tem valores em branco e mescla os dois arquivos em um só

combi <- rbind(train, test)

#converte para caracter
combi$Name <- as.character(combi$Name)

#encontra o MR dentrodo nome separando o texto
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]strsplit(combi$Name[1], split='[,.]')[[1]][2]
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

#combina os titulos que sginificam a mesma coisa
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
ncombi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#converte para fator
combi$Title <- factor(combi$Title)

#soma total de familiaries, somando as duas colunas que possuem essa informaçao
combi$FamilySize <- combi$SibSp + combi$Parch + 1

#separa o sobrenome
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

#classifica familias pequenas
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

#cria tabela somando numero de familias
famIDs <- data.frame(table(combi$FamilyID))

#separa apenas familias que continuam sendo menores que  3 pelo sobrenome e separa
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

#separa novamente os arquivos apos a criaçao das colunas
train <- combi[1:891,]
test <- combi[892:1309,]

#cria a arvore de decisao com base no novo arquivo
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#como existem muitas idades vazias, vamos tentar prever essas idades em uma arvore de decisao

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

#limpa registro em branco da coluna embarcado e afar
which(combi$Embarked == '')
which(is.na(combi$Fare))

combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#reduz nuemro de niveis da coluna de familia
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)