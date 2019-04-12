
#*********************************************************************
# Base titanic
#*********************************************************************

library(tidyverse)
library(titanic)
library(bindrcpp)
library(hrbrthemes)

 library(ggplot2) 
 library(ggthemes) 
 library(scales) 
 library(dplyr) 
 library(mice) 
 library(randomForest)
 library(caret)

treino <- read.csv('train.csv', stringsAsFactors = F) 
teste  <- read.csv('test.csv', stringsAsFactors = F)
#head(teste)

 titanic_teste  <- bind_rows(treino, teste) 
 str(titanic_teste)
  
 #tratamento de dados

 #campo Titulo.....................................................
 titanic_teste$titulo <- gsub("(.*, )|(\\..*)", "", titanic_teste$Name)
 #forma como le os campos pode apresentar erro, deve ser colunas

 #conjunto classe..................................................
 titulo_classe <- c("Dona", "Lady", "the Countess","Capt", "Col", "Don", 
                    "Dr", "Major", "Rev", "Sir", "Jonkheer")

 #campo titulo..................................................
 titanic_teste$titulo[titanic_teste$titulo == "Mlle"]        <- "Miss" 
 titanic_teste$titulo[titanic_teste$titulo == "Ms"]          <- "Miss"
 titanic_teste$titulo[titanic_teste$titulo == "Mme"]         <- "Mrs" 
 titanic_teste$titulo[titanic_teste$titulo %in% titulo_classe] <- "titulo_classe"
 
 #campo sobrenome..................................................
 titanic_teste$Sobrenome <- sapply(titanic_teste$Name,  
                            function(x) strsplit(x, split = "[,.]")[[1]][1])

  #table(titanic_teste$Sex, titanic_teste$titulo)
 
  #campo familia ..................................................
  titanic_teste$Ftam <- titanic_teste$SibSp + titanic_teste$Parch + 1
  titanic_teste$Familia <- paste(titanic_teste$Sobrenome, titanic_teste$Ftam, sep="_")

  titanic_teste$Ftam_dimensao[titanic_teste$Ftam == 1] <- "solteiro"
  titanic_teste$Ftam_dimensao[titanic_teste$Ftam < 5 & titanic_teste$Ftam > 1] <- "Fam_P"
  titanic_teste$Ftam_dimensao[titanic_teste$Ftam > 4] <- "Fam_G"

 #ggplot(titanic_teste[1:891,], aes(x = Ftam, fill = factor(Survived))) +
 # geom_bar(stat="count", position="dodge") +
 # scale_x_continuous(breaks=c(1:11)) +
 # labs(x = "Tamanho da Familia") +
 # theme_few()

 # mosaicplot(table(titanic_teste$Ftam_dimensao, titanic_teste$Survived), 
 #           main="Tamanho da familia por sobrevivencia", shade=TRUE)

 #campo cabine..................................................
 #titanic_teste$Cabin[1:28]
 #strsplit(titanic_teste$Cabin[2], NULL)[[1]]
 titanic_teste$Deck<-factor(sapply(titanic_teste$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
 
 #titanic_teste[c(62, 830), "Embarked"] 

 #ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
 # geom_boxplot() +
 # geom_hline(aes(yintercept=80), 
 #   colour="red", linetype="dashed", lwd=2) +
 # scale_y_continuous(labels=dollar_format()) +
 # theme_few()

 #embark_fare <- titanic_teste %>%
 # filter(PassengerId != 62 & PassengerId != 830)
  titanic_teste$Embarked[c(62, 830)] <- "C"

  #campo fare..................................................

  #ggplot(titanic_teste[titanic_teste$Pclass == '3' & titanic_teste$Embarked == 'S', ], 
  #aes(x = Fare)) +
  #geom_density(fill = '#99d6ff', alpha=0.4) + 
  #geom_vline(aes(xintercept=median(Fare, na.rm=T)),
  #  colour='red', linetype='dashed', lwd=1) +
  #scale_x_continuous(labels=dollar_format()) +
  #theme_few()

  #titanic_teste[1044, ]  
  titanic_teste$Fare[1044] <- median(titanic_teste[titanic_teste$Pclass == '3' & titanic_teste$Embarked == 'S', ]$Fare, na.rm = TRUE)


  #campo idade..................................................
  #sum(is.na(titanic_teste$Age))

  fator_var <- c("PassengerId","Pclass","Sex","Embarked","titulo","Sobrenome",
                "Familia","Ftam_dimensao")

  titanic_teste[fator_var] <- lapply(titanic_teste[fator_var], function(x) as.factor(x))


  set.seed(129)
  mice_mod <- mice(titanic_teste[, !names(titanic_teste) %in% 
              c("PassengerId","Name","Ticket","Cabin","Familia","Sobrenome",
              "Survived")], method="rf") 

  mice_output <- complete(mice_mod)

  #par(mfrow=c(1,2))

  #hist(titanic_teste$Age, freq=F, main="Age: Informação original", 
  #     col="darkblue", ylim=c(0,0.04))

  #hist(mice_output$Age, freq=F, main="Age: Resposta MICE", 
  #     col="lightblue", ylim=c(0,0.04))

  titanic_teste$Age <- mice_output$Age

  #sum(is.na(titanic_teste$Age))

  #ggplot(titanic_teste[1:891,], aes(Age, fill = factor(Survived))) + 
  # geom_histogram() + 
  ## I include Sex since we know (a priori) it's a significant predictor
  #facet_grid(.~Sex) + 
  #theme_few()
  ## mensagem de critica: stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

  #campo crianca..................................................
  titanic_teste$Crianca[titanic_teste$Age < 18] <- "Crianca"
  titanic_teste$Crianca[titanic_teste$Age >= 18] <- "Adulto"
  titanic_teste$Crianca  <- factor(titanic_teste$Crianca)

  #table(titanic_teste$Crianca, titanic_teste$Survived)

  #campo mae..................................................
  titanic_teste$Mae <- "Nao Mae"

  titanic_teste$Mae[titanic_teste$Sex == "female" & 
              titanic_teste$Parch > 0 & titanic_teste$Age > 18 & 
              titanic_teste$titulo != 'Miss'] <- "Mae"

  #table(titanic_teste$Mae, titanic_teste$Survived)

  titanic_teste$Mae <- factor(titanic_teste$Mae)


  testfinal$Fare <- median(testfinal $Fare,na.rm=TRUE)
  testfinal$Embarked[testfinal$Embarked==""] <- "C"

  #####################################################################

  trainfinal <- titanic_teste[1:891,]
  testfinal <- titanic_teste[892:1309,]

  #------------------------------------------aqyu
  ft_var <- c("PassengerId","Pclass","Sex","Embarked")

  testfinal[ft_var] <- lapply(testfinal[ft_var], function(x) as.factor(x))

  set.seed(556)
  
  rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +
                            Fare + Embarked,data = testfinal)

  plot(rf_model, ylim=c(0,0.36))
  legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
  
  ##predict 
  prd <- predict(rf_model,testfinal)
  confusionMatrix(prd,testfinal$Survived)
  
  importance <- importance(rf_model)

  varImportance <- data.frame(Variables = row.names(importance), 
                        Importance = round(importance[,'MeanDecreaseGini'],2))

  rankImportance <- varImportance %>%
                      mutate(Rank = paste0("#",dense_rank(desc(Importance))))


  #ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
  #  y = Importance, fill = Importance)) +
  #geom_bar(stat='identity') + 
  #geom_text(aes(x = Variables, y = 0.5, label = Rank),
  #  hjust=0, vjust=0.55, size = 4, colour = 'red') +
  #labs(x = 'Variables') +
  #coord_flip() + 
  #theme_few()


  solution <- data.frame(PassengerID = testfinal$PassengerId, 
                         Survived = prd )

write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)











