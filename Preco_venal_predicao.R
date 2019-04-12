library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(caret)
library(ranger)
library(rpart)
library(rpart.plot)
library(corrplot)
library(statisticalModeling)
library(mlbench)
library(e1071)

train <- read.csv(file = 'train.csv', sep= ',', stringsAsFactors = F)
test <- read.csv(file = 'test.csv', sep= ',', stringsAsFactors = F)

# Adicionando coluna SalePrice no conjunto teste
test$SalePrice <- NA

# Criando coluna index do data set
train$dataset <- 'train'
test$dataset <- 'test'

# Juntando dataset, melhora 
joined_data <- rbind(train, test)

which(colnames(joined_data) == 'Utilities')
## [1] 10
joined_data <- joined_data[ ,-10]

# Outlier data$GarageYrBlt 2207 to 2007
joined_data$GarageYrBlt[which(joined_data$GarageYrBlt == 2207)] <- 2007

# Ajustando conteudo de variáveis onde NA é um factor
joined_data$Alley[which(is.na(joined_data$Alley))] <- 'no_alley_access'
joined_data$BsmtQual[which(is.na(joined_data$BsmtQual))] <- 'no_basement'
joined_data$BsmtCond[which(is.na(joined_data$BsmtCond))] <- 'no_basement'
joined_data$BsmtFinType1[which(is.na(joined_data$BsmtFinType1))] <- 'no_basement'
joined_data$BsmtFinType2[which(is.na(joined_data$BsmtFinType2))] <- 'no_basement'
joined_data$PoolQC[which(is.na(joined_data$PoolQC))] <- 'no_pool'
joined_data$Fence[which(is.na(joined_data$Fence))]  <- 'no_fence' 
joined_data$MiscFeature[which(is.na(joined_data$MiscFeature))] <- 'none'
joined_data$BsmtExposure[which(is.na(joined_data$BsmtExposure))] <- 'no_basemente'
joined_data$FireplaceQu[which(is.na(joined_data$FireplaceQu))] <- 'no_fireplace'
joined_data$GarageType[which(is.na(joined_data$GarageType))] <- 'no_garage'
joined_data$GarageFinish[which(is.na(joined_data$GarageFinish))] <- 'no_garage'
joined_data$GarageQual[which(is.na(joined_data$GarageQual))] <- 'no_garage'
joined_data$GarageCond[which(is.na(joined_data$GarageCond))] <- 'no_garage'

# Definindo e convertendo as variaveis categoricas (factors)
joined_data$MSSubClass <- as.factor(joined_data$MSSubClass)
joined_data$OverallQual <- as.factor(joined_data$OverallQual)
joined_data$OverallCond <- as.factor(joined_data$OverallCond)
joined_data$GarageYrBlt <- as.factor(joined_data$GarageYrBlt)
joined_data$YrSold <- as.factor(joined_data$YrSold)
joined_data$MoSold <- as.factor(joined_data$MoSold)
joined_data$GarageYrBlt <- as.factor(joined_data$GarageYrBlt)
joined_data$YearBuilt <- as.factor(joined_data$YearBuilt)
joined_data$YearRemodAdd <- as.factor(joined_data$YearRemodAdd)

# Coverte caracteres para factors
joined_data[ ,which(sapply(joined_data,  is.character))] <- 
            lapply(joined_data[ ,which(sapply(joined_data,is.character))], 
                                              as.factor)

# BsmtFinSF1  
na_position <- which(is.na(joined_data$BsmtFinSF1))

# Via de regra, para todo no_basement nas variaveis Bsmt, o valor e zero!
joined_data  %>% select(contains('Bsmt')) %>% 
    filter( BsmtFinType1== 'no_basement') %>%
    summary()

# BsmtFinSF1
joined_data$BsmtFinSF1[which(is.na(joined_data$BsmtFinSF1))] <- 0

# BsmtFinSF2
joined_data$BsmtFinSF2[which(is.na(joined_data$BsmtFinSF2))] <- 0 

# BsmtUnfSF
joined_data$BsmtUnfSF[which(is.na(joined_data$BsmtUnfSF))] <- 0 

# TotalBsmtSF 
joined_data$TotalBsmtSF[which(is.na(joined_data$TotalBsmtSF))] <- 0 

# BsmtFullBath
joined_data$BsmtFullBath[which(is.na(joined_data$BsmtFullBath))] <- 0 

# BsmtHalfBath
joined_data$BsmtHalfBath[which(is.na(joined_data$BsmtHalfBath))] <- 0 

# GarageCars
# Igual ao problema anterior, vemos que nas demais variaveis relacionada
# Temos 'no_garage', logo podemos colocar zero quando nao existir informacao

joined_data$GarageCars[which(is.na(joined_data$GarageCars))] <- 0 

# GarageArea 
joined_data$GarageArea[which(is.na(joined_data$GarageArea))] <- 0 

# GarageYrBlt
# Variavel troll: onde tem NA, esta relacionado com no_garage ... --'
joined_data$GarageYrBlt <- as.character(joined_data$GarageYrBlt)
joined_data$GarageYrBlt[which(is.na(joined_data$GarageYrBlt))] <- 'no_garage'
joined_data$GarageYrBlt <- as.factor(joined_data$GarageYrBlt)

# Separa coluna de factors para analisar
col_factor <- which(sapply(joined_data, is.factor))
joined_data_factors <- joined_data[ , col_factor]

# Separa coluna de numericos
col_numeric <- which(sapply(joined_data, is.numeric))
names(joined_data[,col_numeric])

# Coluna SalePrice: 32
list_of_var_counts <- apply(joined_data_factors, 2, function(x){
    
    table <- data.frame(table(x), as.numeric(table(x)/sum(table(x)))) 
    colnames(table) <- c('factor_type', 'raw_count', 'percentage')
    return(table)
    
})
list_of_var_counts

# Quem possue fatores que representam acima de 90%
lista_90 <- lapply(list_of_var_counts, 
        function(x){
            sub_list <- sum(x$percentage > 0.90)
        }

) 
var_fac_not_over <- which(unlist(lista_90) == 0)

summary(joined_data_factors[ ,var_fac_not_over])

# Pega index de colunas com NA
index_na <- sapply(joined_data,  function(x) which(is.na(x)))
len_na_index <- lapply(index_na, length)
index_coluns_with_na <-  as.numeric(which(unlist(len_na_index) > 0))

# Imprime nome das colunas
colnames(joined_data)[index_coluns_with_na]


# Imprime tipo de variavel das colunas com NA
sapply(joined_data[ ,index_coluns_with_na], is.factor)

sapply(joined_data[ ,index_coluns_with_na], is.numeric)

summary(joined_data[ ,index_coluns_with_na])


# Comeca com as variaveis com menos NA
# Pega colunas numericas com NA 
numeric_cols_na <- names(which(sapply(joined_data[ ,index_coluns_with_na], is.numeric)))
summary(joined_data[ ,numeric_cols_na])

# Tirando lotFrontage, pois tem muitos NA
correlations <- cor(na.omit(joined_data[ , col_numeric[c(-32)] ])) 
corrplot(correlations, method = c('square'))

3.2.Variável ‘MasVnrArea’
# Esse tipo de detalhe nao eh caracteristico de casas antigas. sim ou nao?
joined_data %>% ggplot(aes(x=YearBuilt, y=MasVnrArea, col=MasVnrType)) +
        geom_point(size=2, alpha=0.8) +
        theme_bw()


random_forest_model <- train(MasVnrArea ~ MSSubClass + LotArea + LotShape +  
                              LandContour + Neighborhood + Condition1 + 
                              BldgType + HouseStyle + OverallQual + OverallCond + 
                              YearBuilt +  Foundation + BsmtQual + BsmtFinType1 + 
                              GrLivArea + FireplaceQu + GarageType ,
                              data = na.omit(joined_data[ ,c(-1,-80,-81)]),
                              tuneGrid= data.frame( mtry=c(5,8,10,15,20) ),
                              method= "rf",
                              trControl= trainControl(method='cv', number=5, 
                              verboseIter = TRUE))

print(random_forest_model)
plot(random_forest_model)
 
prediction <- predict(random_forest_model, joined_data)
joined_data$MasVnrArea[ which(is.na(joined_data$MasVnrArea)) ] <- 
            as.integer(prediction[which(is.na(joined_data$MasVnrArea))]) 

3.3.Variável ‘LotFrontage’
lot_var <- joined_data %>% select(contains('Lot'))
lot_var %>% ggplot(aes(x=LotFrontage, y=LotArea, col=LotShape)) +
    geom_point(size=3, alpha=0.6) +
    facet_grid(~ LotShape) +
    scale_y_log10() +
    theme_bw() 

joined_data %>% 
    select(contains('Lot')) %>%
    filter(is.na(LotFrontage)) %>%
    group_by(LotShape) %>%
    summarize(mean_area = mean(LotArea), lotShape_counts = n())

random_forest_model <- train(LotFrontage ~ LotArea + LotShape + LotConfig + 
                             MSSubClass + LandContour + Neighborhood + Condition1 +
                             BldgType + HouseStyle + OverallQual + OverallCond +
                             YearBuilt +  Foundation + BsmtQual + BsmtFinType1 + 
                             GrLivArea + FireplaceQu + GarageType,
                             data = na.omit(joined_data[ ,c(-1,-80,-81)]),
                             tuneGrid= data.frame( mtry=c(5,8,10,12,14,18) ),
                             method= "rf",
                             trControl= trainControl(method='cv', number=5, 
                             verboseIter = TRUE) 
)       
print(random_forest_model)
plot(random_forest_model)

prediction <- predict( random_forest_model, joined_data)
data.frame(original= joined_data$LotFrontage, prediction)
joined_data$LotFrontage[ which(is.na(joined_data$LotFrontage)) ] <- as.integer(prediction[which(is.na(joined_data$LotFrontage))]) 

4.Estimando pseudo valores (categóricos)
4.1.Variável ‘MSZoning’
random_forest_model <- train(MSZoning ~ LotFrontage + LotArea + LotShape + 
                                 LotConfig + MSSubClass +
                                 LandContour + Neighborhood + Condition1 +
                                 BldgType + HouseStyle + OverallQual + OverallCond +
                                 YearBuilt +  Foundation + BsmtQual + BsmtFinType1 + 
                                 GrLivArea + FireplaceQu + GarageType,
                             data = na.omit(joined_data[ ,c(-1,-80,-81)]),
                             tuneGrid= data.frame( mtry=c(5,8,10,12,14,18) ),
                             method= "rf",
                             trControl= trainControl(method='cv', number=5, verboseIter = TRUE) 
)       
print(random_forest_model)
plot(random_forest_model)

prediction <- predict( random_forest_model, joined_data)
data.frame(original= joined_data$MSZoning, prediction)

joined_data$MSZoning[ which(is.na(joined_data$MSZoning)) ] <- prediction[which(is.na(joined_data$MSZoning))] 


4.2.Variável ‘Exterior1st’
# Pegadinha $#%$!, tirar garage year built pq tem NA nas unicas classes

random_forest_model <- train(Exterior1st ~ LotFrontage + LotArea + LotShape + 
                                 LotConfig + MSSubClass +
                                 LandContour + Neighborhood + Condition1 +
                                 BldgType + HouseStyle + OverallQual + OverallCond +
                                 YearBuilt +  Foundation + BsmtQual + BsmtFinType1 + 
                                 GrLivArea + FireplaceQu + GarageType,
                             data = na.omit(joined_data[ ,c(-1,-59,-80,-81)]),
                             tuneGrid= data.frame( mtry=c(5,8,10,12,14,18) ),
                             method= "rf",
                             trControl= trainControl(method='cv', number=5, verboseIter = TRUE) 
)       

print(random_forest_model)
plot(random_forest_model)

prediction <- predict( random_forest_model, joined_data)
View(data.frame(original= joined_data$Exterior1st, prediction))

joined_data$Exterior1st[ which(is.na(joined_data$Exterior1st)) ] <- prediction[which(is.na(joined_data$Exterior1st))] 

4.3.Variável ‘Exterior2nd’
random_forest_model <- train(Exterior2nd ~ LotFrontage + LotArea + LotShape + LotConfig + MSSubClass +
                                 LandContour + Neighborhood + Condition1 +
                                 BldgType + HouseStyle + OverallQual + OverallCond +
                                 YearBuilt +  Foundation + BsmtQual + BsmtFinType1 + 
                                 GrLivArea + FireplaceQu + GarageType,
                             data = na.omit(joined_data[ ,c(-1,-59,-80,-81)]),
                             tuneGrid= data.frame( mtry=c(5,8,10,12,14,18, 20) ),
                             method= "rf",
                             trControl= trainControl(method='cv', number=5, verboseIter = TRUE) 
)       

print(random_forest_model)
plot(random_forest_model)

prediction <- predict( random_forest_model, joined_data)

joined_data$Exterior2nd[ which(is.na(joined_data$Exterior2nd)) ] <- prediction[which(is.na(joined_data$Exterior2nd))] 
4.4.Variável ‘MasVnrType’
random_forest_model <- train(MasVnrType ~ LotFrontage + LotArea + LotShape + LotConfig + MSSubClass +
                                 LandContour + Neighborhood + Condition1 +
                                 BldgType + HouseStyle + OverallQual + OverallCond +
                                 YearBuilt +  Foundation + BsmtQual + BsmtFinType1 + 
                                 GrLivArea + FireplaceQu + GarageType,
                             data = na.omit(joined_data[ ,c(-1,-80,-81)]),
                             tuneGrid= data.frame( mtry=c(5,10,15,28) ),
                             method= "rf",
                             trControl= trainControl(method='cv', number=5, verboseIter = TRUE) 
)       

print(random_forest_model)
plot(random_forest_model)
summary(random_forest_model)

prediction <- predict( random_forest_model, joined_data)
joined_data$MasVnrType[ which(is.na(joined_data$MasVnrType)) ] <- prediction[which(is.na(joined_data$MasVnrType))] 
4.5.Variável ‘Electrical’
random_forest_model <- train(Electrical ~ MasVnrType + LotFrontage + LotArea + LotShape + LotConfig + MSSubClass +
                                 LandContour + Neighborhood + Condition1 +
                                 BldgType + HouseStyle + OverallQual + OverallCond +
                                 YearBuilt +  Foundation + BsmtQual + BsmtFinType1 + 
                                 GrLivArea  + GarageType,
                             data = na.omit(joined_data[ ,c(-1,-80,-81)]),
                             tuneGrid= data.frame( mtry=c(5,10,15,19) ),
                             method= "rf",
                             trControl= trainControl(method='cv', number=5, verboseIter = TRUE) 
)       

print(random_forest_model)
plot(random_forest_model)
summary(random_forest_model)

prediction <- predict( random_forest_model, joined_data)
joined_data$Electrical[ which(is.na(joined_data$Electrical)) ] <- prediction[which(is.na(joined_data$Electrical))] 
4.6.Variável ‘SaleType’
random_forest_model <- train(SaleType ~ MasVnrType + LotFrontage + LotArea + LotShape + LotConfig + MSSubClass +
                                 LandContour + Neighborhood + Condition1 +
                                 BldgType + HouseStyle + OverallQual + OverallCond +
                                 YearBuilt +  Foundation + BsmtQual + BsmtFinType1 + 
                                 GrLivArea  + GarageType,
                             data = na.omit(joined_data[ ,c(-1,-80,-81)]),
                             tuneGrid= data.frame( mtry=c(5,10,15,19) ),
                             method= "rf",
                             trControl= trainControl(method='cv', number=5, verboseIter = TRUE) 
)       

print(random_forest_model)
plot(random_forest_model)

prediction <- predict( random_forest_model, joined_data)
joined_data$SaleType[ which(is.na(joined_data$SaleType)) ] <- prediction[which(is.na(joined_data$SaleType))] 
4.7.Variável ‘Functional’
random_forest_model <- train(Functional ~ SaleType + MasVnrType + LotFrontage + LotArea + LotShape + LotConfig + MSSubClass +
                                 LandContour + Neighborhood + Condition1 +
                                 BldgType + HouseStyle + OverallQual + OverallCond +
                                 YearBuilt +  Foundation + BsmtQual + BsmtFinType1 + 
                                 GrLivArea  + GarageType,
                             data = na.omit(joined_data[ ,c(-1,-80,-81)]),
                             tuneGrid= data.frame( mtry=c(5,10,15,19) ),
                             method= "rf",
                             trControl= trainControl(method='cv', number=5, verboseIter = TRUE) 
)       

print(random_forest_model)
plot(random_forest_model)
summary(random_forest_model)

prediction <- predict( random_forest_model, joined_data)
joined_data$Functional[ which(is.na(joined_data$Functional)) ] <- prediction[which(is.na(joined_data$Functional))] 
4.8.Variável ‘KitchenQual’
random_forest_model <- train(KitchenQual ~ SaleType + MasVnrType + LotFrontage + LotArea + LotShape + LotConfig + MSSubClass +
                                 LandContour + Neighborhood + Condition1 +
                                 BldgType + HouseStyle + OverallQual + OverallCond +
                                 YearBuilt +  Foundation + BsmtQual + BsmtFinType1 + 
                                 GrLivArea  + GarageType,
                             data = na.omit(joined_data[ ,c(-1,-80,-81)]),
                             tuneGrid= data.frame( mtry=c(5,10,15,19) ),
                             method= "rf",
                             trControl= trainControl(method='cv', number=5, verboseIter = TRUE) 
)       

print(random_forest_model)
plot(random_forest_model)

prediction <- predict( random_forest_model, joined_data)
joined_data$KitchenQual[ which(is.na(joined_data$KitchenQual)) ] <- prediction[which(is.na(joined_data$KitchenQual))] 
write.table(file='joined_data_cleaned_without_NA.txt', joined_data, sep = '\t', row.names = F)
5.Explorando os dados
5.1.Carrega os dados salvos
joined_data <- read.table(file='/home/davi/Desktop/house_prices-_advanced_regression_techniques/joined_data_cleaned_without_NA.txt', sep = '\t', header = TRUE)

joined_data$GarageYrBlt <- as.factor(joined_data$GarageYrBlt)
joined_data$Alley[which(is.na(joined_data$Alley))] <- 'no_alley_access'
joined_data$BsmtQual[which(is.na(joined_data$BsmtQual))] <- 'no_basement'
joined_data$BsmtCond[which(is.na(joined_data$BsmtCond))] <- 'no_basement'
joined_data$BsmtFinType1[which(is.na(joined_data$BsmtFinType1))] <- 'no_basement'
joined_data$BsmtFinType2[which(is.na(joined_data$BsmtFinType2))] <- 'no_basement'
joined_data$PoolQC[which(is.na(joined_data$PoolQC))] <- 'no_pool'
joined_data$Fence[which(is.na(joined_data$Fence))]  <- 'no_fence' 
joined_data$MiscFeature[which(is.na(joined_data$MiscFeature))] <- 'none'
joined_data$BsmtExposure[which(is.na(joined_data$BsmtExposure))] <- 'no_basemente'
joined_data$FireplaceQu[which(is.na(joined_data$FireplaceQu))] <- 'no_fireplace'
joined_data$GarageType[which(is.na(joined_data$GarageType))] <- 'no_garage'
joined_data$GarageFinish[which(is.na(joined_data$GarageFinish))] <- 'no_garage'
joined_data$GarageQual[which(is.na(joined_data$GarageQual))] <- 'no_garage'
joined_data$GarageCond[which(is.na(joined_data$GarageCond))] <- 'no_garage'

# Definindo e convertendo as variaveis categoricas (factors)
joined_data$MSSubClass <- as.factor(joined_data$MSSubClass)
joined_data$OverallQual <- as.factor(joined_data$OverallQual)
joined_data$OverallCond <- as.factor(joined_data$OverallCond)
joined_data$GarageYrBlt <- as.factor(joined_data$GarageYrBlt)
joined_data$YrSold <- as.factor(joined_data$YrSold)
joined_data$MoSold <- as.factor(joined_data$MoSold)
joined_data$GarageYrBlt <- as.factor(joined_data$GarageYrBlt)
joined_data$YearBuilt <- as.factor(joined_data$YearBuilt)
joined_data$YearRemodAdd <- as.factor(joined_data$YearRemodAdd)
joined_data$GarageCars <- as.factor(joined_data$GarageCars)

joined_data <- joined_data %>% mutate(age_until_sale =  as.numeric(as.character(YrSold)) - as.numeric(as.character(YearBuilt)))

# Criar nova variavel: area_habitada_casa
# TotalBsmtSF: Total square feet of basement area
# GrLivArea: Above grade (ground) living area square feet
# GarageArea: Size of garage in square feet
# WoodDeckSF: Wood deck area in square feet
# OpenPorchSF: Open porch area in square feet
# EnclosedPorch: Enclosed porch area in square feet
# 3SsnPorch: Three season porch area in square feet
# ScreenPorch: Screen porch area in square feet
# PoolArea: Pool area in square feet

joined_data <- joined_data %>% mutate(house_useful_area = GrLivArea + GarageArea +
                                          WoodDeckSF + OpenPorchSF + PoolArea)
Separando finalmente o cojunto de treino e teste

train <- joined_data %>% 
    filter(dataset == 'train')

test <- joined_data %>%
    filter(dataset == 'test')
Explorando a relacção entre algumas variáveis de interesse para poderem ser usadas na contrução dos modelos

Casas mais novas parecem valer mais no geral? Muito diferente do que esperávamos.

train %>% 
    ggplot(aes(x= age_until_sale, y= SalePrice,  col=Neighborhood)) +
    geom_point(size=4, alpha=0.4)+
    theme_bw()


Basement qual tem muita influência no preço da casa.

train %>% 
    ggplot(aes(x= BsmtQual, y= SalePrice)) +
    geom_boxplot()+
    theme_bw()


GrLivArea vs SalePrice tem uma relação linear aparente

“Above Grade Living Area Total Sq. Ft. represents the finished above grade living area in a house. It does not include unfinished areas, the area occupied by a cathedral ceiling, enclosed non-living areas such as garages and enclosed porches, or basement area or finished basement living space”.

train %>% 
    ggplot(aes(x= GrLivArea, y= SalePrice)) +
    geom_point(size=4, alpha=0.4)+
    theme_bw()


# Fazendo uma regressão com e sem o outlier
summary(lm(SalePrice ~ GrLivArea, data=train))
## 
## Call:
## lm(formula = SalePrice ~ GrLivArea, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -462999  -29800   -1124   21957  339832 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 18569.026   4480.755   4.144 3.61e-05 ***
## GrLivArea     107.130      2.794  38.348  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 56070 on 1458 degrees of freedom
## Multiple R-squared:  0.5021, Adjusted R-squared:  0.5018 
## F-statistic:  1471 on 1 and 1458 DF,  p-value: < 2.2e-16
summary(lm(SalePrice ~ GrLivArea, data=train[-which(train$GrLivArea > 4500), ]))
## 
## Call:
## lm(formula = SalePrice ~ GrLivArea, data = train[-which(train$GrLivArea > 
##     4500), ])
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -197730  -29815    -337   23239  332534 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 7168.970   4432.501   1.617    0.106    
## GrLivArea    115.040      2.782  41.358   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 53920 on 1456 degrees of freedom
## Multiple R-squared:  0.5402, Adjusted R-squared:  0.5399 
## F-statistic:  1710 on 1 and 1456 DF,  p-value: < 2.2e-16
Interessante: Parece que a area do terreno não influencia muito no preço da casa.

train %>% 
    ggplot(aes(x= LotArea, y= SalePrice, col= Neighborhood)) +
    geom_point(size=3, alpha=0.4)+
    theme_bw()


Casas com piscina, sendo elas de boa qualidade, valem muito mais.

train %>% 
    ggplot(aes(x= PoolQC, y= SalePrice)) +
    geom_boxplot()+
    theme_bw()
Qualidade da casa vs preço, ótima relação.

train %>% 
    ggplot(aes(x= OverallQual, y= SalePrice)) +
    geom_boxplot()+
    theme_bw()


Somei todas as “áreas úteis da casa” para testar se a relação melhora. Ao ver, sim!

train %>% 
    ggplot(aes(x= house_useful_area, y= SalePrice, col=OverallQual)) +
    geom_point(size=3, alpha=0.7)+
    theme_bw() + 
    scale_color_brewer(palette='Spectral')


6. Treinando e validando o modelo para o conjunto de teste
6.1. Random forest
random_forest_model <- train(SalePrice~ .,
                             data = train,
                             tuneGrid= data.frame(mtry=c(30, 40, 50, 55, 60,65, 70,75,79,80,82)),
                             method= "rf",
                             trControl= trainControl(method='cv', number=50, verboseIter = TRUE ))       
plot(random_forest_model)
print(random_forest_model)

prediction <- predict(random_forest_model, test)
to_submit <-  data.frame(Id= test$Id , SalePrice = prediction)
write.csv(file='prediction_results.csv', to_submit, row.names = F ) 
6.2.Modelo linear
Testando modelo linear sem os ‘outliers’

lm_model <- lm(SalePrice ~ house_useful_area + OverallQual + GrLivArea +
                   TotalBsmtSF + GarageArea  +         
                   ExterQual + X1stFlrSF + BsmtQual +
                   KitchenQual + FullBath +TotRmsAbvGrd,  data=train[c(-524, -1299), ])
                   
summary(lm_model)
summary(train)

prediction <- predict(lm_model, test)
to_submit <-  data.frame(Id= test$Id , SalePrice = prediction)
write.csv(file='prediction_results.csv', to_submit, row.names = F ) 
summary(to_submit)
6.3.SVM (Support Machine Vector)
svm_model <- train(SalePrice ~ OverallQual + house_useful_area + age_until_sale +  PoolQC + Neighborhood,
      data = train[c(-524, -1299), ],
      method= "svmPoly",
      trControl= trainControl(method='cv', number=3, verboseIter = TRUE ))  

plot(svm_model)
print(svm_model)

prediction <- predict(svm_model, test)
to_submit <-  data.frame(Id= test$Id , SalePrice = prediction)

write.csv(file='prediction_results.csv', to_submit, row.names = F ) 
prediction <- predict(svm_model, train)
6.4.GBM (Gradient Boosting Machine)
metric <- "RMSE"
trainControl <- trainControl(method="cv", number=30)
caretGrid <- expand.grid(interaction.depth= c(1,4,5,6), 
                         n.trees= c(2500,3000, 3200, 3400, 3600) ,
                         shrinkage= c(0.1,0.01,0.02,0.03),
                         n.minobsinnode=10)
set.seed(99)

#gbm.caret <- train(SalePrice ~ .
#                   , data=train
#                   , distribution="gaussian"
#                   , method="gbm"
#                   , trControl=trainControl
#                   , verbose=FALSE
#                   , tuneGrid=caretGrid
#                   , metric=metric
#                   , bag.fraction=0.75
#)                  

train_Data<- as.matrix(train, rownames.force=NA)
train_Data <- as(train_Data, "sparseMatrix")
train_Data <- xgb.DMatrix(data = train_Data, label = train[,"SalePrice"])

param <- list(objective = "reg:linear",
              evalmetric = 'rmse', max.depth =10, eta = 0.01, 
              subsample = 0.9, colsamplebytree = 0.5,
              nthread = 10)
watchlist <- list(eval = train_Data)
xgb.model <- xgb.train(param, data = train_Data, watchlist, 
                       nrounds = 20000, maximize = FALSE, 
                       earlystoppingrounds = 5, printevery_n = 100)



#print(gbm.caret)
#plot(gbm.caret)
#prediction <- predict(gbm.caret, test)
#to_submit <-  data.frame(Id= test$Id , SalePrice = prediction)
#write.csv(file='prediction_results.csv', to_submit, row.names = F ) 

print(xgb.model)
prediction <- predict(xgb.model,test)
to_submit <-  data.frame(ID = test$Id, Preco =prediction)
write.csv(file='prediction_results.csv', to_submit, row.names = F ) 

7.1.Resultados obtidos
Ainda há muito a trabalhar nestes dados. O melhor resultado foi obtido com o algoritmo GBM. No Kaggle, conseguimos alcançar uma posição mediana em relação aos demais competidores. Nossa melhor pontuação foi de 0.13477

Posição no ranking geral