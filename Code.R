#Loading required packages:
require(tidyverse)
require(plyr)
require(dplyr)
require(caret)
require(randomForest)

#----------------------------------------------------------------------------------------------------------------------------------------------------

#Set working directory:
setwd("C:/Users/punee/Desktop/New/House Prices")

#----------------------------------------------------------------------------------------------------------------------------------------------------

#Load CSVs:
train.dat <- read_csv("train.csv",col_names = TRUE,na = c("", "NA"),quoted_na = TRUE)
test.dat <- read_csv("test.csv",col_names = TRUE,na = c("", "NA"),quoted_na = TRUE)
#----------------------------------------------------------------------------------------------------------------------------------------------------

#CLEANING PROCESS:
#Merging data before training and testing:
test.dat$SalePrice <- NA
comb.data <- rbind(train.dat, test.dat)

#Check structure of combined data
str(comb.data)

#Checking Empty/NA column entries
na_count <-sapply(comb.data, function(comb.data) sum(length(which(is.na(comb.data)))))
na_count <- data.frame(na_count)
na_count

#We'll use this na_count to handle NAs:
#Checking each column:

comb.data$MSSubClass <- as.factor(comb.data$MSSubClass)
comb.data$MSSubClass<-revalue(comb.data$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))

which(is.na(comb.data$MSZoning))
median(comb.data$MSZoning,na.rm = T)
comb.data$MSZoning[c(2251, 1916, 2217, 2905)] = 'RL'
comb.data$MSZoning <- as.factor(comb.data$MSZoning)

comb.data$LotFrontage <- as.character( comb.data$LotFrontage )
comb.data$HasLotFrontage <- ifelse(is.na(comb.data$LotFrontage), 0, 1) #Created new variable HasLotFrontage
comb.data$HasLotFrontage <- as.factor(comb.data$HasLotFrontage)
comb.data$LotFrontage <- NULL


comb.data$LotArea <- as.numeric(comb.data$LotArea)

table(comb.data$Street)
comb.data$Street <- as.character(comb.data$Street)
comb.data$Street <- as.factor(comb.data$Street)

table(comb.data$Alley)
comb.data$Alley <- ifelse(is.na(comb.data$Alley), "No Alley", comb.data$Alley) #Houses with no Alley = No Alley
comb.data$Alley <- as.factor(comb.data$Alley)


table(comb.data$LotShape)
comb.data$LotType[comb.data$LotShape == "Reg"] <- "Regular"
comb.data$LotType[comb.data$LotShape != "Reg"] <- "Irregular"
comb.data$LotType <- as.factor(comb.data$LotType)
comb.data$LotShape <- NULL

comb.data$LandContour <- as.factor(comb.data$LandContour)

which(is.na(comb.data$Utilities))
table(comb.data$Utilities)
median(comb.data$Utilities,na.rm = T)
comb.data$Utilities[c(1916,1946)] = 'AllPub'
comb.data$Utilities <- factor(comb.data$Utilities)

comb.data$LotConfig <- as.factor(comb.data$LotConfig)
comb.data$LandSlope <- as.factor(comb.data$LandSlope)
comb.data$Neighborhood <- as.factor(comb.data$Neighborhood)
comb.data$Condition1 <- as.factor(comb.data$Condition1)
comb.data$Condition2 <- as.factor(comb.data$Condition2)
comb.data$BldgType <- as.factor(comb.data$BldgType)
comb.data$HouseStyle <- as.factor(comb.data$HouseStyle)
comb.data$OverallQual <- as.factor(comb.data$OverallQual)
comb.data$OverallCond <- as.factor(comb.data$OverallCond)

str(comb.data$YearBuilt)
str(comb.data$YearRemodAdd)

comb.data$RoofStyle <- as.factor(comb.data$RoofStyle)
comb.data$RoofMatl <- as.factor(comb.data$RoofMatl)

table(comb.data$Exterior1st)
comb.data$Exterior1st[is.na(comb.data$Exterior1st)] = 'VinylSd'
comb.data$Exterior1st <- factor(comb.data$Exterior1st)
table(comb.data$Exterior2nd)
comb.data$Exterior2nd[is.na(comb.data$Exterior2nd)] = 'VinylSd'
comb.data$Exterior2nd <- factor(comb.data$Exterior2nd)

comb.data$MasVnrType <- ifelse(is.na(comb.data$MasVnrType), "None", comb.data$MasVnrType) #Houses with NA MasVnrType = None
comb.data$MasVnrType <- as.factor(comb.data$MasVnrType)

comb.data$MasVnrArea <- as.numeric(comb.data$MasVnrArea)
comb.data$MasVnrArea <- ifelse(is.na(comb.data$MasVnrArea), 0, comb.data$MasVnrArea) #Houses with NA MasVnrArea = 0

comb.data$ExterQual <- as.factor(comb.data$ExterQual)
comb.data$ExterCond <- as.factor(comb.data$ExterCond)
comb.data$Foundation <- as.factor(comb.data$Foundation)

comb.data$HasBsmt <- ifelse(is.na(comb.data$BsmtQual), 0, 1) #Created new variable HasBsmt
comb.data$HasBsmt <- as.factor(comb.data$HasBsmt)

comb.data$BsmtQual <- ifelse(is.na(comb.data$BsmtQual), "No Bsmt", comb.data$BsmtQual) #Houses with no Basement = No Bsmt
bsmt.ql <- c("No Bsmt", "Po", "Fa", "TA", "Gd", "Ex"  ) 
bsmt.ht   <- c("0",  "50", "75", "85", "95", "120" )                                          
comb.data$BsmtHt <- as.numeric( mapvalues(comb.data$BsmtQual, from=bsmt.ql, to=bsmt.ht) ) #Created new variable BsmtHt
comb.data$BsmtHt <- as.factor(comb.data$BsmtHt)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("BsmtQual"))] #Remove BsmtQual column

comb.data$BsmtCond <- ifelse(is.na(comb.data$BsmtCond), "No Bsmt", comb.data$BsmtCond) #Houses with no Basement = No Bsmt
comb.data$BsmtCond <- as.factor(comb.data$BsmtCond)

comb.data$BsmtExposure <- ifelse(is.na(comb.data$BsmtExposure), "No Bsmt", comb.data$BsmtExposure) #Houses with no Basement = No Bsmt
comb.data$BsmtExposure <- as.factor(comb.data$BsmtExposure)

comb.data$BsmtFinType1 <- ifelse(is.na(comb.data$BsmtFinType1), "No Bsmt", comb.data$BsmtFinType1) #Houses with no Basement = No Bsmt
comb.data$BsmtFinType1 <- as.factor(comb.data$BsmtFinType1)

comb.data <- comb.data[ , -which(names(comb.data) %in% c("BsmtFinSF1"))] #Remove BsmtFinSF1 column, only 0 and NA values

na_count <-sapply(comb.data, function(comb.data) sum(length(which(is.na(comb.data)))))
na_count <- data.frame(na_count)
na_count



comb.data$BsmtFinType2 <- ifelse(is.na(comb.data$BsmtFinType2), "No Bsmt", comb.data$BsmtFinType2) #Houses with no Basement = No Bsmt
comb.data$BsmtFinTyp2[comb.data$BsmtFinType2 == "GLQ"] <- 6
comb.data$BsmtFinTyp2[comb.data$BsmtFinType2 == "ALQ"] <- 5
comb.data$BsmtFinTyp2[comb.data$BsmtFinType2 == "BLQ"] <- 4
comb.data$BsmtFinTyp2[comb.data$BsmtFinType2 == "Rec"] <- 3
comb.data$BsmtFinTyp2[comb.data$BsmtFinType2 == "LwQ"] <- 2
comb.data$BsmtFinTyp2[comb.data$BsmtFinType2 == "Unf"] <- 1
comb.data$BsmtFinTyp2[comb.data$BsmtFinType2 == "No Bsmt"] <- 0
comb.data$BsmtFinTyp2 <- as.factor(comb.data$BsmtFinTyp2)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("BsmtFinType2"))]

comb.data <- comb.data[ , -which(names(comb.data) %in% c("BsmtFinSF2"))] #Remove BsmtFinSF2 column, only 0 and NA values

comb.data$BsmtUnfSF <- ifelse(is.na(comb.data$BsmtUnfSF), 0,comb.data$BsmtUnfSF) #Take the 1 NA as 0
comb.data$TotalBsmtSF <- ifelse(is.na(comb.data$TotalBsmtSF), 0,comb.data$TotalBsmtSF) #Take the 1 NA as 0
comb.data$Heating <- as.factor(comb.data$Heating)

comb.data$HeatQC[comb.data$HeatingQC == "Ex"] <- 5
comb.data$HeatQC[comb.data$HeatingQC == "Gd"] <- 4
comb.data$HeatQC[comb.data$HeatingQC == "TA"] <- 3
comb.data$HeatQC[comb.data$HeatingQC == "Fa"] <- 2
comb.data$HeatQC[comb.data$HeatingQC == "Po"] <- 1
comb.data$HeatQC <- as.factor(comb.data$HeatQC)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("HeatingQC"))]

comb.data$CAir[comb.data$CentralAir == "Y"] <- 1
comb.data$CAir[comb.data$CentralAir == "N"] <- 0
comb.data$CAir <- as.factor(comb.data$CAir)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("CentralAir"))]

comb.data$Electrical[is.na(comb.data$Electrical)] = 'SBrkr'
comb.data$Electrical <- as.factor(comb.data$Electrical)

colnames(comb.data)[which(names(comb.data) == "1stFlrSF")] <- "FirstFlrSF"
comb.data$FirstFlrSF<- as.numeric(comb.data$FirstFlrSF)
colnames(comb.data)[which(names(comb.data) == "2ndFlrSF")] <- "SecondFlrSF"
comb.data$SecondFlrSF<- as.numeric(comb.data$SecondFlrSF)

comb.data <- comb.data[ , -which(names(comb.data) %in% c("LowQualFinSF"))] #Remove LowQualFinSF column, very few unique values, remaining all 0

comb.data$GrLivArea<- as.numeric(comb.data$GrLivArea)

comb.data$BsmtFullBath[is.na(comb.data$BsmtFullBath)] <- 0
comb.data$BsmtFullBath <- as.numeric(comb.data$BsmtFullBath)
comb.data$BsmtFullBath <- as.factor(comb.data$BsmtFullBath)
comb.data$BsmtHalfBath[is.na(comb.data$BsmtHalfBath)] <- 0
comb.data$BsmtHalfBath <- as.numeric(comb.data$BsmtHalfBath)
comb.data$BsmtHalfBath <- as.factor(comb.data$BsmtHalfBath)

comb.data$FullBath <- as.numeric(comb.data$FullBath)
comb.data$HalfBath <- as.numeric(comb.data$HalfBath)

comb.data$BedroomAbvGr <- as.factor(comb.data$BedroomAbvGr)
comb.data$KitchenAbvGr <- as.factor(comb.data$KitchenAbvGr)

comb.data$KitchQual[comb.data$KitchenQual == "Ex"] <- 5
comb.data$KitchQual[comb.data$KitchenQual == "Gd"] <- 4
comb.data$KitchQual[comb.data$KitchenQual == "TA"] <- 3
comb.data$KitchQual[comb.data$KitchenQual == "Fa"] <- 2
comb.data$KitchQual[comb.data$KitchenQual == "Po" | is.na(comb.data$KitchenQual)] <- 1
comb.data <- comb.data[ , -which(names(comb.data) %in% c("KitchenQual"))]

comb.data$TotRmsAbvGrd <- as.numeric(comb.data$TotRmsAbvGrd)

comb.data$Functional[is.na(comb.data$Functional)] = 'Typ'
comb.data$Functional <- as.factor(comb.data$Functional)

comb.data$Fireplaces <- as.numeric(comb.data$Fireplaces)

comb.data$FireplaceQu <- ifelse(is.na(comb.data$FireplaceQu), "No Fireplace", comb.data$FireplaceQu) #Houses with no Fireplace = No Fireplace
comb.data$FireplaceQual[comb.data$FireplaceQu == "Ex"] <- 5
comb.data$FireplaceQual[comb.data$FireplaceQu == "Gd"] <- 4
comb.data$FireplaceQual[comb.data$FireplaceQu == "TA"] <- 3
comb.data$FireplaceQual[comb.data$FireplaceQu == "Fa"] <- 2
comb.data$FireplaceQual[comb.data$FireplaceQu == "Po"] <- 1
comb.data$FireplaceQual[comb.data$FireplaceQu == "No Fireplace"] <- 0
comb.data$FireplaceQual <- as.factor(comb.data$FireplaceQual)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("FireplaceQu"))]

comb.data$GarageType <- ifelse(is.na(comb.data$GarageType), "No Garage", comb.data$GarageType) #Houses with no Garage = No Garage
comb.data$GarageType <- as.factor(comb.data$GarageType)

comb.data$HasGarage <- ifelse(is.na(comb.data$GarageYrBlt), 0, 1) #New variable showing whether Has garage or not
comb.data$HasGarage <- as.factor(comb.data$HasGarage)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("GarageYrBlt"))] #Remove GarageYrBlt column, no use

comb.data$GarageFinish <- ifelse(is.na(comb.data$GarageFinish), "No Garage", comb.data$GarageFinish) #Houses with no Garage = No Garage
comb.data$GarageFin[comb.data$GarageFinish == "Fin"] <- 3
comb.data$GarageFin[comb.data$GarageFinish == "RFn"] <- 2
comb.data$GarageFin[comb.data$GarageFinish == "Unf"] <- 1
comb.data$GarageFin[comb.data$GarageFinish == "No Garage"] <- 0
comb.data$GarageFin <- as.factor(comb.data$GarageFin)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("GarageFinish"))]

comb.data$GarageCars <- ifelse(is.na(comb.data$GarageCars), 0, comb.data$GarageCars) 
comb.data$GarageCars <- as.numeric(comb.data$GarageCars)

comb.data$GarageArea <- ifelse(is.na(comb.data$GarageArea), 0, comb.data$GarageArea) 
comb.data$GarageArea <- as.numeric(comb.data$GarageArea)

comb.data$GarageQual <- ifelse(is.na(comb.data$GarageQual), "No Garage", comb.data$GarageQual) #Houses with no Garage = No Garage
comb.data$GarageQl[comb.data$GarageQual == "Ex"] <- 5
comb.data$GarageQl[comb.data$GarageQual == "Gd"] <- 4
comb.data$GarageQl[comb.data$GarageQual == "TA"] <- 3
comb.data$GarageQl[comb.data$GarageQual == "Fa"] <- 2
comb.data$GarageQl[comb.data$GarageQual == "Po"] <- 1
comb.data$GarageQl[comb.data$GarageQual == "No Garage"] <- 0
comb.data$GarageQl <- as.factor(comb.data$GarageQl)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("GarageQual"))]

comb.data$GarageCond <- ifelse(is.na(comb.data$GarageCond), "No Garage", comb.data$GarageCond) #Houses with no Garage = No Garage
comb.data$GarageCnd[comb.data$GarageCond == "Ex"] <- 5
comb.data$GarageCnd[comb.data$GarageCond == "Gd"] <- 4
comb.data$GarageCnd[comb.data$GarageCond == "TA"] <- 3
comb.data$GarageCnd[comb.data$GarageCond == "Fa"] <- 2
comb.data$GarageCnd[comb.data$GarageCond == "Po"] <- 1
comb.data$GarageCnd[comb.data$GarageCond == "No Garage"] <- 0
comb.data$GarageCnd <- as.factor(comb.data$GarageCnd)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("GarageCond"))]

comb.data$PavedDr[comb.data$PavedDrive == "Y"] <- 2
comb.data$PavedDr[comb.data$PavedDrive == "P"] <- 1
comb.data$PavedDr[comb.data$PavedDrive == "N"] <- 0
comb.data$PavedDr <- as.factor(comb.data$PavedDr)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("PavedDrive"))]

comb.data$WoodDeckSF <- as.numeric(comb.data$WoodDeckSF)
comb.data$OpenPorchSF <- as.numeric(comb.data$OpenPorchSF)
comb.data$EnclosedPorch <- as.numeric(comb.data$EnclosedPorch)
colnames(comb.data)[which(names(comb.data) == "3SsnPorch")] <- "ThreeSeasSF"
comb.data$ThreeSeasSF <- as.numeric(comb.data$ThreeSeasSF)
comb.data$ScreenPorch <- as.numeric(comb.data$ScreenPorch)
comb.data$PoolArea <- as.numeric(comb.data$PoolArea)

comb.data$PoolQC <- ifelse(is.na(comb.data$PoolQC), "No Pool", comb.data$PoolQC) #Houses with NA PoolQC = No Pool
comb.data$PoolQual[comb.data$PoolQC == "Ex"] <- 4
comb.data$PoolQual[comb.data$PoolQC == "Gd"] <- 3
comb.data$PoolQual[comb.data$PoolQC == "TA"] <- 2
comb.data$PoolQual[comb.data$PoolQC == "Fa"] <- 1
comb.data$PoolQual[comb.data$PoolQC == "No Pool"] <- 0
comb.data$PoolQual <- as.factor(comb.data$PoolQual)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("PoolQC"))]

comb.data$Fence <- ifelse(is.na(comb.data$Fence), "No Fence", comb.data$Fence) #Houses with NA Fence = No Fence
comb.data$Fence <- as.factor(comb.data$Fence)

comb.data <- comb.data[ , -which(names(comb.data) %in% c("MiscFeature","MiscVal"))] #Remove MiscFeature,MiscVal column, no use

comb.data$SaleType[is.na(comb.data$SaleType)] = 'WD'
comb.data$SaleType <- as.factor(comb.data$SaleType)
comb.data$SaleCondition <- as.factor(comb.data$SaleCondition)
comb.data$SalePrice <- as.numeric(comb.data$SalePrice)


#Again check NA entries
na_count <-sapply(comb.data, function(comb.data) sum(length(which(is.na(comb.data)))))
na_count <- data.frame(na_count)
na_count

#Check Structure of data
str(comb.data)
write.csv(comb.data, "comb.data.csv")

#----------------------------------------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------

#Split back to train and test:

train.fin <-comb.data[1:1460,]
test.fin <-comb.data[(1461):nrow(comb.data),]
test.fin <- test.fin[ , -which(names(test.fin) %in% c("SalePrice"))]

#----------------------------------------------------------------------------------------------------------------------------------------------------

df <- train.fin

#Most houses go for around $120000 - $200000
ggplot(df, aes(x=SalePrice)) +
  geom_histogram(fill="steelblue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = scales::dollar)


#Total Sale Price increases as Overall Quality Increases.
ggplot(df, aes(x=OverallQual,y=SalePrice)) +
  geom_boxplot() +
  xlab("Overall Quality") + 
  ylab("Total Sale Price") +
  scale_y_continuous(labels = scales::dollar)

#Newly built houses are sold more than old houses.
ggplot(df, aes(x=YearBuilt, y=SalePrice)) + 
  geom_point() +
  geom_smooth() +
  ggtitle("Sale Price wrt Year Built") + 
  xlab("Year Built") + 
  ylab("Sale Price") +
  scale_y_continuous(labels = scales::dollar)


#MSZoning: Identifies the general zoning classification of the sale:
#We can see that Residential Areas with a low density have made most in sales. Makes sense!
ggplot(df, aes(x=MSZoning, y=SalePrice)) + 
  geom_bar(stat='identity',fill="steelblue") +
  ggtitle("CHECKING PRICE OF HOUSES WITH RESPECT TO ZONES:") + 
  xlab("MSZoning") + 
  ylab("Sale Price")+ 
  scale_y_continuous(labels = scales::dollar)


#LandSlope: Slope of property
#Houses with a gentle slope have made most in sales. Ofcourse, no one wants to buy property with a moderate or steep slope.
table(df$LandSlope)
ggplot(df, aes(x=LandSlope, y=SalePrice)) + 
  geom_bar(stat='identity',fill="steelblue") +
  ggtitle("CHECKING PRICE OF HOUSES WITH RESPECT TO SLOPE:") + 
  xlab("Slope") + 
  ylab("Sale Price")

#Sale Condition:
#Obviously, Properties with normal condition have made more in sales.
ggplot(df, aes(x=SaleCondition, y=SalePrice)) + 
  geom_bar(stat='identity',fill="steelblue") +
  scale_y_continuous(labels = scales::dollar)

#Neighborhood: Physical locations within Ames city limits
#And Greater Living Area:

ggplot(df, aes(x=Neighborhood, y=SalePrice)) + 
  geom_bar(stat='summary',fill="steelblue",fun.y="median") +
  ggtitle("CHECKING PRICE OF HOUSES WITH RESPECT TO NEIGHBORHOOD:") + 
  xlab("Neighborhood") + 
  ylab("Median Sale Price") + 
  theme(axis.text.x = element_text(angle=45)) +
  scale_y_continuous(labels = scales::dollar)

ggplot(df, aes(x=Neighborhood)) + 
  geom_histogram(stat='count',fill="steelblue") +
  ggtitle("Count of Houses sold wrt Neighborhood") + 
  theme(axis.text.x = element_text(angle=45))

ggplot(df, aes(x=SalePrice, y=GrLivArea, color=Neighborhood)) +
  geom_point(shape=16, alpha=.8, size=2) +
  scale_x_continuous(label=scales::dollar)
#We can see that one property with a very high living area (Edwards) has a low sale price. We can conclude the reason is a poor neighborhood.

#----------------------------------------------------------------------------------------------------------------------------------------------------

#Linear Model
mod1 <- lm(log(SalePrice) ~. , data=train.fin[,!colnames(train.fin) %in% c("MSSubClass")])
summary(mod1)
# Predict on test
pred<- predict(mod1, test.fin, type="response")
pred <- exp(pred)
mod_op <- cbind(test.fin$Id, pred)
colnames(mod_op) <- c("Id", "SalePrice")
write.csv(mod_op,"LMSubmission.csv", row.names=FALSE)

#----------------------------------------------------------------------------------------------------------------------------------------------------

#RANDOM FOREST 
model_rf <- randomForest(log(SalePrice) ~ ., data=train.fin,na.action = na.exclude)
summary(model_rf)
#Finding Important Variables
varImpPlot(model_rf)
model_rf

pred1 <- predict(model_rf, test.fin)
pred1 <- exp(pred1)
mod_op1 <- cbind(test.fin$Id, pred1)
colnames(mod_op1) <- c("Id", "SalePrice")
write.csv(mod_op1,"RFSubmission.csv", row.names=FALSE)


#----------------------------------------------------------------------------------------------------------------------------------------------------

#SVM:

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(12345)
mod_svm <- train(log(SalePrice) ~., data = train.fin, method = "svmLinear",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
summary(mod_svm)
mod_svm

pred_svm <- predict(mod_svm, test.fin)
pred_svm <- exp(pred_svm)
op_svm <- cbind(test.fin$Id, pred_svm)
colnames(op_svm) <- c("Id", "SalePrice")
write.csv(op_svm,"SVMSubmission.csv", row.names=FALSE)


#----------------------------------------------------------------------------------------------------------------------------------------------------

#KNN:

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(9899)
mod_knn <- train(log(SalePrice) ~., data = train.fin, method = "knn",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
summary(mod_knn)
mod_knn

pred_knn <- predict(mod_knn, test.fin)
pred_knn <- exp(pred_knn)
op_knn <- cbind(test.fin$Id, pred_knn)
colnames(op_knn) <- c("Id", "SalePrice")
write.csv(op_knn,"KNNSubmission.csv", row.names=FALSE)



#----------------------------------------------------------------------------------------------------------------------------------------------------

#RF2:

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(765)
mod_rf <- train(log(SalePrice) ~., data = train.fin, method = "rf",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
summary(mod_rf)
mod_rf

pred_rf <- predict(mod_rf, test.fin)
pred_rf <- exp(pred_rf)
op_rf <- cbind(test.fin$Id, pred_rf)
colnames(op_rf) <- c("Id", "SalePrice")
write.csv(op_rf,"RF2Submission.csv", row.names=FALSE)


#----------------------------------------------------------------------------------------------------------------------------------------------------

#XGBLinear Gradient Boosting: (Takes hell of a time)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(348)
mod_xg <- train(log(SalePrice) ~., data = train.fin, method = "xgbLinear",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
summary(mod_xg)
mod_xg

pred_xg <- predict(mod_xg, test.fin)
pred_xg <- exp(pred_xg)
op_xg <- cbind(test.fin$Id, pred_xg)
colnames(op_xg) <- c("Id", "SalePrice")
write.csv(op_xg,"XGBLinearSubmission.csv", row.names=FALSE)

#----------------------------------------------------------------------------------------------------------------------------------------------------


#GBM Gradient Boosting:

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1189)
mod_gbm <- train(log(SalePrice) ~., data = train.fin, method = "gbm",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
summary(mod_gbm)
mod_gbm

pred_gbm <- predict(mod_gbm, test.fin)
pred_gbm <- exp(pred_gbm)
op_gbm <- cbind(test.fin$Id, pred_gbm)
colnames(op_gbm) <- c("Id", "SalePrice")
write.csv(op_gbm,"GBMSubmission.csv", row.names=FALSE)

#----------------------------------------------------------------------------------------------------------------------------------------------------

#CatBoost:

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3199)
mod_cb <- train(log(SalePrice) ~., data = train.fin, method = catboost.caret,trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
summary(mod_cb)
mod_cb

pred_cb <- predict(mod_cb, test.fin)
pred_cb <- exp(pred_cb)
op_cb <- cbind(test.fin$Id, pred_cb)
colnames(op_cb) <- c("Id", "SalePrice")
write.csv(op_cb,"CatBoostSubmission.csv", row.names=FALSE)

#----------------------------------------------------------------------------------------------------------------------------------------------------

library(e1071) #Best!
svm_model<-svm(SalePrice~.,data=train.fin,cost = 3)
svm_pred <- predict(svm_model,newdata = test.fin)
solution <- data.frame(Id=test.fin$Id,SalePrice=svm_pred)
write.csv(solution,"svm_solution.csv",row.names = F)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#GBM New:
CARET.TRAIN.CTRL <- trainControl(method = "repeatedcv", number = 5, repeats = 5, verboseIter = FALSE, allowParallel = TRUE)
gbmFit <- train(SalePrice ~ ., method = "gbm",
                trControl = CARET.TRAIN.CTRL, 
                data = train.fin, verbose = FALSE)

preds1 <- predict(gbmFit, newdata = test.fin)
op_gbm1 <- cbind(test.fin$Id, preds1)
colnames(op_gbm1) <- c("Id", "SalePrice")
write.csv(op_gbm1,"GBMSubmissionNew.csv", row.names=FALSE)

