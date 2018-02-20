#Loading required packages:
if (!require("readr")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("readR"))
}
if (!require("tidyr")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("tidyr"))
}
if (!require("dplyr")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("dplyr"))
}
if (!require("plyr")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("plyr"))
}
if (!require("ggplot2")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("ggplot2"))
}
if (!require("caret")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("caret"))
}
if (!require("randomForest")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("randomForest"))
}

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

comb.data$MSSubClass <- as.numeric(comb.data$MSSubClass) #Because 0 NAs
comb.data$MSZoning[c(2217, 2905)] = 'RL'
comb.data$MSZoning[c(1916, 2251)] = 'RM'
comb.data$MSZoning <- as.factor(comb.data$MSZoning)

comb.data$LotFrontage <- as.character( comb.data$LotFrontage )
comb.data$HasLotFrontage <- ifelse(is.na(comb.data$LotFrontage), 0, 1) #Created new variable HasLotFrontage
comb.data$HasLotFrontage <- as.factor(comb.data$HasLotFrontage)
comb.data$LotFrontage <- ifelse(is.na(comb.data$LotFrontage), "0", comb.data$LotFrontage) #Houses with no Lot Frontage = 0
comb.data$LotFrontage <- as.numeric(comb.data$LotFrontage)

comb.data$LotArea <- as.numeric(comb.data$LotArea)

table(comb.data$Street)
comb.data$StreetType[comb.data$Street == "Pave"] <- 1
comb.data$StreetType[comb.data$Street != "Pave"] <- 0
comb.data$StreetType <- as.factor(comb.data$StreetType)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("Street"))]

comb.data$HasAlley <- ifelse(is.na(comb.data$Alley), 0, 1) #Created new variable HasAlley
comb.data$HasAlley <- as.factor(comb.data$HasAlley)
comb.data$Alley <- ifelse(is.na(comb.data$Alley), "No Alley", comb.data$Alley) #Houses with no Alley = No Alley
comb.data$Alley <- as.factor(comb.data$Alley)

table(comb.data$LotShape)
comb.data$LotType[comb.data$LotShape == "Reg"] <- 1
comb.data$LotType[comb.data$LotShape != "Reg"] <- 0
comb.data$LotType <- as.factor(comb.data$LotType)
comb.data$LotShape <- as.factor(comb.data$LotShape)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("LotShape"))]

comb.data$LandContour <- as.factor(comb.data$LandContour)

comb.data <- comb.data[ , -which(names(comb.data) %in% c("Utilities"))]

comb.data$LotConfig <- as.factor(comb.data$LotConfig) #Because 0 NAs
comb.data$LandSlope <- as.factor(comb.data$LandSlope) #Because 0 NAs
comb.data$Neighborhood <- as.factor(comb.data$Neighborhood) #Because 0 NAs
comb.data$Condition1 <- as.factor(comb.data$Condition1) #Because 0 NAs
comb.data$Condition2 <- as.factor(comb.data$Condition2) #Because 0 NAs
comb.data$BldgType <- as.factor(comb.data$BldgType) #Because 0 NAs
comb.data$HouseStyle <- as.factor(comb.data$HouseStyle) #Because 0 NAs
comb.data$OverallQual <- as.factor(comb.data$OverallQual) #Because 0 NAs
comb.data$OverallCond <- as.factor(comb.data$OverallCond) #Because 0 NAs
comb.data$RoofStyle <- as.factor(comb.data$RoofStyle) #Because 0 NAs
comb.data$RoofMatl <- as.factor(comb.data$RoofMatl) #Because 0 NAs
comb.data$Exterior1st[is.na(comb.data$Exterior1st)] = 'VinylSd'
comb.data$Exterior1st <- factor(comb.data$Exterior1st)
comb.data$Exterior2nd[is.na(comb.data$Exterior2nd)] = 'VinylSd'
comb.data$Exterior2nd <- factor(comb.data$Exterior2nd)

comb.data$MasVnrType <- ifelse(is.na(comb.data$MasVnrType), "None", comb.data$MasVnrType) #Houses with NA MasVnrType = None
comb.data$MasVnrType <- as.factor(comb.data$MasVnrType)

comb.data$MasVnrArea <- as.numeric(comb.data$MasVnrArea)
comb.data$MasVnrArea <- ifelse(is.na(comb.data$MasVnrArea), 0, comb.data$MasVnrArea) #Houses with NA MasVnrArea = 0

comb.data$ExterQuality[comb.data$ExterQual == "Ex"] <- 5
comb.data$ExterQuality[comb.data$ExterQual == "Gd"] <- 4
comb.data$ExterQuality[comb.data$ExterQual == "TA"] <- 3
comb.data$ExterQuality[comb.data$ExterQual == "Fa"] <- 2
comb.data$ExterQuality[comb.data$ExterQual == "Po"] <- 1
comb.data$ExterQuality <- as.factor(comb.data$ExterQuality)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("ExterQual"))]

comb.data$ExterCondn[comb.data$ExterCond == "Ex"] <- 5
comb.data$ExterCondn[comb.data$ExterCond == "Gd"] <- 4
comb.data$ExterCondn[comb.data$ExterCond == "TA"] <- 3
comb.data$ExterCondn[comb.data$ExterCond == "Fa"] <- 2
comb.data$ExterCondn[comb.data$ExterCond == "Po"] <- 1
comb.data$ExterCondn <- as.factor(comb.data$ExterCondn)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("ExterCond"))]

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
comb.data$BsmtCondn[comb.data$BsmtCond == "Ex"] <- 5
comb.data$BsmtCondn[comb.data$BsmtCond == "Gd"] <- 4
comb.data$BsmtCondn[comb.data$BsmtCond == "TA"] <- 3
comb.data$BsmtCondn[comb.data$BsmtCond == "Fa"] <- 2
comb.data$BsmtCondn[comb.data$BsmtCond == "Po"] <- 1
comb.data$BsmtCondn[comb.data$BsmtCond == "No Bsmt"] <- 0
comb.data$BsmtCondn <- as.factor(comb.data$BsmtCondn)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("BsmtCond"))]

comb.data$BsmtExposure <- ifelse(is.na(comb.data$BsmtExposure), "No Bsmt", comb.data$BsmtExposure) #Houses with no Basement = No Bsmt
comb.data$BsmtExpos[comb.data$BsmtExposure == "Gd"] <- 4
comb.data$BsmtExpos[comb.data$BsmtExposure == "Av"] <- 3
comb.data$BsmtExpos[comb.data$BsmtExposure == "Mn"] <- 2
comb.data$BsmtExpos[comb.data$BsmtExposure == "No"] <- 1
comb.data$BsmtExpos[comb.data$BsmtExposure == "No Bsmt"] <- 0
comb.data$BsmtExpos <- as.factor(comb.data$BsmtExpos)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("BsmtExposure"))]


comb.data$BsmtFinType1 <- ifelse(is.na(comb.data$BsmtFinType1), "No Bsmt", comb.data$BsmtFinType1) #Houses with no Basement = No Bsmt
comb.data$BsmtFinTyp1[comb.data$BsmtFinType1 == "GLQ"] <- 6
comb.data$BsmtFinTyp1[comb.data$BsmtFinType1 == "ALQ"] <- 5
comb.data$BsmtFinTyp1[comb.data$BsmtFinType1 == "BLQ"] <- 4
comb.data$BsmtFinTyp1[comb.data$BsmtFinType1 == "Rec"] <- 3
comb.data$BsmtFinTyp1[comb.data$BsmtFinType1 == "LwQ"] <- 2
comb.data$BsmtFinTyp1[comb.data$BsmtFinType1 == "Unf"] <- 1
comb.data$BsmtFinTyp1[comb.data$BsmtFinType1 == "No Bsmt"] <- 0
comb.data$BsmtFinTyp1 <- as.factor(comb.data$BsmtFinTyp1)
comb.data <- comb.data[ , -which(names(comb.data) %in% c("BsmtFinType1"))]

comb.data <- comb.data[ , -which(names(comb.data) %in% c("BsmtFinSF1"))] #Remove BsmtFinSF1 column, only 0 and NA values

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

#Split back to train and test:

train.fin <-comb.data[1:1460,]
test.fin <-comb.data[(1461):nrow(comb.data),]
test.fin <- test.fin[ , -which(names(test.fin) %in% c("SalePrice"))]

#----------------------------------------------------------------------------------------------------------------------------------------------------

#Linear Model
mod1 <- lm(SalePrice ~. , data=train.fin)
summary(mod1)
# Predict on test
pred<- predict(mod1, test.fin, type="response")
mod_op <- cbind(test.fin$Id, pred)
colnames(mod_op) <- c("Id", "SalePrice")
write.csv(mod_op,"LMSubmission.csv", row.names=FALSE)

#----------------------------------------------------------------------------------------------------------------------------------------------------

#RANDOM FOREST
model_rf <- randomForest(SalePrice ~ ., data=train.fin,na.action = na.exclude)
summary(model_rf)
#Finding Important Variables
varImpPlot(model_rf)
model_rf

pred1 <- predict(model_rf, test.fin)
mod_op1 <- cbind(test.fin$Id, pred1)
colnames(mod_op1) <- c("Id", "SalePrice")
write.csv(mod_op1,"RFSubmission.csv", row.names=FALSE)

