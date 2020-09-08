#---- Loading data and cleaning where necessary ----------------------------
library(mboost)
library(tidyverse)
library(readr)
library(stringr)
library(caret)


trainData = read.csv("train.csv", na.strings = c("", " ", "NA"))
str(trainData)

trainData$Store_Start_Year = as.factor(trainData$Store_Start_Year)
trainData$Item_Store_ID = as.character(trainData$Item_Store_ID)
str(trainData)
View(trainData)


#---- Dealing with the missing values ----------------------------------------

df_nrow_trainData = nrow(trainData)

           # Missing values in "Item_Weight" variable

for (theRow in 1:df_nrow_trainData) {
  if(is.na(trainData[theRow, "Item_Weight"]) == TRUE){
    if(trainData[theRow - 1, "Item_ID"] == trainData[theRow, "Item_ID"]){
      trainData[theRow, "Item_Weight"] = trainData[theRow - 1, "Item_Weight"]
    } else if(trainData[theRow + 1, "Item_ID"] == trainData[theRow, "Item_ID"]){
      trainData[theRow, "Item_Weight"] = trainData[theRow + 1, "Item_Weight"]
    }
  }
}

          # Missing values in "Store_Size" variable

summary(trainData[, "Store_Size"])

for (theRow in 1:df_nrow_trainData) {
  if(is.na(trainData[theRow, "Store_Size"]) == TRUE){
    if(theRow - 1 > 0 && trainData[theRow - 1, "Store_ID"] == trainData[theRow, "Store_ID"]){
      if(trainData[theRow - 1, "Store_ID"] == "High"){
        trainData[theRow, "Store_ID"] = "High"
      }else if(trainData[theRow - 1, "Store_ID"] == "Medium"){
        trainData[theRow, "Store_ID"] = "Medium"
      } else if(trainData[theRow - 1, "Store_ID"] == "Small") {
        trainData[theRow, "Store_ID"] = "Small"
      }
    } else if(theRow + 1 < df_nrow_trainData + 1 && trainData[theRow + 1, "Store_ID"] == trainData[theRow, "Store_ID"]){
      if(trainData[theRow + 1, "Store_ID"] == "High"){
        trainData[theRow, "Store_ID"] = "High"
      }else if(trainData[theRow + 1, "Store_ID"] == "Medium"){
        trainData[theRow, "Store_ID"] = "Medium"
      } else if(trainData[theRow + 1, "Store_ID"] == "Small") {
        trainData[theRow, "Store_ID"] = "Small"
      }
    }
  }
}

trainData$Store_Size = as.character(trainData$Store_Size)
for (theRow in 1:df_nrow_trainData) {
  if(is.na(trainData[theRow, "Store_Size"]) == TRUE){
    if(theRow - 1 > 0 && trainData[theRow - 1, "Store_ID"] == trainData[theRow, "Store_ID"]){
      trainData[theRow, "Store_Size"] = trainData[theRow - 1, "Store_Size"]
    } else if(theRow + 1 < df_nrow_trainData + 1 && trainData[theRow + 1, "Store_ID"] == trainData[theRow, "Store_ID"]){
      trainData[theRow, "Store_Size"] = trainData[theRow + 1, "Store_Size"]
    }
  }
}

View(trainData)



#---- Spliting data into train and test ---------------------------------------------

smp_size = floor(0.75 * nrow(trainData))
set.seed(1234)

train_ind = sample(seq_len(nrow(trainData)), size = smp_size)
df_train = trainData[train_ind, ]
df_test = trainData[-train_ind, ]

dim(df_train)
dim(df_test)

#------- Model and avaluation --------------------------------------

df_model_joint_ItemVisibilityPrice_StoreSizeLocation_StoreType = 
  lm(Item_Store_Returns ~ poly(Item_Visibility, degree = 3) * Item_Price * Store_Size * Store_Location_Type * Store_Type,
     data = df_train)
summary(df_model_joint_ItemVisibilityPrice_StoreSizeLocation_StoreType)

df_test_prediction = df_test[, -13]

predicted_examp = predict(df_model_joint_ItemVisibilityPrice_StoreSizeLocation_StoreType, 
        df_test_prediction, type = "response")
summary(predicted_examp)
View(predicted_examp)
predicted_examp[is.na(predicted_examp)] <- mean(na.omit(predicted_examp))


#eval_value = RMSE(pred = predicted_examp, obs = df_test[, 13], model = df_model_joint_ItemVisibilityPrice_StoreSizeLocation_StoreType)


mean(abs(predicted_examp - df_test[, 13]))

plot(df_model_joint_ItemVisibilityPrice_StoreSizeLocation_StoreType)



cor.test(trainData$Item_Visibility, trainData$Item_Weight, method = "pearson")

