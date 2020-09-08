library(MASS)
library(ISLR)


head(trainData)
str(trainData)



df_submittinTest = read.csv("test.csv", na.strings = c("", " ", "NA"))
str(df_submittinTest)
df_nrow_submittinTest = nrow(df_submittinTest)

for (theRow in 1:df_nrow_submittinTest) {
  if(is.na(df_submittinTest[theRow, "Item_Weight"]) == TRUE){
    if(df_submittinTest[theRow - 1, "Item_ID"] == df_submittinTest[theRow, "Item_ID"]){
      df_submittinTest[theRow, "Item_Weight"] = df_submittinTest[theRow - 1, "Item_Weight"]
    } else if(df_submittinTest[theRow + 1, "Item_ID"] == df_submittinTest[theRow, "Item_ID"]){
      df_submittinTest[theRow, "Item_Weight"] = df_submittinTest[theRow + 1, "Item_Weight"]
    }
  }
}



df_submittinTest$Store_Size = as.character(df_submittinTest$Store_Size)
df_submittinTest$Store_Size = droplevels(df_submittinTest$Store_Size, 
                                         exclude = if(anyNA(levels(df_submittinTest$Store_Size))) NULL)
str(df_submittinTest$Store_Size)
levels(df_submittinTest$Store_Size) <- c("High", "Medium", "Small")
df_submittinTest$Store_Size[df_submittinTest$Store_Size == "NA"] <- NULL

df_submittinTest$Store_Size = droplevels(df_submittinTest$Store_Size)


# effectOf_ItemStoreID = lm(Item_Store_Returns ~ Item_Store_ID, data = trainData)
# effectOf_ItemStoreID

effectOf_ItemWeight= lm(Item_Store_Returns ~ poly(Item_Weight, degree = 2), data = trainData)
summary(effectOf_ItemWeight)

effectOf_ItemSugarContent = lm(Item_Store_Returns ~ (1 / Item_Sugar_Content), data = trainData)
summary(effectOf_ItemSugarContent)

effectOf_Item_Visibility = lm(Item_Store_Returns ~ (Item_Visibility ^ 2), data = trainData)
summary(effectOf_Item_Visibility)

effectOf_ItemPrice = lm(Item_Store_Returns ~ poly(log(sqrt(Item_Price)), degree = 2), data = trainData)
summary(effectOf_ItemPrice)
plot(effectOf_ItemPrice)

#------ Joint Effect models ------------------------------------------------------------

jointEffect_ItemVisibility_Price = lm(Item_Store_Returns ~ Item_Visibility * Item_Price, 
                                      data = trainData)
summary(jointEffect_ItemVisibility_Price)


joint_ItemVisibilityPrice_StoreSizeLocation = 
  lm(Item_Store_Returns ~ Item_Visibility * Item_Price * Store_Size * Store_Location_Type,
     data = trainData)
summary(joint_ItemVisibilityPrice_StoreSizeLocation)


joint_ItemVisibilityPrice_StoreSizeLocation_StoreType = 
  lm(Item_Store_Returns ~ Item_Visibility * Item_Price * Store_Size * Store_Location_Type * Store_Type,
     data = trainData)
summary(joint_ItemVisibilityPrice_StoreSizeLocation_StoreType)


#---------------------------------------------------------------------------------------

is.factor(trainData$Store_Location_Type)
trainData$Store_Size = as.factor(trainData$Store_Size)

jointEffect_StoreSize_LocationType = lm(Item_Store_Returns ~ Store_Size * Store_Location_Type, 
                                        data = trainData)
summary(jointEffect_StoreSize_LocationType)

jointEffect_StoreSize_LocationType_StoreType =
  lm(Item_Store_Returns ~ Store_Size * Store_Location_Type * Store_Type, data = trainData)
summary(jointEffect_StoreSize_LocationType_StoreType)

is.factor(trainData$Item_Type)

effectOf_ItemType = lm(Item_Store_Returns ~ Item_Type, data = trainData)
summary(effectOf_ItemType)


#----------- Developed model and prediction -------------------------------

joint_ItemVisibilityPriceType_StoreSizeLocation_StoreType = 
  lm(Item_Store_Returns ~ Item_Visibility * Item_Price * Store_Size * Store_Location_Type *
       Store_Type * Item_Type, data = trainData)
summary(joint_ItemVisibilityPriceType_StoreSizeLocation_StoreType)

submission = predict(joint_ItemVisibilityPriceType_StoreSizeLocation_StoreType, df_submittinTest, 
                     type = "response")
submission[is.na(submission)] <- mean(na.omit(submission))
# View(submission)

Item_Store_ID = as.character(df_submittinTest$Item_Store_ID)

myFirstSubmission = as.data.frame(cbind(Item_Store_ID, submission))


write.csv(myFirstSubmission, file = "myFirstSubmission.csv")
nrow(df_submittinTest)
View(myFirstSubmission)
