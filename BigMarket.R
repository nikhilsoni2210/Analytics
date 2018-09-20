#install.packages("data.table")
library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining
#install.packages(("ggplot2"))
library(ggplot2)    # used for ploting 
#install.packages('caret')
library(caret)      # used for modeling
#install.packages("corrplot")
library(corrplot)   # used for making correlation plot
#install.packages("xgboost")
library(xgboost)    # used for building XGBoost model
#install.packages("cowplot")
library(cowplot)    # used for combining multiple plots 


train = fread("C:\\Users\\user\\Desktop\\Projects\\Big Mart sales prediction\\Train_UWu5bXk.csv") 
test = fread("C:\\Users\\user\\Desktop\\Projects\\Big Mart sales prediction\\Test_u94Q5KV.csv")
submission = fread("C:\\Users\\user\\Desktop\\Projects\\Big Mart sales prediction\\SampleSubmission_TmnO39y.csv")

dim(train)
dim(test)


# Variable names
names(train)
names(test)

str(train)
str(test)


#==========================Combine Train and Test==============================#

test[,Item_Outlet_Sales := NA]
combi = rbind(train,test)
dim(combi)



#==============================================================================#
#==========================Univariate Analysis=================================#
#==============================================================================#

#==========================Numerical Variables=================================#
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales),binwidth = 100,fill = "darkgreen") +
 xlab("Item_Outlet_Sales")

p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")  
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package

#There seems to be no clear-cut pattern in Item_Weight.
#Item_Visibility is right-skewed and should be transformed to curb its skewness.
#We can clearly see 4 different distributions for Item_MRP. It is an interesting insight.

#=========================/////////////////////================================#




#===========================Categorical variable=================================#


ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

combi$Item_Fat_Content[combi$Item_Fat_Content== "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +
  geom_bar(aes(Item_Fat_Content,Count), stat = "identity", fill= "coral1")


# plot for Item_Type
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +
  xlab("") +
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Item_Type")


# plot for Outlet_Identifier
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# plot for Outlet_Size
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

second_row = plot_grid(p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)

#In Outlet_Size's plot, for 4016 observations, Outlet_Size is blank or missing.
#We will check for this in the bivariate analysis to substitute the missing 
#values in the Outlet_Size.


# plot for Outlet_Establishment_Year
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + 
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +
  xlab("Outlet_Establishment_Year") +
  theme(axis.text.x = element_text(size = 8.5))

# plot for Outlet_Type
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))


# ploting both plots together
plot_grid(p7, p8, ncol = 2)


#Lesser number of observations in the data for the outlets ...
#...established in the year 1998 as compared to the other years.
#Supermarket Type 1 seems to be the most popular category of Outlet_Type.

#==============================////////////////////=======================================



#==============================================================================#
#===========================Bivariate Analysis=================================#
#==============================================================================#


# extracting train data from the combined data
train =  combi[1:nrow(train)]


#================================Numerical Variable=============================#

# Item_Weight vs Item_Outlet_Sales
p9 = ggplot(train) + 
  geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))


# Item_Visibility vs Item_Outlet_Sales
p10 = ggplot(train) + 
  geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))



# Item_MRP vs Item_Outlet_Sales
p11 = ggplot(train) + 
  geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))


second_row_2 = plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow = 2)


#Item_Outlet_Sales is spread well across the entire range of the Item_Weight 
#without any obvious pattern.


#In Item_Visibility vs Item_Outlet_Sales, there is a string of points at
#Item_Visibility = 0.0 which seems strange as item visibility cannot be completely zero. We will take note of this issue and deal with it in the later stages.


#In the third plot of Item_MRP vs Item_Outlet_Sales, we can clearly see 
#4 segments of prices that can be used in feature engineering to create a 
#new variable.
#=================================/////////====================================#




#============Target Variable vs Independent Categorical Variables===============#
#===============================================================================#

# Item_Type vs Item_Outlet_Sales
p12 = ggplot(train) + 
  geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))


# Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(train) + 
  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))


# Outlet_Identifier vs Item_Outlet_Sales
p14 = ggplot(train) + 
  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))


second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)


#Distribution of Item_Outlet_Sales across the categories of Item_Type is not 
#very distinct and same is the case with Item_Fat_Content.

#The distribution for OUT010 and OUT019 categories of Outlet_Identifier are 
#quite similar and very much different from the rest of the categories of 
#Outlet_Identifier.

#In the univariate analysis, we came to know about the empty values in 
#Outlet_Size variable. Let's check the distribution of the target variable 
#across Outlet_Size.

ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

#The distribution of 'Small' Outlet_Size is almost identical to the distribution
#of the blank category (first vioin) of Outlet_Size. So, we can substitute the 
#blanks in Outlet_Size with 'Small'.

p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta")

p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")

plot_grid(p15, p16, ncol = 1)

#Tier 1 and Tier 3 locations of Outlet_Location_Type look similar.

#In the Outlet_Type plot, Grocery Store has most of its data points around 
#the lower sales values as compared to the other categories.



#=============================//////////////////===============================#









#===============================================================================#
#============================Missing Value Imputation===========================#
#===============================================================================#



# Item_weight 
sum(is.na(combi$Item_Weight))

sample(combi[1:5])

missing_index = which(is.na(combi$Item_Weight))

for (i in missing_index)
{
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier==item],na.rm=T)
}

sum(is.na(combi$Item_Weight))


#Replacing 0's in Item_Visibility variable

sum(is.na(combi$Item_Visibility))
ggplot(combi) + geom_histogram(aes(Item_Visibility),bins = 100)
zero_index = which(combi$Item_Visibility==0)


sum(is.na(combi$Item_Identifier))
for (i in zero_index)
{
  item = combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier==item],na.rm = T)
}

ggplot(combi) + geom_histogram(aes(Item_Visibility),bins = 100)


#==================================////////////////=============================#


#===============================================================================#
#==============================Feature Engineering==============================#
#===============================================================================#



#===================================Item_Type_new===============================#
unique(combi$Item_Type)

#We can have a look at the Item_Type variable and classify the categories 
#into perishable and non_perishable as per our understanding and make it 
#into a new feature.

perishable = c("Breads","Breakfast","Dairy","Fruits and Vegetables","Meat","Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", 
                   "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

# create a new feature 'Item_Type_new'
combi[,Item_Type_new := ifelse(combi$Item_Type %in% perishable,"perishable",
      ifelse(combi$Item_Type %in% non_perishable,"non_perishable","not sure"))]


#=================================Item_Category=================================#


#Let's compare Item_Type with the first 2 characters of Item_Identifier, 
#i.e., 'DR', 'FD', and 'NC'. These identifiers most probably stand for drinks, 
#food, and non-consumable.


table(combi$Item_Type,substr(combi$Item_Identifier,1,2))

combi[,Item_category := substr(combi$Item_Identifier,1,2)]


#We will also change the values of Item_Fat_Content wherever Item_category is 
#'NC' because non-consumable items cannot have any fat content.

unique(combi$Item_Fat_Content)
combi$Item_Fat_Content[combi$Item_Type_new =="NC"] == "Non Edible"

#. We will also create a couple of more features-Outlet_Years(years of operation)

combi[,Outlet_Years := 2013 - combi$Outlet_Establishment_Year]

combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)


#price_per_unit_wt (price per unit weight).

combi[,price_per_unit_wt := Item_MRP/Item_Weight]



#Earlier in the Item_MRP vs Item_Outlet_Sales plot, we saw Item_MRP was 
#spread across in 4 chunks. Now let's assign a label to each of these chunks 
#and use this label as a new variable.

ggplot(train) + 
  geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st", 
                              ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
                                ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]   


                                                                                            

#==================================////////////////=============================#


#===============================================================================#
#===========================Encoding Categorical Variables======================#
#===============================================================================#



#================Label encoding for the categorical variables===================#
#We will label encode Outlet_Size and Outlet_Location_Type as these are 
#ordinal variables.

combi[,Outlet_Size_num := ifelse(combi$Outlet_Size=="Small",0,
                                 ifelse(combi$Outlet_Size=="Medium",1,2))]
combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,
                                  ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]





# removing categorical variables after label encoding
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]


#=============One hot encoding for the categorical variable=====================#


ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[,"Item_Identifier"], ohe_df)

View(combi[1:5])
combi[1:5]

#==================================////////////////=============================#


#===============================================================================#
#===============================PreProcessing Data==============================#
#===============================================================================#


#variables Item_Visibility and price_per_unit_wt are highly skewed. 
#So, we will treat their skewness with the help of log transformation.

#Removing Skewness
combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)] 



#Scaling numeric predictors
#Let's scale and center the numeric variables to make them have a mean of zero, 
#standard deviation of one and scale of 0 to 1. Scaling and centering is 
#required for linear regression models.

# index of numeric features
num_vars = which(sapply(combi, is.numeric)) 
num_vars_names = names(num_vars)

combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)
combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables
combi = cbind(combi, combi_numeric_norm)




#Splitting the combined data combi back to train and test set.

train = combi[1:nrow(train)]
test = combi[(nrow(train) + 1):nrow(combi)]
test[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA for test dataset

#Correlated Variables
cor_train = cor(train[,-c("Item_Identifier")])
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)




#==================================////////////////=============================#


#===============================================================================#
#==================================Linear Regression============================#
#===============================================================================#



train$Item_Identifier

linear_reg_model = lm(Item_Outlet_Sales~.,data = train[,-c("Item_Identifier")])
summary(linear_reg_model)

# preparing dataframe for submission and writing it in a csv file
pred = predict(linear_reg_model, test[,-c("Item_Identifier")])

sqrt(sum((train$Item_Outlet_Sales - pred)^2)/length(test))


#===============================================================================#
#==================================Ridge Regression=============================#
#===============================================================================#


set.seed(1240)
my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002))

ridge_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,
                             method='glmnet', trControl= my_control, tuneGrid = Grid)

pred = predict(ridge_linear_reg_mod, test[,-c("Item_Identifier")])

sqrt(sum((train$Item_Outlet_Sales - pred)^2)/length(test))



#===============================================================================#
#==================================Lasso Regression=============================#
#===============================================================================#
set.seed(1235)
my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002))

lasso_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,
                             method='glmnet', trControl= my_control, tuneGrid = Grid)


pred = predict(lasso_linear_reg_mod, test[,-c("Item_Identifier")])

sqrt(sum((train$Item_Outlet_Sales - pred)^2)/length(test))


#=======================****************************===============================










































        









