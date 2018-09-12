#remove all the objects stored
rm(list = ls())
#set current working directory
setwd("E:/")

#Load Libraries
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
library(xgboost)    # used for building XGBoost model
library(cowplot)    # used for combining multiple plots
library(readxl)     # used to read excel file
library(VIM)        # used to impute missing value
library(corrgram)   # used to plot corgram plot
library(glmnet)     # used for Ridge Regression
library(usdm)       # used for multicollinearity
library(MASS)
library(DAAG)
library(ridge)
library(rpart)
library(ranger)


#Reading the Data
train = read_excel("Absenteeism_at_work_Project.xls")

#Understand the Data
#Dimension of the Data
dim(train)

#Features of Data
names(train)

#Structure of the data
str(train)

#converting the variable to its appropriate data type
train$Seasons=as.factor(train$Seasons)
train$`Disciplinary failure`=as.factor(train$`Disciplinary failure`)
train$Education=as.factor(train$Education)
train$Son=as.factor(train$Son)
train$`Social drinker`=as.factor(train$`Social drinker`)
train$`Social smoker`=as.factor(train$`Social smoker`)
train$Pet=as.factor(train$Pet)

#Exploratory Data Analysis
#Univariate Analysis
#For targe variable
ggplot(data = train, aes(x = `Absenteeism time in hours`)) + geom_histogram(color = "black", 
                                                                            fill = "green", binwidth = 10)

#Now we will check Numerical independent variable

p1 = ggplot(data = train, aes(x = ID)) + geom_histogram(color = "black", 
                                                                                 fill = "red", binwidth = 10)
p2 = ggplot(data = train, aes(x = `Reason for absence`)) + geom_histogram(color = "black", 
                                                                                 fill = "red", binwidth = 10)
p3 = ggplot(data = train, aes(x = `Month of absence`)) + geom_histogram(color = "black", 
                                                                        fill = "red", binwidth = 1)
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package

p4 = ggplot(data = train, aes(x = `Day of the week`)) + geom_histogram(color = "black", 
                                                                              fill = "red", binwidth = 1)
p5 = ggplot(data = train, aes(x = `Transportation expense`)) + geom_histogram(color = "black", 
                                                                                 fill = "red", binwidth = 100)
p6 = ggplot(data = train, aes(x = `Distance from Residence to Work`)) + geom_histogram(color = "black", 
                                                                                 fill = "red", binwidth = 10)
plot_grid(p4, p5, p6, nrow = 1)

p7 = ggplot(data = train, aes(x = `Service time`)) + geom_histogram(color = "black", 
                                                                                 fill = "red", binwidth = 10)
p8 = ggplot(data = train, aes(x = `Age`)) + geom_histogram(color = "black", 
                                                                                 fill = "red", binwidth = 10)
p9 = ggplot(data = train, aes(x = `Work load Average/day`)) + geom_histogram(color = "black", 
                                                                                 fill = "red", binwidth = 50000)
plot_grid(p7, p8, p9, nrow = 1)

p10 = ggplot(data = train, aes(x = `Hit target`)) + geom_histogram(color = "black", 
                                                                                 fill = "red", binwidth = 5)
p11 = ggplot(data = train, aes(x = Weight)) + geom_histogram(color = "black", 
                                                                                 fill = "red", binwidth = 10)

p12 = ggplot(data = train, aes(x = Height)) + geom_histogram(color = "black", 
                                                                                  fill = "red", binwidth = 5)
plot_grid(p10, p11, p12, nrow = 1)

ggplot(data = train, aes(x = `Body mass index`)) + geom_histogram(color = "black", 
                                                                                  fill = "red", binwidth = 3)

#BarPlot for categorical variable

# Barplot for Seasons
p13 = ggplot(train, aes(x = Seasons)) + geom_bar() +
  labs(title = "Count of Seasons", x = "Different Seasons", y = "Count of different seasons") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.25)

# Barplot for Disciplinary failure
p14 = ggplot(train, aes(x = `Disciplinary failure`)) + geom_bar() +
  labs(title = "Disciplinary Failure", x = "Different Disciplinary Failure", y = "Count of Disciplinary Failure") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.25)

# Barplot for Education
p15 = ggplot(train, aes(x = Education)) + geom_bar() +
  labs(title = "Education", x = "Different Education", y = "Count of each Education") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.25)

# Barplot for Son
p16 = ggplot(train, aes(x = Son)) + geom_bar() +
  labs(title = "Sons", x = "Number of sons", y = "Count of different sons") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.25)
plot_grid(p13, p14, p15,p16, nrow = 2)

#Barplot for Social Drinker
p17 = ggplot(train, aes(x = `Social drinker`)) + geom_bar() +
  labs(title = "Social Drinker", x = "Different Social Drinker", y = "Count of each Social Drinker") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.25)

#Barplot for Social Smoker
p18 = ggplot(train, aes(x = `Social smoker`)) + geom_bar() +
  labs(title = "Social Smoker", x = "Different Social smoker", y = "Count of each Social smoker") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.25)

#Barplot for pets
p19 = ggplot(train, aes(x = Pet)) + geom_bar() +
  labs(title = "Pets", x = "Number of Pets", y = "Count of each Pets") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.25)
plot_grid(p17, p18, p19, nrow = 2)

#Bivariate Analysis
#Relation Between continous and categorical variable
#Relation between ID and Absenteeism time in hours
theme_set(theme_bw())
b1 = ggplot(train, aes(ID, `Absenteeism time in hours`)) + geom_point() +
  geom_smooth(method = "lm", se = F)

b2 = ggplot(train, aes(`Reason for absence`, `Absenteeism time in hours`)) + geom_point() +
  geom_smooth(method = "lm", se = F)

b3 = ggplot(train, aes(`Transportation expense`, `Absenteeism time in hours`)) + geom_point() +
  geom_smooth(method = "lm", se = F)

b4 = ggplot(train, aes(`Distance from Residence to Work`, `Absenteeism time in hours`)) + geom_point() +
  geom_smooth(method = "lm", se = F)
plot_grid(b1, b2, b3,b4, nrow = 2)

b5 = ggplot(train, aes(`Service time`, `Absenteeism time in hours`)) + geom_point() +
  geom_smooth(method = "lm", se = F)

b6 = ggplot(train, aes(Age, `Absenteeism time in hours`)) + geom_point() +
  geom_smooth(method = "lm", se = F)

b7 = ggplot(train, aes(`Work load Average/day`, `Absenteeism time in hours`)) + geom_point() +
  geom_smooth(method = "lm", se = F)

b8 = ggplot(train, aes(`Hit target`, `Absenteeism time in hours`)) + geom_point() +
  geom_smooth(method = "lm", se = F)
plot_grid(b5, b6, b7, b8, nrow = 2)

b9 = ggplot(train, aes(Weight, `Absenteeism time in hours`)) + geom_point() +
  geom_smooth(method = "lm", se = F)

b10 = ggplot(train, aes(Height, `Absenteeism time in hours`)) + geom_point() +
  geom_smooth(method = "lm", se = F)

b11 = ggplot(train, aes(`Body mass index`, `Absenteeism time in hours`)) + geom_point() +
  geom_smooth(method = "lm", se = F)
plot_grid(b9, b10, b11, nrow = 2)

b12 = ggplot(train, aes(`Month of absence`, `Absenteeism time in hours`)) + geom_point() +
  geom_smooth(method = "lm", se = F) 

b13 = ggplot(train, aes(`Day of the week`, `Absenteeism time in hours`)) + geom_point() +
  geom_smooth(method = "lm", se = F)
plot_grid(b12, b13, nrow = 1)

#Relation between categorical variable and continous variable
#Boxplot between Month of absence and Absenteeism time in hours
theme_set(theme_classic())

b14 = ggplot(train, aes(`Seasons`, `Absenteeism time in hours`)) + geom_boxplot(varwidth = T, fill = "plum")
b15 = ggplot(train, aes(`Disciplinary failure`, `Absenteeism time in hours`)) + geom_boxplot(varwidth = T, fill = "plum")
b16 = ggplot(train, aes(Education, `Absenteeism time in hours`)) + geom_boxplot(varwidth = T, fill = "plum")
b17 = ggplot(train, aes(Son, `Absenteeism time in hours`)) + geom_boxplot(varwidth = T, fill = "plum")
plot_grid(b14, b15, b16, b17, nrow = 2)

b18 = ggplot(train, aes(`Social drinker`, `Absenteeism time in hours`)) + geom_boxplot(varwidth = T, fill = "plum")
b19 = ggplot(train, aes(`Social smoker`, `Absenteeism time in hours`)) + geom_boxplot(varwidth = T, fill = "plum")
b20 = ggplot(train, aes(Pet, `Absenteeism time in hours`)) + geom_boxplot(varwidth = T, fill = "plum")
plot_grid(b18, b19, b20, nrow = 2)


#MISSING VALUE ANALYSIS
missing_val = data.frame(apply(train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(train)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
missing_val

#Visualization of Mssing Value
ggplot(data = missing_val[,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
ggtitle("Missing data percentage (Train)") + theme_bw()

#IMPUTING MISSING VALUE
#For Weight and Height
train$Weight[is.na(train$Weight)] = mean(train$Weight, na.rm = T)
train$Height[is.na(train$Height)] = mean(train$Height, na.rm = T)

#For Body Mass Index
for(i in 1:nrow(train)){
  if(is.na(train$`Body mass index`[i])){
    train$`Body mass index`[i] = (train$Weight[i]*10000)/train$Height[i]^2
  }
}

#KNN imputation to impute Missing Value
train = kNN(train, k=5)

#Since 0 is not the number for month so replace the data points with 0 value with NA and then imputing it
for(i in 1:nrow(train)){
  if(train$`Month of absence`[i] == 0){
    train$`Month of absence`[i] = NA
  }
}

#Since 0 is not the reason for absence so here we first replace it with NA and then imputing it
for(i in 1:nrow(train)){
  if(train$`Reason for absence`[i] == 0){
    train$`Reason for absence`[i] = NA
  }
}


#Again using KNN imputation for imputing the Missing value
train = kNN(train, variable = c("Month of absence","Reason for absence"), k = 3)

#Dropping the Useless variable
train = subset(train,
               select = -c(ID_imp,`Reason for absence_imp`,`Month of absence_imp`,`Day of the week_imp`,Seasons_imp,
                         `Transportation expense_imp`,`Distance from Residence to Work_imp`,`Service time_imp`,
                         Age_imp,`Work load Average/day_imp`,`Hit target_imp`,`Disciplinary failure_imp`,Education_imp,
                         Son_imp,`Social drinker_imp`,`Social smoker_imp`,Pet_imp,Weight_imp,Height_imp,
                         `Body mass index_imp`,`Absenteeism time in hours_imp`))

#FEATURE SELECTION
## Correlation Plot 
numeric_index = sapply(train, is.numeric)

corrgram(train[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#DIMNESIONAL REDUCTION
train = subset(train,
               select = -c(ID, `Service time`, Weight, `Work load Average/day`, `Hit target`,`Month of absence`, Pet))

#Feature Scaling
cnames = c("Reason for absence","Distance from Residence to Work","Age","Transportation expense", "Height","Body mass index")

for(i in cnames){
  print(i)
  train[,i] = (train[,i] - min(train[,i]))/
    (max(train[,i] - min(train[,i])))
}

#MODEL DEVELOPMENT
#Divide data into train and test using stratified sampling method
set.seed(1234)
train.index = createDataPartition(train$`Absenteeism time in hours`, p = .80, list = FALSE)
train = train[ train.index,]
test  = train[-train.index,]

#Check multicollinearity
vifcor(train[, -c(2,3,7,8,9,10,11)], th = 0.9)

#Linear Regression Model
model1 = lm(`Absenteeism time in hours` ~ ., data = train)

summary(model1)

#Predictting Test Data
predictions_LR = predict(model1, test[,1:13])

#Calculate RMSE
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

RMSE(test[,14], predictions_LR)

#K-folda cross validation
model2 = cv.lm(data = train, model1, m=5) # 5 fold cross-validation

#Lasso Regression
X = data.matrix(train)
Y = data.matrix(test)

set.seed(1235)

my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002))

lasso_linear_reg_mod = train(x = X[, 1:13], y = X[,14],
                             method='glmnet', trControl= my_control, tuneGrid = Grid)

#Prediction
prediction_LL = predict(lasso_linear_reg_mod, Y[, -14])

#Calculate RMSE
RMSE(Y[,14], prediction_LL)

#Ridge Regression
set.seed(1236)
my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002))

ridge_linear_reg_mod = train(x = X[, 1:13], y = X[, 14],
                             method='glmnet', trControl= my_control, tuneGrid = Grid)

#Prediction
prediction_RR = predict(ridge_linear_reg_mod, Y[, -14])

#Calculate RMSE
RMSE(Y[, 14], prediction_RR)

#Random Forest
set.seed(1237)
my_control = trainControl(method="cv", number=5) # 5-fold CV
tgrid = expand.grid(
  .mtry = c(1:13),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
)
rf_mod = train(x = train[, 1:13], 
               y = train$`Absenteeism time in hours`,
               method='ranger', 
               trControl= my_control, 
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")

plot(rf_mod)
plot(varImp(rf_mod))

#Predictions
prediction_RF = predict(rf_mod, test[, -14])

#Calculate RMSE
RMSE(test[, 14], prediction_RF)


#XGBoost
param_list = list(
  
  objective = "reg:linear",
  eta=0.01,
  gamma = 1,
  max_depth=6,
  subsample=0.8,
  colsample_bytree=0.5
)


dtrain = xgb.DMatrix(data = as.matrix(X[,-14]), label= X[,14])
dtest = xgb.DMatrix(data = as.matrix(Y[,-14]), label = Y[,14])

set.seed(112)
xgbcv = xgb.cv(params = param_list, 
               data = dtrain, 
               nrounds = 1000, 
               nfold = 5, 
               print_every_n = 10, 
               early_stopping_rounds = 30, 
               maximize = F)
xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 205)

prediction_XG = predict(xgb_model, dtest)

#Calculate RMSE
RMSE(test[, 14], prediction_XG)

var_imp = xgb.importance(feature_names = setdiff(names(train), c("Absenteeism time in hours")), 
                         model = xgb_model)
xgb.plot.importance(var_imp)

