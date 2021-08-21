

# Problem statement................................................................................#

# The data set is of a firm who wants to understand why some of their best and most experienced employees are leaving the company. 
# The company also wishes to predict which valuable employees will leave next
# About the data set - This has 15,000 rows and 10 columns 
#..................................................................................................#

# import the data set 

df=read.csv("F:\\Analytics\\Learnings\\Python\\Pratice data\\HR_comma_sep.csv")

# check on structure of data set

str(df)

class(df)  # the datastructure is a dataframe

# check the first 10 observations

head(df,10)

# check the dimensions of dataset

dim(df) 

nrow(df) # 15,000 rows

ncol(df) # 10 columns

# check statiscal summary of all the columns

summary(df)

# check the null values in the data set

sum(is.na(df)) # No null values in the data set.


# check outliers in the dataset 

OutVals = boxplot(df$average_montly_hours)$out

which(df$average_montly_hours %in% OutVals)

OutVals = boxplot(df$average_montly_hours, plot=FALSE)


# outlier check using Hampel filter

lower_bound = median(df$average_montly_hours) - 3 * mad(df$average_montly_hours, constant = 1)
lower_bound

upper_bound = median(df$average_montly_hours) + 3 * mad(df$average_montly_hours, constant = 1)
upper_bound

outlier_ind = which(df$average_montly_hours < lower_bound |df$average_montly_hours > upper_bound)
outlier_ind

# inference -According to the Hampel filter, there are 325 outliers for the average_montly_hour variable.


# Feature scaling - converting cateogorical values to numeric

df$Department=as.numeric(as.factor(df$Department))

df$salary=as.numeric(as.factor(df$salary))

head(df,5)


#pie chart to show percentage analysis of employee leaving the company

a=table(df$left)

pie(a,col=rainbow(3),labels=lbs,main="Retention analysis of employees")

legend("topright",c("left the company","still n company"),fill=rainbow(3),cex=0.8)


# Correlation analysis

dumm_y=df[,c(1,2,3,4,5,6,8,9,10)]

head(dumm_y,5)

m=cor(df$left,dumm_y)   # How to sort these by desc or asce

sort(m,decreasing = TRUE)

# inference - based on coorelation analysis-satisfaction_level,time_spend_company,Work_accident are considered

# Correlation analysis using heat map

library(ggcorrplot)

ggcorrplot(cor(df$left,dumm_y))

# Graphs using ggplot2

library(ggplot2) 

q=table(df$left)

ggplot(df,aes(x = m, y = left)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with mpg") +
  xlab("Variable")

#...........................................Feature Engineering..................

# defining  x and y variables

df = df[,c(1,5,6,7)] # satisfaction_level,time_spend_company,Work_accident

head(df,3)


# scaling the variables

df[-4]=scale(df[-4])

head(df,3)

#Train test  split

library(caTools)

set.seed(123) #Selecting random dataset

split=sample.split(df$left,SplitRatio=1/4)

train_set = subset(df,split == FALSE)

test_set = subset(df,split==TRUE)

#.........................................Logistic Model development............

#Fitting dataset into logistic model

library(class)

log_model = glm(left~satisfaction_level+time_spend_company+Work_accident,
                data = train_set,
                family= "binomial")

summary(log_model)

install.packages('tableone')

library(tableone)

#covariates we are using

xvars =c("satisfaction_level","time_spend_company","Work_accident")

table1 = CreateTableOne(vars = xvars,strata = "left",data = train_set, test = FALSE)

print(table1, smd = TRUE)

# predicting the values

y_pred =predict(log_model,test_set,type="response")  # obtain the class (0/1)

yp=ifelse(y_pred>0.5,1,0)


# set the type="response" when using the predict function on a logistic regression model. 
# Else, it will predict the log odds of P, that is the Z value, instead of the probability itself.

# confusion matrx for test accuracy

library(caret)

cm=table(test_set[,4],yp)

print(cm)

accuracy=(2631+223)/(2631+226+670+223)

print(accuracy)

# the accuracy using logistic model is 76.1

..........................# Decision tree with party pacakage .....................

# import the data set from folder

df=read.csv("F:\\Analytics\\Learnings\\Python\\Pratice data\\HR_comma_sep.csv")

# Feature scaling - converting cateogorical values to numeric

df$Department=as.numeric(as.factor(df$Department))

df$salary=as.numeric(as.factor(df$salary))

head(df,5)


# defining  x and y variables

df = df[,c(1,5,6,7)] # satisfaction_level,time_spend_company,Work_accident

head(df,3)


# scaling the variables

df[-4]=scale(df[-4])

head(df,3)

#Train test  split

library(caTools)

set.seed(123) #Selecting random dataset

split=sample.split(df$left,SplitRatio=1/4)

train_set = subset(df,split == FALSE)

test_set = subset(df,split==TRUE)

# Defining the model for decision tree

library(party)

mytree = ctree(left~satisfaction_level+time_spend_company+Work_accident, 
               data = train_set,
               controls=ctree_control(mincriterion=0.9, minsplit=50))

plot(mytree,type="simple")

y_pred =predict(mytree,test_set,type="response")


library(caret)

cm=table(test_set[,4],y_pred)

# Checking accuracy of the model

confusionMatrix(y_pred,test_set$left)

print(cm)

accuracy=(2631+223)/(2631+226+670+223)

print(accuracy)

# the accuracy using decision tree model is 76.1

# Inference and future scope ...........................................................#
# The model helps to find probability of attrition using a logistic regression or decision tree model.
# Based on coorelation analysis-satisfaction_level,time_spend_company,Work_accident are the key factor for workforce churn 
# The accuracy of model using decision tree and logistic model is 76.1
# The results thus obtained will be used by the management to understand what changes they should make to their workplace, in order to get most of their employees to stay.
