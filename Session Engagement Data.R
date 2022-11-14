Session_Engagement <- read_excel("Session Engagement Data.xlsx")
View(Session_Engagement)

Session_Engagement <- na.omit(Session_Engagement)
Session_Engagement <- Session_Engagement %>% mutate(Score=avg_session_rating*`session_completion%`/50)

library(caTools)

Split <- sample.split(Session_Engagement,SplitRatio = 0.7)

training <- subset(Session_Engagement,Split==TRUE)
View(training)
test <- subset(Session_Engagement,Split==FALSE)
View(test)

training <- training %>% select(-1,-2,-11,-12,-13,-15,-16,-17,-18,-19,-20,-21,-22,-25,-27,-28,-29,-32,-33,-34,-35
                                ,-37,-38,40,-41,-42,-43,-45,-47,-48,-49,-50,-51,-52)
View(training)
Correlation <- round(cor(training),2)
cor(Var1,Var2,method = "pearson")
View(Correlation)
install.packages("corrplot")
library(corrplot)

Corr_Matrix <- corrplot(Correlation,method = "number",type = "full")
View(Corr_Matrix)
library(DataExplorer)
create_report(training)

install.packages("car")
library(car)
model <- lm(Score~.-`%loved the class participation`-`%teaching was awesome`,data = training)
vif(model)
step(model,direction = "backward")
summary(model)

#Check multicollinearity from corr matrix and find variables highly correlated.
#Then find Vif if vif>5,remove one of those variable,otherwise not.

result <- predict(model,test)
final <- cbind(ActualValue=test$Score,PredictedValue=result)
final <- as.data.frame(final)
final <- cbind(final,error=final$ActualValue-final$PredictedValue)
View(final)
final %>% mutate(Mean=mean(error))

#Assumptions 
#1. linear relationship btw Score(y) and independent variables(x). Run scatterplot to eliminate variables not showing 
#linear relationship.Include positive or negative relationship
#2.Mean value of residuals or errors is 0
#3.Independent residuals or Errors
#4.residuals are normally distributed for given X value
#5.Constant variance of residuals for different x values

plot(model) #diagnostic plots to verify assumptions

#Residual plot1 - X axis - Predicted or Fitted Y values, Y-Axis- Residuals or errors
#Here we see no pattern i.e., curve or red line is flat. Hence linear assumption is met.
#Plus we see constant variance of residuals and since no pattern so residuals are independent.
#Residual plot2 - Y axis-ordered, observed and standardized residuals.X-axis - Theoretical residuals.
#This is what we expect the residuals to be if the errors are normally distributed.
#Points should fall on a diagonal line if residuals are normally distributed.

