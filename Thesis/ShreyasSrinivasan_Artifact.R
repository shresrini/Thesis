#read the file
data=read.csv("Train123.csv")
head(data)
summary(data$Team)
str(data$Team)

#converting categorica;l variables

data$Team=factor(data$Team, levels = c("ARS","BHA","BOU","BUR","CAR","CHE","CRY","EVE","FUL",
                                       "HUD","LEI","LIV","MCI","MUN","NEW","SOU","TOT","WAT","WHU","WOL"), 
                 labels=c(1:20))

summary(data$Position)

data$Position=factor(data$Position, levels=c("DEF","FWD","GKP","MID"),labels = c(1:4))
# Correlation matrix
data1=data[4:21]
install.packages('corrplot')
library(corrplot)
a=cor(data1)
corrplot(a,method = 'circle')
install.packages('ggplot2')
library(ggplot2)

#Distribution of target variable
ggplot(data = data, aes(Points)) +
  geom_histogram(bins = 10) +
  labs(title = 'Distribution of Points')

#Distribution of cost variable


ggplot(data = data, aes(Cost)) +
  geom_freqpoly(bins = 20) +
  labs(title = 'Distribution of Cost')

#team Cost and postion threat analysis visualization
plot(data$Team,data$Cost)
plot(data$Position,data$Threat)

#elimination of variable
data=data[2:21]
library(caTools)
#Splitting the data
set.seed(123)
split = sample.split(data$Position, SplitRatio = 0.8)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)



#####################################################################################################################
###Linear Regression

lin_reg=lm(Points~., data = training_set)
summary(lin_reg)

#prediction
pred_lin_reg= predict(lin_reg,test_set)


residuals=resid(lin_reg)

summary(lin_reg)

#Root mean square error and R square calculation
rss_linreg <- sum((pred_lin_reg - test_set[,20]) ^ 2)  
tss <- sum((test_set[,20] - mean(test_set[,20])) ^ 2) 
rsq_linreg <- 1 - rss_linreg/tss

rss_linreg
rsq_linreg

# Visualizing the model output
plot(test_set$Points, type="l",lty=100, col="green")
lines(pred_lin_reg, type="l", col="red")

#################################################################################################################

#Artificial Neural Network

install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1)
model = h2o.deeplearning(y = 'Points',
                         training_frame = as.h2o(training_set),
                         activation = 'Rectifier',
                         hidden = c(5,5),
                         epochs = 100,
                         train_samples_per_iteration = -1)


  
summary(model)
#implimenting the model in the test set 
y_pred = h2o.predict(model, newdata = as.h2o(test_set))
pred<- as.vector(y_pred)
#View(test_set[,26])
#View(pred)
comparision<-data.frame(pred, test_set[,20])
summary(model)
#View(comparision)


##Calculating the rmse and rsquare for ANN


rss_ann <- sum((pred - test_set[,20]) ^ 2)  ## residual sum of squares
tss <- sum((test_set[,20] - mean(test_set[,20])) ^ 2)  ## total sum of squares
rsq_ann <- 1 - rss_ann/tss


rsq_ann


summary(model)

#Visualizing the output


plot(test_set$Points, type="l",lty=100, col="green")
lines(pred, type="l", col="blue")

###################################################################################################################

# Fitting SVM to the Training set using regression kernel
library(e1071)


regressor = svm(formula =  Points~ .,
                data = training_set,
                type = 'eps-regression',
                kernel = 'radial')

summary(regressor)
y_pred_svm = predict(regressor, newdata = test_set[-20])

##Calculating the rmse and rsquare for SVM




rss_svm <- sum((y_pred_svm - test_set[,20]) ^ 2)  
tss <- sum((test_set[,20] - mean(test_set[,20])) ^ 2) 
rsq_svm <- 1 - rss_svm/tss


rsq_svm

#Visualizing the output

plot(test_set$Points, type="l",lty=100, col="green")
lines(y_pred_svm, type="l", col="black")
