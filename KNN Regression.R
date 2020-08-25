# Initialize variables to load data from provided excel
autotest <- read.csv(file.choose(),header = T)
autotrain <- read.csv(file.choose(),header = T)

#Initialize variables to load data from specific columns
train.x = autotrain[[6]]
train.y = autotrain[[2]]
test.x =  autotest[[6]]
test.y =  autotest[[2]]
kvalue = 30

#Function to perform KNN regression
kNN <- function(k,x.train,y.train,x.pred) {
  ## Initialize:
  n.pred <- length(x.pred);		y.pred <- numeric(n.pred)
  
  ## Main Loop
  for (i in 1:n.pred){
    d <- abs(x.train - x.pred[i])
    dstar = d[order(d)[k]]
    y.pred[i] <- mean(y.train[d <= dstar])		
  }
  ## Return the vector of predictions
  invisible(y.pred)
}

# load prediction by calling the KNN function and assign value into the variable
fit_train <- kNN(kvalue,x.train = train.x, y.train = train.y,x.pred =train.x)
fit_test <- kNN(kvalue,x.train = train.x, y.train = train.y,x.pred =test.x)

#calculate training and testing MSE
mse_train <- mean((train.y-fit_train)^2)
mse_test <- mean((test.y-fit_test)^2)
print (mse_train)
print (mse_test)

#plot data

plot(test.x,fit_test,main = "Plot for KNN model at K=30 with all data",
     xlab = "Weights", ylab = "MPG",  ylim=c(0,45),col = "red")
points(train.x,fit_train,col = "red")
points(train.x,train.y, col="blue")
points(test.x,test.y,col = "darkgoldenrod1")

#Insert legends on top right corner
legend("topright",
       legend = c("Pred X vs Prediction", "Test X vs Test Y", "Train X vs Train Y"),
       col = c("red","darkgoldenrod1","blue"),
       lwd = 4,
       text.col = "black",
       horiz = FALSE)

