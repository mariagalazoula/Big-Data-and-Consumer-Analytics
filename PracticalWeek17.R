# set a mirror
options(repos = c(CRAN = "http://cran.rstudio.com"))
# test and install
if (!is.element("caret", installed.packages())) install.packages("caret", 
                                                                 dep = T)
if (!is.element("plotly", installed.packages())) install.packages("plotly", 
                                                                  dep = T)
if (!is.element("sp", installed.packages())) install.packages("sp", 
                                                              dep = T)
# load the packages
library(caret)
library(plotly)
library(sp)

hp <- read.csv("hpdemo.csv")

head(hp)
#east, north are the coordinates
#fl_area units in square meters
#price of houses

#get rid of a column
hp <- hp[,-1]

#scale the data 
#we want to predict the house prices in London
#so we only scale the other variables 
X <- scale(hp[, c(1, 2, 4)])

head(X)

#we have to put in each observation one at a time, not a dataframe
hp.data.scaler.func <- function(x) {
  stdevs <- apply(hp[, c(1, 2, 4)], 2, sd)
  means <- apply(hp[, c(1, 2, 4)], 2, mean)
  x.i <- (x - means)/stdevs
  return(x.i)
}

#this gives us the same results as the scale function
hp.data.scaler.func(data.frame(east = 533200, 
                         north = 170900, fl_area = 66))

head(hp.data.scaler.func(hp[,-3]))

#to pass multiple observations and still scale the results correctly

#t is for transpose
#apply the function to multiple rows

#t(apply(hp[(33:35), c(1,2,4)], c(1,2), hp.data.scaler.func()))

### Cross-validation grid search

#tuning parameters: 1.the number of nearest neighbours to look at
# 2. where to use a distance weighted mean, or a simple mean
# 3. which distance metric to use (euclidean or city block)


#MAE and RMSE measure the degree to which the predicted responses 
#differ from the actual ones in the test data set

### CHOICE OF TUNING PARAMETERS
# looping through each posible combination of k, p and the weighting method
# AND applying the cross validation procedure for each combination of values

#training data set S and test data set S'
#the function knn (k-nearest neighbours)
#This function produces nearest neighbour regression objects

#copy the values form the the hp dataframe into an array called price
#create the price variable below and join it to X
#we divide it by 1000 as sometimes big values lead to rounding errors
price = hp[, "price"]/1000
X <- data.frame(price = price, X)

#basic model for the dataset and default parameters 
#for the function
knnFit <- train(price ~ ., data = X, method = "knn")

# inspect the outputs
class(knnFit)
knnFit

# Good one!
predict(knnFit, newdata = X[508, ])

# compare with actual value
price[508] #this is a good prediction

# A Bad one!
predict(knnFit, newdata = X[56, ])
price[56] #bad prediction

#the knn has learned to make predictions for the price variable
#predict the price for any set of values 

predict(knnFit, newdata = hp.data.scaler.func(data.frame(east = 523800, 
                                                         north = 409750, fl_area = 55)))
#further model parameters
set.seed(300) #ensures repeatable results
knnFit <- train(price ~ ., data = X, method = "knn", tuneLength = 20)
#tuneLenght -> the number of k nearest neighbours that can be determined by allowing the model 
#to evalueate different values of k
knnFit
plot(knnFit) #the lowest value would be 27 (remember always odd numbers)

#we add the metric here and we get a better result
#metric function-> function returning a measure of how close to y' that f(X') manages to get
set.seed(300)
knnFit <- train(price ~ ., data = X, method = "knn", tuneLength = 20, 
                metric = "MAE")
knnFit #different k = 25, due to the differences between the measures of 
#model fit given by the different metrics 



#Cross validation: outside the model
# we don't have to pass a value to set.seed (arbitrarily)
#we stabilise the results by the trainControl function


ctrl <- trainControl(method = "repeatedcv", repeats = 3, number = 100)
knnFit <- train(price ~ ., data = X, method = "knn", tuneLength = 20, 
                metric = "MAE", trControl = ctrl)
#trControl -> stabilise the results

hp2 <- hp
hp2$price <- hp2$price/1000
knnFit <- train(price ~ ., data = hp2, method = "knn", tuneLength = 20, 
                metric = "MAE", trControl = ctrl, preProcess = c("center", 
                                                                 "scale"))
#normalise the data within the function, this time k =13

knnFit

#cover the area of the data, setting the eastings and northings range
grid = SpatialPoints(expand.grid(x = seq(505000, 555800, by = 100), 
                                 y = seq(158400, 199900, by = 100)))

#creates a new array of the same length as east mesh but containing with 
#every element of this array with the average floor size
fl <- rep(mean(hp[, "fl_area"]), length(grid))

df <- data.frame(coordinates(grid), fl)

head(df)
names(df) <- c("east", "north", "fl_area")
predprice <- as.vector(predict(knnFit, newdata = df))

# create SPDF 1
pred.surf = SpatialPointsDataFrame(df[, 1:2], data.frame(predprice), 
                                   match.ID = F)
# create SPDF 2
g <- as(pred.surf, "SpatialPixelsDataFrame")
# convert to matrix
price <- as.matrix(g)
p <- plot_ly(z = ~price) %>% add_surface()
p


plot.pred.3D <- function(floor.area){
  # make the grid
  grid = SpatialPoints(expand.grid(x = seq(505000, 555800, 
                                           by = 100), y = seq(158400, 199900, by = 100)))
  # generate a surface of the floor area
  fl <- rep(floor.area, length(grid))
  # create df
  df <- data.frame(coordinates(grid), fl)
  names(df) <- c("east", "north", "fl_area")
  # do prediction
  predprice <- as.vector(predict(knnFit, newdata = df))
  # create SPDF 1
  pred.surf = SpatialPointsDataFrame(df[, 1:2], data.frame(predprice), 
                                     match.ID = F)
  # create SPDF 2
  g <- as(pred.surf, "SpatialPixelsDataFrame")
  # convert to matrix
  price <- as.matrix(g)
  # make interactive plot
  p <- plot_ly(z = ~price) %>% add_surface()
  return(p)
}

plot.pred.3D(50)
plot.pred.3D(100)
plot.pred.3D(150)
plot.pred.3D(200)

hp <- read.csv("hpdemo2.csv")
head(hp[, 2:7])

hp2 <- hp[, -1]
hp2 <- hp2[, c(1:4, 6)]
# ensure the OAC class is a factor
hp2$OAC <- as.factor(hp2$OAC)

#try to predict the OAC categories 
OACfit <- train(OAC ~ ., data = hp2, method = "knn", tuneLength = 40, 
                metric = "Kappa", trControl = ctrl, preProcess = c("center", 
                                                                   "scale"))
OACfit

# compare predicted against observed
tab <- table(hp2$OAC, predict(OACfit, newdata = hp2))
# ovaell accuracy
sum(diag(tab))/sum(tab)

predict(OACfit, newdata = (data.frame(east = 523800, north = 179700, 
                                      price = 150000, fl_area = 50)))
predict(OACfit, newdata = (data.frame(east = 523800, north = 179700, 
                                      price = 1e+05, fl_area = 50)))
predict(OACfit, newdata = (data.frame(east = 516000, north = 171000, 
                                      price = 180000, fl_area = 100)))
predict(OACfit, newdata = (data.frame(east = 516000, north = 171000, 
                                      price = 1e+05, fl_area = 100)))
predict(OACfit, newdata = (data.frame(east = 525700, north = 172900, 
                                      price = 104000, fl_area = 70)))
predict(OACfit, newdata = (data.frame(east = 525700, north = 172900, 
                                      price = 104000, fl_area = 100)))


# Random Forest
rfFit <- train(OAC ~ ., data = hp2, method = "rf", metric = "Kappa", 
               trControl = ctrl)
tab <- table(hp2$OAC, predict(rfFit, newdata = hp2))
sum(diag(tab))/sum(tab)








