# test for package existance and install
if (!is.element("tidyverse", installed.packages()))
  install.packages("tidyverse", dep = T)
library(tidyverse)
#we cannot just load the dataset mpg, we have to download the ggplot2 package first and the library
install.packages("ggplot2", dep = T)
library(ggplot2)
mpg
#displ = a car's engine size, in litres.
#hwy = a car's fuel efficiency on the highway, in miles per gallon (mpg)
View(mpg) #234 rows and 11 columns

#To plot mpg, run this code to put displ on the x-axis and hwy on the y-axis:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) #we don't see anything

?mpg #view info about the dataset

#Make a scatterplot of hwy vs cyl
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy))
# make a scatterplot of class vs drv
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv))

ggplot(data = mpg) + geom_point(mapping =  aes(hwy, cyl))

#we can check for the outliers by adding another value by adding an aesthetic
#An aesthetic is a visual property of the objects in your plot. 
#Aesthetics include things like the size, the shape, or the colour of your points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = class))
#we can also use different sizes for each class
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# Left, or map it on alpha, which is basically the transparency
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

#Right, or map it on different symbols
# Right
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

#we can set a colour of our choice to all the points in the scatterplots
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), colour = "blue")
?geom_point()
#the colour is inside the mapping 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = "blue"))

##MAP A CONTINUOUS VARIABLE

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = cty))
#we can also use different sizes for each class
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = cty))

# Left, or map it on alpha, which is basically the transparency
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = cty))

#Right, or map it on different symbols
##Error: A continuous variable can not be mapped to shape
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = cty))

#when we display the displ <5, all the observations that have displ <5 are TRUE, the others are false
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = displ < 5))

#the following code does not work 
ggplot(data = mpg) 
+ geom_point(mapping = aes(x = displ, y = hwy))

