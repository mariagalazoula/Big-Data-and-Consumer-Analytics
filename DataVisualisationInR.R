# test for package existance and install
if (!is.element("tidyverse", installed.packages()))
  install.packages("tidyverse", dep = T)

install.packages("dplyr", dep = T)
library(dplyr)
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

ggplot(data = mpg) + 
  geom_jitter(mapping = aes(x = displ, y = hwy))

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

##FACETS, split the plot in subplots that each display a subset of the data 
#in this case, we will have a plot for each of the values that the variable class will have
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

##combination of two variables
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

#facet on a continuous variable~a lot of plots 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ cty, nrow = 5)

#WHAT DOES THE "." MEAN???
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

#advantages of facet, each variable one (sub)plot 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

?facet_wrap

?facet_grid

# plots the points exactly
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# plots a line that fits the data 
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

#each of the values for the variable drv has a different linetype
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

#each of the values for the variable drv has a different linewidth
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, lwd = drv))

#overlaying the two plots from before
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = drv))+
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, colour = drv))

#does not show legend
ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, colour = drv),
    show.legend = FALSE
  )

#lines for each of the values in drv
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
#multiple geoms in the same plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

#avoid the duplication in the code
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

#change properties for one plot only
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(colour = class)) + 
  geom_smooth()

#different data for each layer
#filter needs the dplyr package
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(colour = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

##Statistical transformations

View(diamonds)
#bar plot for diamonds dataset for count of diamonds by cut
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

#create the same plot using different command
ggplot(data = diamonds) + 
    stat_count(mapping = aes(x = cut))

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)
ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

#change the default mapping from transformed to aesthetics
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

#draw attention to the stat transformation
#summarise the y value for each unique x value
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )
#not very useful, the colour is behind the bars
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))

#use fill instead
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

#
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar( position = "identity")

?geom_bar
#position = "fill"
# works like stacking, but makes each set of stacked bars the same height.
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

#position = "dogde"
#places overlapping objects right next to each other
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")


#SOLVING THE OVERPLOTTING ISSUE
#sometimes points overlap each other because the values on the axes are rounded
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

#adding randomness seems strange, but it reveals more at large scales


#switch the x and y axis
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
#this is not very useful, that is why we will flip the axes

#this plot is more useful
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()+
  coord_flip()

#set the aspect ratio correctly for maps
nz <- map_data("nz")
#not proper ratio
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

#polar coordinates
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()






