# set a mirror
#
options(repos = c(CRAN = "http://cran.rstudio.com"))
# test for package existance and install
if (!is.element("tidyverse", installed.packages()))
  install.packages("tidyverse", dep = T)
if (!is.element("sf", installed.packages())) 
  install.packages("sf", dep = T)
if (!is.element("GISTools", installed.packages()))
  install.packages("GISTools", dep = T)
if (!is.element("RCurl", installed.packages()))
  install.packages("RCurl", dep = T)
if (!is.element("rjson", installed.packages()))
  install.packages("rjson", dep = T)
if (!is.element("tmap", installed.packages()))
  install.packages("tmap", dep = T)
if (!is.element("nycflights13", installed.packages()))
  install.packages("nycflights13", dep = T)
if (!is.element("OpenStreetMap", installed.packages()))
  install.packages("OpenStreetMap", dep = T)
if (!is.element("rgdal", installed.packages()))
  install.packages("rgdal", dep = T)
if (!is.element("dplyr", installed.packages()))
  install.packages("dplyr", dep = T)
# load into the R session
library(dplyr)
library(sf)
library(GISTools)
library(RCurl)
library(rjson)
library(tmap)
library(nycflights13)
library(tidyverse)
library(OpenStreetMap)
library(rgdal)

#working with an API to get data, in this case the police API
#this is how we request data from an API in R
crimes.buf <- getForm("http://data.police.uk/api/crimes-street/all-crime",
                      lat= 53.7997, 
                      lng= -1.5492,
                      date="2016-10")

crimes <- fromJSON(crimes.buf)
#check the type of the data, in this case list
typeof(crimes)

#check the first observation, i.e. the first row
crimes[[1]]

#create a function to extract the locations of the dataset
getLonLat <- function(x) as.numeric(c(x$location$longitude,
                                      x$location$latitude))
crimes.loc <- t(sapply(crimes,getLonLat))
head(crimes.loc)

#create a function to extract the attributes of the crime
getAttr <- function(x) c(
  x$category,
  x$location$street$name,
  x$location_type,
  x$month)
crimes.attr <- as.data.frame(t(sapply(crimes,getAttr)))
colnames(crimes.attr) <- c("category","street","location_type", "date")
head(crimes.attr)


#
crimes.pts <- SpatialPointsDataFrame(crimes.loc,
                                     data = crimes.attr, proj4string = CRS("+proj=longlat "))
#writePointsShape(crimes.pts, fn = "crime.shp")
#the head will only show us the crime attributes, not the spatial data
# Note that 'head' doesn't work directly on SpatialPointsDataFrames
head(crimes.pts@data)
head(crimes.pts)[1,1]

# define upper left, lower right corners 
ul <- as.vector(cbind(bbox(crimes.pts)[2,2], 
                      bbox(crimes.pts)[1,1]))
lr <- as.vector(cbind(bbox(crimes.pts)[2,1], 
                      bbox(crimes.pts)[1,2]))
# download the map tile
MyMap <- openmap(ul,lr)
plot(MyMap)

# now plot the layer(crimes) and the backdrop(the map we have)
par(mar = c(0,0,0,0))
plot(MyMap, removeMargin=FALSE)
plot(spTransform(crimes.pts, osm()), add = TRUE, pch=19,col="#DE2D2650")

##get data for every month
# set up some empty matrices
crimes.loc <- matrix(nrow = 0, ncol = 2)
crimes.attr <- matrix(nrow = 0, ncol = 4)
# make a look for each month
for (i in 1:12) {
  # get the data
  month.i <- (1:12)[i]
  date.i <- paste0("2016-",month.i)
  # create the URL
  url.i <- paste0("http://data.police.uk/api/crimes-street/all-crime?poly=",
                  "53.839153,-1.637277:53.839153,-1.450116:53.767379,-1.450116:53.767379,-1.637277",
                  "&date=",date.i)
  # pass to the API
  crimes.buf <- getForm(url.i)
  crimes.i <- fromJSON(crimes.buf)
  # extract and add the locations
  crimes.loc.i <- t(sapply(crimes.i,getLonLat))
  crimes.loc <- rbind(crimes.loc, crimes.loc.i)
  crimes.attr.i <- as.data.frame(t(sapply(crimes.i,getAttr)))
  # extract and add the crimes types
  colnames(crimes.attr.i) <- c("category","street","location_type", "date")
  crimes.attr <- rbind(crimes.attr, crimes.attr.i)
  # print out a little indicator of progress
  cat(i, "\t")
}

crimes.pts <- SpatialPointsDataFrame(crimes.loc,crimes.attr)
proj4string(crimes.pts) <- CRS("+proj=longlat")
# convert to sf format
crimes.sf <- st_as_sf(crimes.pts)
head(data.frame(crimes.sf))
#dimension of data frame
dim(data.frame((crimes.sf)))

##Select has three different packages, we have to pick the right one, in our case dplyr
#have a look at the data
as_tibble(crimes.sf)

#map the data
tmap_mode('view')
tm_shape(crimes.sf) +
  tm_bubbles(col = "#DE2D26", alpha = 0.05, size = 0.1)
tmap_mode('plot')

#intro to package dplyr
vignette("dplyr", package = "dplyr")

crimes <- as_tibble(crimes.sf)
crimes %>% # %>% magrittr, is a pipe like operator
  #extract a subset let's say
  count(category) %>%
  ggplot(aes(x = reorder(category, n), y = n)) + 
  geom_col() + 
  labs(x = "Crime Type",
       y = "Number of Crimes",
       title = "Crimes commited in 2016") +
  coord_flip() +
  theme_minimal()

library(dplyr)
leeds.weather <- c(3, 4, 5, 7, 10, 12, 14, 15, 12, 9, 5, 3)
# see http://www.holiday-weather.com/leeds/averages/
crimes %>% 
  dplyr::filter(category == "anti-social-behaviour") %>% 
  count(date)  %>% cbind(leeds.weather) %>%
  ggplot(aes(x = reorder(date, n), y = n, fill = leeds.weather)) + 
  geom_col() + 
  labs(x = "Month",
       y = "Count",
       title = "ASBOs in 2016") +
  coord_flip() +
  theme_minimal()

crimes %>%
  dplyr::select(category)

crimes %>% 
  filter(category == "anti-social-behaviour") %>% 
  count(date) 
#load the data
load("LSOA.RData")
# this loads an R object called lsoa
head(lsoa)

dim(lsoa)
class(lsoa)

load("Claimants2016LSOA.RData")
# this loads an R object called claimants
head(claimants)

dim(claimants)
class(claimants)

load("IMD.RData")
head(imd)
dim(imd)
class(imd)
vignette("two-table", package = "dplyr")

head(lsoa)
head(claimants)
head(imd)
imd %>% dplyr::select(LSOA.code, IMD) -> imd2
claim2 <- claimants %>% dplyr::select(-LSOA2011.name) 

imd2
claim2
#left join to keep all the records of lsoa
lsoa%>%left_join(imd2,c("code"="LSOA.code"))%>%left_join(claim2,c("code"="LSOA2011.code"))->lsoa
head(lsoa)

head(crimes.sf)

#the data needs to have a projection in order to be transformed!!!!!!!!!!

# Using spTransform in sp
#new.spatial.data <- spTransform(old.spatial.data, new.Projection)
# Using st_transform in sf
#new.spatial.data.sf <- st_transform(old.spatial.data.sf, new.Projection)

os.gb.proj <- "+init=epsg:27700"
crimes.sf <- st_transform(crimes.sf, os.gb.proj)
head(crimes.sf)

tm_shape(crimes.sf) +
  tm_dots(col = "red", alpha = 0.2) +
  tm_shape(lsoa) + tm_borders()

#subsetting the lsoa data
lsoa.sf <- lsoa[crimes.sf,]
head(lsoa.sf)
tm_shape(lsoa.sf) +
  tm_fill("IMD" , alpha = 0.8)

###spatial overlay
#point-in-polygon operation that simply takes each crime point 
#and determines the intersecting LSOA polygon and attaches the 
#attribute values from that polygon to the crime data point.

data.int <- st_intersection(crimes.sf, lsoa.sf)
#attributes of the intersecting polygons
head(data.int)
dim(data.int)
#for tidyverse, convert data to tibble
data.t <- as_tibble(data.int[,-c(2,3,5)])

install.packages("hexbin", dep = T)
library(hexbin)
#plot hexbins of the IMD scores
ggplot(data = data.t) +
  geom_hex(mapping = aes(x = Mean, y = IMD)) +
  labs(title = "IMD against Mean Claimant number")

by_cc <- group_by(data.t, code)
data.s <- summarise(by_cc, CrimeCount = n())

# do the join
lsoa.sf %>% left_join(data.s, "code") %>%
  # pipe the result to ggplot
  ggplot(aes(y=CrimeCount,x=Mean)) +
  geom_point() +
  geom_smooth(method='lm') +
  # set the y axis limits
  ylim(0, 500) + # try commenting this out-removes some outliers
  # label the axes and the plot
  xlab("Mean number of Benefit Claimants") +
  ylab("Total Crimes") +
  labs(title = "Crime against Benefit Claimants")

lsoa.sf %>% left_join(data.s, "code") -> AllCrime
head(AllCrime)

#include dates in the analysis
by_cc <- group_by(data.t, code, category, date)
data.s <- summarise(by_cc, CrimeCount = n())

data.c <- spread(data.s, category, value = "CrimeCount")
data.c

by_c <- group_by(data.t, code)
data.s <- summarise(by_c, CrimeCount = n())
lsoa.sf %>% left_join(data.s, "code") %>% 
  mutate(Claimants = Mean) %>%
  dplyr::select(code, Claimants, CrimeCount) %>% as_tibble -> AllCrime

by_cc <- group_by(data.t, code, category)
data.s <- summarise(by_cc, CrimeCount = n())
lsoa.sf %>% left_join(data.s, "code") %>% 
  mutate(Claimants = Mean) %>%
  dplyr::select(code, category, Claimants, IMD, CrimeCount) %>% as_tibble -> IndividCrime


IndividCrime

# call ggplot and specify the aesthetics
ggplot(data = AllCrime, aes(y=CrimeCount,x=Claimants)) +
  # specify the type of plot
  geom_point() +
  # then any additional parameters
  geom_smooth(method='lm') +
  # set the y axis limits
  ylim(0, 500) + # try commenting this out
  # label the axes and the plot
  xlab("Mean number of Benefit Claimants") +
  ylab("Total Crimes") +
  labs(title = "Crime against Benefit Claimants")

by_ccd <- group_by(data.t, code, category, date)
data.s <- summarise(by_ccd, CrimeCount = n())
lsoa.sf %>% left_join(data.s, "code") %>% 
  mutate(Claimants = Mean) %>% 
  dplyr::select(code, date, category, CrimeCount,
         Jan.16, Feb.16, Mar.16, Apr.16, May.16, Jun.16, Jul.16, Aug.16,
         Sep.16, Oct.16, Nov.16, Dec.16) %>% as_tibble -> IndDateMonthCrime

IndDateMonthCrime 

#creating histograms
ggplot(AllCrime, aes(x=CrimeCount)) + 
  geom_histogram(binwidth=20,colour="white") +
  scale_x_continuous(limits = quantile(AllCrime$CrimeCount, c(0.0, 0.95))) 

ggplot(AllCrime, aes(x=CrimeCount)) + 
  geom_histogram(aes(y=..density..),
                 binwidth=40,colour="white") +
  scale_x_continuous(limits = quantile(AllCrime$CrimeCount, c(0.0, 0.95))) +
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=median(CrimeCount, na.rm=T)),   # Ignore NA values for mean na.rm= na values removed
             color="#DE2D26", linetype="dashed", size=1)

#map the distributions of the crime data spatially
lsoa.sf$CrimeCount <- AllCrime$CrimeCount
tm_shape(lsoa.sf) +
  tm_polygons("CrimeCount", style = "quantile", palette = "GnBu") +
  tm_legend(legend.outside=TRUE)

#the crime per Benefit Claimant
lsoa.sf$CrimePerClaimant <- AllCrime$CrimeCount / AllCrime$Claimants
tm_shape(lsoa.sf) +
  tm_polygons("CrimePerClaimant", style = "quantile", palette = "OrRd") +
  tm_legend(legend.outside=TRUE)

#examine how 2 continuous variables relate to each other using scatterplots
ggplot(data = AllCrime) + 
geom_point(mapping = aes(x = Claimants, y = CrimeCount)) + 
  ylim(0, 500)  # to remove the outliers form the plot

#place the variables in boxplots
ggplot(data = AllCrime, mapping = aes(x = Claimants, y = CrimeCount)) + 
  geom_boxplot(mapping = aes(group = cut_width(Claimants, 10))) +
  scale_y_continuous(limits = quantile(AllCrime$CrimeCount, c(0.1, 0.9)))

#examine counts of different types of crime in lsoa areas 
# boxplots
ggplot(IndividCrime, aes(category, CrimeCount)) + 
  geom_boxplot(fill = "lightgrey", colour = "#3366FF", outlier.colour = "#08519C") +
  #geom_boxplot(binwidth=10,colour="white") +
  scale_y_continuous(limits = quantile(IndividCrime$CrimeCount, c(0.1, 0.9))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#generate scatterplots of the crime levels for each type
levels(IndividCrime$category) <- c("ASBO", "Bike theft", "Burglary", "Damage-Arson",
                                   "Drugs", "Other", "Other Theft", "Weapons", "Public Order", "Robbery", "Shoplifting", "Theft", 
                                   "Vehicle", "Violence")

#with mean claimant number
ggplot(data = IndividCrime, aes(y=CrimeCount,x=Claimants)) +
  geom_point(col = "#A50F15", alpha = 0.5, shape = 1) +
  facet_wrap( ~ category, ncol = 7) +
  scale_y_continuous(limits = quantile(IndividCrime$CrimeCount, c(0.1, 0.9))) +
  geom_smooth(method='lm', lwd = 0.7) +
  xlab("Mean number of Benefit Claimants") +
  ylab("Total Crimes")

#with IMD
ggplot(data = IndividCrime, aes(y=CrimeCount,x=IMD)) +
  geom_point(alpha = 0.5, shape = 16) +
  facet_wrap( ~ category, ncol = 7) +
  scale_y_continuous(limits = quantile(IndividCrime$CrimeCount, c(0.1, 0.9))) +
  geom_smooth(method='lm', lwd = 0.5) +
  # label the axes and the plot
  xlab("Index of Multiple Deprivation") +
  ylab("Total Crimes")

by_tmp <- group_by(IndividCrime, category)
by_crime <- as.data.frame(summarise(by_tmp, Claim = mean(Claimants), 
                                    IMD = mean(IMD), count = sum(CrimeCount)))
# create new column for crime names
by_crime$crime <- by_crime$category  
# compute normalized IMD
by_crime$imd_z <- round((by_crime$IMD - mean(by_crime$IMD))/sd(by_crime$IMD), 2)  
# create an above / below avg flag
by_crime$imd_type <- ifelse(by_crime$imd_z < 0, "below", "above")
# sort
by_crime <- by_crime[order(by_crime$imd_z), ]  
# convert to factor to retain sorted order in plot.
by_crime$crime <- factor(by_crime$crime, levels = by_crime$crime) 

ggplot(by_crime, aes(x=crime, y=imd_z, label=imd_z)) + 
  geom_point(stat='identity', fill="black", size=6)  +
  geom_segment(aes(y = 0, 
                   x = crime, 
                   yend = imd_z, 
                   xend = crime), 
               color = "black") +
  geom_text(color="white", size=2) +
  labs(title="Diverging Lollipop Chart") +  
  ylim(-2.5, 2.5) +
  coord_flip()

#examining temporal trends
# pre-processing
by_tmp <- group_by(IndDateMonthCrime, date, category)
by_tmp <- summarise(by_tmp, count = sum(CrimeCount))
levels(by_tmp$date) <- c("Jan","Feb","Mar","Apr","May",   
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec")
levels(by_tmp$category) <- c("ASBO", "Bike theft", "Burglary", "Damage-Arson",
                             "Drugs", "Other", "Other Theft", "Weapons", "Public Order", "Robbery", "Shoplifting", "Theft", 
                             "Vehicle", "Violence")
# plot
ggplot(by_tmp, aes(date, count)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(~category, ncol = 4, scales = "free") +coord_flip() 
