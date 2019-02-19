# test for package existance and install
if (!is.element("tidyverse", installed.packages()))
  install.packages("tidyverse", dep = T)
if (!is.element("nycflights13", installed.packages()))
  install.packages("nycflights13", dep = T)
# load into the R session
library(tidyverse)
library(nycflights13)
install.packages("OpenStreetMap", dep = T)

#load the data as tibble
library(tidyverse)
cl <- as_tibble(read.csv("dunnhumby/dh_causal_lookup.csv"))
pl <- as_tibble(read.csv("dunnhumby/dh_product_lookup.csv"))
sl <- as_tibble(read.csv("dunnhumby/dh_store_lookup.csv"))
tr <- as_tibble(read.csv("dunnhumby/dh_transactions.csv"))

#inspect the data to find the common field
head(cl)
head(pl)
head(sl)
head(tr)

#common fields
#cl, pl, tr <- upc
#tr, sl <- store

vignette("dplyr", package = "dplyr")

vignette("two-table", package = "dplyr")

# Q1: What is the household penetration of `PRIVATE LABEL THIN SPAGHETTI`?
# That is, out of all customers purchasing Pasta, what percent purchase this brand?

#1. create a table for the upcs that correspond to the desired product
#and for the specific commodity, in this case pasta

#filter for private label thin spaghetti (plts)
plts <- pl %>%
  filter(pl$product_description == "PRIVATE LABEL THIN SPAGHETTI" )
#this object now has three observations

#filter for pasta (pasta)
pasta <- pl %>%
  filter(pl$commodity == "pasta" )
#this object now has three observations

#2.left_join of the data new_tr with the plts
#we do this as we want to keep only the data for the specific label 
#that we created the subset for
pen_house<- left_join(plts,new_tr)
pasta_house<- left_join(pasta, new_tr)

#3.drop the rows with NA values 
pen_house<-na.omit(pen_house)  
pasta_house<-na.omit(pasta_house)

#4.find unique number of households in both datasets
n.housebought<- length(unique(pen_house$household))
n.housegen <- length(unique(pasta_house$household))        

#5. find the percentage
percentage <- n.housebought*100/n.housegen
#the percentage is approximately 22.07% (22.065%) 



#create a boxplot for private label thin spaghetti
tr%>%
  left_join(pl)%>%
  filter(commodity == "pasta")%>%
  count(household) -> n.households

length(unique(pasta$household))

 # mutate(brand_search = 
          # ifelse(product_descripti on == "PRIVATE LABEL THIN SPAGHETTI",
           #       "plts","not_plts"))

#Q2: How does the household penetration of the product 
#PRIVATE LABEL THIN SPAGHETTI vary within the two regions, 
#relative to the sales of Pasta in each region? 

head(pen_house)
factor(pen_house$geography)
#there is the variable for store in the dataset that we created before 

#in order to see the variance between the two regions, we need
#to join the dataset with the store dataset

#there are two regions in the column geography

#subset using filter in the two datasets we created before

#subset for region 1 buying this pasta
pen_house %>%
  dplyr::filter(geography == 1) -> pen_house_1

#subset for region 2 buying this pasta
pen_house %>%
  dplyr::filter(geography == 2) -> pen_house_2

#subset for region 1 buying this pasta
pasta_house %>%
  dplyr::filter(geography == 1) -> pasta_house_1

#subset for region 2 buying this pasta
pasta_house %>%
  dplyr::filter(geography == 2) -> pasta_house_2


















