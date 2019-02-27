
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

?group_by
?summarise
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


#count(household) -> n.households %>%

#Join the dataset 
tr%>%
  left_join(pl)%>%
  filter(commodity == "pasta")%>%
  count(household) -> n.households# %>%
#  filter(product_description == "PRIVATE LABEL THIN SPAGHETTI") %>%
#  count(household) -> n.hhpen

length(unique(n.hhpen))



tr%>%
  left_join(pl) %>%
  select("upc", "household", "basket", "product_description") %>%
  #filter(product_description == "PRIVATE LABEL THIN SPAGHETTI")%>%
  filter(household == "8283")->meta

View(meta)
length(unique(meta$household))
length(unique(meta$upc))
length(unique(meta$basket))






# Q1: What is the household penetration of `PRIVATE LABEL THIN SPAGHETTI`?
# That is, out of all customers purchasing Pasta, what percent purchase this brand?

#begin with the transaction file
tr %>%
  left_join(pl) %>% #join the files of transaction and product look up
  select("upc", "geography", "household","basket", "product_description")%>% #keep the variables that are of our interest
  filter(product_description == "PRIVATE LABEL THIN SPAGHETTI")%>% #select only the purchases with the desired label
  count(household) -> n.hhpen #get the count by household and save it in a table

tr %>%
  left_join(pl) %>% #join the files of transaction and product look up
  select("upc", "geography", "household","basket", "product_description", "commodity")%>%#keep the variables that are of our interest
  filter(commodity == "pasta")%>% #select only the purchases with the desired commodity
  count(household) -> n.hhpen2 #get the count by household and save it in a table

#in order to calculate the household penetration, we need to calculate the percentage of 
#people who bought the label "Private label thin spaghetti" and 
#the people who bought "pasta"(commodity) in general

#divide by length of each table, which will be the unique households in each case
n2 <- length(n.hhpen$household)*100/length(unique(n.hhpen2$household))
#Therefore, the answer to the first question is 22.065% or 22.07%


#Q2: How does the household penetration of the product 
#PRIVATE LABEL THIN SPAGHETTI vary within the two regions, 
#relative to the sales of Pasta in each region? 

#Again, begin with the transaction file
tr %>%
  left_join(pl) %>% #join the files of transaction and product look up
  select("upc", "geography", "household","basket", "product_description")%>% #keep the variables that are of our interest
  filter(product_description == "PRIVATE LABEL THIN SPAGHETTI") -> label #save it in a table

#View the top observations
head(label)

#the same for the commodity pasta
tr %>%
  left_join(pl) %>%
  dplyr::select("upc", "geography", "household","basket", "product_description", "commodity")%>%
  filter(commodity == "pasta")-> pasta_gen #save it in a table


  
#Q3: For which category of products (pasta, pasta sauces, syrups and pancake mixes) does the 
#provision of coupons appear to have a positive impact? For this you should consider which customers first
# purchased an item in a category using a coupon, and then how many of these customers made 
#additional purchases of the item in the category. (30 marks) 

##we can create a function for these commodities
##first we will calculate everything for pasta

#find the households that bought with a coupon

#join product info data and transaction data
tr%>%
  left_join(pl)%>%
  filter(coupon == 1)%>% #filter for use of coupon
  dplyr::select("upc",  "household","day", "product_description", "commodity","coupon")%>%
  filter(commodity == "pasta")-> r1 #filter for commodity

#find number of households that bought a product with a coupon
n.hh<- unique(r1$household)
View(n.hh)

n.hh<- as.data.frame(n.hh)
n.hh[3,1]
View(n.hh)
n.hh1<- as.data.frame(n.hh[1:100, ])

View(n.hh1)
n.hh1$household<-n.hh1$`n.hh[1:100, ]`
n.hh1$`n.hh[1:100, ]`<-NULL
#examine each household particularly 
tr%>%
  left_join(pl)%>% #join the data 
  dplyr::select("upc",  "household","day", "product_description", "commodity","coupon")%>%
  filter(commodity == "pasta")%>%
  arrange(day, -coupon)->r2 #filter for the commodity that we have and save

View(r2)

#create a function

r<-vector("numeric")
#this is the solution!!!!!!!
for (row in 1:nrow(n.hh)){
  r2%>%
    filter(household == n.hh[row, 1])%>%
    arrange(day,-coupon)%>%
    select(coupon)%>%
    unlist->tmp
  if (tmp[1]== 1 & length(tmp)>1)
    r<- append(r,n.hh[row,1])
}

View(r)

length(unique(r))

length(unique(r1$household))

n5<-length(r)*100/length(unique(r1$household))

























#check_for_hh<- function(n.hh){
 # for (row in 1:nrow(n.hh)):
  #  r2%>%
   # filter(household == n.hh[row, 1])%>%
    #arrange(day,-coupon)%>%
    #select(coupon)%>%
    #unlist->tmp
#  }

check_for_hh<- function(n.hh1){
#check the households  
  for (row in 1:nrow(n.hh1)) {
    r2%>%
      filter(household == n.hh1[row, 1])%>%
      arrange(day,-coupon)%>%
      select(coupon)%>%
      unlist->tmp
#check for the coupons    
    if (tmp[n.hh1[row, 1]] == 1 & length(tmp) > 1) 
      return(n.hh1[row, 1])
  }
} 

View(tmp)

n.hh1<- as.data.frame(n.hh[1:300, ])
n.hh1$household<-n.hh1$`n.hh[1:300, ]`
n.hh1$`n.hh[1:300, ]`<-NULL

for (row in 1:nrow(n.hh1)){
  #check the households  
  check_for_hh<- function(n.hh1){
    r2%>%
      filter(household == n.hh1[row, 1])%>%
      arrange(day,-coupon)%>%
      select(coupon)%>%
      unlist->tmp
    #check for the coupons    
    if (tmp[1] == 1 & length(tmp) > 1) 
      return(n.hh1[row, 1])
  }
} 

#this is the solution!!!!!!!
for (row in 1:nrow(n.hh1)){
  r2%>%
    filter(household == n.hh1[row, 1])%>%
    arrange(day,-coupon)%>%
    select(coupon)%>%
    unlist->tmp
  if (tmp[1]== 1 & length(tmp)>1)
      pastpen$household<-append(n.hh1[row,1])
}
View(tmp)
n.hh1[300,1]
