
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

levels(pl$commodity)
?group_by
?summarise
#common fields
#cl, pl, tr <- upc
#tr, sl <- store

vignette("dplyr", package = "dplyr")

vignette("two-table", package = "dplyr")

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
impact<-function(commod){
  #join product info data and transaction data
  tr%>%
    left_join(pl)%>%
    filter(coupon == 1)%>% #filter for use of coupon
    dplyr::select("upc",  "household","day", "product_description", "commodity","coupon")%>%
    filter(commodity == commod)-> r1 #filter for commodity
  
  #find number of households that bought a product with a coupon
  n.hh<- unique(r1$household)
  # View(n.hh)
  
  n.hh<- as.data.frame(n.hh)
  # View(n.hh)

  #examine each household particularly 
  tr%>%
    left_join(pl)%>% #join the data 
    dplyr::select("upc",  "household","day", "product_description", "commodity","coupon")%>%
    filter(commodity == commod)%>%
    arrange(day, -coupon)->r2 #filter for the commodity that we have and save
  
  # View(r2)
  
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
  
  # View(r)
  
  # length(unique(r))
  
  # length(unique(r1$household))
  
  n5<-length(r)*100/length(unique(r1$household))
  print(n5)
}


impact("pasta")
impact("pasta sauce")
impact("pancake mixes")
impact("syrups")






















