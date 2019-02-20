
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
  left_join(pl) %>% #keep 
  select("upc", "geography", "household","basket", "product_description")%>%
  filter(product_description == "PRIVATE LABEL THIN SPAGHETTI")%>%
  count(household) -> n.hhpen 

tr %>%
  left_join(pl) %>%
  select("upc", "geography", "household","basket", "product_description", "commodity")%>%
  filter(commodity == "pasta")%>%
  count(household) -> n.hhpen2

length(unique(tr$household))

n <- length(n.hhpen$household)*100/length(unique(tr$household))

n2 <- length(n.hhpen$household)*100/length(unique(n.hhpen2$household))



#Q2: How does the household penetration of the product 
#PRIVATE LABEL THIN SPAGHETTI vary within the two regions, 
#relative to the sales of Pasta in each region? 

tr %>%
  left_join(pl) %>% #keep 
  dplyr::select("upc", "geography", "household","basket", "product_description")%>%
  filter(product_description == "PRIVATE LABEL THIN SPAGHETTI") -> pasta_label




tr %>%
  left_join(pl) %>%
  dplyr::select("upc", "geography", "household","basket", "product_description", "commodity")%>%
  filter(commodity == "pasta")%>%
  count(geography)-> gen_pasta

tr %>%
  left_join(pl) %>% #keep 
  dplyr::select("upc", "geography", "household","basket", "product_description")%>%
  filter(product_description == "PRIVATE LABEL THIN SPAGHETTI")%>%
  count(geography) ->geo3

  
geo <- count(pasta_label, geography)

geo2<- count(gen_pasta, geography)
  
  
remove(geo2) 

geo3%>%
  left_join(gen_pasta, "geography")%>%
  mutate(percentage = n.x*100/n.y)->calc
  
  

tr %>%
  left_join(pl) %>%
  dplyr::select("upc", "geography", "household","basket", "product_description", "commodity")%>%
  filter(commodity == "pasta", geography == "1")-> just_pasta

length(unique(just_pasta$household))






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
  left_join(pl) %>% #keep 
  select("upc", "geography", "household","basket", "product_description")%>%
  filter(product_description == "PRIVATE LABEL THIN SPAGHETTI")%>%
  count(household) -> n.hhpen 

tr %>%
  left_join(pl) %>%
  select("upc", "geography", "household","basket", "product_description", "commodity")%>%
  filter(commodity == "pasta")%>%
  count(household) -> n.hhpen2

length(unique(tr$household))

n <- length(n.hhpen$household)*100/length(unique(tr$household))

n2 <- length(n.hhpen$household)*100/length(unique(n.hhpen2$household))



#Q2: How does the household penetration of the product 
#PRIVATE LABEL THIN SPAGHETTI vary within the two regions, 
#relative to the sales of Pasta in each region? 

tr %>%
  left_join(pl) %>% #keep 
  dplyr::select("upc", "geography", "household","basket", "product_description")%>%
  filter(product_description == "PRIVATE LABEL THIN SPAGHETTI") -> pasta_label




tr %>%
  left_join(pl) %>%
  dplyr::select("upc", "geography", "household","basket", "product_description", "commodity")%>%
  filter(commodity == "pasta")%>%
  count(geography)-> gen_pasta

tr %>%
  left_join(pl) %>% #keep 
  dplyr::select("upc", "geography", "household","basket", "product_description")%>%
  filter(product_description == "PRIVATE LABEL THIN SPAGHETTI")%>%
  count(geography) ->geo3

  
geo <- count(pasta_label, geography)

geo2<- count(gen_pasta, geography)
  
  
remove(geo2) 

geo3%>%
  left_join(gen_pasta, "geography")%>%
  mutate(percentage = n.x*100/n.y)->calc
  
  

tr %>%
  left_join(pl) %>%
  dplyr::select("upc", "geography", "household","basket", "product_description", "commodity")%>%
  filter(commodity == "pasta", geography == "1")-> just_pasta

length(unique(just_pasta$household))

tr %>%
  left_join(pl) %>%
  dplyr::select("upc", "geography", "household","basket", "product_description", "commodity")%>%
  filter(commodity == "pasta", geography == "2")-> just_pasta

length(unique(just_pasta$household))

#Q3: For which category of products (pasta, pasta sauces, syrups and pancake mixes) does the 
#provision of coupons appear to have a positive impact? For this you should consider which customers first
# purchased an item in a category using a coupon, and then how many of these customers made 
#additional purchases of the item in the category. (30 marks) 

#we can create a function for these commodities
#first we will calculate everything for pasta


#find the households that bought with a coupon

#join product info data and transaction data
tr%>%
  left_join(pl)%>%
  filter(coupon == 1)%>% #filter for use of coupon
  filter(commodity == "pasta")-> r1 #filter for commodity

#find number of households that bought a product with a coupon
n.hh<- unique(r1$household)
view(n.hh)

#examine each household particularly 
tr%>%
  left_join(pl)%>% #join the data 
  filter(commodity == "pasta")->r2 #filter for the commodity that we have and save

n.hhcom<- as.data.frame(unique(r2$household))

r2%>%
  arrange(day, -coupon)%>%
  dplyr::select(household, upc, commodity, day, coupon)-> r3

#for (x in n.hh$`unique(r1$household)`)

extract-> function(x){
  r2%>%
    filter(household == x)%>%
    arrange(day, -coupon)%>%
    select(coupon)%>%
    unlist->tmp
}

apply(n.hh, 1, extract)

for (x in n.hh$`unique(r1$household)`)
