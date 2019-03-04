
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
hh_pen <- length(n.hhpen$household)*100/length(unique(n.hhpen2$household))

#Therefore, the answer to the first question is 22.065% or 22.07%


Household_penetration<-"Household penetration"
Percentage<- round(hh_pen,2)
graph<- data.frame(Household_penetration, Percentage)
head(graph)

ggplot(graph, mapping=aes(Household_penetration, Percentage, label="22.07%"))+
  geom_bar(stat="identity", width = 0.30, fill= "steelblue")+
  theme(axis.text.x=element_blank())+
  coord_cartesian(ylim = c(0, 25))+
  geom_text(size = 3, position = position_stack(vjust = 0.5))




#Q2: How does the household penetration of the product 
#PRIVATE LABEL THIN SPAGHETTI vary within the two regions, 
#relative to the sales of Pasta in each region? 

#Again, begin with the transaction file
tr %>%
  #join the files of transaction and product look up
  left_join(pl) %>% 
  #keep the variables that are of our interest
  select("geography", "household", "product_description")%>% 
  #save it in a table
  filter(product_description == "PRIVATE LABEL THIN SPAGHETTI")-> private_label

length(unique(private_label$household))

private_label%>%#filter for geography 1
  filter(geography==1)%>%
  count(household)->private_label_1

length(unique(private_label_1$household))

private_label%>%#filter for geography 2
  filter(geography==2)%>%
  count(household)->private_label_2

length(unique(private_label_2$household))


#the same for the commodity pasta
tr %>%
  left_join(pl) %>%
  dplyr::select("upc", "geography", "household", "product_description", "commodity")%>%
  filter(commodity == "pasta")-> pasta_gen #save it in a table
length(unique(pasta_gen$household))

pasta_gen%>% #filter for geography 1
  filter(geography==1)%>%
  count(household)->pasta_gen_1

length(unique(pasta_gen_1$household))

pasta_gen%>%#filter for geography 2
  filter(geography==2)%>%
  count(household)->pasta_gen_2 

length(unique(pasta_gen_2$household))

hh_pen_1<-length(unique(private_label_1$household))*100/length(unique(pasta_gen_1$household))
hh_pen_2<-length(unique(private_label_2$household))*100/length(unique(pasta_gen_2$household))


Regions<- c("Region 1", "Region 2")
Percentage<-c(round(hh_pen_1,2), round(hh_pen_2,2) ) 
graph<- data.frame(Regions, Percentage)
head(graph)

ggplot(graph, mapping=aes(Regions, Percentage, label=c("18.32%", "27.01%")))+
  geom_bar(stat="identity", width = 0.30, fill= c("steelblue","deeppink3"))+
  coord_cartesian(ylim = c(0, 30))+
  geom_text(size = 3, position = position_stack(vjust = 0.5))






#Q3: For which category of products (pasta, pasta sauces, syrups and pancake mixes) does the 
#provision of coupons appear to have a positive impact?

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
  n.hh<- as.data.frame(n.hh)

  #examine each household particularly 
  tr%>%
    left_join(pl)%>% #join the data 
    dplyr::select("upc",  "household","day", "product_description", "commodity","coupon")%>%
    filter(commodity == commod)%>%#filter for the commodity that we have and save
    arrange(day, -coupon)->r2 

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

  n5<-length(r)*100/length(unique(r1$household))
  print(n5)
}

impact_pasta<- impact("pasta")
impact_pasta_sauce<-impact("pasta sauce")
impact_pancake_mixes<-impact("pancake mixes")
impact_syrups<-impact("syrups")


Commodity<- c("Pasta", "Pasta sauce", "Pancake mixes", "Syrups")
Percentage<-c(25.34, 29.53, 27.34, 26.74) 
graph<- data.frame(Commodity, Percentage)
head(graph)

ggplot(graph, mapping=aes(Commodity, Percentage, label=c("25.34%", "29.53%", "27.34%", "26.74%")))+
  geom_bar(stat="identity", width = 0.50, fill= c("cadetblue","cadetblue1", "cadetblue2", "cadetblue4"))+
  coord_cartesian(ylim = c(0, 30))+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle("Impact of coupons on each commodity", position_dodge(middle)) 

?ggtitle

















