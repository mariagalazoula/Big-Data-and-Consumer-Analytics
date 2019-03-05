---
  title: "Big Data and Consumer Analytics Assignment 1"
author: "Student ID: 201075916"
date: "5 March 2019"
output: pdf_document
---
  
  
  ## Data Exploration 
  
  In order to perform any analysis, it is crucial to identify how the files can be linked, in this case the variables "upc" and "store".  This can be seen in the code and figures below. The common fields are: the "upc" and "store" variables. 

```{r}
# test for package existance and install
if (!is.element("tidyverse", installed.packages()))
  install.packages("tidyverse", dep = T)
if (!is.element("nycflights13", installed.packages()))
  install.packages("nycflights13", dep = T)
# load into the R session
library(tidyverse)
library(nycflights13)


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

```

## Q1: What is the household penetration of `PRIVATE LABEL THIN SPAGHETTI`? That is, out of all customers purchasing Pasta, what percent purchase this brand?

In order to answer this task, the number of people that have bought this specific brand, i.e. "Private Label This Spaghetti" must be calculated along with the number of people that have bought the respective commodity, in this case "Pasta". In order to do this, the file of transactions was joined with the product file through the variable "upc". The variables necessary were then kept through the "select" command. Then, the two datasets that were created were filtered to keep records only with "Private Label This Spaghetti" and "Pasta" respectively.Finally, the two datasets were saved in two dataframes, from which the number of unique values was calculated, in order to estimate the percentage of people that bought that specific pasta. The code that was used in this part is visible in figure 2 and in figure 3 a graph showing the household penetration can be seen. The final percentage is 22.07% of household penetration of "Private Label This Spaghetti". 

```{r}

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
print(hh_pen)
#Therefore, the answer to the first question is 22.065% or 22.07%

```


```{r, echo= FALSE}

Household_penetration<-"Household penetration"
Percentage<- round(hh_pen,2)
graph<- data.frame(Household_penetration, Percentage)
head(graph)

ggplot(graph, mapping=aes(Household_penetration, Percentage, label="22.07%"))+
  geom_bar(stat="identity", width = 0.30, fill= "steelblue")+
  theme(axis.text.x=element_blank())+
  coord_cartesian(ylim = c(0, 25))+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle("Household Penetration of Private Label Thin Spaghetti")
```
Figure 1. Graph for household penetration of "Private Label Thin Spaghetti" (RStudio, 2019).  

## Q2: How does the household penetration of the product PRIVATE LABEL THIN SPAGHETTI vary within the two regions, relative to the sales of Pasta in each region?

For this task, the previous code with the variable of "geography" was replicated, so that the two regions could be studied separately. Then the datasets that were created were filtered for each region in order to calculate the percentage. This was conducted twice, once for the "Private Label This Spaghetti" and once for the commodity "Pasta". The household penetration for each region is 18.32% and 27.01% respectively.  
```{r}
#begin with the transaction file
tr %>%
  #join the files of transaction and product look up
  left_join(pl) %>% 
  #keep the variables that are of our interest
  select("geography", "household", "product_description")%>% 
  #filter for product
  filter(product_description == "PRIVATE LABEL THIN SPAGHETTI")%>%
  #get the unique values of households by geography
  distinct(household, geography)%>%
  #get the count of households for each geography
  count(geography)->product_region

#the same for the commodity pasta
tr %>%
  #join the files of transaction and product look up
  left_join(pl) %>%
  #keep the variables that are of our interest
  dplyr::select("upc", "geography", "household", "product_description", "commodity")%>%
  #filter for commodity 
  filter(commodity == "pasta")%>% 
  #get the unique values of households by geography
  distinct(household, geography)%>%
  #get the count of households for each geography
  count(geography)->commodity_region

#household penetration for region 1, in row 1 and column 2 for each dataset
hh_pen_1<-as.numeric((product_region[1,2])*100/(commodity_region[1,2]))
print(hh_pen_1)

#household penetration for region 2, in row 2 and column 2 for each dataset
hh_pen_2<-as.numeric((product_region[2,2])*100/(commodity_region[2,2]))
print(hh_pen_2)

```


```{r, echo = FALSE}
Regions<- c("Region 1", "Region 2")
Percentage<-c(round(hh_pen_1,2), round(hh_pen_2,2) ) 
graph<- data.frame(Regions, Percentage)
head(graph)

ggplot(graph, mapping=aes(Regions, Percentage, label=c("18.32%", "27.01%")))+
  geom_bar(stat="identity", width = 0.30, fill= c("steelblue","deeppink3"))+
  coord_cartesian(ylim = c(0, 30))+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle("Variance of household Penetration of PRIVATE LABEL THIN SPAGHETTI in the two regions")
```