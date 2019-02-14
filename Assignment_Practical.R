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
cl <- as_tibble(read.csv("dh_causal_lookup.csv"))
pl <- as_tibble(read.csv("dh_product_lookup.csv"))
sl <- as_tibble(read.csv("dh_store_lookup.csv"))
tr <- as_tibble(read.csv("dh_transactions.csv"))

#inspect the data to find the common field
cl
pl
sl
tr

#common fields
#cl, pl, tr <- upc
#tr, sl <- store

vignette("dplyr", package = "dplyr")

vignette("two-table", package = "dplyr")

# Q1: What is the household penetration of `PRIVATE LABEL THIN SPAGHETTI`?
# That is, out of all customers purchasing Pasta, what percent purchase this brand?

pl %>%
  count(pl$product_description == "PRIVATE LABEL THIN SPAGHETTI")

table<- count(pl, product_description)
unique(pl$commodity)


# There are a number of stages to this
# 1. Identify the upc codes for product and commodity
index1 <- which(pl$product_description == "PRIVATE LABEL THIN SPAGHETTI")
index2 <- which(pl$commodity == "pasta")
# product
upc.1 <- unlist(pl[index1,"upc"])
# commodity
upc.2 <- unlist(pl[index2,"upc"])
# 2. then match these to the transactions upc
# filter and get rid of the NAs
tmp1 <- filter(tr, !is.na(match(tr$upc, upc.1)))
tmp2 <- filter(tr, !is.na(match(tr$upc, upc.2)))
# 3. then subset the transactions data
# and determine the how many hosueholds buy product / commodity
n.product <- length(unique(tmp1$household))
n.commod <- length(unique(tmp2$household))

n.households2 <- length(unique(tr$household))

#omit the households with NAs
n.households <- length(unique(na.omit(tr$household)))
(View(n.households))

View(tmp1)

View(pl)

#find the precentage of households
percentage<-100*(n.product/n.households)

#filter for private label thin spaghetti
plts <- pl %>%
  filter(pl$product_description == "PRIVATE LABEL THIN SPAGHETTI" )
#View to make sure everything went okay
View(plts)

#create new column in new_tr
new_tr <- tr
new_tr %>%
  mutate(new_tr, 
         gain = if())