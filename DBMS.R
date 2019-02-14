# set a mirror
options(repos = c(CRAN = "http://cran.rstudio.com"))
# test for package existance and install
if (!is.element("tidyverse", installed.packages()))
  install.packages("tidyverse", dep = T)
if (!is.element("RSQLite", installed.packages())) 
  install.packages("RSQLite", dep = T)
if (!is.element("rgdal", installed.packages()))
  install.packages("rgdal", dep = T)
if (!is.element("tmap", installed.packages())) 
  install.packages("tmap", dep = T)
# load into the R session
library(tidyverse)
library(RSQLite)
library(rgdal)
library(tmap)

#Database Management Systems (DBMSs) 
#data storage and access considerations

#in order to use the database 
#connecting to the database
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")

#most databases live on another server, so in real life your code will look like this
#do not run

#con <- DBI::dbConnect(RMySQL::MySQL(), 
#                      host = "database.lexcomber.com",
#                      user = "lexcomber",
#                      password = lexcomberapi::askForPassword("Database password")
#)

#our temporary database has no data in, so we will load some prescription data
load("prescript.RData")

head(prescript)

#copy the database to con by the copy_to function
#makes only these four variables available
copy_to(con, prescript, "pr",
        temporary = FALSE, 
        indexes = list(
          "PCT", 
          "PRACTICE", 
          "BNF.CODE",
          "FAM")
)

#create a table to take a reference to the data
pr_db <- tbl(con, "pr")
pr_db 

#this dataset exists in computer's memory REMOTELY
#it does not behave like a data table in R's memory
dim(pr_db)
pr_db[,1]
summary(pr_db)

#to interact with a database, you would usually use SQL
#the goal of dbplyr is to automatically generate SQL for us so we are not forced to use it

# selecting variables
pr_db %>% select(PCT:BNF.CODE, ACT.COST)

# filtering for matching variable values 
pr_db %>% filter(PCT == "02W")

# grouping and summarising
pr_db %>% 
  group_by(PCT) %>%
  summarise(tot_cost = sum(ACT.COST, na.rm = T)) 

#working with remote databases in R:
#the R code is translated in SQL and none is executed in R,
#rather in the database

pct_db <- pr_db %>% 
  group_by(PCT) %>%
  summarise(
    mean_cost = mean(ACT.COST, na.rm = T),
    n = n()
  ) %>% 
  arrange(desc(mean_cost)) %>%
  filter(n > 100)
pct_db

#the translation from R to SQL happens behind the scenes
#in order to see the "translation" we do:

pct_db %>% show_query()

vignette("sql-translation")

#pull the data that you need 
pct_spend <- pct_db %>% collect()
pct_spend

#no way to determine how many rows a query will return 
#unless you actually run it,nrow() is always NA
nrow(pct_db)

#you can't find the last few rows without executing the 
#whole query, you can't use tail
tail(pct_db)

###
### http://gmmmg.nhs.uk/html/formulary_bnf_chapters.html
###
Drugs <- "0106"   ; Title="Laxatives"
Drugs <- "0205"   ; Title="Hypertension and Heart Failure"
Drugs <- "0206"   ; Title="Antianginal Drugs"
Drugs <- "0301"   ; Title="Bronchodilators"
Drugs <- "0307"   ; Title="Mucolytics"
Drugs <- "0401"   ; Title="Hypnotics and Anxiolytics"
Drugs <- "0403"   ; Title="Antidepressants"
Drugs <- "0405"   ; Title="Obesity"              # one NA
Drugs <- "0409"   ; Title="Dementia"
Drugs <- "0601"   ; Title="Diabetes"

#when you have chosen, assign one of these to match with the FAM attribute

Drugs <- "0403"; Title="Antidepressants"

#summarise over CCGs
pr_db %>% 
  group_by(PCT) 

#select the family of drugs we are interested in
pr_db %>% 
  group_by(PCT) %>%
  filter(FAM == Drugs) 

#add up the total costs of these for each CCG and the number of prescriptions
pr_db %>% 
group_by(PCT) %>%
  filter(FAM == Drugs) %>%
  summarise(
    tot_cost = sum(ACT.COST, na.rm = T),
    n = n()
  )

#extract the data from the remote server so that it can be linked to our map data

ccg_sum <- pr_db %>% 
  group_by(PCT) %>%
  filter(FAM == Drugs) %>%
  summarise(
    tot_cost = sum(ACT.COST, na.rm = T),
    n = n()
  )
ccg_sum <- ccg_sum %>% collect()

#load the map 
ccg <- readOGR("ccg_2017.shp")

# force factors to be numeric
ccg@data$ccg.reg.pa <- as.numeric(as.character(ccg@data$ccg.reg.pa))

# and do a quick map of population 
tm_shape(ccg)+
  tm_fill("ccg.reg.pa", title = "CCG patients")
#if we run this, we arrange the dataset by the total cost 
#that way later, there will be a different index when we match the data
ccg_sum<- ccg_sum %>%
  arrange(ccg_sum$tot_cost)

# match the data 
index <- match(ccg$CCGcode, ccg_sum$PCT) #finds the row in the second dataset that matches the value on the first dataset
ccg_sum <- ccg_sum[index, ]#assigns the index to the dataset, in this case is just alphabetical so it is 1-209
# then calculate the cost rate 
# and add to the ccg spatial data
ccg$cost_pp <- ccg_sum$tot_cost/ccg$ccg.reg.pa
# finally calculate the number 
ccg$n_pp <- ccg_sum$n/ccg$ccg.reg.pa

#map the results
tm_shape(ccg) +
  tm_fill(c("cost_pp", "n_pp"), 
          title = c("Cost per patient", "No per patient"),
          palette = "Blues", style = "kmeans")

### CREATE THE FUNCTION TO DO THE MAPS FOR EACH DRUG FAMILY

DrugRate <- function(Drugs, Title, palette) {
  #create the ccg_sum
  ccg_sum <- pr_db %>% 
    group_by(PCT) %>%
    filter(FAM == Drugs) %>%
    summarise(
      tot_cost = sum(ACT.COST, na.rm = T),
      n = n()
    )
  ccg_sum <- ccg_sum %>% collect()
  
  #load the map 
  ccg <- readOGR("ccg_2017.shp")
  
  # force factors to be numeric
  ccg@data$ccg.reg.pa <- as.numeric(as.character(ccg@data$ccg.reg.pa))
  
  # and do a quick map of population 
  tm_shape(ccg)+
    tm_fill("ccg.reg.pa", title = "CCG patients")
  #if we run this, we arrange the dataset by the total cost 
  #that way later, there will be a different index when we match the data
  ccg_sum<- ccg_sum %>%
    arrange(ccg_sum$tot_cost)
  
  # match the data 
  index <- match(ccg$CCGcode, ccg_sum$PCT) #finds the row in the second dataset that matches the value on the first dataset
  ccg_sum <- ccg_sum[index, ]#assigns the index to the dataset, in this case is just alphabetical so it is 1-209
  # then calculate the cost rate 
  # and add to the ccg spatial data
  ccg$cost_pp <- ccg_sum$tot_cost/ccg$ccg.reg.pa
  # finally calculate the number 
  ccg$n_pp <- ccg_sum$n/ccg$ccg.reg.pa
  
  #map the results
  map<- tm_shape(ccg) +
    tm_fill(c("cost_pp", "n_pp"), 
            title = c("Cost per patient", "Number per patient"),
            palette = palette, style = "kmeans")
  return(map)
}

DrugRate(Drugs = "0307", Title="Mucolytics", palette = "Blues")

DrugRate(Drugs = "0206", Title="Antianginal Drugs", palette = "Reds")

DrugRate(Drugs = "0403", Title="Antidepressants", palette = "Reds")

