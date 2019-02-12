library(tidyverse)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>% 
  count(cut)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat, 0.5))

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

unusual <- diamonds %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual
View(unusual)

#distribution of x
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = x), binwidth = 0.5)

#distribution of y
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

#distribution of z
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = z), binwidth = 0.5)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = price), binwidth = 1000)



diamonds %>%
  filter(carat >= 0.99, carat <= 1) %>%
  count(carat)


diamonds %>%
  mutate(id = row_number()) %>%
  select(x, y, z, id) %>%
  gather(variable, value, -id) %>%
  ggplot(aes(x = value)) +
  geom_density() +
  geom_rug() +
  facet_grid(variable ~ .)

#improve the visualisation of the departure times of cancelled vs not cancelled
nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  
  ggplot() +
  geom_boxplot(mapping = aes(y = sched_dep_time, x = cancelled))

#important variable for predicting the price of variable
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()
