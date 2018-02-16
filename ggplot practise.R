#-----------------------
library(ggplot2)
library(dplyr)
library(mgcv)
library(MASS)
library(directlabels)
library(nlme)
library(babynames)
library(ggmap)
library(tidyverse)
library(nycflights13)
library(XML)
library(RCurl)
library(htmltab)
library(Lahman)
library(hexbin)
library(modelr)
library(maps)
library(mapdata)
library(devtools)
library(ggmap)
library(stringr)
library(plyr)
library(pryr)
devtools::install_github("dkahle/ggmap")
#----------------------

# Read files------
drivers <- read.csv("drivers.csv", T, ",")
results <- read.csv("results.csv", T, ",")
races <- read.csv("races.csv", T, ",")

# Join Files-------
results1 <- left_join(results, drivers, by = "driverId")
results2 <- left_join(results1, races, by = "raceId")

# Sub join files------
(yr2009 <- results2 %>%
  group_by(driverId) %>%
  filter(year == 2009) %>%
  mutate(total_points = cumsum(points)) %>%
  filter(raceId == 17) %>%
  ggplot(aes(reorder(surname, total_points), total_points, fill = total_points)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60),
                                      plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = total_points), vjust = -0.2) +
  labs(x = "Driver", y = "Total Points", fill = "Total Points",
       title = "2009 Drivers Championship"))

results4 <- results2 %>%
  group_by(driverId) %>%
  filter(year == 2007) %>%
  mutate(total_points = cumsum(points)) %>%
  filter(round == 17)

driver_points <- results1 %>%
  group_by(driverId) %>%
  mutate(total_points = cumsum(points)) %>%
  filter(total_points == max(total_points), total_points != 0, total_points > 1000) %>%
  arrange(total_points, driverId)


driver_points <- results1 %>%
  group_by(driverId) %>%
  mutate(total_points = cumsum(points),
         the_rank = rank(-total_points, ties.method = "random")) %>%
  filter(the_rank == 1) %>% select(-the_rank)

# Graphics---------
# 2009 WDC
ggplot(results3, aes(reorder(surname, total_points), 
                     total_points, fill = total_points)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60),
        plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = total_points), vjust = -0.2) +
  labs(x = "Driver", y = "Total Points", fill = "Total Points",
       title = "2009 Drivers Championship")


# 2007 WDC
ggplot(results4, aes(reorder(surname, total_points), 
                     total_points, fill = total_points)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", y = "Total Points", 
       fill = "Total Points", title = "2007 WDC") +
  geom_text(aes(label = total_points), vjust = -0.2)

# Career Points
ggplot(driver_points, aes(reorder(surname, total_points), 
                          total_points, fill = total_points)) + 
         geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", y = "Total Points", title = "Career Points", 
       fill = "Total Points") +
  geom_text(aes(label = total_points), vjust = -0.2)

# View Files-----------
View(results1)
View(results2)
View(results3)
View(results4)
View(driver_points)



set.seed(1)
df <- expand.grid(list(A = 1:5, B = 1:5, C = 1:5))
df$value <- runif(nrow(df))
ddply(df, .(A, B), function(x) x[which.max(x$value),])

df %>%
  group_by(A, B) %>%
  summarise(max = max(value))

result <- df %>%
  group_by(A, B) %>%
  filter(value == max(value)) %>%
  arrange(A, B, C)
result

identical(as.data.frame(result),
          ddply(df, .(A, B), function(x) x[which.max(x$value),]))
df %>% group_by(A, B) %>% top_n(n = 1)

df %>% group_by(A, B) %>% slice(which.max(value))

df %>% group_by(A, B) %>%
  mutate(the_rank = rank(-value, ties.method = "random")) %>%
  filter(the_rank == 1) %>% select(-the_rank)


