library(tidyverse)
library(dplyr)
library(plotly)
library(gapminder)
setwd("C:\\Users\\sophc\\OneDrive\\Desktop\\Data 151\\data science project 1")
ds = read_csv("pokemon.csv")
ag = ds[,c(2:18, 31)]
#dividing by the max of each collumn for attack, defense, sp, and speed we can see who scores the highest overall

#by adding up each Pokemon's abilities against abilities we can see which pokemon are the most well rounded
#to get the data set of that column
TV <- ag %>% mutate(total = rowSums(select_if(., is.numeric)))
# to find the top 6 of these pokemon
head(sort(points_TB$agt, decreasing = TRUE), n = 6)
stat = ds[,c(29, 34:36)]
hp_points <- stat$hp/255
speed_points <- stat$speed/180
attack_points <- stat$sp_attack/194
defense_points <- stat$sp_defense/230
agt <- TV[,18:19]
points_TB <- data.frame(hp_points, speed_points, attack_points, defense_points, agt)
maxag = max(points_TB$agt)

points_TB$agt <- sort(points_TB$agt, decreasing = TRUE)


#-------------------------------------
  #pt 2
#to get only the atk, def,speed, hp, name
stat = ds[,c(20,31, 26, 36)] 
norm = ds %>% mutate(attack.norm = attack/max(attack), speed.norm = speed/max(speed), defense.norm = defense/max(defense), totalStat = attack.norm+speed.norm+defense.norm)
ag = ds[,c(2:18, 31)]
TV <- ag %>% mutate(total = rowSums(select_if(., is.numeric)))
ds2 = cbind(norm, total = TV$total)

ds3 = ds2 %>% mutate(totaldist = sqrt(totalStat^2 + total^2))
ds3 %>% arrange(desc(totaldist)) %>% head(6) %>% select(name, totalStat, total)
pipe= ds3 %>% arrange(desc(totalStat)) %>% head(6) %>% select(name, totalStat, total)
pipeb= ds3 %>% arrange(totalStat) %>% head(6) %>% select(name, totalStat, total)
pkm <- rbind(pipe, pipeb)
pkmGraph <- ggplot(pkm, aes(x = totalStat, y = total)) +geom_point(aes(color = name)) + xlab("Total Stats") + ylab("Amount of advantages") + ggtitle("Highest and Lowest 6 Pokemon")
ggplotly(pkmGraph)

