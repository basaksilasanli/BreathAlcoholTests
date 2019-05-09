library(tidyverse) 
library(readr)

#import dataset
breath_alcohol_ames <- read_csv("Desktop/datasets/breath_alcohol_ames.csv") 

View(breath_alcohol_ames)
head(breath_alcohol_ames) #verimizin ilk altı satırını görüyoruz

ba_year <- count(breath_alcohol_ames,year) #her yıl kaç test yapıldığını inceledik

pds <- count(breath_alcohol_ames, location) #departmanların kaç alkol testi yaptığını inceledik

hourly <- count(breath_alcohol_ames, hour) #saatlere göre alkol testlerini saydık
ggplot(hourly, aes(x=hour, weight = n)) + geom_bar()

monthly <- count(breath_alcohol_ames, month) # aylara göre 
monthly$month <- as.factor(monthly$month)
ggplot(monthly, aes(x=month, weight=n)) + geom_bar()

gender <- breath_alcohol_ames %>% count(gender) #cinsiyete göre
clean_gender <- breath_alcohol_ames %>% filter(!is.na(gender)) %>% mutate(meanRes = (Res1 + Res2) / 2) # create a dataset with no NAs in gender and create a mean test result variable 
ggplot(clean_gender, aes(x=gender, weight = meanRes)) + geom_bar()

duis <- breath_alcohol_ames %>% filter(Res1 > 0.08 | Res2 > 0.08) # Filter the data
glimpse(duis)
p_dui <- nrow(duis) / nrow(breath_alcohol_ames) # DUI ile sonuçlanacak testlerin oranı

breath_alcohol_ames <- breath_alcohol_ames %>% mutate(date = ymd(paste(year,month,day,sep='-')))
glimpse(breath_alcohol_ames)

weekly <- count(breath_alcohol_ames, week, year) 
weekly <- weekly %>% ungroup() 
weekly <- weekly %>% mutate(year = as.factor(year)) 
ggplot(weekly, aes(x = week, n)) + 
  geom_line() + 
  geom_point(aes(color = year)) +  # included to make the plot more readable 
  scale_x_continuous(breaks = seq(0,52,2))  # to make the x-axis more readable 
