
library(tidyverse)
library(forcats)

 getwd()


students <- read.csv("C:\\Users\\thiag\\Documents\\learning-r\\r_coursera\\practice-in-r\\StudentsPerformance.csv")

data(students)

colnames(students)

skim_without_charts(students)

glimpse(students)
 
view(students)


#PRA VALER

#race
ggplot(students, aes(x=race.ethnicity))+
    geom_bar()

#gender

ggplot(students, aes(x=gender))+
  geom_bar()
  

ggplot(students, aes(x = race.ethnicity, fill = gender)) +
  geom_bar(position = "dodge", color = "black", stat = "count")

#parental_education
ggplot(students)+
  aes(x = fct_rev(fct_infreq(parental.level.of.education)))+
  geom_bar()+
  coord_flip()


ggplot(data= students)+
  geom_point(mapping = aes(x=reading.score  , y=writing.score))

colnames(students)

View(students)
#math ethnicy

  
  
  library(ggplot2)
  
  mean_students <- students %>%
    group_by(race.ethnicity) %>%
    summarise(mean_math_score = mean(math.score),
              sd_math_score = sd(math.score))
  
    
  ggplot(mean_students, aes(x = race.ethnicity, y = mean_math_score)) +
    geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7)
  
  
  
  #per education
  colnames(students)
  mean_education_students <- students %>%
    group_by(parental.level.of.education) %>%
    summarise(mean_math_score = mean(math.score),
              sd_math_score = sd(math.score))
  
  
  ggplot(mean_education_students, aes(x =parental.level.of.education, y = mean_math_score)) +
    geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7)
  
  
  mean_read_students <- students %>%
    group_by(parental.level.of.education) %>%
    summarise(mean_read_score = mean(reading.score),
              sd_read_score = sd(reading.score))
  
  ggplot(mean_read_students, aes(x =parental.level.of.education, y=mean_read_score)) +
    geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7)
  
  
  mean_writing_students <- students %>%
    group_by(parental.level.of.education) %>%
    summarise(mean_writing_score = mean(writing.score),
              sd_writing_score = sd(writing.score))
  
  ggplot(mean_writing_students, aes(x =parental.level.of.education, y=mean_writing_score)) +
    geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7)
  
      
    