students <- read.csv("StudentsPerformance.csv")


library(tidyverse)

head(students)

str(students)

colnames(students)

glimpse(students)

getwd()


students <- students %>%
  rename(race_ethnicity =race.ethnicity ,
         education= parental.level.of.education,
         preparation_course = test.preparation.course ,
         math_score = math.score,
         writing_score = writing.score,
         reading_score = reading.score
         )

  
names(students)

view(students)

#etnia
ggplot(students, aes(x=race_ethnicity))+
  geom_bar(width= 0.8, position = "dodge",  fill="#69b3a2") +
  labs(
    title = "Distribuição de Raça/Etnia dos Estudantes",
    x= "Etnia",
    y= "Frequency"
  )+
  theme_minimal()

#genero
ggplot(students, aes(x=gender))+
  geom_bar(width= 0.5, position = "dodge",  fill="#69b3a2") +
  labs(
    title = "Distribuição de Raça/Etnia dos Estudantes",
     x= "Gender",
    y= "Frequency"
  )+
  theme_minimal()
  
#preparation course
ggplot(students, aes(x=preparation_course))+
  geom_bar(width= 0.5, position = "dodge",  fill="#69b3a2") +
  labs(
    title = "Comparação da Matrícula em Curso Preparatório",
     x= "Preparation Course",
    y= "Frequency"
  )+
  theme_minimal()


#etnia e genero
ggplot(students, aes(x = race_ethnicity, fill = gender)) +
  geom_bar(position = "dodge", stat = "count") +
  geom_bar() +
  labs(
    x= "Etnia"
  )+
  theme_minimal()

# nivel de educação
ggplot(students)+
  aes(x = fct_rev(fct_infreq(education)))+
  geom_bar(width= 0.7, position = "dodge",  fill="#69b3a2") +
  coord_flip() +
  labs(
    title = "Parental Leve of Education",
    x= "Frequency",
    y= "Education"
  )+
  theme_minimal()






#relação entre leitura e escrita
ggplot(data= students, aes(x=reading_score, y=writing_score))+
  geom_point(color= "blue", alpha= 0.5, size = 4) +
  labs(x = "Pontuação de Leitura", y = "Pontuação de Escrita",
        title = "Relação entre Pontuação de Leitura e Escrita dos Alunos")




#mat e entinia
mean_students <- students %>%
  group_by(race_ethnicity) %>%
  summarise(mean_math_score = mean(math_score),
            sd_math_score = sd(math_score))


ggplot(mean_students, aes(x = race_ethnicity, y = mean_math_score)) +
  geom_bar(stat = "identity", fill = "#69b3a2", alpha = 0.7, width = 0.8) +
  geom_errorbar(aes(ymin = mean_math_score - sd_math_score, ymax = mean_math_score + sd_math_score), width = 0.5) +
  labs(x = "Raça/Etnia", y = "Média da Pontuação de Matemática", title = "Média da Pontuação de Matemática por Raça/Etnia")


#read e etinia
mean_students_read <- students %>%
  group_by(race_ethnicity) %>%
  summarise(mean_read_score = mean(reading_score),
            sd_read_score = sd(reading_score))


ggplot(mean_students_read, aes(x = race_ethnicity, y = mean_read_score)) +
  geom_bar(stat = "identity", fill = "#69b3a2", alpha = 0.7, width = 0.8) +
  geom_errorbar(aes(ymin = mean_read_score - sd_read_score, ymax = mean_read_score + sd_read_score), width = 0.5) +
  labs(x = "Raça/Etnia", y = "Média da Pontuação de Leitua", title = "Média da Pontuação de Leitura por Raça/Etnia")




#write e etnia
mean_students_write <- students %>%
  group_by(race_ethnicity) %>%
  summarise(mean_write_score = mean(writing_score),
            sd_write_score = sd(writing_score))


ggplot(mean_students_write, aes(x = race_ethnicity, y = mean_write_score)) +
  geom_bar(stat = "identity", fill = "#69b3a2", alpha = 0.7, width = 0.8) +
  geom_errorbar(aes(ymin = mean_write_score - sd_write_score, ymax = mean_write_score + sd_write_score), width = 0.5) +
  labs(x = "Raça/Etnia", y = "Média da Pontuação de Escrita", title = "Média da Pontuação de Escrita por Raça/Etnia")


ggplot(mean_students_write, aes(x = race_ethnicity, y = mean_write_score)) +
  geom_bar(stat = "identity", fill = "#69b3a2", alpha = 0.7)



  #per education
  
  
  #math education
  mean_education_math <- students %>%
    group_by(education) %>%
    summarise(mean_math_score = mean(math_score),
              sd_math_score = sd(math_score))
  
  
  
  ggplot(mean_education_math, aes(x = education, y = mean_math_score)) +
    geom_bar(stat = "identity", fill = "#69b3a2", alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_math_score - sd_math_score, ymax = mean_math_score + sd_math_score), 
                  width = 0.2, 
                  position = position_dodge(0.9)) +
    labs(x = "Nível de Educação", y = "Média da Pontuação de Matemática", title = "Média da Pontuação de Matemática por Nível de Educação")
  
  #rad educatin
  mean_education_read <- students %>%
    group_by(education) %>%
    summarise(mean_read_score = mean(reading_score),
              sd_read_score = sd(reading_score))
  
  
  ggplot(mean_education_read, aes(x = education, y = mean_read_score)) +
    geom_bar(stat = "identity", fill = "#69b3a2", alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_read_score - sd_read_score, ymax = mean_read_score +  sd_read_score), 
                  width = 0.2, 
                  position = position_dodge(0.9)) +
    labs(x = "Nível de Educação", y = "Média da Pontuação de Leituta", title = "Média da Pontuação de Leitura por Nível de Educação")
  
  
  
  mean_education_write <- students %>%
    group_by(education) %>%
    summarise(mean_writing_score = mean(writing_score),
              sd_writing_score = sd(writing_score))
  
  
  ggplot(mean_education_write, aes(x = education, y = mean_writing_score)) +
    geom_bar(stat = "identity",  position = position_dodge(),fill = "#69b3a2", alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_writing_score - sd_writing_score, ymax = mean_writing_score + sd_writing_score),
                  width = 0.7,
                  position = position_dodge(0.5)) +
    labs(x = "Nível de Educação", y = "Média da Pontuação de Escrita", title = "Média da Pontuação de Escrita por Nível de Educação")

