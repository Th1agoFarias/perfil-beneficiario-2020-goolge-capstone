install.packages("tidyverse")


library(tidyverse)
dat <- read.csv("ProuniRelatorio2020.csv")
library(viridis)


View(dat)
glimpse(dat)
str(data)
head(dat)

colnames(dat)

dat <- rename(dat,
              instituicao = NOME_IES_BOLSA,
              modalidade = MODALIDADE,
              curso = NOME_CURSO_BOLSA,
              turno = TURNO,
              sexo = SEXO_BENEFICIARIO,
              raca = RACA_BENEFICIARIO,
              data_nasc = DATA_NASCIMENTO,
              regiao = REGIAO_BENEFICIARIO,
              uf = UF_BENEFICIARIO,
              municipio = MUNICIPIO_BENEFICIARIO,
              tipos_bolsa= TIPO_BOLSA
)


names(dat)


dat$data_nasc <- as.Date(dat$dat_nasc, "%d/%m/%Y")

str(dat)
view(dat)


raca <- as.factor(dat$raca)
sexo <- as.factor(dat$sexo)
curso <- as.factor(dat$curso)


dat$curso <- tolower(dat$curso)

# turno por curso

count_cursos <- dat %>% 
  group_by(turno) %>% 
  summarise(curso_unicos = n_distinct(curso))
count_cursos <- count_cursos %>%
  mutate(turno = factor(turno, levels = unique(turno[order(curso_unicos, decreasing = FALSE)])))
 
 turno +scale_fill_manual(values = hcl.colors(n = 5,
                                              palette = "Dark 2"))
 

 
 
turno<- ggplot(count_cursos, aes(x = curso_unicos, y = turno, fill = turno)) +
   geom_bar(stat = "identity", position = "dodge", width = 0.6) + 
   theme_minimal() +
   labs(
     title = "Distribuição de Beneficiários por Turno") +
   theme(
     axis.text.x = element_text(angle = 0, hjust = 1),
     legend.position = "none"
   )+
   theme(
     axis.title.x = element_blank(),
     axis.title.y = element_blank()
   )
 
library(knitr)


#urno mais comun
kable(count_cursos, caption = "Contagem de Cursos")


#modalidade por tipo de bolsa

modalidade_bolsa<-ggplot(dat, aes(x = modalidade, fill = tipos_bolsa)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribuição de Tipo de Bolsa por Modalidade",
    x = "Modalidade",
    fill = "Tipo de Bolsa"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_blank())

modalidade_bolsa +scale_fill_manual(values = hcl.colors(n = 4,
                                               palette = "YlGnBu"))





#cursos mais frequentes
top_cursos <- dat %>%
  count(curso) %>%
  top_n(10, n)



cursos_frequentes +scale_fill_manual(values = hcl.colors(n = 10,
                                                        palette = "Emrld"))


top_cursos <- top_cursos %>%
  mutate(curso = factor(curso, levels = curso[order(n, decreasing = TRUE)]))

cursos_frequentes <- ggplot(top_cursos, aes(x = curso, y = n, fill = curso)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Cursos mais Frequentes",
    x = "Curso"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank()
  ) +
  scale_fill_manual(values = hcl.colors(n = 10, palette = "dark2"))+ guides(fill= "none")




   
#distribuiçao por uf

ggplot(dat, aes(x = uf, fill = uf)) +
  geom_histogram(stat = "count", position = "dodge") +  # Definindo a cor da borda das barras
  theme_minimal() +
  labs(
    title = "Concentração de Beneficiário por Estado",
    x = "Estado"
  ) +
  theme(axis.title.y = element_blank()) +
  scale_fill_manual(values = hcl.colors(n = 28, palette = "Temps"))+  guides(fill = "none") 


#distribuiçao por região 


ggplot(dat) +
  aes(x = fct_rev(fct_infreq(regiao)), fill = regiao) +  # Adicionando fill = regiao
  geom_bar(width = 0.5, position = "dodge") +
  coord_flip() +
  labs(
    title = "Beneficiários por Região",
    x = "Região",
    y = ""
  ) +
  theme_minimal() +
  scale_fill_manual(values = hcl.colors(n = 5, palette = "Temps"))+guides(fill= "none")


#distribuição por raça
raca_e_cursos <- dat %>% 
  group_by(raca) %>% 
  summarise(curso_unicos = n_distinct(curso))


raca_por_curso<-ggplot(raca_e_cursos, aes(x = raca, y = curso_unicos, fill = raca)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(
    title = "Número de Cursos Únicos por Raça",
    x = "Raça",
    y = "Número de Cursos Únicos",
    fill = "Raça"
  ) +
  theme_minimal()+
  theme(
    axis.title.y = element_blank()
  )+guides(fill= "none")


raca_por_curso +scale_fill_manual(values = hcl.colors(n = 6,
                                                       palette = "Temps"))




#distruibuição por sexo
sexo_e_cursos <- dat %>% 
  group_by(sexo) %>% 
  summarise(curso_unicos = n_distinct(curso))



sexo_por_curso<-ggplot(sexo_e_cursos, aes(x = sexo, y = curso_unicos, fill = sexo)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(
    title = "Distribuição por Curso e Sexo",
    x = "Sexo",
    y = "Número de Cursos Únicos"
  ) + 
  theme_minimal()+
  theme(
    axis.title.y = element_blank()
  )



sexo_por_curso +scale_fill_manual(values = hcl.colors(n = 2,
                                                      palette = "Geyser"))


top_cursos_sexo <- dat %>%
  filter(curso %in% top_cursos$curso) %>%
  group_by(curso, sexo) %>%
  summarise(n = n()) %>%
  arrange(curso, sexo) %>%
  mutate(sexo = factor(sexo, levels = c("M", "F")))


labels_1 <- ggplot(top_cursos_sexo, aes(fill = sexo, y = curso, x = n)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "Contagem de Cursos por Sexo",
       x = "Proporção",
       y = "Curso") +
  theme_minimal()

# Adicionando rótulos de dados
labels_1 + geom_text(aes(label = n),                  
              position = position_fill(vjust = 0.5), 
              
              color = "black",                      
              size = 3)  


top_cursos_raca <- dat %>%
  filter(curso %in% top_cursos$curso) %>%
  group_by(curso, raca) %>%
  summarise(n = n(), na.rm = TRUE) %>%  # Remover valores NA
  arrange(curso, raca) %>%
  mutate(raca = factor(raca, levels = c("Amarela", "Branca", "Ind¡gena", "Não Informada", "Parda", "Preta")))




labels <- ggplot(top_cursos_raca, aes(fill = raca, y = curso, x = n)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "Contagem de Cursos por Sexo",
       x = "Proporção",
       y = "Curso") +
  theme_minimal()+
  

# Adicionando rótulos de dados com proporção ajustada
labels + geom_text(aes(label = scales::percent(n, accuracy = 0.01)),  # Convertendo para porcentagem com precisão de 0.01
                   position = position_fill(vjust = 0.5), 
                   color = "black",                      
                   size = 3)  # Tamanho dos rótulos


unique_racas <- select(dat, unique(raca))
  print(unique_racas)
