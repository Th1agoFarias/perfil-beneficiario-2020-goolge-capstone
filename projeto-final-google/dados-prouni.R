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
              modalidade = MODALIDADE_ENSINO_BOLSA,
              curso = NOME_CURSO_BOLSA,
              turno = NOME_TURNO_CURSO_BOLSA,
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
 
 turno +scale_fill_manual(values = hcl.colors(n = 5,
                                              palette = "harmonic"))
 
turno<- ggplot(count_cursos, aes(x = curso_unicos, y = turno, fill = turno)) +
   geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
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
 

#turno 

#modalidade por tipo de bolsa


modalidade_bolsa<-ggplot(dat, aes(x = modalidade, fill = tipos_bolsa)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = cores) +
  labs(
    title = "Distribuição de Tipo de Bolsa por Modalidade",
    x = "Modalidade",
    fill = "Tipo de Bolsa"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_blank())

modalidade_bolsa +scale_fill_manual(values = hcl.colors(n = 4,
                                               palette = "harmonic"))


#cursos mais frequentes
top_cursos <- dat %>%
  count(curso) %>%
  top_n(10, n)

cursos_frequentes<-ggplot(top_cursos, aes(x = reorder(curso, n), y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Cursos mais Frequentes",
       x = "Curso",
       ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
    axis.title.y = element_blank()
  )


cursos_frequentes +scale_fill_manual(values = hcl.colors(n = 10,
                                                        palette = "harmonic"))



cursos_frequentes <- ggplot(top_cursos, aes(x = reorder(curso, n), y = n, fill = curso)) +
  geom_bar(stat = "identity") +
  labs(title = "Cursos mais Frequentes",
       x = "Curso") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = c("red", "blue", "green", "yellow", "orange"))  # Exemplo de cores personalizadas



   
#distribuiçao por uf


uf<-ggplot(dat, aes(x=uf)) +
  geom_bar(stat = "count", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Concentração de Beneficiário por Estado",
    x = "Estado"
  ) +
  theme(axis.title.y = element_blank())

regiao +scale_fill_manual(values = hcl.colors(n = 28,
                                                         palette = "TealRose"))



uf <- ggplot(dat, aes(x = uf, fill=uf)) +
  geom_bar(stat = "count", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Concentração de Beneficiário por Estado",
    x = "Estado"
  ) +
  theme(axis.title.y = element_blank())

uf + scale_fill_manual(values = hcl.colors(n = 28, palette = "Dark2"))


  


#distribuiçao por região 

regiao_por_cuso<-ggplot(dat)+
  aes(x = fct_rev(fct_infreq(regiao)))+
  geom_bar(width= 0.7, position = "dodge") +
  scale_fill_manual(values = pastel) + 
  coord_flip() +
  labs(
    title = "Beneficiários por Região",
    x = "Região",
    y = ""
  ) +
  theme_minimal()

regiao_por_cuso +scale_fill_manual(values = hcl.colors(n = 5,
                                              palette = "Dark2"))


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
  )




raca_por_curso +scale_fill_manual(values = hcl.colors(n = 6,
                                                       palette = "Dark2"))

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
                                                      palette = "Dark2"))

