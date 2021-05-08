#==============================
"Script de teste das questões 05 e 06"
"Rosenilda Pereira Barreto"
"rosenildararidade123@gmail.com"
#==============================

# carregando o tidyverse

library(tidyverse)


# Lendo os dados

library(readr)
contracheque <- read_csv("prova/dados/brutos/contracheque.csv",
                         col_types = cols(rendimento_liquido = col_number()))
View(contracheque)

#q.5 Ordenando de forma crescente os valores da variável rendimento_liquido

contracheque%>%
  select(rendimento_liquido)%>%
  arrange(desc(rendimento_liquido))

#q.6 Descobrindo quantos magistrados recebem acima de 39293,32 reais ok

contracheque%>%
  select(rendimento_liquido)%>%
  filter(rendimento_liquido > 39293.32)%>%
count()

#q.6 recebem acima de 100000 reais
contracheque%>%
  select(rendimento_liquido)%>%
  filter(rendimento_liquido > 100000)%>%
  count()

#q.6 letra b
contracheque%>%
  select(rendimento_liquido, tribunal)%>%
  group_by(tribunal)%>%
  summarise(
    media_rendimento = mean(rendimento_liquido, na.rm = TRUE),
    desvio = sd(rendimento_liquido, na.rm = TRUE),
    cv = ((desvio/media_rendimento)*100)
  )%>%
  arrange(desc(cv))


