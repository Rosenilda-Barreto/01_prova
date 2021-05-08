#==============================
"Script de teste das questões 07, 08 e 09"
"Rosenilda Pereira Barreto"
"rosenildararidade123@gmail.com"
#==============================

# carregando o tidyverse

library(tidyverse)


# Lendo os dados
library(readr)
cursos_prouni <- read_csv("prova/dados/brutos/cursos-prouni.csv")
View(cursos_prouni)

#q.7b
cursos_prouni%>%
  group_by(turno)%>%
  summarise(
    media_nota = mean(nota_integral_ampla, na.rm = TRUE),
    mediana_nota = median(nota_integral_ampla, na.rm = TRUE)
  )
#q.comparando a homogeneidade dos turnos por meio do coeficiente de variação(cv)

cursos_prouni%>%
  select(turno, nota_integral_ampla)%>%
  group_by(turno)%>%
  summarise(
    media_nota = mean(nota_integral_ampla, na.rm = TRUE),
    desvio_nota = sd(nota_integral_ampla, na.rm = TRUE),
    cv = ((desvio_nota/media_nota)*100)
    )
arrange(desc(cv))

#posição do estado baiano quanto a variável uf_busca

cursos_prouni%>%
  group_by(uf_busca)%>%
  count()%>%
  arrange(desc(n))

#q.9 Otendo quantos tipos de cursos há na variável curso_busca

cursos_prouni%>%
  distinct(nome)

#NA total em cada coluna do dataset

summary(nome)


