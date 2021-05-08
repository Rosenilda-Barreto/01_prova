#==============================
"Script de teste das questões 01, 02, 03 e 04"
"Rosenilda Pereira Barreto"
"rosenildararidade123@gmail.com"
#==============================

# carregando o tidyverse

library(tidyverse)


# Lendo os dados

peixes_rio_madeira <- read_csv("prova/dados/brutos/peixes_rio_madeira.csv")
peixes_rio_madeira%>%
  glimpse()
peixes_rio_madeira%>%view()

#Obtendo a tabela de frequência em ordem crescente da variável ordem

peixes_rio_madeira%>%
  count(ordem)%>%
  arrange(n)

#q.2 Obtendo média, desvio padrão e coeficiente de variação da varíavel peso referente Rio Guaporé

peixes_rio_madeira%>%
  select(bacia, ordem, peso_g)%>%
  filter(bacia == "Rio Guaporé")%>%
  group_by(ordem)%>%
  summarise(
    media_peso = mean(peso_g,na.rm = TRUE),
    desvio_peso = sd(peso_g,na.rm = TRUE),
    coeficiente_variacao_peso = ((desvio_peso/media_peso)*100)
  )
# q.3 Observando as categorias e respectivas frequências absolutas da variável sexo

peixes_rio_madeira%>%
  group_by(sexo)%>%
  summarise(
    n = n()
  )

#Obtendo a frequência relativa de Machos e Fêmeas, fazendo fêmea igual a Fêmea

peixes_rio_madeira%>%
mutate(
sexo_recode = recode(
      sexo,
"Fêmea" = "Fêmea",
"fêmea" = "Fêmea",
"Macho" = "Macho"
    )
    )%>%
  count(sexo_recode)%>%
  summarise(
    sexo_recode = sexo_recode,
    frequencia = ((n/49800)*100)
  )

#Comparando o peixe de maior peso ok
peixes_rio_madeira%>%
  group_by(peso_g)%>%
  mutate(
    sexo_recode = recode(
            sexo,
      "Fêmea" = "Fêmea",
      "fêmea" = "Fêmea",
      "Macho" = "Macho"
    )
  )%>%
  count(sexo_recode)%>%
  summarise(
    sexo_recode = sexo_recode,
    frequencia = ((n/49800)*100)
  )%>%
  arrange(desc(peso_g))%>%
  filter(sexo_recode == "Macho" | sexo_recode == "Fêmea")

#q.4 frequência do hábito alimentar por sexo ok

  peixes_rio_madeira%>%
    group_by(sexo, habito_alimentar)%>%
    count(habito_alimentar)%>%
    summarise(
      habito_alimentar = habito_alimentar,
      frequencia = n,
      sexo = sexo
    )


