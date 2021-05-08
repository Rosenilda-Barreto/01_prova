prova\_01
================
Rosenilda Pereira Barreto</br>
Introdução a Estatística 2020.1

**Questão 01**

**(a)** Fiz a distribuição de frequência da variável ordem em ordem
crecente usando o seguinte código:

``` r
peixes_rio_madeira <- read_csv("dados/brutos/peixes_rio_madeira.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_character(),
    ##   id = col_double(),
    ##   data = col_datetime(format = ""),
    ##   mes = col_double(),
    ##   ano = col_double(),
    ##   local = col_double(),
    ##   cp_cm = col_double(),
    ##   peso_g = col_double()
    ## )
    ## i Use `spec()` for the full column specifications.

``` r
peixes_rio_madeira %>% 
  count(ordem) %>% 
  arrange(n)
```

    ## # A tibble: 12 x 2
    ##    ordem                  n
    ##    <chr>              <int>
    ##  1 Lepidosireniformes     2
    ##  2 Pleuronectiformes      2
    ##  3 Beloniformes           5
    ##  4 Não identificado      17
    ##  5 Myliobatiformes       41
    ##  6 Osteoglossiformes    433
    ##  7 Gymnotiformes        693
    ##  8 Acanthuriformes     1602
    ##  9 Cichliformes        1947
    ## 10 Clupeiformes        2821
    ## 11 Siluriformes       27451
    ## 12 Characiformes      64356

**(b)** Observando a tabela de frequência criada no item (a), a maioria
dos peixes pertencem a ordem Characiformes, ordem que totaliza 64356
peixes.

**(c)** Observando a tabela de frequência do item (a), 17 peixes não
foram identificados na variável ordem.

**Questão 02** Fiz uma tabela de frequência relacionando a média, o
desvio padrão e o coeficiente de variação para simultaneamente obter as
médias, compará-las e observar se é ou não o mais adequado usar o
coeficiente de variação para comparar a variabilidade. Segue o código
que usei:

``` r
peixes_rio_madeira%>%
  select(bacia, ordem, peso_g)%>%
  filter(bacia == "Rio Guaporé")%>%
  group_by(ordem)%>%
  summarise(
    media_peso = mean(peso_g,na.rm = TRUE),
    desvio_peso = sd(peso_g,na.rm = TRUE),
    coeficiente_variacao_peso = ((desvio_peso/media_peso)*100)
  )
```

    ## # A tibble: 7 x 4
    ##   ordem           media_peso desvio_peso coeficiente_variacao_peso
    ##   <chr>                <dbl>       <dbl>                     <dbl>
    ## 1 Acanthuriformes      198         250.                      126. 
    ## 2 Characiformes         65.8       106.                      162. 
    ## 3 Cichliformes         179.        242.                      136. 
    ## 4 Clupeiformes         300.        235.                       78.2
    ## 5 Gymnotiformes         80.6        79.8                      99.0
    ## 6 Myliobatiformes      NaN          NA                        NA  
    ## 7 Siluriformes          92.3       206.                      223.

**(a)** Observando a tabela de frequência criada, como as médias de cada
categoria da variável ordem são distintas,o coeficiente de variação é a
melhor forma para avaliar a variabilidade da variável ordem.

**(b)** Como os peixes Clupeiformes possuem menor coeficiente de
variação (78.2), então esses são peixes de distribuição do peso mais
homogênea.

**Questão 03** Para recodificar a variável sexo por sexo\_recode,
primeiro observei quais são todas as categorias da variável sexo. Para
tanto,usei o código:

``` r
peixes_rio_madeira%>%
  group_by(sexo)%>%
  summarise(
    n = n()
  )
```

    ## # A tibble: 4 x 2
    ##   sexo             n
    ##   <chr>        <int>
    ## 1 fêmea            3
    ## 2 Fêmea        28328
    ## 3 Macho        21469
    ## 4 Não coletado 49570

Assim, foi preciso igualar as categorias Fêmea e fêmea para fazer a
recodificação. Usei, então, esse código, onde para além de fazer a
recodificação fornece as frequências percentual de todas as categorias
Fêmea, Macho e Não coletado:

``` r
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
```

    ## # A tibble: 3 x 2
    ##   sexo_recode  frequencia
    ##   <chr>             <dbl>
    ## 1 Fêmea              56.9
    ## 2 Macho              43.1
    ## 3 Não coletado       99.5

**(a)** Fazendo a diferença entre a frequência percentual entre Fêmea e
Macho
56.9 − 43.1
, que dá
13.8
, é preciso que haja um aumento percentual de
13.8
no total da categoria Machos para igualar a quantidade percentual de
Machos e Fêmeas.

**(b)** Para saber qual o sexo de peixe que apresenta o maior peso, usei
o código a seguir, onde a ordem do peso\_g aparece de forma decrescente.
Rodando o código, percebe-se que o peixe de maior peso é um peixe fêmea.

``` r
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
```

    ## `summarise()` has grouped output by 'peso_g'. You can override using the `.groups` argument.

    ## # A tibble: 2,283 x 3
    ## # Groups:   peso_g [1,471]
    ##    peso_g sexo_recode frequencia
    ##     <dbl> <chr>            <dbl>
    ##  1  14600 Fêmea          0.00201
    ##  2  12405 Fêmea          0.00201
    ##  3  11600 Fêmea          0.00201
    ##  4  11230 Fêmea          0.00201
    ##  5  10770 Fêmea          0.00201
    ##  6  10545 Fêmea          0.00201
    ##  7  10030 Fêmea          0.00201
    ##  8   9696 Fêmea          0.00201
    ##  9   9200 Fêmea          0.00201
    ## 10   9110 Fêmea          0.00201
    ## # ... with 2,273 more rows

**Questão 04** Para determinar qual o sexo correspondente a maior
quantidade de peixes são carnívoros, fiz uma tabela de frequência, onde
é possível observar a quantidade de peixes de cada categoria da variável
sexo que possue cada tipo hábito alimentar. Rodando o código que segue,
8552
peixes machos são carnívoros.

``` r
peixes_rio_madeira%>%
    group_by(sexo, habito_alimentar)%>%
    count(habito_alimentar)%>%
    summarise(
      habito_alimentar = habito_alimentar,
      frequencia = n,
      sexo = sexo
    )
```

    ## `summarise()` has grouped output by 'sexo'. You can override using the `.groups` argument.

    ## # A tibble: 16 x 3
    ## # Groups:   sexo [4]
    ##    sexo         habito_alimentar frequencia
    ##    <chr>        <chr>                 <int>
    ##  1 fêmea        Carnívoro                 3
    ##  2 Fêmea        Carnívoro             13900
    ##  3 Fêmea        Detritívoro            3588
    ##  4 Fêmea        Herbívoro              1483
    ##  5 Fêmea        Indeterminado           565
    ##  6 Fêmea        Onívoro                8792
    ##  7 Macho        Carnívoro              8552
    ##  8 Macho        Detritívoro            3546
    ##  9 Macho        Herbívoro              1504
    ## 10 Macho        Indeterminado           581
    ## 11 Macho        Onívoro                7286
    ## 12 Não coletado Carnívoro             11476
    ## 13 Não coletado Detritívoro           15196
    ## 14 Não coletado Herbívoro              2081
    ## 15 Não coletado Indeterminado          1298
    ## 16 Não coletado Onívoro               19519

**Questão 05** Rodando o primeiro código a seguir eu pode ler o dataset,
alterando a variável rendimento\_liquido para numeric. E rodando o
segundo, obtive que o maior rendimento líquido é
132984
.

``` r
contracheque <- read_csv("dados/brutos/contracheque.csv", col_types = cols(rendimento_liquido = col_number()))
```

    ## Warning: 6643 parsing failures.
    ## row          col           expected actual                            file
    ## 412 cpf          delimiter or quote      ; 'dados/brutos/contracheque.csv'
    ## 412 lotacao      delimiter or quote      J 'dados/brutos/contracheque.csv'
    ## 412 lotacao      delimiter or quote      ; 'dados/brutos/contracheque.csv'
    ## 412 indenizacoes delimiter or quote      4 'dados/brutos/contracheque.csv'
    ## 412 indenizacoes delimiter or quote      ; 'dados/brutos/contracheque.csv'
    ## ... ............ .................. ...... ...............................
    ## See problems(...) for more details.

``` r
contracheque%>%
  select(rendimento_liquido)%>%
  arrange(desc(rendimento_liquido))
```

    ## # A tibble: 2,433 x 1
    ##    rendimento_liquido
    ##                 <dbl>
    ##  1            132984.
    ##  2            108085.
    ##  3             97976.
    ##  4             93633.
    ##  5             93093.
    ##  6             91889.
    ##  7             88278.
    ##  8             83580.
    ##  9             83476.
    ## 10             83264.
    ## # ... with 2,423 more rows

**Questão 06** Rodando o código que segue é possível determinar a
quantidade de funcionários públicos que recebem salário acima de
$$R$39293,32$$
, que são
106
no total.

``` r
contracheque%>%
  select(rendimento_liquido)%>%
  filter(rendimento_liquido > 39293.32)%>%
count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   106

\*\*(a) Recebem acima de
$$R$100000,00$$
duas pessoas, conforme o código que segue

``` r
contracheque%>%
  select(rendimento_liquido)%>%
  filter(rendimento_liquido > 100000)%>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1     2

**(b)** Para saber qual o tribunal possue maior variabilidade usei o
coeficiente de variação, sendo o maior identificado o correspondente ao
tribunal mais homogêneo. Assim, precisei calcular média e desvio padrão
para determinar o coeficiente de variação e ordenar de forma decrescente
o coeficiente de variação para visualizar o tribunal mais variável.
Rodando o código abaixo, percebi que o tribunal de maior variabilidade é
o Tribunal Superior Eleitoral.

``` r
contracheque%>%
  select(rendimento_liquido, tribunal)%>%
  group_by(tribunal)%>%
  summarise(
    media_rendimento = mean(rendimento_liquido, na.rm = TRUE),
    desvio = sd(rendimento_liquido, na.rm = TRUE),
    cv = ((desvio/media_rendimento)*100)
  )%>%
  arrange(desc(cv))
```

    ## # A tibble: 24 x 4
    ##    tribunal                                        media_rendimento desvio    cv
    ##    <chr>                                                      <dbl>  <dbl> <dbl>
    ##  1 Tribunal Superior Eleitoral                                5543.  8886. 160. 
    ##  2 Conselho Nacional de Justiça                               5247.  7563. 144. 
    ##  3 Tribunal Regional do Trabalho da 2ª Região                22763. 17493.  76.8
    ##  4 Tribunal Regional do Trabalho da 3ª Região                59568. 40858.  68.6
    ##  5 Tribunal Regional Eleitoral de Roraima                     3136.  2036.  64.9
    ##  6 Conselho da Justiça Federal                                 951.   573.  60.3
    ##  7 Tribunal Regional Eleitoral de Sergipe                     3031.  1646.  54.3
    ##  8 Tribunal Superior do Trabalho / Conselho Super~           20315. 11019.  54.2
    ##  9 Tribunal Regional Eleitoral de Rondônia                    3225.  1643.  50.9
    ## 10 Tribunal Regional Eleitoral de Tocantins                   3302.  1543.  46.7
    ## # ... with 14 more rows

**Questão 07** **(a)** O turno que possui maior mediana das notas é o
turno integral, pois o segundo quartil representado na caixa do Boxplot
do turno integral, que representa a mediana, é o que está mais acima
(eixo y) em relação aos demais Boxplots. **(b)** Rodando o código
abaixo, obtive que a média e a mediana das notas do turno integral, são
aproximadamente, respectivamente,
663
e
657
.

``` r
library(readr)
cursos_prouni <- read_csv("dados/brutos/cursos-prouni.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_double(),
    ##   grau = col_character(),
    ##   turno = col_character(),
    ##   curso_busca = col_character(),
    ##   cidade_busca = col_character(),
    ##   uf_busca = col_character(),
    ##   cidade_filtro = col_character(),
    ##   universidade_nome = col_character(),
    ##   campus_nome = col_character(),
    ##   nome = col_character()
    ## )
    ## i Use `spec()` for the full column specifications.

    ## Warning: 380 parsing failures.
    ## row col   expected    actual                             file
    ##  17  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ##  39  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ## 445  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ## 549  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ## 550  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ## ... ... .......... ......... ................................
    ## See problems(...) for more details.

``` r
cursos_prouni%>%
  group_by(turno)%>%
  summarise(
    media_nota = mean(nota_integral_ampla, na.rm = TRUE),
    mediana_nota = median(nota_integral_ampla, na.rm = TRUE)
  )
```

    ## # A tibble: 6 x 3
    ##   turno             media_nota mediana_nota
    ##   <chr>                  <dbl>        <dbl>
    ## 1 Curso a Distância       545.         552 
    ## 2 Integral                663.         657.
    ## 3 Matutino                609.         610.
    ## 4 Noturno                 602.         602.
    ## 5 Vespertino              622.         621.
    ## 6 <NA>                    NaN           NA

**(c)** Rodando o código seguinte, obtive que o curso de menor
homogeneidade é o Curso a Distância, pois o coeficiente de variação (cv)
foi o maior encontrado.

``` r
cursos_prouni%>%
  select(turno, nota_integral_ampla)%>%
  group_by(turno)%>%
  summarise(
    media_nota = mean(nota_integral_ampla, na.rm = TRUE),
    desvio_nota = sd(nota_integral_ampla, na.rm = TRUE),
    cv = ((desvio_nota/media_nota)*100)
    )%>%
arrange(desc(cv))
```

    ## # A tibble: 6 x 4
    ##   turno             media_nota desvio_nota    cv
    ##   <chr>                  <dbl>       <dbl> <dbl>
    ## 1 Curso a Distância       545.        53.2  9.77
    ## 2 Integral                663.        57.9  8.73
    ## 3 Matutino                609.        43.5  7.15
    ## 4 Noturno                 602.        41.3  6.86
    ## 5 Vespertino              622.        41.0  6.58
    ## 6 <NA>                    NaN         NA   NA

**Questão 08** A quinta posição, conforme o exibido ao rodar o código
que segue.

``` r
cursos_prouni%>%
  group_by(uf_busca)%>%
  count()%>%
  arrange(desc(n))
```

    ## # A tibble: 28 x 2
    ## # Groups:   uf_busca [28]
    ##    uf_busca     n
    ##    <chr>    <int>
    ##  1 SP       11501
    ##  2 MG        4128
    ##  3 PR        3821
    ##  4 RS        3060
    ##  5 BA        2496
    ##  6 SC        2160
    ##  7 RJ        1425
    ##  8 GO        1238
    ##  9 PA        1192
    ## 10 PE        1148
    ## # ... with 18 more rows

**Questão 09** Rodando o código abaixo, obtive que há 295 cursos
distintos.

``` r
cursos_prouni%>%
  distinct(nome)
```

    ## # A tibble: 295 x 1
    ##    nome                    
    ##    <chr>                   
    ##  1 Medicina                
    ##  2 Enfermagem              
    ##  3 Psicologia              
    ##  4 Engenharia de Computação
    ##  5 Educação Física         
    ##  6 <NA>                    
    ##  7 Direito                 
    ##  8 Engenharia de Produção  
    ##  9 Fisioterapia            
    ## 10 Administração           
    ## # ... with 285 more rows

**Questão 10** O gráfico do curso de direito possui distribuição mais
homogênea do que o do curso de medicina já que aparentemente se trata de
uma curva normal. Assim, provavelmente a média,a mediana e a moda desse
gráfico coincidem. Já o gráfico do curso de medicina representa uma
distribuição assimétrica para a direita.Logo, a mediana é a melhor
medida de posição central para representar as informações desse gráfico.
Devido a concentração de valores muito altos do gráfico referente ao
curso de medicina, comparando-o co o gráfico de direito, o curso de
medicina tem média maior. Com respeito a mediana, provavelmente a do
curso de direito é maior do que o do curso de medicina, devido a
simetria desse grafico.
