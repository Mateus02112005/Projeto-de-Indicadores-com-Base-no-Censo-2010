# Carregar os pacotes:
library(writexl)
library(tidyverse)
library(readxl)
library(tibble)

# Vamos realizar o calculo dos indicadores um por um e adaptá-los ao nivel municipal:

# Para obtermos os códigos dos municipios do Rio de Janeiro:
install.packages("geobr")
library(geobr)
muni = read_municipality(year = 2010)
muni_rj = subset(muni, code_state == 33)
muni_rj[, c('code_muni','name_muni')]
codigos_rj = muni_rj[, c('code_muni','name_muni')]
head(codigos_rj)

codigos_rj$geom = NULL

rm(muni)
rm(muni_rj)
## IMF-DAR:

tabela1 = read_excel('tabela1.xlsx') # Total de domicilios onde a pessoa responsável é do sexo feminino com renda per capita abaixo da linha da pobreza
### renomeando as colunas da tabela e eliminando as linhas que faziam parte do titulo mas foram colocadas como itens:

tabela1 = tabela1 %>%
  rename(municipio = 'Tabela 1161 - Domicílios particulares permanentes, por situação do domicílio e a espécie de unidade doméstica, segundo o sexo e os grupos de idade da pessoa responsável pelo domicílio e as classes de rendimento nominal mensal domiciliar per capita')
tabela1 = tabela1 %>%
  na.omit()
tabela1 = tabela1 %>%
  rename(total = ...2)

tabela2 = read_excel('tabela2.xlsx') # Total de domicilios monoparentais femininos por municipio

tabela2 = tabela2 %>%
  rename(municipio = 'Tabela 1161 - Domicílios particulares permanentes, por situação do domicílio e a espécie de unidade doméstica, segundo o sexo e os grupos de idade da pessoa responsável pelo domicílio e as classes de rendimento nominal mensal domiciliar per capita')
tabela2 = tabela2 %>%
  na.omit()
tabela2 = tabela2 %>%
  rename(total = ...2)
# Agora temos tudo que precisamos para realizar o cálculo do IMF-DAR:

indicador1= (tabela1$total/tabela2$total)*100


Base_final = tibble(
  'Código do Município' = integer(),
  'Município' = character(),
  'IMF-DAR' = integer()
)

Base_final = Base_final %>%
  add_row(
    Município = tabela2$municipio,
    `IMF-DAR` = indicador1,
    'Código do Município' = codigos_rj$code_muni
  )

## RIBR:

### Carregar e organizar as tabelas que serão utilizadas:

tabela3 = read_excel('tabela3.xlsx') # Total de domicilios chefiados por jovens (15 a 29 anos) classificados como baixa renda (até meio salário mínimo)

tabela3 = tabela3 %>%
  rename(municipio = 'Tabela 1161 - Domicílios particulares permanentes, por situação do domicílio e a espécie de unidade doméstica, segundo o sexo e os grupos de idade da pessoa responsável pelo domicílio e as classes de rendimento nominal mensal domiciliar per capita')
tabela3 = tabela3 %>%
  na.omit()
tabela3 = tabela3 %>%
  rename(total = ...2)


tabela4 = read_excel('tabela4.xlsx')# Total de domicílios chefiados por idosos (60+ anos) classificados como baixa renda (até meio salário mínimo)

tabela4 = tabela4 %>%
  rename(municipio = 'Tabela 1161 - Domicílios particulares permanentes, por situação do domicílio e a espécie de unidade doméstica, segundo o sexo e os grupos de idade da pessoa responsável pelo domicílio e as classes de rendimento nominal mensal domiciliar per capita')
tabela4 = tabela4 %>%
  na.omit()
tabela4 = tabela4 %>%
  rename(total = ...2)
# Agora iremos realizar o cálculo do indicador:
indicador2 = (tabela3$total/tabela4$total)

# Atualizando a base final:
Base_final = tibble(
  'Código do Município' = integer(),
  'Município' = character(),
  'IMF-DAR' = integer(),
  'RIBR' = integer()
)

Base_final = Base_final %>%
  add_row(
    Município = tabela2$municipio,
    `IMF-DAR` = indicador1,
    'Código do Município' = codigos_rj$code_muni,
    'RIBR' = indicador2
  )
## IBI:

### Carregar e organizar as tabelas que serão utilizadas:
tabela5 = read_excel('tabela5.xlsx') # Total de domicílios com banheiro inadequado

tabela5 = tabela5 %>%
  rename(municipio = 'Tabela 3216 - Domicílios particulares permanentes e Moradores em domicílios particulares permanentes, por situação do domicílio, segundo o tipo do domicílio, a condição de ocupação, a existência de banheiro ou sanitário e esgotamento sanitário e a existência e número de banheiros de uso exclusivo do domicílio')
tabela5 = tabela5 %>%
  na.omit()
tabela5 = tabela5 %>%
  rename(total = ...2)

tabela6 = read_excel('tabela6.xlsx') # Total de domicílios

tabela6 = tabela6 %>%
  rename(municipio = 'Tabela 3216 - Domicílios particulares permanentes e Moradores em domicílios particulares permanentes, por situação do domicílio, segundo o tipo do domicílio, a condição de ocupação, a existência de banheiro ou sanitário e esgotamento sanitário e a existência e número de banheiros de uso exclusivo do domicílio')
tabela6 = tabela6 %>%
  na.omit()
tabela6 = tabela6 %>%
  rename(total = ...2)

# Realizando o cálculo do indicador:
indicador3 = (tabela5$total/tabela6$total)*100

# Atualizando a base final:
Base_final = tibble(
  'Código do Município' = integer(),
  'Município' = character(),
  'IMF-DAR' = integer(),
  'RIBR' = integer(),
  'IBI' = integer()
)

Base_final = Base_final %>%
  add_row(
    Município = tabela2$municipio,
    `IMF-DAR` = indicador1,
    'Código do Município' = codigos_rj$code_muni,
    'RIBR' = indicador2,
    'IBI' = indicador3
  )

## IDC-CHI:

### Carregar as tabelas que serão utilizadas

tabela7 = read_excel('tabela7.xlsx') # Tabela com o total de domicilios com uma ou mais criança por municipio

tabela7 = tabela7 %>%
  rename(municipio = 'Tabela 1161 - Domicílios particulares permanentes, por situação do domicílio e a espécie de unidade doméstica, segundo o sexo e os grupos de idade da pessoa responsável pelo domicílio e as classes de rendimento nominal mensal domiciliar per capita')
tabela7 = tabela7 %>%
  na.omit()
tabela7 = tabela7 %>%
  rename(total = ...2)

# Para calcular o indicador de risco basta apenas usar as colunas "total" da tabela7 e tabela5:

indicador4 = tabela5$total/tabela7$total

# ALterando a base de dados:

Base_final = Base_final %>%
  mutate(
    'IDC-CHI' = indicador4
  )

## CAAD

### Vamos carregar e organizar as tabelas que serão utilizadas:

tabela8 = read_excel('tabela8.xlsx') # Tabela com o total de domicilios com acesso a rede geral de água

tabela8 = tabela8 %>%
  rename(municipio = 'Tabela 1395 - Domicílios particulares permanentes, por situação do domicílio e existência de banheiro ou sanitário e número de banheiros de uso exclusivo do domicílio, segundo o tipo do domicílio, a forma de abastecimento de água, o destino do lixo e a existência de energia elétrica' )
tabela8 = tabela8 %>%
  na.omit()
tabela8 = tabela8 %>%
  rename(total = ...2)

# Para realizar o cálculo do indicador:

indicador5 = (tabela8$total/tabela6$total)*100

# Como o CAAD é um indicador qualitativo, divido em "adequado" e "inadequado", vamos estabelecer um "piso". Todo município com porcentagem maior que esse "piso" será classificado como "adequado".

piso = 80.0


caad = tibble(
  'Município' = character(),
  'ind' = integer()
)

caad = caad %>%
  add_row(
    Município = tabela2$municipio,
    ind = indicador5
    
  )

adequado = caad[caad$ind >= piso,] # Aqui temos selecionados todos os municipios "adequados"

adequado$ind = "Adequado"


inadequado = caad[caad$ind <= piso,] # Aqui temos selecionados todos os municipios "inadequados"

inadequado$ind = "Inadequado"

final = bind_rows(adequado, inadequado) # Combinando as respostas
final = final %>% # organizando em ordem alfabética
  arrange(Município)

# Adicionando na base de dados final:

Base_final = Base_final %>%
  mutate(
    'CAAD' = final$ind
  )


## ASD

# Realizaremos o mesmo processo feito no indicador anterior

# Carregar e organizar a tabela que será utilizada:

tabela9 = read_excel('tabela9.xlsx') # Tabela com o total de domicilios com acesso ao tratamendo adequado de esgoto

tabela9 = tabela9 %>%
  rename(municipio = 'Tabela 1394 - Domicílios particulares permanentes, por situação do domicílio e existência de banheiro ou sanitário e número de banheiros de uso exclusivo do domicílio, segundo o tipo do domicílio, a condição de ocupação e o tipo de esgotamento sanitário')
tabela9 = tabela9 %>%
  na.omit()
tabela9 = tabela9 %>%
  rename(total = ...2)

# Para realizar o cálculo do indicador:

indicador6 = (tabela9$total/tabela6$total)*100


# Como o ASD é um indicador qualitativo, divido em "SIM" e "NÃo", vamos estabelecer um "piso". Todo município com porcentagem maior que esse "piso" será classificado como "SIM".
asd = tibble(
  'Município' = character(),
  'ind' = integer()
)

asd = asd %>%
  add_row(
    Município = tabela2$municipio,
    ind = indicador6
    
  )

sim = asd[asd$ind >= piso,] # Selecionando todos os domicilios adequados

nao = asd[asd$ind <= piso,] # Selecionando todos os domicilios inadequados

sim$ind = 'SIM'
nao$ind = 'NÃO'

final2 = bind_rows(sim,nao)
final2 = final2 %>%
  arrange(Município)

# Adicionando na base final:
Base_final = Base_final %>%
  mutate(
    'ASD' = final2$ind
  )




## IEPN-EM

### Carregando as tabelas que serão usadas

tabela10 = read_excel('tabela10.xlsx') # Tabela com o total de pessoas negras com o ensino médio completo

tabela10 = tabela10 %>%
  rename(municipio = 'Tabela 3540 - Pessoas de 10 anos ou mais de idade, por nível de instrução, segundo a situação do domicílio, o sexo, a cor ou raça e os grupos de idade')
tabela10 = tabela10 %>%
  na.omit()
tabela10 = tabela10 %>%
  rename(total = ...2)


tabela11 = read_excel('tabela11.xlsx') # Tabela com o total de pessoas negras

tabela11 = tabela11 %>%
  rename(municipio = 'Tabela 3540 - Pessoas de 10 anos ou mais de idade, por nível de instrução, segundo a situação do domicílio, o sexo, a cor ou raça e os grupos de idade')
tabela11 = tabela11 %>%
  na.omit()
tabela11 = tabela11 %>%
  rename(total =...2)


# Calculando o indicador:

indicador7 = (tabela10$total/tabela11$total)*10

# Alterando a tabela:

Base_final = Base_final %>%
  mutate(
    'IEPN-EM' = indicador7
  )


## IDR-DP

# Vamos carregar as tabelas que serão usadas:

tabela12 = read_excel('tabela12.xlsx') # Tabela com o total de domicilios com pessoas negras

tabela12 = tabela12 %>%
  rename(municipio = 'Tabela 3175 - População residente, por cor ou raça, segundo a situação do domicílio, o sexo e a idade')
tabela12 = tabela12 %>%
  na.omit()
tabela12 = tabela12 %>%
  rename(total = ...2)

# Para calcular o indicador:
# Este indicador fornece uma proporção entre o total de pessoas negras e o total de domicilios, mostrando a média de pessoas negras por domicilio

indicador8 = (tabela12$total/tabela6$total)

# Adicionando na base final:

Base_final = Base_final %>%
  mutate('IDR-DP' = indicador8)



## IPRS

# Vamos calcular o IPRS para o gênero feminino, da religião católica

# Carregar e organizar as tabelas que serão usadas:

tabela13 = read_excel('tabela13.xlsx') # Tabela com o total de indivíduos do sexo feminino pertecentes a religião católica

tabela13 = tabela13 %>%
  rename(municipio = 'Tabela 1489 - População residente, por cor ou raça, segundo o sexo a a religião - Resultados Gerais da Amostra')
tabela13 = tabela13 %>%
  na.omit()
tabela13 = tabela13 %>%
  rename(total = ...2)


tabela14 = read_excel('tabela14.xlsx') # Tabela com o total de indivíduos do sexo feminino

tabela14 = tabela14 %>%
  rename(municipio = 'Tabela 1489 - População residente, por cor ou raça, segundo o sexo a a religião - Resultados Gerais da Amostra')
tabela14 = tabela14 %>%
  na.omit()
tabela14 = tabela14 %>%
  rename(total = ...2)

# calculando o indicador:

indicador9 = (tabela13$total/tabela14$total)*100

# Alterando a base final:

Base_final = Base_final %>%
  mutate(
    "IPRS" = indicador9
  )


## RDI

# Carregar e organizar as tabelas que serão utilizadas:

tabela15 = read_excel('tabela15.xlsx') # Tabela com o total de populção indígena de 0 a 14 anos e 65+ anos

tabela15 = tabela15 %>%
  rename(municipio = 'Tabela 8175 - População indígena, por localização do domicílio, grupos de idade e sexo')
tabela15 = tabela15 %>%
  na.omit()
tabela15 = tabela15 %>%
  rename(total = ...2)

tabela16 = read_excel('tabela16.xlsx') # Tabela com o total de população indígena em idade potencialmente ativa (15 a 64 anos)

tabela16 = tabela16 %>%
  rename(municipio = 'Tabela 8175 - População indígena, por localização do domicílio, grupos de idade e sexo')
tabela16 = tabela16 %>%
  na.omit()
tabela16 = tabela16 %>%
  rename(total = ...2)

# Para calcular o indicador:

indicador10 = (tabela15$total/tabela16$total)

# Alterando a base final:

Base_final = Base_final %>%
  mutate(
    "RDI" = indicador10
  )

## IJR

# Carregar e organizar as tabelas que serão utilizadas:

tabela17 = read_excel('tabela17.xlsx') # Tabela com o total de população jovem em áreas rurais

tabela17 = tabela17 %>%
  rename(municipio = 'Tabela 2093 - População residente por cor ou raça, sexo, situação do domicílio e grupos de idade - Amostra - Características Gerais da População')
tabela17 = tabela17 %>%
  na.omit()
tabela17 = tabela17 %>%
  rename(total = ...2)

tabela18 = read_excel('tabela18.xlsx') # Tabela com o total de população jovem

tabela18 = tabela18 %>%
  rename(municipio = 'Tabela 2093 - População residente por cor ou raça, sexo, situação do domicílio e grupos de idade - Amostra - Características Gerais da População')
tabela18 = tabela18 %>%
  na.omit()
tabela18 = tabela18 %>%
  rename(total = ...2)

# Caculando o indicador:

indicador11 = (tabela17$total/tabela18$total)*100


# Alterando a tabela:

Base_final = Base_final %>%
  mutate('IJR' = indicador11)

## IEEU

# Carregar e organizar as tabelas que serão utilizadas:

tabela19 = read_excel('tabela19.xlsx') # Tabela com o total de população de 0 a 14 anos em área urbana

tabela19 = tabela19 %>%
  rename(municipio = 'Tabela 2093 - População residente por cor ou raça, sexo, situação do domicílio e grupos de idade - Amostra - Características Gerais da População')
tabela19 = tabela19 %>%
  na.omit()
tabela19 = tabela19 %>%
  rename(total = ...2)

tabela20 = read_excel('tabela20.xlsx') # Tabela com o total de população com 60+ anos

tabela20 = tabela20 %>%
  rename(municipio = 'Tabela 2093 - População residente por cor ou raça, sexo, situação do domicílio e grupos de idade - Amostra - Características Gerais da População')
tabela20 = tabela20 %>%
  na.omit()
tabela20 = tabela20 %>%
  rename(total = ...2)

# Calculando o indicador:

indicador12 = (tabela19$total/tabela20$total)

# Alterando a base final

Base_final = Base_final %>%
  mutate(
    'IEEU' = indicador12
  )


# Escrevendo a base final como uma tabela no excel:

write_xlsx(x = Base_final, path = 'Base_final.xlsx')
  
######
  