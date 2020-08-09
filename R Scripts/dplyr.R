#### dplyr ####

# pacote do tidyverse para manipulacao de data sets
# baseado nos verbos e no pipe %>%
# pipe na verdade eh do magrittr

require(dplyr)

data() #data sets disponiveis

data("starwars") #carrega promise o data set
starwars #carrega de fato o data set
#esse data set eh do dplyr / tibble
#vem em um formato um pouco diferente do data.frame

glimpse(starwars) #preview

# selecionar colunas
select(starwars, name, height, mass)
starwars %>% select(name, height, mass)
starwars %>% select(name, height1 = height)
sw = starwars %>% select(name, height, mass)
starwars %>% select(name, mass, height) #na ordem que quiser
starwars %>% select(name:mass)
starwars %>% select(contains("a"))
?select_helpers
starwars %>% select(-name)
starwars %>% rename(altura = height)

# ordenar
arrange(starwars, height)
arrange(starwars, desc(height))

# filtrar observacoes
filter(starwars, height < 100, is.na(hair_color))
starwars %>% filter(height < 100, is.na(hair_color))

# multiplas acoes
starwars %>%
  select(name, height, mass) %>%
  filter(height < 100) %>%
  View()

# novas variaveis
starwars %>% mutate(peso = height*10)
starwars %>% mutate(height = height*10) #transforma a variavel existente
starwars %>% transmute(name, height2 = height*10) #select com mutate

# case when
POS = case_when((IPouts/3)>100 | GS>=4 ~ "SP",
                GS<=1 ~ "RP",
                TRUE ~ "Undefined") #else

x = c(3, 5, 16, 8)
lag(x) #anda uma casa no vetor
x - lag(x) #diferenca de um obs pra outra

# sumarizar info
starwars %>% count(skin_color, sort=T) #qtd de obs por categoria de uma variavel
starwars %>% count(skin_color, hair_color) #multiplas variaveis
starwars %>% count(skin_color, wt=height, sort=T) #soma de outra variavel, ao inves da contagem

starwars %>% filter(height > 0) %>%
  summarize(soma = sum(height), mean(height), median(height), min(height))
starwars %>% filter(height > 0) %>%
  group_by(skin_color, hair_color) %>%
  summarize(soma = sum(height)) %>%
  ungroup()
starwars %>% filter(height > 0) %>%
  group_by(skin_color) %>%
  top_n(3, height) #3 maiores height de cada skin_color

# left join
left_join(data1, data2, by=c("a" = "b"))
right_join()
full_join()
...

# pipe
x %>% f #f(x)
x %>% f(y) #f(x, y) #primeiro argumento
x %>% f %>% g %>% h #h(g(f(x)))

x %>% f(y, .) #f(y, x)
x %>% f(y, z = .) #f(y, z = x)

f <- . %>% cos %>% sin #f <- function(.) sin(cos(.)) #cria function

data.frame(z = rnorm(100)) %$% #expoe as variaveis do data set
  ts.plot(z)

