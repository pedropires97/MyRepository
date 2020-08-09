### Script com algumas coisas básicas do R ###

# Pacotes #

install.packages("pacote") #instalar
library(pacote) #carregar
require(pacote) #tenta carregar, retorna T ou F
remove.packages("pacote") #desinstalar
detach("package:pacote ", unload=TRUE) #descarregar
installed.packages() #quais pacotes estão instalados
update.packages() #atualiza todos os pacotes

# Entrar com dados usando R base #

x <- scan()
x <- c()
x <- matrix(data, nrow, ncol, byrow)
x <- data.frame(vetores)
x <- list()

as.matrix() #transforma em matriz
as.data.frame() #transforma em df
as.factor() #transforma em fator
rbind() #adiciona uma linha
cbind() #adiciona uma coluna
df$coluna <- NULL #retira a coluna

getwd() #mostra qual o diretório atual
setwd("C:/Users/USUARIO/Desktop") #muda o diretório atual

read.table("file.ext", header, sep, na.strings) #txt ou csv
read.csv("file.csv") #csv (sep = ,)
read.csv2("file.csv") #csv (sep = ;)
#R consegue ler vírgula

#exportar banco de dados
write.csv2(df, file = "arquivo.csv") #para excel (dps salva como pasta de trabalho do excel)

#salvar objetos
save(objeto, "nomeobjeto.RData") #salva no diretório atual
ggsave(file="nomeobjeto.png", plot, width= , height= ) #gráficos

# Lists #

list()
list[[1]] ou list[[""a""]] ou list$a #seleciona elemento na lista
list[[1]][1] #seleciona elemento dentro da estrutura dentro da lista

# Alguns comandos úteis #

== #é igual?
  != #é diferente?
  > ; < ; <= ; >=
  x|y #ou
x&y #e

F = 0; T = 1
class() #identifica o tipo de dado ou estrutura
sapply(x, class) #identifica o tipo de dado das colunas de um df ou dos elementos de uma matrix

sort(x) #coloca em ordem crescente
sort(x, decreasing = F) #coloca em ordem decrescente
order(x) #retorna a posição dos elementos (?)

x[] #seleciona elementos
x[x>n] #seleciona elementos que satisfazem uma condição
# [,1:3] coluna 1 a 3 (exemplo)

vector(mode,length) #inicializa um vetor
x <- 0 #incializa uma variável
as.numeric #transforma em númerico

unique(x) #retorna os valores sem repetição

set.seed(n) #seta uma seed

names(objeto) #acessa os valores numa função, por exemplo
attributes(objeto) #acessa os valores numa função, por exemplo
attr(objeto, "atributo") #acessa os valores numa função, por exemplo

#criar um função
myfunction <- function(arg1, arg2, ... ){
  statements
  return/print(object)
}

f <- function(x){oldfunction(x)} #mudar nome de função / abreviar

return() #normalmente usado para retornar algo de um função
x #retorna a própria variável
print() #...
cat(a,"a\n") #printf (printa o que quiser) #retorna o objeto a, texto "a", salta uma linha (\n)
paste(); paste0(); sprintf() #...

rep(x, each = n) #1,1,1,2,2,2...
rep(x, times = n) #1,2,1,2...
seq(from,to,by) #cria um vetor com números em uma sequência

ctrl + l #limpar console