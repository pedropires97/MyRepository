## Probabilidade ##

ddist(q, parâmetros) #fdp (x=x)    #fç. de prob. (discreto) e fç. densidade
pdist(q, parâmetros) #fda (x=<x)   #fç. dist. acumulada
qdist(p, parâmetros) #quantil para tal prob.
rdist(n, parâmetros) #gera amostra aleatória

#distribuições: norm, exp, unif, f
#             : binom, geom, nbinom, hyper, pois


## Conjuntos ##

x <- c()
x <- unique(x) #conjunto
length(x) #cardinalidade
y <- c()
setequal(x,y) #x é igual a y?
all(y %in% x) #y é subconjunto de x?
sum(a == x) == 1 #elemento a pertence ao conjunto?

require(ggm); powerset(x, sort = T, nonempty = F) #conjunto potência / conjunto das partes
2^length(y) #tamanho do powerset

#operações em conjuntos / vetores
expand.grid(x,y) #produto cartesiano / todas as combinações dos elementos de x e y
union(x,y) #união
intersect(x,y) #interseçào
setdiff(x,y) #diferença / elementos que pertencem a x mas não a y / complemento de y em relação a x


## Análise combinatória ##

#permutação: quantas maneiras podemos ordenar n elementos?
require(combinat)
permn(x)
factorial(length(x))
factorial(n)

#permutação com repetição
require(combinat)
unique(permn(x))
length(unique(permn(x)))
factorial(n)/(factorial(n1)*factorial(n2)*...)

#combinação: quantas maneiras se pode selecionar / alocar k itens de um total de n itens?
require(utils)
t(combn(x, k))
nrow(t(combn(x, k)))
choose(n,k)

#arranjo: combinação, de forma ordenada
b = expand.grid(rep(list(x), 3)) #k = 3
b = data.frame(b, Var1 == Var2, Var1 == Var3, Var2 == Var3) #tem que comparar todas as colunas de b
require(dplyr)
b = filter(b, b[,4]==F, b[,5]==F, b[,6]==F) #tira os que tem elementos repetidos
b[,c(1,2,3)] #só um exemplo
nrow(b)
factorial(n)/factorial(n-k)

#arranjo com repetição/reposição
expand.grid(rep(list(x), k))
nrow(expand.grid(rep(list(x), k)))
n^k (princípio fundamental da contagem)


## Outros ##

floor(a) #piso
ceiling(a) #teto