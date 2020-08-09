### Amostragem ###

#Sorteio
sample(x, n, replace, prob) #x é um vetor ou número; n é a quantidade de elementos a serem escolhidos

#com reposicao / pop. finita:   var(.)
#com reposicao / pop. infinita:	var(.)
#sem reposicao / pop. finita:	var(.)*fpc
#sem reposicao / pop. infinita:	var(.)
#fpc (fator de correcao) = (N-n)/(N-1)
#conferir o resto do script, se está de acordo com isso ^

#usualmente considera com reposição e pop. infinita

y = vetor de dados
x = vetor de dados (auxiliar)
n = length(y)
N = tamanho da pop.

## AA Simples com reposição ##

#método: cada observação tem a mesma probabilidade de ser escolhida

#fi = quantidade de vezes que o elemento i aparece na amostra S
#fi ~ bin(n, 1/N); E[fi] = n/N; V[fi] = (n/N)*(1-(1/N))
#pii = probabilidade do elemento i pertencer a amostra S
#pii = 1 - (1-(1/N))^n

#estimadores pontuais
media = mean(y)
total = sum(y) = N*mean(y)
variancia = var(y)
prop = sum(y)/n #dados binários
prop = m/n #dados categóricos, m tem a característica

#variância dos estimadores
var.media = var(y)/n
var.total = (N^2)*var.media
var.prop = (prop*(1-prop))/n-1
(prop*(1-prop)) = 0.25 #se conservador

#ic's
conf =   #nível de confiança
  z = qnorm((1-conf)/2, lower.tail = F)
ic.m.l = media - z*sqrt(var.media)
ic.m.u = media + z*sqrt(var.media)
ic.t.l = total - z*sqrt(var.total)
ic.t.u = total + z*sqrt(var.total)
ic.p.l = prop - z*sqrt(var.prop)
ic.p.u = prop + z*sqrt(var.prop)

#erro padrão ou erro de estimação = desvio padrão do estimador
#diferença máxima entre a média real e a estimada
ep = sqrt(var.) #ou arbitrário
b = z*ep
d = (b/z)^2 #vai variar de acordo a estimativa desejada
#pode entrar com a variância / erro padrão desejado ou com o erro (b)

#tamanho da amostra
#leva em consideração o erro, o nível de sig., e a variância estimada (ou chute)
n.m = var(y)/d
n.t = ((N^2)*(var(y)))/d
n.p = (prop*(1-prop))/d
n.p = 0.25*d #se conservador


## AA Simples sem reposição ##

#método: cada observação tem a mesma probabilidade de ser escolhida

#fi ~ bern(n/N); E[fi] = n/N; V[fi] = (n/N)*(1-(n/N))\
#pii = n/N

#estimadores pontuais
#são os mesmos

#variância dos estimadores
f = n/N
var.media = (1-f)*var(y)/n
var.total = (1-f)*(N^2)*var.media
var.prop = (1-f)*(prop*(1-prop))/n-1
(prop*(1-prop)) = 0.25 #se conservador

#ic's
#são os mesmos, variâncias mudam

#erro padrão ou erro de estimação = desvio padrão do estimador
#diferença máxima entre a média real e a estimada
ep = sqrt(var.) #ou arbitrário
b = z*ep
d = (b/z)^2 #vai variar de acordo a estimativa desejada
#pode entrar com a variância / erro padrão desejado ou com o erro (b)

#tamanho da amostra
#leva em consideração o erro, o nível de sig., e a variância estimada (ou chute)
n.m = 1/((d/var(y))+(1/N))
n.t = N/((d/(N*var(y)))+(1))
n.p = N/(((N-1)*d)/((prop*(1-prop))+1))


## Estimadores de razão para AA Simples ##

#usa uma variável auxiliar x, quando é difícil de se medir y
#x e y tem que ser aproximadamente proporcionais, ou seja, positivamente correlacionados
#estimadores podem ser viesados
#viés diminui quando: n grande, f grande, mean(x) grande, sd(x) pequeno, p -> 1
#mean(x) ou sum(x) precisa ser conhecido

cor(x,y)
r = sum(y)/sum(x)
r = mean(y)/mean(x)

# AA Simples com reposição #

#estimativas pontuais
media = r*mean(y)
total = N*r*mean(y)
var.y = (sum(y-r*x)^2)/(n-1)

#variância dos estimadores
var.r = var.y/(n*(mean(x)^2))
var.media = var.y/n
var.total = (N^2)*var.media

#ic's
conf =   #nível de confiança
  z = qnorm((1-conf)/2, lower.tail = F)
ic.m.l = media - z*sqrt(var.media)
ic.m.u = media + z*sqrt(var.media)
ic.t.l = total - z*sqrt(var.total)
ic.t.u = total + z*sqrt(var.total)
ic.r.l = r - z*sqrt(var.r)
ic.r.u = r + z*sqrt(var.r)

#erro padrão ou erro de estimação = desvio padrão do estimador
#diferença máxima entre a média real e a estimada
ep = sqrt(var.) #ou arbitrário
b = z*ep
d = (b/z)^2 #vai variar de acordo a estimativa desejada
#pode entrar com a variância / erro padrão desejado ou com o erro (b)

#tamanho da amostra
#leva em consideração o erro, o nível de sig., e a variância estimada (ou chute)
n.m = var.y/d
n.t = ((N^2)*(var.y)/d
n.r = var.y/(d*mean(x))

# AA Simples sem reposição #

#estimadores pontuais
#são os mesmos

#variância dos estimadores
f = n/N
var.r = ((1-f)*var.y)/(n*(mean(x)^2))
var.media = ((1-f)*var.y)/n
var.total = (1-f)*(N^2)*var.media

#ic's
#são os mesmos, variâncias mudam

#erro padrão ou erro de estimação = desvio padrão do estimador
#diferença máxima entre a média real e a estimada
ep = sqrt(var.) #ou arbitrário
b = z*ep
d = (b/z)^2 #vai variar de acordo a estimativa desejada
#pode entrar com a variância / erro padrão desejado ou com o erro (b)

#tamanho da amostra
#leva em consideração o erro, o nível de sig., e a variância estimada (ou chute)
n.m = 1/((d/var.y)+(1/N))
n.t = 1/((d/(N*var.y))+1)
n.r = 1/(((d*mean(x)^2)/var.y)+(1/N))


## Estimadores de regressão para AA Simples ##

#mesma ideia do estimador de razão

bo = cov(x,y)/var(x)

# AA Simples com reposição #

#estimativas pontuais
media = mean(y) + bo(ux - mean(x)) #??
total = N*media

#variância dos estimadores
var.media = (var(y) - 2*bo*cov(x,y) + bo*bo*var(x))/n
var.media = (var(y)*(1-cor(x,y)^2))/n #var mínima ?? (usa uma das duas)
var.total = (N^2)*var.media

#ic's
conf =   #nível de confiança
  z = qnorm((1-conf)/2, lower.tail = F)
ic.m.l = media - z*sqrt(var.media)
ic.m.u = media + z*sqrt(var.media)
ic.t.l = total - z*sqrt(var.total)
ic.t.u = total + z*sqrt(var.total)

#erro padrão ou erro de estimação = desvio padrão do estimador
#diferença máxima entre a média real e a estimada
ep = sqrt(var.) #ou arbitrário
b = z*ep
d = (b/z)^2 #vai variar de acordo a estimativa desejada
#pode entrar com a variância / erro padrão desejado ou com o erro (b)

#tamanho da amostra
#leva em consideração o erro, o nível de sig., e a variância estimada (ou chute)
n.m = (var(y)*(1-cor(x,y)^2))/d
n.t = (N^2)*n.m

# AA Simples sem reposição #

#estimadores pontuais
#são os mesmos

#variância dos estimadores
f = n/N
var.media = (1-f)*(var(y) - 2*bo*cov(x,y) + bo*bo*var(x))/n
var.media = (1-f)*(var(y)*(1-cor(x,y)^2))/n #var mínima ?? (usa uma das duas)
var.total = (1-f)*(N^2)*var.media

#ic's
#são os mesmos, variâncias mudam

#erro padrão ou erro de estimação = desvio padrão do estimador
#diferença máxima entre a média real e a estimada
ep = sqrt(var.) #ou arbitrário
b = z*ep
d = (b/z)^2 #vai variar de acordo a estimativa desejada
#pode entrar com a variância / erro padrão desejado ou com o erro (b)

#tamanho da amostra
#leva em consideração o erro, o nível de sig., e a variância estimada (ou chute)
n.m = 1/((d/((N/N-1)*var(y)*(1-cor(x,y)^2)))+(1/N))
n.t = (N^2)*n.m


## AA Sistemática ##

#método: seleciona os elementos de k em k (pseudo aleatória)
#k: escolha inicial, intervalo
N = n*k
#estimativas são iguais da AA Simples
#existem alternativas para calcular as variâncias dos estimadores, mas pode usar os comuns mesmo


## PPS ##

#sampling with probabilities proportional to size
#método: dá mais chance de um elemento ser escolhido dado o tamanho dele
zi = pi =  #probabilidade de seleção de unidade amostral
  pii =  #probabilidade de que a unidade i faça parte da amostra s
  #para definir zi, tem que usar uma variável auxiliar (tamanho, por exemplo) que seja altamente correlacionada com y
  
  zi = mi/m
sum(mi) = m
z =  #vetor com os zi
  pii = n*zi #sem reposição
pii = 1-(1-zi)^n #com reposição
pi =  #vetor com os pii
  
  # Hansen-Hurwitz (só com reposição)
  
  #estimadores pontuais
  t = sum(y-z)/n
m = t/N

#variância dos estimadores
var.t = (sum((y/z)-t)^2)/n*(n-1)
var.m = (sum((y/z)-t)^2)/(n*(n-1)*N*N)

#ic's igual AA Simples
#erro padrão igual AA Simples
#tamanho da amostra tem que isolar o n...

# Horwitz-Thompson

#pode ser com ou sem reposição, muda o pii

#estimadores pontuais
T = sum(y/pi)
m = t/N

#variância dos estimadores
#v: elementos diferentes (id); sem reposição -> v = n
t = (v*y)/pi #tamanho v
st2 = sum(t-T)/(v-1) #somatório até v
var.t = ((N-v)/N)*(st2/v)
var.m = var.t/(N^2)

#ic's igual AA Simples
#erro padrão igual AA Simples
#tamanho da amostra tem que isolar o n...


## AA Estratificada ##

#método: divide a população em estratos
#grupos homogêneos dentro, heterogêneos entre

H: número de estratos
Nh: vetor com os tamanhos dos estratos
nh: vetor com os tamanhos das amostras
N = sum(Nh): tamanho da população
Wh = Nh/N: vetor com os pesos/proporção dos estratos

#estimativas pontuais
yi: vetor de observações do estrato i
y: vetor com todas observações
ti = sum(yi) #total do estrato i
mi = mean(yi) #média do estrato i
pi = m/length(yi) #prop do estrato i; m tem a característica
th: vetor com os totais de cada estrato
mh: vetor com as medias de cada estrato
ph: vetor com as proporções de cada estrato
var.i = var(yi) #var do estrato i
var.i.p = pi*(1-pi) #var do estrato i, prop
var.i.v : vetor das var.i ou var.i.p
t = sum(th)
m = t/N
p = sum(Wh*ph)
var = var(y) #var geral
var.p = p*(1-p) #var geral, prop

#variância dos estimadores

#var do design dentro do estrato
#com reposição
var.a = var.i.v/nh
#sem reposição
fi = length(yi)/Nh[i]
var.a = fi*var.i.v/nh

var.t = sum(Nh^2*var.a)
var.m = sum(Wh^2*var.a)
var.p = sum((Wh^2*var.i.v)/(nh-1))

#ic's é equivalente
#erros padrão também

#tamanho da amostra
ah = Wh #alocação proporcional
ah = 1/H #alocação uniforme
#tem alocação ótima de Neyman também
n = sum(Wh^2*(var.i.v/ah))/(v+(sum(Wh*var.i.v))/N)
v: var (?)


## AA por conglomerados/cluster ##

#método: seleciona clusters (de vários indivíduos)

x: indivíduos
y: conglomerados
N: número de clusters
n: tamanho amostra clusters
m: tamanho de cada cluster (de preferência iguais)
M = sum(m): número de indivíduos

#estimadores pontuais

t: vetor com o total de cada cluster
T = (N/n)*(sum(t))
y = T/(N*m) = T/M

#variância dos estimadores

#com reposição
st2 = (sum(t-(T/N))^2)/(n-1)
var.t = (N^2)*(st2/n)
var.m = st2/(n*M*M)

#sem reposição
st2 = (sum(t-(T/N))^2)/(n-1)
f = n/N
var.t = (1-f)*(N^2)*(st2/n)
var.m = ((1-f)*st2)/(n*M*M)

#ic's igual AA Simples
#erro padrão igual AA Simples
#tamanho da amostra tem que isolar o n..