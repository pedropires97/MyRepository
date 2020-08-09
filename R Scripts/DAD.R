### Descrição e apresentação de dados ###

#Fazer antes de demais análises

head(df) #primeiras observaçòes do banco
tail(df) #últimas observações
table(df) #cruzamento entre todas as variáveis (pode escolher as variáveis) / cria tabela de contingência
table(df[]) #frequência absoluta (uma variável)
table(x)/length(x) #frequência relativa
prop.table(table, margin = 1, 2, 3) #frequência relativa
str(df) #informações sobre as variáveis
summary(df) #estatísticas do banco ou vetor (min, max, 1st q, 3rd q, mean, median)
View(x) #visualiza o objeto/dados em uma janela separada

names(df) <- c("a","b","c"); data.frame("x"=x,...)
df[order(x),] #ordernar df pela var. x

mean(x) #média
median(x) #mediana
mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux)); ux[tab == max(tab)]
} #moda

require(moments)
skewness(x)
kurtosis(x)

max(x) #máximo
min(x) #mínimo
max(x)-min(x) #amplitude
var(x) #variância amostral
sd(x) #desvio padrão amostral
var.p <- function(x){var(x)*(length(x)-1)/length(x)} #variância pop.
sd(x)/abs(mean(x)) #CV coeficiente de variação (o quão grande é a dispersão)
cov(x,y) #covariância amostral

colMeans #Média de cada coluna
apply(dados,2,sd) #Desvio padrão de cada coluna

z = (x-mean(x))/sd(x) #escore padronizado
#desempenho global = média de z de um indivíduo
#DG ponderado (CV) = média de z*CV de um indivíduo

#gráficos para var. contínuas
hist(x) #histograma
plot(density(x)) #histograma de densidade
boxplot(x) #box-plot
stem(x) #ramo e folha

#gráficos para var. discretas ou categórica
barplot(height) #barras para contagem pronta
barplot(table(x)) #barras
pie(height) #pizza para contagem pronta
pie(table(x)) #pizza

plot(x,y) #gráfico de x-y
cor(x,y) #correlação linear de Pearson

windows() #abre janela de gráfico
par(mfrow=c(2,2)) #janela com vários gráficos

quantile(x, prob) #percentil e faixa de referência

#as vezes é útil categorizar os dados para fazer as análises