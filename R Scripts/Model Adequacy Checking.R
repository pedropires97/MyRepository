### Script para Model Adequacy Checking ###

# Análise de Resíduos #
# Suposições: ei ~iid~ N(0,sigma2) e linearidade entre X e Y #
# Se não respeitar as suposições, modelo não vale (não pode fazer inferência) #
# Mas se o modelo for só para estimativas pontuais, passa #

modelo$residuals
modelo$fitted.values

# Gráficos e testes #

ana.res <- function(modelo){
  par(mfrow=c(2,2))
  #windows()
  plot(fitted(modelo), residuals(modelo), xlab="Valores Ajustados", ylab="Resíduos"); abline(h=0)
  require(car); leveneTest(mod) #Ho: homocedasticidade (testar pra ver se funciona!)
  plot(1:length(residuals(modelo)), residuals(modelo), xlab="Tempo", ylab="Resíduos"); abline(h=0) #tem que ser medido ao longo do tempo?
  hist(modelo$residuals, xlab="Resíduos", ylab="Frequência", main="Histograma")
  qqnorm(residuals(modelo), ylab="Resíduos", xlab="Quantis teóricos", main="QQ Plot"); qqline(residuals(modelo))
  shapiro.test(residuals(modelo))
  require(lmtest); dwtest(mod) #Durbin Watson para independência
}
ana.res(modelo)
plot(x, residuals(modelo), xlab="x", ylab="Resíduos"); abline(h=0) #res vs cada variável
plot.default(, y) #plot residuo x cada fator -> ver qual fator está ferrando com a variância do modelo

# Identificando violações #

# Resíduos x ajustados: heterocedasticidade, não linearidade, cauda pesada
# Resíduos x tempo: auto-correlação (padrão)
# Histograma: não normalidade
# QQ Plot: não normalidade (cauda pesada, cauda leve, assimetria + e -)
# Shapiro Wilk: não normalidade

# Corrigindo violações #

# Não normalidade: muda distribuição
# Média diferente de 0: não acontece
# Auto correlação: não vê no curso
# Heterocedasticidade: transformações em Y (1) (tem que ser 1:1)
# Não linearidade: transformações em Y e X (2)

# Correção para heterocedasticidade #

y = sqrt(y) #Var(ei) cresce proporcionalmente a Xi
y = log(y) #Var(ei) cresce proporcionalmente a Xi^2
y = asin(y) # y ~ bin

# Correção para não linearidade (checar) #

y = log(y)
y = log10(y)
y = 1/y
x = 1/x

# Box Cox #
# método para corrigir não linearidade
require(MASS); boxcox(modelo)
# retorna lambda, transformação: y = y^lambda (?) y = (y^lambda - 1)/lambda
# pode usar apenas quando y assume somente valores positivos (?)
# IC (?)

# Outliers #
# descobrir origem, tentar corrigir / avaliar o efeito / mudar dist.
det.outlier(modelo) #(função separada!)

# Método MQP #
# corrige heterocedasticidade
lm(y~x,weights=w)
# w = vetor com os pesos
# pode ser por exemplo: ni, 1/ni, 1/xi, 1/Sy^2
# 1/Sy^2 => 1/(1-lm.influence(mod)$hat)

# Resíduos parciais #
#resíduo sem a covariável Xj
#especificar a covariável
modelo$residuals + modelo$coefficients["xj"]*xj

# Acessar covariáveis #
get_all_vars(modelo)

# Diagnóstico de influência #
# detecta observações que influenciam muito nos B`s, SE(Bj), ...
sigma = summary(mod)$sigma

b1 = mod$coefficients[2]
x1i = get_all_vars(mod)[,2]
x1b = mean(get_all_vars(mod)[,2])

b2 = mod$coefficients[3]
x2i = get_all_vars(mod)[,3]
x2b = mean(get_all_vars(mod)[,3])

.
.
.

WSSDi = ((b1*((x1i-x1b)^2)) + (b2*((x2i-x2b)^2)) + ... ) / sigma

plot(WSSDi)
# pontos que saem muito tem grande influencia. mesmo tratamento que outlier

# Cook`s D #
# detecta observações que influenciam muito nos B`s, SE(Bj), ...
cd <- cooks.distance(mod)
cd[cd > 4/length(cooks.distance(mod))]

# Teste de bondade de ajuste #
#Ho: a relação entre Y e os X's é linear
#precisa de pelo menos uma repetição em cada covariável
anova(lm(y~x1+x2+...), lm(y~factor(x1)*factor(x2)*...)) (??)