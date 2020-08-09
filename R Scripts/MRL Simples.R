### Análise de Regressão Linear Simples ###

# Relação da variável explicativa X (covariável) com a variável resposta Y (contínua) #
plot(x,y) #plota um gráfico X vs Y
cor.test(x, y) #correlação de Pearson

# Ajustando o modelo e observando os parâmetros #
modelo <- lm(y~x)
modelo #coeficientes
summary(modelo) #sig. coeficientes, se coeficientes, erro padrão
confint(modelo, level=0.95) #intervalo de confiança para os coeficientes
ic.sigma2(modelo, 0.95) #intervalo de confiança para MSE (função separada!)

anova(modelo) #teste de significância do modelo

plot(x,y); abline(modelo, col="red") #gráfico com a reta de regressão

# Coeficiente de determinação #
summary(modelo)$r.squared #R2 (Multiple R-squared)

# Predições #
predict(modelo,data.frame(x=x),interval="confidence") #resposta média
predict(modelo,data.frame(x=x),interval="prediction") #nova observação
#especificar o x

# Regressão através da origem / sem intercepto #
# usa quando Bo não é significativo ou quando x = y = 0 faz sentido
lm(y~0+x)

# Teste de falta de ajuste #
#Ho: a relação entre Y e o X é linear
#precisa de pelo menos uma repetição em cada covariável
anova(lm(y~x1), lm(y~factor(x1))) #(??)