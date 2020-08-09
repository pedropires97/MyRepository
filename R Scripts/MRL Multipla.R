### Análise de Regressão Linear Múltipla ###

# Relação das variáveis explicativas X (covariáveis) com a variável resposta Y (contínua) #
plot(x,y) #plota um gráfico X vs Y
cor.test(x, y) #correlação de Pearson
#fazer para todas as variáveis

# Ajustando o modelo e observando os parâmetros #
modelo <- lm(y~x1+x2...)
modelo #coeficientes
summary(modelo) #sig. coeficientes, se coeficientes, erro padrão
confint(modelo, level=0.95) #intervalo de confiança para os coeficientes
ic.sigma2(modelo, 0.95) #intervalo de confiança para MSE (função separada!)

anova(modelo) #teste de significância do modelo (H1: pelo menos uma regressora é sig.)

# Coeficiente de determinação ajustado #
summary(modelo) #R2 (Adjusted R-squared)

# Predições #
predict(modelo,data.frame(x1=x1,x2=x2,...),interval="confidence") #resposta média
predict(modelo,data.frame(x1=x1,x2=x2,...),interval="prediction") #nova observação
#especificar o x

# Regressão através da origem / sem intercepto #
# usa quando Bo não é significativo ou quando x1 = x2 = ... = y = 0 faz sentido
lm(y~0+x)

# Teste F Parcial #
fparcial(modelo,modelo2) #(função separada!; equivalente ao anova(mod1,mod2))

# Teste de falta de ajuste #
#Ho: a relação entre Y e os X's é linear
#precisa de pelo menos uma repetição em cada covariável
anova(lm(y~x1+x2+...), lm(y~factor(x1)*factor(x2)*...)) #(??)

# Multicolinearidade #
#quando variáveis são correlacionadas (dependentes)
#afeta na estimativa dos B's, infla a variância da estimativa e MSE
#solução: excluir covariáveis e analisar o modelo; considerar modelo com interação
#         transformar em (Xi - Xibarra) (modelos polinomiais se o intervalo de X é pequeno)
require(Hmisc); rcorr(cbind(x1,x2,...)) #matrix de correlação (1 a 1) com coef. e teste
require(car); vif(modelo) #fator de inflação da variância
#1-5 fraco; 5-10 moderado; 10+ forte

# Variável indicadora #
lm(y~...) #alguma covariável é fator
#k categorias = k-1 coeficientes
predict(modelo,data.frame(x1=x1,x2="x2",...),interval="confidence") #resposta média
predict(modelo,data.frame(x1=x1,x2="x2",...),interval="prediction") #nova observação
# Análise de resíduos
#suposição: variância do erro é comum aos dois grupos e constante em cada grupo
#especificar a variável indicadora e a quantidade de categorias
plot(fitted(modelo), residuals(modelo), col=c("red","blue",...)[x2], xlab="Valores Ajustados", ylab="Resíduos"); abline(h=0)
lm(y~x1+relevel(x2,"b")) #muda o grupo que vai ser = 0

# Modelo com interação #
#interação entre uma variável contínua e uma indicadora
#usado quando esperamos interceptos e inclinações diferentes para cada grupo
plot(x1,y,col=c("blue","red")[x2]) #exemplo (x1=contínua; x2=indicadora)
legend(x=900,y=40,legend=c("a","b"),fill=c("blue","red")) #exemplo
#a interação vai ser entre x1 e x2
lm(y~x1+x2+x1*x2)
#tem que testar a sig. do coeficiente da interaçao; se sig., mantêm todas as variáveis involvidas
#                                                 ; se não sig., avalia efeito com e sem
#Predictions (coloca os valores das covariáveis, não dos coeficientes)
predict(modelo,data.frame(x1=x1,x2="x2",...),interval="confidence") #resposta média
predict(modelo,data.frame(x1=x1,x2="x2",...),interval="prediction") #nova observação
#Interpretação (Y = Bo + B1X1 + B2X2 + B3X1*X2)
#E(Y|X2=0) = Bo + B1X1
#E(Y|X2=1) = Bo + B1X1 + B2 + B2X1 = (Bo + B2) + (B1 + B3)X1

# Seleção de variáveis / modelo #
summary(modelo)$r.squared #R^2
summary(modelo)$adj.r.squared #R^2 ajustado
(summary(modelo)$sigma)^2 #MSe
require(DAAG); press(modelo) #PRESS (quanto menor, melhor)
mallows.cp(modelo,modeloc) #CP Mallows (função separada) (quanto compara subsets) (quanto menor, melhor)
require(DAAG); require(car); reg.info(modelo,modeloc) #todas as informações (função separada)
AIC(modelo) #AIC (quanto menor, melhor) compara subsets

# Stepwise #
step(modelo, direction = c("both", "backward", "forward"),...)

# Introdução MR Polinomial #
#usado quando a relação entre X e Y não é linear
plot(x,y)
modelo <- lm(y~poly(x,2)) #grau 2
# Y = B0 + B1X1 + B2X1^2