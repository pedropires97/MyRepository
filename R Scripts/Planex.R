### Planex ###

### Modelos de Análise de Variáncia ###

# Dados #
dados = data.frame(y, as.factor(bloco1), as.factor(bloco2), as.factor(trat), ..., cov1, ...)
attach(dados)

# Descritivas #
boxplot(y ~ trat, data=dados) #sem bloco
par(mfrow=c(1,2)) #com bloco
plot(y ~ bloco + trat)
#descritivas das comp multiplas -> depois de setar o modelo
hist(y) #ver dist da var resposta, mas é uma mistura de distribuições

# Modelos #
# se balanceado, melhor!

# ANOVA (experimento completamente aleatorizado, efeitos fixos, 1 fator de trat)

mod = aov(x~y,data=df)

# ANOVA (experimento completamente aleatorizado, efeitos aleatórios, 1 fator de trat)
#usado quando o efeito do tratamento é escolhido em meio a uma população ("contínuo")

require(nlme)
mod = lme(y ~ 1, data = dados, random = ~ 1|trat)
VarCorr(mod) #variâncias do modelo
intervals(mod) #intervalos para variâncias e média geral

# ANOVA (experimento com 1 bloco completo balanceado, efeitos fixos, 1 fator de trat)
#usado para controlar um fonte de variação

mod = aov(y ~ bloco + trat, data=dados)
#funciona para mais de uma obs de cada tratamento em cada bloco
mod = aov(y ~ Error(bloco) + trat, data=dados) #efeito aleatório de bloco

# ANOVA (experimento com 1 bloco incompleto balanceado, efeitos fixos, 1 fator de trat)

modelo = aov(y ~ bloco + trat)
#pode considerar bloco como aleatório tbm
#cuidado com as comparações múltiplas, pois nem todos os blocos estão em todos os tratamentos (verificar isso!)

# ANOVA (experimento com 2 blocos, efeitos fixos, 1 fator de trat) QUADRADO LATINO

require(agricolae); design.lsd(trat,serie=2) #aleatorização do quadrado (serie 1 ou 2)
mod = aov(y ~ bloco1 + bloco2 + trat)

# ANOVA (experimento com 2 blocos, efeitos fixos, 1 fator de trat) QUADRADO LATINO com replicação
#usado para aumentar número de obs., poder estimar sigma2 melhor
#SQbloco = SQreplica + SQbloco:replica

mod = aov(y ~ bloco1 + bloco2 + replica + trat) #caso 1: blocos são os mesmos nas replicações
mod = aov(y ~ bloco1 + bloco2:replica + replica + trat) #caso 2: um dos blocos mudam nas replicações (aninhado/nested)
mod = aov(y ~ bloco1:replica + bloco2:replica + replica + trat) #caso 3: ambos blocos mudam nas replicações (aninhado/nested)

# ANOVA (experimento com 3 blocos, efeitos fixos, 1 fator de trat) QUADRADO GRECO LATINO
require(agricolae); design.graeco(trat,bloco3,serie=2) #aleatorização do quadrado (serie 1 ou 2)
mod = aov(y ~ bloco1 + bloco2 + bloco3 + trat)

# ANOVA (experimento completamente aleatorizado, efeitos fixos, 2 fatores de trat)
require(agricolae); design.ab(..., design="crd")
interaction.plot(trat1, trat2, y)
mod = aov(y ~ trat1*trat2)
require(lsmeans); m = lsmeans(mod, ~trat1|trat2) #média e IC de cada grupo (pode inverter os trat)
require(lsmeans); contrast(m, ...) #comp. múltiplas (pode inverter os trat)
require(dae); tukey.1df(mod, dados) #teste para interação (Ho: não interação) 

# ANOVA (experimento completamente aleatorizado, efeitos fixos, 3 fatores de trat)
interaction.plot(trat1, trat2, y) #tem que filtrar o trat3
mod = aov(y ~ trat1*trat2*trat3)
mod = aov(y ~ trat1*trat2*trat3 - trat1:trat2:trat3) #sem a interação entre os 3, por exemplo
require(lsmeans); m = lsmeans(mod, ~trat1|trat2|trat3) #média e IC de cada grupo (pode inverter os trat)
require(lsmeans); contrast(m, ...) #comp. múltiplas (pode inverter os trat)
require(dae); tukey.1df(mod, dados) #teste para interação (Ho: não interação)

# ANOVA (experimento completamente aleatorizado, efeitos aleatórios, 2 fatores de trat)
mod = aov(y~Error(factor(trat1)+factor(trat2)))
summary(mod)
#calcular variâncias na mão, através das fórmulas

# ANOVA (experimento completamente aleatorizado, efeitos mistos, 2 fatores de trat)
#trat1 fixo, trat2 aleatório
#não diferencia modelo restrito e irrestrito

require(nlme)
mod = lme(y ~ trat1, random=~1|trat2, data=dados, method="REML")

# ANCOVA (análise de covariância)
#fator de perturbação conhecido e não controlável = covariável contínua
#não pode ter interação entre cov1 e trat1, para checar só testar a sig. ou fazer um gráfico

plot(y, cov1, col=trat1) #ver relação entre y e cov1 (tem que ser linear)
mod = aov(y ~ cov1 + trat1)

# MODELO HIERÁRQUICO
#níveis de um tratamento estão aninhados nos níveis de outro tratamento
#trat2 dentro de trat1

mod = aov(y ~ trat1/trat2) #trat1, trat2 fixos
mod = aov(y ~ Error(trat1/trat2)) #trat1, trat2 aleatórios 
mod = aov(y ~ trat1 + Error(trat1:trat2)) #trat1 fixo, trat2 aleatório

# Inferências #
anova(mod)
summary(mod)

mod$coefficients #ver se eles são grandes ou pequenos
model.tables(mod, "means") #média dos grupos
model.tables(mod, "effects") #efeito dos grupos
sigma(mod)^2 #variância do modelo

require(lsmeans); lsmeans(mod, ~trat) #IC de cada grupo/tratamento
; summary(contrast(lsmtemp, list(C1=c(1,1,-1,-1)), infer=c(T,T), level=0.95, side="two.sided") #outros contrastes
          
# Comparações múltiplas #
#modelo tem que estar válido antes de comparar
contrast(lsmeans(mod, ~trat), method="pairwise", adjust="bonferroni", infer=c(T,T),level=0.95, side="two-sided") #poucas comp.
contrast(lsmeans(mod, ~trat1|trat2), method="pairwise", adjust="tukey", infer=c(T,T),level=0.95, side="two-sided") #melhor balanceado (n iguais)
#quando tem interacao, fixa um trat e ve o outro, não importa a ordem

# Modelo de regressão análogo #
anova(lm(y ~ ..., data=dados))

# Análise de Resíduos #

# Eij ~iid~ N(0,sigma2) e não interação entre bloco e tratamento

qqnorm(residuals(mod), ylab="Resíduos", xlab="Quantis teóricos", main="QQ Plot"); qqline(residuals(mod))
shapiro.test(residuals(mod))
plot(fitted(mod), residuals(mod), xlab="Valores Ajustados", ylab="Resíduos"); abline(h=0)
require(car); leveneTest(mod) #Ho: homocedasticidade
plot(1:length(residuals(mod)), residuals(mod), xlab="Tempo", ylab="Resíduos"); abline(h=0) #tem que ser medido ao longo do tempo?
interaction.plot(trat, bloco, y) #interação bloco e trat
require(lmtest); dwtest(mod) #Durbin Watson para independência (Ho: independente)
plot.default(trat, y) #plot residuo x cada fator -> ver qual fator está ferrando com a variância do modelo

# Corrigindo violações #
# Não normalidade / N<10: transformação na variável (?), métodos NP ou outros modelos
# Heterocedasticidade: transformação na variável resposta, MQP ou outros modelos
# Independência: modelos mais complexos, ex: ANCOVA, ANOVA para dados pareados
# Interação: outro modelo (?)

# Correção para heterocedasticidade #

y = sqrt(y) #Var(ei) cresce proporcionalmente a Xi  (tem que ser 1:1)
y = log(y) #Var(ei) cresce proporcionalmente a Xi^2
y = asin(y) # y ~ bin

# Outliers #
# descobrir origem, tentar corrigir / avaliar o efeito / mudar dist.
det.outlier(mod) #(função separada!)

# Poder ou outro parâmetro #
power.anova.test(groups = a, n = ni, between.var = var(groupmeans=c(10,5,7,...), within.var = sigma(mod)^2, sig.level = 0.05, power = )
                           