### Testes paramétricos e IC ###

shapiro.test(x) #teste de normalidade
ks.test(x, rnorm(parâmetros), alternative="two.sided") #teste de normalidade
Ho = normalidade

var.test(x, y, alternative="", conf.level) #teste variância
varTest(x, sigma.squared) #teste para uma variância e IC

#testes seguem dist. normal ou n>30, amostras independentes

#teste para média populacional
require(BSDA); z.test(x, mu, sigma.x) #teste z (var. pop. conhecida)
t.test(x, mu, ...) #teste t (var. pop. desc.)

#comparação de médias
require(BSDA); z.test(z, y, ..., sigma.x, sigma.y) #teste z (var. pop. conhecidas e iguais)
t.test(x, y, ..., var.equal = T) #teste t (var. pop. desc. e igual)
t.test(x, y, ..., var.equal = F) #teste t (var. pop. desc. e diferente)
t.test(x, y, ..., paired = T) #teste t pareado
power.t.test(...) #poder ou outro parâmetro dado o poder para testes t

#testes de proporção
prop.test(success, trials, prob) #teste para uma proporção
prop.test(c(success), c(trials)) #comparando várias proporções

#realizar testes usando informações (estimativas dos parâmetros) e não os dados colhidos
require(BSDA); tsum.test(mean.x, s.x, n.x, mean.y, s.y, n.y, alt, mu = 0, var.equal = F, conf.level)
#1 ou 2 amostras
#ou calcula a estatística de teste no R, usando as fórmulas

#associação de 2 variáveis qualitativas
chisq.test(x) #Qui quadrado ; x = tabela de contingência (matriz)
require(DescTools); ContCoef(x) #coeficiente de contingência (quantifica a associação)
; CramerV(x) #C de Cramér (varia entre 0 e 1)
