### Testes não paramétricos e outros testes ###

# (uma opção antes dos testes não paramétricos é transformar os dados para normais e usar NP só se ainda precisar)

# Comparação #
# Não requer normalidade; amostra pequena

#comparação estocástica, mediana; k=2
require(exactRankTests)
wilcox.exact(x,y,...) #Wilcoxon para grupos independentes (Mann-Whitney)
wilcox.exact(x,y,paired=T,...) #Wilcoxon para grupos pareados (postos sinalizados)
wilcox.exact(x, mu, paired=T) #Wilcoxon para uma amostra
binom.test(success,trials,...) #Teste do sinal (success = +)
ks.test(x, y, alternative="greter/less") #kolmogorov-smirnov

#comparação estocástica, mediana; k>2
kruskal.test(x~y) #df de 2 colunas, x=dados, y=grupos (fatores)
dunn.test(x,y,method="bonferroni") ou pairwise.wilcox.test(...,p.adjust.method="b") #comparações múltiplas (melhor usar alpha=0.1)

friedman.test(x) #x = matriz com as colunas sendo os grupos e as linhas sendo os blocos (dados pareados; "anova np de 2 fatores")
require(PMCMR); posthoc.friedman.conover.test(x, p.adjust.method="b") #value = matriz (1st linha = 2nd coluna de x)
#pareado = grupos tem que ter mesmo tamanho

#comparação de proporções pareadas (blocos); k>2
require(RVAideMemoire); cochran.qtest(x,p.method="b") #compara proporções, reposta 0 ou 1; similar ao Friedman
#já faz comparações múltiplas
#x=modelo...(?)

#comparação de variabilidade
require(DescTools); SiegelTukeyTest(x,y,...) #Teste de Siegel e Tukey
require(lawstat); levene.test(x,y) # x=dados, y=grupos (fatores)


# Concordância #

#Quantitativa k=2
cor.test(x, y, method="pearson", ...) 
cor.test(x, y, method="kendall", ...) #NP
cor.test(x, y, method="spearman", ...) #NP

#Quantitativa k>2
cor(df, method="") #apenas os coeficientes (não faz o teste de sig.)
require(psych); cortest.bartlett(r, n=...) #teste de pearson generalizado; r = matriz de correlação
require(Hmisc); rcorr(matrix(c(x1,x2,...),...)) #coef. e teste; só pearson e spearman
require(corrplot); corrplot(r) #plot das correlações

#Qualitativa k=2
chisq.test(x) #Qui quadrado ; x = tabela de contingência (matriz) #assumption: eij > 5 
require(DescTools); ContCoef(x) #coeficiente de contingência (quantifica a associação)
; CramerV(x) #C de Cramér (varia entre 0 e 1)
fisher.test(x,...) #Teste Exato de Fisher (indicado para tabelas 2x2, amostra pequena, eij<5 para algum ij)
mcnemar.test(x,...) #McNemar (2x2, mesmas categorias, pouco útil)
require(irr); kappa2(x) #x = matrix n*2 (dados brutos); compara dois "raters"; mesmas categorias

#Qualitativa k>2
require(irr); kappam.fleiss(x) #x = matrix n*k (dados brutos)


# Amostra aleatória #
require(tseries); runs.test(as.factor(x),alternative) #Teste das corridas
#(t = geral, g = violação da indep., l = violação id)
#variável categórica (2)
require(snpar); runs.test(x,...) #(?) ; para variável quantitativa


# Bondade de ajuste #

#Ho = segue uma dist(param) / tem a mesma dist
#se não tiver nenhuma informação, estima os parâmetros (média, var, p,...)
ks.test(x, rdist(parâmetros), alternative="two.sided") #kolmogorov-smirnov
ks.test(x, y, alternative="two.sided") #testa se as amostras vem de uma mesma dist