#### ggplot2 ####

# visualizacao de dados eh uma ferramenta muito importante
# exploratorio: revela a estrutura dos dados
# explicativo: apresentar de forma consisa a informacao

# baseado camadas/gramatica
# adiciona as camadas com o "+", na linha superior
# pode ser salvo como um objeto para mexer depois

# boas praticas: grafico facil de entender,
# evitar muita informacao, colocar so o essencial
# alinhar eixos, visualizar todos os pontos, jitter se precisar
# diminuir alpha, size
# cuidado plotar axis para valores que nao existem na pratica
# solucao: factor(x)

# data: dados
# aesthetics: variaveis usadas, os argumentos modificam os aspectos das variaveis
# geometries: tipo de gráfico, os argumentos modificam os aspectos em geral
# scale: mexer na aparencia das variaveis (nome, labels, cores, limits)
# stats: adiciona elementos de estatsitca ao grafico (pode ser substituido por geom equivalente)
# coord: mexe nas coordenadas e escala
# facets: varios graficos juntos, em uma mesma escala ou nao
# themes: aparencia de elementos que nao sao dados
# outros: labs, annotate, etc

#### geral ####

require(ggplot2)

# exemplo generico
ggplot(data, aes(x = , y = , ...)) +
  geom_() +
  scale_() +
  stat_() +
  coord_() +
  facet_() +
  theme_()

mtcars = mtcars
iris = iris

#### estrutura dos dados ####

# a estrutura do seu banco de dados depende do grafico que quer montar
# ex: wide ou tidy 
# cria novas variaveis categoricas e junta as numericas

require(tidyr)
require(dplyr)

head(iris)
str(iris)

# wide
iris$Flower <- 1:nrow(iris)

iris.wide <- iris %>%
  gather(key, value, -Species, -Flower) %>%
  separate(key, c("Part", "Measure"), "\\.") %>%
  spread(Measure, value)

# tidy
iris.tidy <- iris %>%
  gather(key, value, - Species) %>%
  separate(key, c("Part", "Measure"), "\\.")

#### aesthetics ####

# x e y
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# destacar 3a variavel
# grupos: color, fill, size, alpha, labels, shape, lines
ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point()

# pode usar mais de uma ao mesmo tempo
#l embrar de alterar no geom, caso necessario
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl), shape = factor(vs))) +
  geom_point()

# cuidado pra nao overwrite
ggplot(mtcars, aes(x=wt, y=mpg, col=cyl))+
  geom_point(col="#4ABEFF")

# muda posicao da barra
ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) +
  geom_bar(position = "dodge")

# univariado: declara a outra var pq eh obrigatoria
ggplot(mtcars, aes(x = 0, y = mpg)) +
  geom_point()

# boas praticas
ggplot(mtcars, aes(x=wt, y=mpg, col=factor(cyl), fill=factor(cyl))) +
  geom_point(size = 4, shape=21, alpha=0.5)

#### geom scatterplot ####

# tem varias geom, saber qual explica melhor os dados

# atributos: modificam tudo
# color, fill, size, alpha, labels, shape, lines
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(shape = 24, col = "yellow")

# shape pode ser qualquer coisa
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(shape = "p", col = "yellow")

# mais de um geom
ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point() +
  geom_smooth()

# aes dentro do geom
# pode usar banco de dados diferentes
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(aes(color=factor(cyl)))

# opacidade
ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point(alpha = 0.5) #muda geral

# jitter
# adiciona um pouco de ruido as observacoes, bom pra variaveis discretas
ggplot(mtcars, aes(x = cyl, y = wt)) +
  geom_jitter(width = 0.1)

# pontos: categorica vs continua
ggplot(mtcars, aes(factor(cyl), wt, fill = factor(am))) +
  geom_dotplot(binaxis = "y", stackdir = "center")

#### geom barras ####

# histograma
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram()

# ajustar tamanho das barras
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 1)

# densidade em y
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(aes(y=..density..))

# outra opcao para y
y = ..count../sum(..count..)

# atalho para geom_bar(stat = "identity")
geom_col()

# barras
ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) +
  geom_bar()

# tamanho das barras
ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) +
  geom_bar(width = 0.5)

# posicao das barras
ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) +
  geom_bar(position = "fill")

# overlap
ggplot(mtcars, aes(x = cyl, fill = factor(am))) +
  geom_bar(position = position_dodge(width = 0.2))

# freqpoly
ggplot(mtcars, aes(mpg, color=cyl)) +
  geom_freqpoly(binwidth = 1, position="identity")

# barra horizontal
+ coord_flip()

#### geom linha / temporal ####

# linha simples
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line()

# com marcacoes no fundo
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_rect(data = recess,
            aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf),
            inherit.aes = FALSE, fill = "red", alpha = 0.2) +
  geom_line()

# separar linhas
ggplot(ChickWeight, aes(x = Time, y = weight)) +
  geom_line(aes(group = Chick))

# com smooth
ggplot(ChickWeight, aes(x = Time, y = weight,color = Diet)) +
  geom_line(aes(group = Chick), alpha=0.3) +
  geom_smooth(lwd=2, se=F)

#### geom boxplot ####

# y continuo
# x pode ser categorico pra varios boxplot
geom_boxplot()

# expessura varia de acordo com tamanho amostral de cada grupo/boxplot
+ geom_boxplot(varwidth = T)

#### geom density ####

+ geom_density()

# linha vertical
+ geom_vline(xintercept = mode, col = "red")

# duas density, de grupos diferentes
ggplot(test_data2, aes(x = value, fill = dist, col = dist)) +
  geom_density(alpha = 0.6)

# violino
+ geom_violin()

#### geom mapas ####

# mapa simples
require(ggmap)
usa <- map_data("usa")
ggplot(usa, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_map() +
  theme_nothing()

# pontos no mapa
# so dar merge com outra tabela com as infos
require(maps)
ggplot(usa, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  geom_point(data = cities, aes(group = State, size = Pop_est),
             col = "red", shape = 16, alpha = 0.6) +
  coord_map() +
  theme_map()

# colorir estado
state <- map_data("state")
ggplot(state, aes(x = long, y = lat, fill = region, group = group)) +
  geom_polygon(col = "white") +
  coord_map() +
  theme_nothing()

# colorir estado 2
state2 <- merge(state, pop)
ggplot(state2, aes(x = long, y = lat, fill = Pop_est, group = group)) +
  geom_polygon(col = "white") +
  coord_map() +
  theme_map()

# dah pra pegar qualquer mapa que quiser
# tem que baixar os arquivos necessarios para tal na internet
# .shp / .shx / .dbf
require(rgdal)
germany <- readOGR(dsn = "shapes", layer = "DEU_adm1")

# pegar mapas de fato, nao choropleths
# precisa de registro no google
london_map_13 <- get_map(location = "London, England", zoom = 13)
ggmap(london_map_13)

# pontos no mapa
ggmap(london_ton_13) +
  geom_point(data=xx, aes(col=location), size=6)

#### outros geom ####

# mosaic plot
require(lattice)
myColors <- brewer.pal(9, "Reds")
ggplot(barley, aes(x = year, y = variety, fill = yield)) +
  geom_tile() +
  scale_fill_gradientn(colors = myColors) 

# mostra os pontos no eixo x
+ geom_rug()

# 2d/cartografico
p <- ggplot(faithful, aes(x = waiting, y = eruptions)) +
  scale_y_continuous(limits = c(1, 5.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(40, 100), expand = c(0, 0)) +
  coord_fixed(60 / 4.5)
p + geom_density_2d()
p + stat_density_2d(aes(col = ..level..), h = c(5, 0.5))

# paleta multi-hue
library(viridis)
ggplot(faithful, aes(x = waiting, y = eruptions)) +
  scale_y_continuous(limits = c(1, 5.5), expand = c(0,0)) +
  scale_x_continuous(limits = c(40, 100), expand = c(0,0)) +
  coord_fixed(60/4.5) +
  stat_density_2d(geom = "tile", aes(fill = ..density..), h=c(5,.5), contour = FALSE) +
  scale_fill_viridis()

# bom para visualizacao high density
+ geom_bin2d()
+ geom_bin2d(bins = 100)
+ geom_hex()
+ geom_hex(bins = 100)

# ternery plot
# pra tres eixos continuos
library(ggtern)
ggtern(africa, aes(x = Sand, y = Silt, z = Clay)) +
  geom_point(shape = 16, alpha = 0.2)

# tern cartografico
ggtern(africa, aes(x = Sand, y = Silt, z = Clay)) +
  geom_density_tern()

# tern calor
ggtern(africa, aes(x = Sand, y = Silt, z = Clay)) +
  stat_density_tern(geom = "polygon", aes(fill = ..level.., alpha = ..level..)) +
  guides(fill = FALSE)

# network
require(geomnet)
ggplot(data = mmnet, aes(from_id = Name1, to_id = Name2)) +
  geom_net()

#### scale ####

# camada que modifica a aparencia das variaveis
# cada variavel do aes tem sua propria camada
# tem que falar qual (x, y, fill...)
# e o tipo (discrete, continuous...)
# da pra mudar nome, labels, cores, limits

require(RColorBrewer)

val = c("#E41A1C", "#377EB8")
lab = c("Manual", "Automatic")
ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) +
  geom_bar(position = "dodge") +
  scale_x_discrete("Cylinders") + 
  scale_y_continuous("Number", limits = c(0,15)) +
  scale_fill_manual("Transmission", 
                    values = val,
                    labels = lab)

# limits
# statistcs calculada depois do zoom

# statistics calculada depois da transformacao
+ scale_y_log10()

# se quiser mexer so nos nomes
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(position = "jitter") +
  labs(x = "Sepal Length", y = "Sepal Width", col = "Species")

# brewer: paleta de cores
?brewer.pal
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1")

# criar paleta de cores hexa
new_col <- colorRampPalette(c("#FFFFFF", "#0000FF"))
new_col(4) #numero de cores
munsell::plot_hex(new_col(4))

# extrapolar 
blues <- brewer.pal(9, "Blues") #vetor gerado com 9 cores
blue_range <- colorRampPalette(blues) #joga na funcao
ggplot(Vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = blue_range(11)) #preciso de 11 cores

+ scale_color_brewer() 
+ scale_color_gradientn()
+ scale_size()

# cria uma variavel com as cores #aaaaa e usa ela dentro do col
+ scale_color_identity()

#### qplot ####

# funcao para plotar graficos mais simples
# eh mais rapido, mas tem menos opcao tbm
qplot(wt, mpg, data = mtcars)

# especificando o geom
qplot(x=factor(cyl), y=factor(vs), data=mtcars, geom="jitter")

#### stats ####

# adiciona elementos de estatsitca ao grafico
# retas, curvas, estimativas, IC...
# pode ser geom_... ou stat_...

# adcionando uma linha; lm, loess
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = 'lm', se=F)

# lm grupo e geral
myColors <- c(brewer.pal(3, "Dark2"), "black")
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(inherit.aes = F, aes(x = wt, y = mpg, col="All"), method = "lm", se = FALSE) +
  scale_color_manual("Cylinders", values = myColors)

# plotar curva por cima
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, colour ='red',
                args = list(mean = mean(mtcars$mpg), sd = sd(mtcars$mpg)))

# qqplot
mtcars$slope <- diff(quantile(mtcars$mpg, c(0.25, 0.75))) / diff(qnorm(c(0.25, 0.75)))

mtcars$int <- quantile(mtcars$mpg, 0.25) - mtcars$slope * qnorm(0.25)

ggplot(mtcars, aes(sample = mpg)) +
  stat_qq() +
  geom_abline(aes(slope = slope, intercept = int), col = "red")

# summary
# fun.y: funcao so pra y
# fun.data: funcao tem que retorna tres valores
# fun.args: argumentos adicionais da funcao, alem do aes

ggplot(mtcars, aes(x = cyl,y = wt, col = am, fill = am, group = am)) +
  stat_summary(geom = "point", fun.y = median,
               size = 3, col = "black", shape = "X")

ggplot(mtcars, aes(x = cyl,y = wt, col = am, fill = am, group = am)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=1))

ggplot(mtcars, aes(x = cyl,y = wt, col = am, fill = am, group = am)) +
  stat_summary(fun.data = mean_cl_normal)

ggplot(mtcars, aes(x = cyl,y = wt, col = am, fill = am, group = am)) +
  stat_summary(geom="errorbar", fun.data = mean_sdl, fun.args = list(mult = 1), width=0.1)  

# exemplos de funcoes customizaveis
gg_range <- function(x) {
  data.frame(ymin = min(x),
             ymax = max(x))
}

med_IQR <- function(x) {
  data.frame(y = median(x),
             ymin = quantile(x)[2],
             ymax = quantile(x)[4])
}

#### coord ####

# zoom
# statistics calculada antes do zoom
ggplot(mtcars, aes(x = wt, y = hp, col = factor(am))) +
  geom_point() +
  geom_smooth() +
  coord_cartesian(xlim = c(3, 6))

# statistics calculada antes da transformacao
+ coord_trans(y = "log10")

# escala
# forca escala para 1:1, bom quando variaveis estao na mesma medida
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  coord_equal()

# pizza
ggplot(mtcars, aes(x = 1, fill = factor(cyl))) +
  geom_bar() +
  coord_polar(theta = "y")

# circulo
ggplot(mtcars, aes(x = 1, fill = factor(cyl))) +
  geom_bar(width = 0.1) +
  scale_x_continuous(limits = c(0.5,1.5)) + 
  coord_polar(theta = "y")

#### themes ####

# elementos que nao sao dados

# forma geral
# objeto: text, line, rect
# element: _text(), _line(), _rect(), _blank()
# args: size, color, position
+ theme(objeto = element(args))

# retangulos
+ theme(plot.background = element_rect(fill = myPink, color="black", size=3))
+ theme(rect = element_blank())

# linhas
+ theme(panel.grid = element_blank(),
        axis.line = element_line(color="red"),
        axis.ticks = element_line(color="red"))
# texto
+ theme(strip.text = element_text(size = 16, color = myRed),
        axis.title = element_text(color = myRed, hjust = 0, face = "italic"),
        axis.text = element_text(color = "black"))

# legenda
+ theme(legend.position = c(0.85, 0.85))
+ theme(legend.direction = "horizontal")
+ theme(legend.position = "bottom")
+ theme(legend.position = "none")

# espacamento entre figuras
require(grid)
+ theme(panel.spacing.x = unit(2, "cm"),
        plot.margin = unit(c(1,2,1,1), "cm"))

# salvar tema especifico em um objeto e usa-lo 
meu_tema <- theme(rect = element_blank())
p + meu_tema

# usar um tema pre determinado do ggplot2
p + theme_bw()

# usar um tema pre determinado do ggthemes
require(ggthemes)
p + theme_tufte()

# setar um tema como padrao
theme_set(theme_bw())

# voltar tema padrao
theme_set(theme_grey())

#### texto e figuras ####

# observacoes como texto
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
  geom_text()

# observacoes como label
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
  geom_label()

# add texto avulso
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p + annotate("text", x = 4, y = 25, label = "Some text")

# add forma avulsa
p + annotate("rect", xmin = 3, xmax = 4.2, ymin = 12, ymax = 21, alpha = .2)

# add linha avulsa
p + annotate("segment", x = 2.5, xend = 4, y = 15, yend = 25, colour = "blue")

# titulo
+ ggtitle()

#### facets ####

# graficos por variavel categorica
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

p + facet_grid(am ~ .)
p + facet_grid(. ~ cyl)
p + facet_grid(am ~ cyl)
#ROWS ~ COL

# deixar algumas linhas vazias de fora, pra suavizar visualizacao
# nao tem o dataset
ggplot(mamsleep, aes(x = time, y = name, col = sleep)) +
  geom_point() +
  facet_grid(vore ~ ., scale = "free_y", space = "free_y")

# facet_wrap
require(lattice)
ggplot(barley, aes(x = year, y = variety, fill = yield)) +
  geom_tile() + # Geom layer
  facet_wrap( ~ site, ncol = 1)

# facet pelo em col/row do grupo
+ facet_wrap( ~ vore, nrow = 2)

# combinar varios graficos
require(gridExtra)
gride.arrange(p1, p2, nrow = , ncol = )

#### grid package ####

# pacote serve para fazer alteracoes em graficos existentes
# add elementos soltos
# criar graficos do zero
# da pra criar uma legenda personalizada por exemplo

# add elementos
grid.rect(gp = gpar(fill = "grey90"))
grid.text("null viewport")
grid.lines(x = c(0, 0.75), y = c(0.25, 1),
           gp = gpar(lty = 2, col = "red"))

p <- ggplot()
names(p)

# editar um plot
p2 <- p1 + 
  theme(legend.position = "none")

#### outros ####

# transforma base em ggplot2
library(ggfortify)
autoplot(res, ncol = 2)
# tbm serve pra time series

# multi
iris_k <- kmeans(iris[-5], 3)
autoplot(iris_k, data = iris, frame = TRUE)
autoplot(iris_k, data = iris, frame = TRUE, shape = 'Species')

# SPOM
# nao eh ggplot2
pairs(iris[1:4])

library(PerformanceAnalytics)
chart.Correlation(iris[1:4])

library(GGally)
ggpairs(mtcars_fact[1:3])

# salvar plot como arquivo
ggsave(filename = "nome.ext", plot = , width = , height = )

#### visualizacoes avancadas ####

# animated
require(gganimate)
theme_set(theme_bw())
require(gapminder)
require(gifski)

# dot
p <- ggplot(gapminder, aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")

p2 = p + transition_time(year) +
  labs(title = "Year: {frame_time}")

# lines
p <- ggplot(airquality, aes(Day, Temp, group = Month, color = factor(Month))) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")

p2 <- p + geom_point() +
  transition_reveal(Day)

# visualizar
animate(p2, renderer = gifski_renderer())

# salvar ultima animacao
anim_save(filename = "output.gif")

# grafico interativo
# show in new window

require(plotly)
require(htmlwidgets)

p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

ggplotly(p)

p <- ggplot(mtcars, aes(x = wt, y = mpg,
                        text = paste("teste"))) +
  geom_point()

ggplotly(p)

# 3d
# pacote rayshader

# montar ggplot no mouse
install.packages("esquisse")
esquisse::esquisser()
