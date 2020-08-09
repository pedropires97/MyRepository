### Script para plotar funções / cálculo ###

# Plotar uma função #
x <- seq(-10, 10, 0.1)  #eixo x
y <-                    #escrever f(x) no eixo y
plot(x,y,type="l")
abline(h=0,v=0)

# Somatório e produtório #
i <- 0:n #índice
x <- f(i) #função
sum(x) #somatório
prod(x) #produtório

# Algumas funções #

sqrt(x) #raiz
cos(x)
sin(x)
tan(x)
log(x, base) #base default = e
log10(x) #base 10
exp(x) #e^x
factorial(x) #x!

pi = pi
e = exp(1)

ceiling(x) #arredondar para cima
floor(x) #arredondar para baixo
trunc(x) #tira o decimal
round(x, digits=n) #arredondar
abs(x) #módulo

integrate(f, l1, l2)
require(pracma); integral(f, l1, l2)
deriv(~formula, "x")

require(numbers)
mGCD(x) #mdc, vetor
mLCM(x) #mmc, vetor
