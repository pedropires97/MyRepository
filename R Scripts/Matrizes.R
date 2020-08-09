### Script para trabalhar com matrizes ###

x <- matrix(data, nrow, ncol, byrow)
as.matrix() #transforma em matriz

det(x) #determinante
x %*% y #multiplicação de matriz
t(x) #transposta
solve(x) #inversa
eigen(x) #autovalores e autovetores
solve(A,b) #resolve sistema, retorna x, b = Ax
sum(diag(x)) #traço
diag(x) #tira a diagonal principal ou cria uma matriz só com a diagonal
diag(n) #cria uma matriz identidade de tamanho nxn

#escalonamento
require(pracma)
rref(x)
require(matlib)
echelon(x, verbose=T) #passo a passo

#posto
require(Matrix)
rankMatrix(x)[1]

#decomposição matriz
chol(x) #Cholesky A=LL'
svd(x) #decomposição em valores singulares
require(matrixcalc); lu.decomposition(x) #LU
(eigen(x)$vectors) %*% diag(eigen(x)$values) %*% t(eigen(x)$vectors) #Spectral

rowMeans(x) #médias das linhas
colSums(x) #somas das colunas

# x^n => fazer uma função

# det=0 <=> LD <=> não tem inversa <=> linha de zero