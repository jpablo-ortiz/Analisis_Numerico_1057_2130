install.packages("pracma")
library(pracma)

fp <- function(x) { 1/(1-x) }
print(fp)

x <- 0:4 
print(x)

y <- fp(x)
print(y)

xx <- linspace(0, 4, 51)
print(xx)

yy <- lagrangeInterp(x, y, xx)
print(yy)
yy <- newtonInterp(x, y, xx)
print(yy)

## Not run: 
ezplot(fp, 0, 4)
points(xx, yy)
## End(Not run)

# a function that returns a function (like ssplinefun) but with the lagrange interpolation
lagrangeInterpFun <- function(x, y) {
  function(xx) {
    lagrangeInterp(x, y, xx)
  }
}


plot(x, y, type='l', ylab = "Temperatura interna", xlab = "Indice", col= "black")

lines(spline(xx, yy, n=tamArch1), col= "purple")

lines(lagrangeInterp(x, y, xx), col= "green")


yy <- lagrangeInterp(x, y, xx)
yy <- newtonInterp(x, y, xx)
cubicspline(x, y, xx)


# EXAMPLE  para funcion
sample_i = sample(720, round(720*0.7))
sample_i <- append(sample_i, 1, 0) # Agregar al inicio
sample_i <- append(sample_i, 1) # Agregar al final

print(sample_i)

print(sample_i[1])

library(Metrics)
actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
# Error Absoluto
errorAbsoluto = ae(actual, predicted)
print(errorAbsoluto)

# Error Media
ErrorMedia = mae(actual, predicted)
print(ErrorMedia)

# Error Máximo
maxError = max(errorAbsoluto)
print(maxError)

# Error Mínimo
minError = min(errorAbsoluto)
print(minError)

# Error medio cuadrado (EMC)
EMC = mse(actual, predicted)
print(EMC)

# Índice de Jaccard
#install.packages('philentropy')
library(philentropy)
jaccard(actual, predicted, testNA = FALSE)