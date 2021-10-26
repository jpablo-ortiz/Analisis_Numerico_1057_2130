# --------------------------------------------------
# ---------------- Instalar paquetes ---------------
# --------------------------------------------------

# install.packages("pracma")
# install.packages("readxl")
# install.packages("Metrics")
# install.packages('philentropy')

# --------------------------------------------------
# -------------- Importar librer�as ----------------

library(pracma)
library(readxl)
library(Metrics)
library(philentropy)

# --------------------------------------------------
# --------------- Variables Globales ---------------
# --------------------------------------------------

minX = min(0)
maxX = max(720)
minY = min(0)
maxY = max(50)

# --------------------------------------------------
# ---------------- Funciones -----------------------
# --------------------------------------------------

funInterpGenerator <- function(pp, xs) {
    ppfun <- function(xs) ppval(pp, xs)
    return(ppfun)
}

errores() <- function(actual, predicted) {
    # actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
    # predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
    
    # Error Absoluto
    errorAbsoluto = ae(actual, predicted)
    print(errorAbsoluto)

    # Error Media
    ErrorMedia = mae(actual, predicted)
    print(ErrorMedia)

    # Error M�ximo
    maxError = max(errorAbsoluto)
    print(maxError)

    # Error M�nimo
    minError = min(errorAbsoluto)
    print(minError)

    # Error medio cuadrado (EMC)
    EMC = mse(actual, predicted)
    print(EMC)

    # Indice de Jaccard
    IndiceJaccard = jaccard(actual, predicted, testNA = FALSE)
}

cicloDeInterpolaciones <- function(x, y, xTrain, yTrain) {
    plot(x, y, ylab = "Temperatura interna", xlab = "Indice", col= "black") 
    #        ,xlim = c(minX, maxX), ylim = c(minY, maxY))
    
    # Impresi�n de los datos originales}
    lines(x, y, col = "black")

    # Interpolaci�n lineal
    # (Los approx tienen la funci�n approxfun())
    line0 = approx(xTrain, yTrain)
    #lines(line0, col = "purple")
    
    # Interpolaci�n Polinomial
    # (Los spline tienen los splinefun())
    
    # M�todo FMM (fast marching method) (m�todo de marcha r�pida)
    # Este m�todo utiliza Forsythe, Malcolm y Moler. Se ajusta un un c�bico exacto
    line1 = spline(xTrain, yTrain, method = "fmm")
    #lines(line1, col = "red")
    
    # M�todo Spline Periodic
    # TODO - Creo que no se puede porque no es periodico los datos
    line2 = spline(xTrain, yTrain, method = "periodic")
    #lines(line2, col = "blue")

    # M�todo Spline Natural
    line3 = spline(xTrain, yTrain, method = "natural")
    #lines(line3, col = "green")
    
    # M�todo Spline MonoH.FC
    # Calcula un spline de Hermite monot�nico (creciente o decreciente) seg�n el m�todo de Fritsh y Carlson.
    #line4 = spline(xTrain, yTrain, method = "monoH.FC")
    ###lines(line4, col = "yellow")
    
    # M�todo Spline Hyman
    # Calcula un spline c�bico mon�tono usando el filtrado de Hyman.
    #line5 = spline(xTrain, yTrain, method = "hyman")
    ###lines(line5, col = "orange")

    # M�todo Barycentrico (barycentric lagrange) de interpolaci�n
    #line6 = barylag(xTrain, yTrain, xTrain)
    ##lines(line6, col = "purple")
    
    # M�todo spline c�bico de interpolaci�n natural.
    #line7 = cubicspline(xTrain, yTrain, xTrain)
    ###lines(line7, col = "brown")

    # M�todo Lagrange de interpolaci�n
    #line8 = lagrangeInterp(xTrain, yTrain, xTrain)
    #lines(line8, col = "grey")
    
    # M�todo de interpolaci�n de Newton
    #line9 = newtonInterp(xTrain, yTrain, x)
    ##lines(line9, col = "pink")
}

# cicloDeUnaEstacion() <- function() {}

# --------------------------------------------------
# ------------ Cargar datos Principales ------------
# --------------------------------------------------

procesamientoDosEstaciones("Itatira", "Santa Quit�ria")
procesamientoDosEstaciones("Pentecoste", "S�o Gon�alo do Amarante")
procesamientoDosEstaciones("Quixad�", "quixeramobim")

procesamientoDosEstaciones(estacion1, estacion2) <- function(estacion1, estacion2) {

    arch1 = read_excel("D:/Juanpa/U/Semestre 8/Analisis Numerico/Analisis_Numerico_1057_2130/Reto Parcial 2/DatosReto2.xls", sheet = estacion1)
    arch2 = read_excel("D:/Juanpa/U/Semestre 8/Analisis Numerico/Analisis_Numerico_1057_2130/Reto Parcial 2/DatosReto2.xls", sheet = estacion2)

    tempInteranArch1 = arch1$`Temp. Interna (�C)`
    diasArch1 = arch1$`Dia Juliano`
    horasArch1 = arch1$Hora

    tempInteranArch2 = arch2$`Temp. Interna (�C)`
    diasArch2 = arch2$`Dia Juliano`
    horasArch2 = arch2$Hora

    tamArch1 = length(tempInteranArch1)
    tamArch2 = length(tempInteranArch2)

    x = seq(from = 1, to = 720, by = 1)
    y = tempInteranArch1

    # Seleccionar un 70% aleatorio de los datos para el entrenamiento 
    sample_i = sample(720, round(720*0.7))

    # XTrain y YTrain el 70% de los datos
    # Se toma el primero y el �ltimo valor de X y Y para manejar una correcta interpolaci�n
    xTrain = x[sample_i]
    xTrain = append(xTrain, x[0], 0) # Agregar el primer valor de x (al inicio)
    xTrain = append(xTrain, x[length(x)]) # Agregar el ultimo valor de x (al final)

    yTrain = y[sample_i]
    yTrain = append(yTrain, y[0], 0) # Agregar el primer valor de y (al inicio)
    yTrain = append(yTrain, y[length(y)]) # Agregar el ultimo valor de y (al final)

}

# El calculo del error se hace con base al 30% de los datos y de estos es los datos reales y los datos predecidos