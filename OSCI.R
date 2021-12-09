library(deSolve)
# ESPECIFICACION DEL MODELO
# Guardemos los parámetros como vector
parameters <- c(b = 0.0013, c = 1, mu = 0.022, v = 0.005, T = 205, R = 50)

# VARIABLES DE ESTADO
# Guardamos las variables, con su valor inicial, como vector
state <- c(W = 1, Q = 0)

# MODELADO DE LA ECUACIONES
Osci <- function(t,state, parameters){
  with(as.list(c(state, parameters)),{
    # taza de cambio
    dW <- (b * (1-(2*t/205)) * R * W) - (mu * W)
    dQ <- (b * c * (1 - (1-(2*t/205))) * R * W) - (v * Q) 
    
    # Retorna tazas de cambio
    list(c(dW, dQ))
    })
}

# APLICACIÓN DEL MODELO
# Especificamos el tiempo
times <-seq(0,205)

# Integramos el modelo
out <- ode(y = state, times = times, func = Osci, parms = parameters)
head(out)

# Graficando los resultados
par(oma = c(0,0,3,0))
plot(out, xlab = "Días de la temporada", ylab = "Producción de insectos")
mtext(outer = TRUE, side = 3, "Modelo OSCI", cex = 1.5)

# Con el método de Euler
oute <- ode(y = state, times = times, func = Osci, parms = parameters, method = "euler", hini = 0.01)
plot(oute)


