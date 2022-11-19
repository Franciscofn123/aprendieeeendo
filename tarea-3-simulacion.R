#######tarea 3


#1
#a
#se definen la posteriori en el informe

#b
set.seed(2022)
logis <- rlogis(n = 20, location = 5, scale =6)
#como location=5, entonces la media es 5
# y como scale=6, entonces la desviacion estandar es 6




#c

l <- function(alpha,beta){
  y <- -n*log(6)+((sum(x)-alpha*n)/beta) -2*sum(log(1+exp((x-alpha)/beta)))    
  return(y)
  }
c0 <- c(1,2)
f_aux <- function(c0)
{
  return(-l(c0[1], c0[2]))
}

minimo <- optim(c0, f_aux, hessian = T)
minimo <- nlm(f = f_aux, p = c0, print.level = 2, hessian = T)

#Definimos x*
x_opt <- minimo$par

#Definimos el hessiano H*
H_opt <- -minimo$hessian

#d



#Usaremoss la librería mvtnorm para poder utilizar normales multivariadas
library(mvtnorm)


#Definimos la posteriori objetivo: f(x)
f <- function(alpha, beta)
{
  #Definimos la función que calculamos en (b)
  exp(l(alpha, beta)) * dmvnorm(c(alpha, beta), m, V)
}


#Definimos g(x)
g <- function(alpha, beta)
{
  #Hacemos la normal con los parámetros calculados mu y S.
  dmvnorm(c(alpha, beta), mu, S)
}


#Definimos la división entre f y g (en escala logarítmica)
h <- function(theta)
{
  #Hacemos la división f / g
  f(theta[1], theta[2]) / g(theta[1], theta[2])
  
  #log(f(theta[1], theta[2])) - log(g(theta[1], theta[2]))
}


#Calculamos el óptimo, que sería evaluar f(x_opt) / g(x_opt)
c <- h(x_opt)


#Realizamos el algoritmo como lo hemos hecho siempre:

#Definimos la función que recibe la cantidad de iteraciones
acept_rech <- function(n, c)
{
  #Definimos un contador para guardar la cantidad de valores aceptados
  aceptados <- 0
  
  #y otro para guardar los valores rechazados
  rechazados <- 0
  
  #También creamos un vector para guardar los valores aceptados
  simulaciones <- matrix(nrow = n, ncol = 2)
  
  #Hacemos un contador para las filass de la matriz
  contador <- 0
  
  #Realizamos el método:
  
  #Iteramos hasta que tengamos las muestras requeridas
  while(all(is.na(simulaciones[n, ]))){
    
    #Simulamos un valoir de la distribución de propuestas
    y <- rmvnorm(1, mu, S)
    
    #Generamos un valor desde la distribución uniforme
    u <- runif(1)
    
    #Revisamos si es que la uniforme es menor o no
    if (u < h(y)*(1/c)){
      #Sumamos 1 al contador
      contador <- contador + 1
      
      #Si es que es menor, entonces el valor se acepta y sumamos 1 a los aceptados
      aceptados <- aceptados + 1
      
      #Guardamos el valor
      simulaciones[contador, ] <- y
    }
    else
    {
      #Si es que no es menor, entonces no agregamos el valor y sumamos 1 a los rechazados
      rechazados <- rechazados + 1
    }
  }
  
  #Calculamos la tasa de aprobación
  tasa_aceptados <- aceptados / (aceptados + rechazados)
  
  #Lo pasamos a porcentaje
  porceentaje_aceptados <- paste0(round(tasa_aceptados * 100, 2), "%")
  
  #Mostramos el porcentaje de aceptados:
  print(sprintf("La tasa de aprobación fue de: %s", porceentaje_aceptados))
  
  #Retornamos la lista pedida
  return(list(muestra = simulaciones, tasa_aprobacion = tasa_aceptados))
}


#Corremos el algoritmo y guardamos la lista de resultados.
lista_res <- acept_rech(10000, c)

#Guardamos la muestra
muestra <- lista_res$muestra

#Guardamos la tasa de aceptación
tasa_acep <- lista_res$tasa_aprobacion


#e

#con mu=(8,8)


m <- matrix(c(8, 8), nrow = 2)
V <- matrix(c(1, 1,
              1, 3), byrow = TRUE, nrow = 2)

#Sigma:
(S <- solve(solve(V) - H_opt / 2))

#Mu:
(mu <- S %*% (solve(V) %*% m - H_opt %*% (x_opt / 2)))



lista_res <- acept_rech(10000, c)

#Guardamos la muestra
muestra <- lista_res$muestra

#Guardamos la tasa de aceptación
tasa_acep <- lista_res$tasa_aprobacion


#con mu=(10,10)


m <- matrix(c(10, 10), nrow = 2)
V <- matrix(c(1, 1,
              1, 3), byrow = TRUE, nrow = 2)

#Sigma:
(S <- solve(solve(V) - H_opt / 2))

#Mu:
(mu <- S %*% (solve(V) %*% m - H_opt %*% (x_opt / 2)))



lista_res <- acept_rech(10000, c)

#Guardamos la muestra
muestra <- lista_res$muestra

#Guardamos la tasa de aceptación
tasa_acep <- lista_res$tasa_aprobacion

#con mu=(12,12)


m <- matrix(c(12, 12), nrow = 2)
V <- matrix(c(1, 1,
              1, 3), byrow = TRUE, nrow = 2)

#Sigma:
(S <- solve(solve(V) - H_opt / 2))

#Mu:
(mu <- S %*% (solve(V) %*% m - H_opt %*% (x_opt / 2)))



lista_res <- acept_rech(10000, c)

#Guardamos la muestra
muestra <- lista_res$muestra

#Guardamos la tasa de aceptación
tasa_acep <- lista_res$tasa_aprobacion





#2----
#a

theta <- function(x){
  aux <- x/(1+exp(x)) 
  return(aux)
}




cambio_variable <- function(x){
  aux <- (25*x+25)/(1+exp(5*x+5))
  return(aux)
}

a <- runif(10000)
monte <- mean(cambio_variable(a))


#b
v <- integrate(theta,5,10)$value

#c


fy <- function(y){
  return(exp(-1/2) / (1 + y^2))
}

fz <- function(z){
  return(z + 1)
}
#primero tomando como variable de control Y
U <- runif(10^5)
X <- cambio_variable(U)
Y <- fy(U)

c_y <- -cov(X,Y) / var(Y)

z1 <- X + c_y*(Y - mean(Y))

#Calculamos la simulación mediante la variable de control
(est_y <- mean(z1)) 


#ahora se prueba con z como variable de control



U <- runif(10^5)
X <- cambio_variable(U)
Z <- fz(U)

c_z <- -cov(X,Z) / var(Z)

z2 <- X + c_z*(Z - mean(Z))

#Calculamos la simulación mediante la variable de control
(est_z <- mean(z2)) 


#reduccion de varianza

r_y <- 100*cor(X,Y)^2
r_z <- 100*cor(X,Z)^2

#con z se reduce mucho mas la varianza, puesto que, esta muy correlacionada con X



#d

(error_monte <- 100 * abs(monte - v) / v)
(error_y <- 100 * abs(est_y - v) / v)
(error_z <- 100 * abs(est_z - v) / v)

#e ?






#f
m <- NULL

for (i in 1:1000){
  a <- runif(10000)
  monte <- mean(cambio_variable(a))
  m[i] <- monte
}
m

q <- NULL

for (i in 1:1000){
  U <- runif(10^5)
  X <- cambio_variable(U)
  Y <- fy(U)
  
  c_y <- -cov(X,Y) / var(Y)
  
  z1 <- X + c_y*(Y - mean(Y))
  
  #Calculamos la simulación mediante la variable de control
  (est_y <- mean(z1)) 
  q[i] <- est_y
}
q

p <- NULL
for (i in 1:1000){
  U <- runif(10^5)
  X <- cambio_variable(U)
  Z <- fz(U)
  
  c_z <- -cov(X,Z) / var(Z)
  
  z2 <- X + c_z*(Z - mean(Z))
  
  #Calculamos la simulación mediante la variable de control
  (est_z <- mean(z2)) 
  p[i] <- est_z
}
p










plot(m, type = "l", col = "skyblue", lwd = 4,
     xlab = "Cantidad de estimaciones", ylab = "Valores theta",
     main = "Monte Carlo vs Variable de Control")

# Agregamos a la figura las estimaciones de theta utilizando la variable de control
lines(q, type = "l", lwd = 3, col = "gold")
lines(p, type = "l", lwd = 3, col = "lightsalmon1")
# Agregamos una leyenda
legend("topright", legend = c("Monte Carlo", "Variable de Control Y", "Variable de Control Z"),
       col = c("skyblue", "gold","lightsalmon1"), lwd = 4:3, lty = 1:1, cex = 0.7)

















































