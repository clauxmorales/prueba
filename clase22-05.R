#LISTA 4 PROBLEMA 3

x=c(-0.1, +0.1)

if (sign(x[1]*x[2])==+1){
  #signo positivo
  points(x, col="blue")
} else {
  #signo negativo
  points(x, col="red")
}
#al correr sale que no se creo un lienzo, plot.new nos da un lienzo para 
#dibujar de 0 y 1, plot.new no tiene argumentos
#si queremos graficar los rangos de los ejes necesitamos plot.window
# con plot.window le damos en rango, tanto el eje y y x van desde -1 a 1
#ya conocemos como agregar ejes y cajas axis y box

x=c(-0.1, +0.1)

plot.new()
plot.window(xlim=c(-1,1), ylim=c(-1,1))
axis(1)
axis(2)
box()

if (sign(x[1]*x[2])==+1){
  #signo positivo
  points(x[1],x[2], col="blue", pch=19)
} else {
  #signo negativo
  points(x[1],x[2], col="red", pch=19)
}
# nos piden hacer esta operacion 1000 veces, entonces debemos
#usar for
x=c(-0.1, +0.1)

plot.new()
plot.window(xlim=c(-1,1), ylim=c(-1,1))
axis(1)
axis(2)
box()

for(i in 1:1000){
  x= runif(2, min=-1, max=1)
  if (sign(x[1]*x[2])==+1){
  #signo positivo
  points(x[1],x[2], col="blue", pch=19)
  } else {
  #signo negativo
  points(x[1],x[2], col="red", pch=19)
  }
}
#si queremos crear un grafico mas complejo, empezamos con plot.new y luego
#plot.window, y de ahi le agregamos lo que queramos

#LISTA 4 PROBLEMA 7
#Calcular las raices del polinomiop(x) =ax2+bx+c
#c/queramos operar raices negativa, factorizamos el 1 y le agregamos i
#asi podremos realizar la operacion

coef =c(1,3,5) #(a,b,c)

a=coef[1]
b=coef[2]
c=coef[3]
deter=b^2 - 4*a*c

if(deter<0){
  #determinante negativo
  deter=-1*deter
  raices= (-b + c(+1,-1)*sqrt(deter)*1i)/(2*a)
} else{
  #determinante positivo
  raices= (-b + c(+1,-1)*sqrt(deter))/(2*a)
}
print (raices)

# c(+1,-1)*2, el 2 multiplica a ambos elementos del vector

#la forma como procederemo para no volver a correr una y otra vez con diferentes polinomios
#luego de que tengamos el algoritmo, definimos nuestra propia funcio
#crearemos esta funcio en R
# nombreFunction=function (args){ return () }
#arg=nombre de lo que quiero cambiar, en polinomios cambiaremos
#los coeficientes
#mi funcion recibira un arg llamado coef, este coef lo puedo usar como una variable

calcularRaices=function (coef){
  a=coef[1]
  b=coef[2]
  c=coef[3]
  deter=b^2 - 4*a*c
  
  if(deter<0){
    #determinante negativo
    deter=-1*deter
    raices= (-b + c(+1,-1)*sqrt(deter)*1i)/(2*a)
  } else{
    #determinante positivo
    raices= (-b + c(+1,-1)*sqrt(deter))/(2*a)
    }
  return (raices)
}

calcularRaices(coef=c(1,1,1))
calcularRaices(coef=c(16,-5,6))
calcularRaices(coef=c(-1,-101,1001))

#al definir una funcion, dejamos un registro
#cuando queramos nuevamente usar essa funcion, debemos realizar 
#un llamado como en las lineas 105-106
#pensar cuales seran los argumentos de la funcion
#datos que nos proporcionen datos de entrada que queramos modificar

#dependiendo del problema, los argumentos variaran, la cantidad de puntos
#y los valores max y min variaran
#los detalles son menos importante, la cantidad de puntos son
#mas importantes que el color de los mismos

x=c(-0.1, +0.1)
n=1000
plot.new()
plot.window(xlim=c(-1,1), ylim=c(-1,1))
axis(1)
axis(2)
box()

for(i in 1:n){
  x= runif(2, min=-1, max=1)
  if (sign(x[1]*x[2])==+1){
    #signo positivo
    points(x[1],x[2], col="blue", pch=19)
  } else {
    #signo negativo
    points(x[1],x[2], col="red", pch=19)
  }
}

# cambiaremos, numero de puntos "n", valores min y max, col y pch
#efecto secundario=no quiero un valor si no un grafico
# en este caso no quiero que devuelva nada uso "invisible"
graficarNumeros = function(n, min,max,col,pch) {
  plot.new()
  plot.window(xlim=c(-1,1), ylim=c(-1,1))
  axis(1)
  axis(2)
  box()
  
  for(i in 1:n) {
    x= runif(2, min=min, max=max)
    if (sign(x[1]*x[2])==+1) {
      #signo positivo
      points(x[1],x[2], col=col[1], pch=pch)
    } else {
      #signo negativo
      points(x[1],x[2], col=col[2], pch=pch)
    }
  }
  return(invisible())
}

graficarNumeros(n=100,min=-1, max=1, col=c("blue","red"),pch=19)
graficarNumeros(n=1e4,min=-1, max=1, col=c("blue","red"),pch=19)

#debemos especificar cada argumento, especificamos valores por defecto
graficarNumeros2 = function(n, min=-1,max=+1,col=c("blue", "red"),pch=19) {
  plot.new()
  plot.window(xlim=c(-1,1), ylim=c(-1,1))
  axis(1)
  axis(2)
  box()
  
  for(i in 1:n) {
    x= runif(2, min=min, max=max)
    if (sign(x[1]*x[2])==+1) {
      #signo positivo
      points(x[1],x[2], col=col[1], pch=pch)
    } else {
      #signo negativo
      points(x[1],x[2], col=col[2], pch=pch)
    }
  }
  return(invisible())
}

graficarNumeros2(n=100,min=-1, max=1, col=c("blue","red"))
graficarNumeros2(n=100,min=-1, max=1)
graficarNumeros2(n=100)
graficarNumeros2(n=1000)

#queremos cambiar el tamaño de los puntos
#cex, como no lo he usado, debo definirlo
graficarNumeros3 = function(n, min=-1,max=+1,col=c("blue", "red"),pch=19, cex=1) {
  plot.new()
  plot.window(xlim=c(-1,1), ylim=c(-1,1))
  axis(1)
  axis(2)
  box()
  
  for(i in 1:n) {
    x= runif(2, min=min, max=max)
    if (sign(x[1]*x[2])==+1) {
      #signo positivo
      points(x[1],x[2], col=col[1], pch=pch, cex=cex)
    } else {
      #signo negativo
      points(x[1],x[2], col=col[2], pch=pch, cex=cex)
    }
  }
  return(invisible())
}
graficarNumeros3(n=100, cex=1.5)
graficarNumeros3(n=100, cex=0.1)

#cuando tenemos muchos arg que modificar, tenemos la opcion de las elipses que se han visto
# "..." todos los arg que se les den q no sean arg y pueden ser pasados a un a funcion
# en vez de pone cex pondre "..." cualquier argumento que le de a point, lo pasara
#indirectamente no esta cex como argumento, y yo lo podre agregar

graficarNumeros4 = function(n, min=-1,max=+1,col=c("blue", "red"),...) {
  plot.new()
  plot.window(xlim=c(-1,1), ylim=c(-1,1))
  axis(1)
  axis(2)
  box()
  
  for(i in 1:n) {
    x= runif(2, min=min, max=max)
    if (sign(x[1]*x[2])==+1) {
      #signo positivo
      points(x[1],x[2], col=col[1], ...)
    } else {
      #signo negativo
      points(x[1],x[2], col=col[2], ...)
    }
  }
  return(invisible())
}

graficarNumeros4(n=100, cex=1.5)
graficarNumeros4(n=100, pch=14)

#primero trabajas en la funcion y luego crear una nueva funcion con valores por defecto
# n, mis nuevos colores y lo demas en "..."
#esto es en caso la funcion ya esta establecida (por otras personas) pero yo quiero cambiar algunos argumentos
#solo para cambiar los arg por defectos, como lo hacemos con un WRAPPER
# WRAPPER= hemos "empaquetado" una funcion solo para cambiar argumentos por defecto a una funcion


nuevaGraficarNumeros = function(n,col=c("green","orange"),...) {
  graficarNumeros4(n=n,col=col, ...)
  return(invisible())
}

nuevaGraficarNumeros(n=100)

#read.csv es un wrapper de read.table

#Para funciones:
#Identificar los argumentos arg que son usados en el codigo
#tendremos varios arg, identificar los principales y secundarios
#a los secudarios se les pone por defecto
#hacemos funciones para hacer calculos, nos interesa sus efectos secundarios
#efectos secundarios, usar "invisble"
#funciones para modificar arg por defectos y hacer mas facil el llamado, usamos WRAPPER
#elipsis, para no estar especificando cada uno de los argumentos

#LISTA 4 EJERCICIO 12
# Calcular n! paran N (numeros naturales).
#
n=-5

factorial= 1
for (i in 1:n) {
  factorial = factorial*i
}
  print (factorial)

#limita los numeros que podre usar, asi que debemos crear una condicional

n=0

factorial= 1
if (n==0){  
  factorial= 1
}else{
  factorial= 1
  for (i in 1:n) {
    factorial = factorial*i
  }
}
  print (factorial)

#no esta definido para numeros negativos, comprobar
  n=-5
  
  if (n<0) stop("factorial no definido para numeros negativos")
  if (n==0){  
    factorial= 1
  }else{
    factorial= 1
    for (i in 1:n) {
      factorial = factorial*i
    }
  }
  print (factorial)

#realizar una funcion para factorial
  factorial=function (n){
    if (n<0) stop("factorial no definido para numeros negativos.")
    if (n==0){  
      factorial = 1
    }else{
      factorial = 1
      for (i in 1:n) {
        factorial = factorial*i
      }
    }
    return (factorial)
  }
  
factorial(3)
factorial(0)
factorial(-5)

#como averiguar si un numero es entero, 54%%1 == 0; 5.4%%1==0, el residuo es diferente de 0
#no es un numero entero y necesitamos que la funcion este definida SOLO para enteros
#para que la funcion solo sea para numeros enteros
#stop detiene la funcion

factorial=function (n){
  if (n<0) stop("factorial no definido para numeros negativos.")
  if (n%%1 !=0) stop("Factorial definido solo para numeros enteros.")
  if (n==0){  
    factorial = 1
  }else{
    factorial = 1
    for (i in 1:n) {
      factorial = factorial*i
    }
  }
  return (factorial)
}

#warning si ejecuta la funcion
factorial=function (n){
  if (n<0) stop("factorial no definido para numeros negativos.")
  if (n%%1 !=0) warning("Factorial definido solo para numeros enteros,
                        usando parte entera.")
  if (n==0){  
    factorial = 1
  }else{
    factorial = 1
    for (i in 1:n) {
      factorial = factorial*i
    }
  }
  return (factorial)
}
