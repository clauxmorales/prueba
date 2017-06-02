sign(-1233)
sign(2345)
#sign indica si un numero es positivo o negativo

#DETERMINAR QUE NUMERO ES MENOR
#pseudocodigo, comandos que escribimos para 
#estructura de control de flujo, permiten controlar para donde va el flujo, uno de esllos es IF
#INICIO
#Calcular la diferencia entre "x" e "y"
#IF
#el signo de la diferencia es "-", entonces "x es menor"
#ELSE
#"y es menor"
#FIN
# if (condicional) {caso principal} else {caso alterno}
#matlabR.pdf en la linea 195, mas info de estos comandos

print("hola")
paste(3, "hola")
print(paste(3,"hola", 323232))


print (paste(x[1],"es menor"))

sign(-3)== +1 #false
sign(+5)== +1 #true

#Cual es menor?"
x=c(20,3)
if(sign(x[1]-x[2])==-1) {
print(paste(x[1], "es menorque", x[2]))
 } else {
print(paste(x[2], "es menor que", x[1]))}
dev.copy(png, file="figura2.png")
dev.off()

#clase 08 de mayo

# Creando un buqle: for (i in CONJUNTO){ ALGORITMO }, conjunto misma longitud que el vector
#EJEMPLO
x=c(2,3,5,9,1)

minimo=min(x) #1 es equivalente a la linea 46
    which.min(x) #encuentra la posicion del minimo dentro del conjunto

    x [which.min(x)] # donde esta el minimo y que lo extraiga

    x[-c(5,1)]# significa quita los elementos menos el 5 y el 1
    x [-which.min(x)] # le quito el elmenento que esta en la posicion del minimo
  
x= x [-which.min(x)] #2

reultado = NULL #null concatena creamos un conjunto vacio "el resultado es un conj vacio
#y cada  vez que corra empezara a juntar en orden ascendente a los elementos del conjunto
resultado = c(resultado, minimo) #3

#Ejemplo 1: correr 

x=c(2,3,5,9,1)

#inicializacion
resultado = NULL

for(i in 1:5) {minimo=min(x)
x= x [-which.min(x)]
resultado = c(resultado, minimo)}

resultado

#funcion: numeric
vector("numeric", 10) #funcion que crea vector, "crea un vector numerico de 10 elementos)
# resultado [i] estara dando vuelta hasta que use todos los elementos de x

x=c(2,3,5,9,1)

#inicializacion
resultado = numeric(5)

for(i in 1:5) {minimo=min(x)
x= x [-which.min(x)]
resultado[i] = minimo
print (resultado)}

# tercera forma de hacer el ejemplo 1
#resultado []= NA , reemplaza cada elemento resultado por NA, asi evitamos 
#confundir los 0 como resultado

x=c(2,3,5,9,1)

#inicializacion
resultado = numeric(5)
resultado []= NA 

for(i in 1:5) {minimo=min(x)
              x= x [-which.min(x)]
              resultado[i] = minimo

              print (resultado)}

#HACIENDOLO MAS GENERAL
# si tenemos mas numeros, debemos cambiar la cantidad de numeros que tiene el vector
#para que sea eficiente la cantidad de elemntos sera la longitud del vector
#osea "length(x)", ya no importa la cantidad de elementos que agregue
#ahora aguanta todos los elementos, sea mas general posible
#i "integrador", debe ser reemplazado cuidadosamente por cualquier otro elemento

x=c(2,3,5,9,1,13,15,11,7,8,123)

#inicializacion
resultado = numeric(length(x))
resultado []= NA 

for(i in 1:length(x)) {minimo=min(x)
x= x [-which.min(x)]
resultado[i] = minimo

print (resultado)}

#Ejercicio: calcular la suma de todos los elementos
x=c(2,3,5,9,1,13,15,11,7,8,123)

#inicializacion
suma=0

for(i in 1:length(x)) {
  suma = suma + x[i]
  print (suma)}

#Ejercicio calculando la suma de los cuadrados
x=c(2,3,5,9,1,13,15,11,7,8,123)

#inicializacion
suma=0

for(i in 1:length(x)) {
  suma = suma + x[i]^2 
   print (suma)}

# sumando los cuadrados y suma
x=c(2,3,5,9,1,13,15,11,7,8,123)

#inicializacion
suma=0
cuadrados=0

for(i in 1:length(x)) {
  suma = suma + x[i] 
  cuadrados = cuadrados + x[i]^2
  resultado=suma +cuadrados
  print (resultado)}

# Calculando el indice de shannon BUSCAR 
x=c(1,2,3,5,7,9,11,13,15,1234)

suma=0

for(i in 1:length(x)) {
  suma = suma + x[i] 
 
H=0
p=x/suma
for(i in 1:length(x)) {
  H = H + -p[i]*log2(p(i))}

#sort, sum 
