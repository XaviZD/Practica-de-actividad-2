
#Resolución de problemas U1 – ACTIVIDAD # 2
#Pico Loor Adonis Alexander
#Zambrano Demera Xavier Alejandro

#Librerias utilizadas

if(!require(tidyverse)) {install.packages("tidyverse")}
library("tidyverse") 

if(!require(dplyr)) {install.packages("dplyr")}
library("dplyr")

if(!require(ggplot2)) {install.packages("ggplot2")}
library("ggplot2") 

library(readxl)



#1.Explorar las variables, ver el tipo de dato y ciertos datos.

class (DATASET_RP_1$`Documento Cliente`)

DATASET_RP_1 <- read_excel("Datos/DATASET_RP_1.xlsx")
View(DATASET_RP_1)

#Podemos utilizar el comando class para ver el tipo de dato, los datos del 
#data frame los podemos revisar con el comando read.excel 




#2. Ver las primeras 15 filas de la base de datos.

head(DATASET_RP_1, 15) 

#Mostrar las 5 últimas filas de la base de datos.

tail(DATASET_RP_1,5)

#Aqui se utilizo los comandos head y tail, usando head para vizualizar las 15 primeras filas y tail para
# las 5 ultimas filas del data frame "DATASET_RP_1"






#3. Mostrar únicamente las columnas nombre cliente, fecha deuda, producto, total 
#de crédito, plazo, cuotas canceladas, y rango mora.

select(DATASET_RP_1,"Nombre Cliente", "Fecha deuda","Producto", "Total credito","Plazo","Cuotas canceladas", "Rango mora")

#Se utilizo el comando select para mostrar las columnas pertenecientes a los atributos indicados en la actividad




#4.Mostrar todos los datos, de los créditos con cuotas canceladas mayores a 10.

filter(DATASET_RP_1,`Cuotas canceladas` >10)

#Utilizamos el comando filter para realizar este apartado, definimos el data frame que en este caso es el 
#"DATASET_PR_1" y definimos la columna junto con la condicion para que muentre la informacion en caso de que
#esta se cumpla





#5.Ordenar la base de datos por total de crédito en orden descendente.

DATASET_RP_1 <- 
arrange(DATASET_RP_1,desc(`Total credito`))

#Utilizamos el comando arrange en la base de datos DATASET_RP_1 para ordenar la base de datos segun lo indicado en la actividad





#6. En la columna cuotas canceladas, son considerados valores inusuales aquellos
#valores superiores a 10 cuotas, debido a las políticas de crédito de la tienda. Por
#tanto, se desea crear una nueva columna reemplazando estos como valores
#faltantes. Reemplazar estos datos con el valor “NA”.


DATASET_RP_1 <- DATASET_RP_1 %>% 
  mutate(cuotaslimpias = ifelse(`Cuotas canceladas`> 10 , "NA", `Cuotas canceladas`))

#Usamos el comando mutate para crear una nueva columna que en este caso se llamara cuotaslimpias, luego 
#con el comando ifelse definimos si se cumple la condicion asignada, en caso de que las cuotas canceladas sean
#mayores a 10, su valor cambiara por NA, en caso contrario su valor no cambiara.




#7. Crear una nueva columna, de nombre H_CREDITO, que contenga los valores:
#“habilitado para crédito” si las cuotas canceladas son mayores a 6, caso contrario
#el valor será “no habilitado para crédito”.

DATASET_RP_1 <- DATASET_RP_1 %>%
  mutate(DATASET_RP_1, H_credito= `Cuotas canceladas`,
         H_credito=ifelse(H_credito>6,"habilitado para crédito","no habilitado para crédito"))

#Como se indica en la actividad crearemos una nueva columna utilizando los datos de cuotas canceladas, para hacer esto utilizamos el comando mutate 
#configurando las condiciones establecidas en la actividad


#8. Se requiere que el nombre de un producto en específico se encuentre
#correctamente escrito. Existen productos ingresados como Audifono, audifono y
#audifon, secado, secador, cecadora. Crear una nueva columna que contenga el
#dato adecuado establecido como “Audifonos” y “Secadora”, para todos los
#registros.

DATASET_RP_1 <- DATASET_RP_1 %>% 
  mutate(Producto_limpio = case_when (
    str_detect(Producto, "[A|a]udifo[n|no]" ) ~ "Audifonos", 
    str_detect(Producto, "[c|s]ecad[o|ora|or]" ) ~ "Secadora",
    negate =TRUE ~  as.character(Producto)))


#Nuevamente utilizamos el comando mutate para la transformación de los datos correspondiente a productos
#donde se agregaran los nombres de Audifonos y Secadora correctamente escritos, para eso se uso el
#comando str_detect para que detecte los versiones de Audifono, audifon y audifono y los cambie por Audifonos
#luego realizamos lo mismo con los datos de Secadora.




#9. Crear un gráfico de barras para visualizar distribución de rango mora.


ggplot (data = DATASET_RP_1) + geom_bar(mapping = aes(x =`Rango mora`, fill = `Rango mora` )) +
labs(title = 'Grafico de barra del Rango Mora', y = "Usuarios")

#Para realizar esta actividad utilizamos el comando ggplot para la creacion del grafico, en data asignamos 
#el data frame "DATASET_RP_1", seguidamente con geom_bar asignamos los datos del rango mora a las barras
#ademas de tambien asignarles color con el comando fill
#por ultimo se agregaron labs como el titulo y una etiqueta para y



#10. Dibujar un diagrama de cajas y bigotes para analizar la distribución de la variable
#plazo en función de la variable rango mora.


ggplot(data = DATASET_RP_1) +
  geom_boxplot(mapping = aes(x = `Rango mora`, y = `Plazo`, fill = `Rango mora`, `Plazo` ))+
  coord_flip()

#Se utilizo el comando ggplot para crear un diagrama de bigote y cajas utilizando los datos pertenecientes a Plazo y Rango mora,
#se les agrego color utilizando fill y se voltearon las coordenadas utilizando el comando coord_flip()

names(DATASET_RP_1)


#                                                   CONCLUSION:
#
#             Como conclusion esta practica nos ha servido para familiarizarnos con el programa,
#             ayudandonos a conocer las formas basicas como crear proyecto o importar data frames
#             tambien pudimos conocer las utilidades de algunas de las librerias, el como
#             instalarlas y como utilizarlas.
#
#             Ademas poner a prueba los comandos nos ayuda a entenderlos mejor y a compreder su estructura
#             asi mismo el permitirnos aprender como graficar de dos formas distintas utilizando los datos de 
#             diversas columnas.
#
#
