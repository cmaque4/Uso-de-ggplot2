#Cargar paquetes
library(car); library(tidyverse); library(ggpubr)
library(viridis); library(ggsci); library(ggthemes) #MÃ©todo 1
pacman::p_load(car, tidyverse, ggpubr, viridis, ggsci) #MÃ©todo 2
library(carData)
library(ggthemes)
library(ggplot2)
data(iris)
data(mpg)
data()
### graficos predeterminados en R

hist(iris$Sepal.Length)
plot(iris$Sepal.Length,iris$Pepal.Width) #gráfico de disperciÃ³n
boxplot(iris$Sepal.Length~iris$Species) #una variable continua se ver¡ en función³n a una variable categorica

hist(iris$Sepal.Length,
     col = "darkorange3",
     border="black",
     main = "DistribuciÃ³n de las flores",
     xlab = "Longitud", ylab = "Frecuencias")

plot(x=iris$Petal.Length,y=iris$Petal.Width,
     col="firebrick1", #color
     pch=2, #forma del punto
     cex=0.9, #tamaÃ±o del punto
     main="Relacines", #titulo
     xlab="Longitud", ylab="Ancho");abline(lm(Petal.Width~Petal.Length,iris), col="firebrick4")

boxplot(iris$Sepal.Length~iris$Species,
        col=c("deeppink4","darkviolet","darkslategrey"),
        main="GrÃ¡fico de Cajas",
        xlab="Especies",ylab="Longitud")

# https://www.rstudio.com/wp-content/uploads/2015/04/ggplot2-spanish.pdf
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

####---- GrÃ¡ficos en ggplot2 ----####

ggplot(data = iris,mapping = aes(x=Sepal.Length))+geom_density()
ggplot(data = iris,mapping = aes(x=Sepal.Length))+geom_histogram()

# grafica con una varible continua

ggplot(data = iris,mapping = aes(x=Sepal.Length))+geom_freqpoly()
ggplot(data = iris,mapping = aes(x=Sepal.Width))+geom_histogram()
ggplot(data = iris,mapping = aes(x=Petal.Length))+geom_dotplot()
ggplot(data = iris,mapping = aes(x=Petal.Width))+geom_area(stat = "bin")

# para una variable discreta

ggplot(data = iris,mapping = aes(x=Species))+geom_bar()

# el argumento "stat" nos indica que trasnformaciÃ³n estadistica se va a hacer
# pongo 3, hay mÃ¡s 
## stat=bin (separa una variable continua en bloques y cuenta la frecuencia)
## stat=count (cuenta el numero de veces que se repite una variable) en geom_bar ejm
## stat=identidy (deja las variables tal como estan)

ggplot(data = iris,mapping = aes(x=Species))+geom_bar(stat = "count")
ggplot(data = iris,mapping = aes(x=Sepal.Length))+geom_histogram()

# otra soluciÃ³n es usar un geom que contenga por default el stat que necesitemos
# ejemplo geom_col() este necesita dos variables x/y

ggplot(data = iris,mapping = aes(x=Species,y=Sepal.Width))+geom_col()

# otra soluciÃ³n es usar la misma funciÃ³n pero espeficicando el stat

ggplot(data = iris,mapping = aes(x=Species,y=Petal.Width))+geom_bar(stat = "identity")+
  xlab("Especies")+ ylab("Longitud")

# grafica para dos variables continuas

ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Sepal.Width))+geom_jitter() #geom_jitter sirve para suavizar lo de geom_point
ggplot(data = iris,mapping = aes(x=Petal.Length,y=Sepal.Width))+geom_point()

ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Sepal.Width))+geom_rug()+geom_point()

ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Petal.Length))+
  geom_point()+geom_smooth(method = "lm")

# grafica para una variable categorica (discreta) y continua

ggplot(data = iris,mapping = aes(x=Species,y=Sepal.Length))+geom_boxplot()
ggplot(data = iris,mapping = aes(x=Species,y=Petal.Width))+geom_violin()
ggplot(data = iris,mapping = aes(x=Species,y=Sepal.Width))+geom_col()
ggplot(data = iris,mapping = aes(x=Species,y=Sepal.Width))+geom_point()+geom_jitter()
ggplot(data = iris,mapping = aes(x=Species,y=Sepal.Width))+geom_jitter()

####---- Fin de la primera parte ----####

# uso de faceting: sirve para agrupar, segÃºn una variable para ver subdivisiones 
# de cada variable

data(mpg)

ggplot(data = mpg,mapping = aes(x=displ,y=hwy))+geom_point()
ggplot(data = mpg,mapping = aes(x=manufacturer,y=hwy))+geom_col()+facet_grid(rows = vars(year))

ggplot(data = mpg,mapping = aes(x=manufacturer,y=hwy))+geom_col()+facet_grid(rows = vars(year))
ggplot(data = mpg,mapping = aes(x=class,y=hwy))+geom_col()+facet_grid(cols = vars(year))

ggplot(data=mpg,mapping = aes(x=displ,y=hwy))+geom_point()+
  facet_wrap(~year,nrow=1,ncol=2)

ggplot(data = mpg,mapping = aes(x=displ,y=hwy))+geom_point()+
  facet_grid(rows = vars(cyl))
ggplot(data = mpg,mapping = aes(x=displ,y=hwy))+geom_point()+
  facet_grid(rows = vars(cyl),scales = "free")

ggplot(data = mpg,mapping = aes(x=displ,y=hwy))+geom_point()+
  facet_grid(year~cyl)

ggplot(data = mpg,mapping = aes(x=displ,y=hwy))+geom_point()+
  facet_wrap(year~cyl, nrow = 2)

#Practicar con diferentes bases de datos

ggplot(data = mpg,mapping = aes(x=class,y=displ))+geom_boxplot()+facet_grid(rows = vars(year))

ggplot(data = iris,mapping = aes(x=Sepal.Length))+geom_histogram()+ facet_grid(rows = vars(Species))

ggplot(data = mpg,mapping = aes(x=year,y=model))+geom_jitter()

####---- Como agrupar y guardar graficos de ggplot2 ----####

Fi1<- ggplot(data = iris,mapping = aes(x=Species,y=Sepal.Length))+geom_boxplot()
Fi2<- ggplot(data = iris,mapping = aes(x=Petal.Length,y=Petal.Width))+
  geom_point()+geom_smooth(method = "lm")

# ggarrange sirve para juntar figuras
library(ggpubr)
Figu <- ggarrange(Fi1,Fi2, labels = c("Figura A","Figura B"))

##ggsave sirve para guardar la figura

ggsave(filename = "Figu.jpg", plot = Figu,
       units = c("cm"),width = 35, height = 20,
       dpi = 200, limitsize = FALSE)

####---- Uso de Faceting ----#####
##FACETING
#Usaremos la base de datos mpg
data(mpg)
?mpg
table(mpg$manufacturer)
table(mpg$year)
table(mpg$cyl)
summary(mpg$hwy)

ggplot(mpg, aes(x=displ, y=hwy)) + geom_point() #Graficamos las millas por hora en funciÃ³n del desplazamiento del motor

#Podemos desglosar esta relaciÃ³n segÃºn el aÃ±o de fabricaciÃ³n con facet_grid
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(year)) #En filas
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(cols = vars(year)) #En columnas

#TambiÃ©n podemos utilizar la funciÃ³n facet_wrap
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~year)

#Estos subgrÃ¡ficos pueden tener todos la misma escala (default)
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(cyl))
#O podemos permitir que tengan escalas distintas (scales="free")
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(rows = vars(cyl), scales="free")

#TambiÃ©n podemos desglosar la grÃ¡fica en funciÃ³n de 2 variables al mismo tiempo
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(year~cyl)
#Con facet_wrap se eliminan los subgrÃ¡ficos vacÃ­os y se tiene mÃ¡s control sobre la distribuciÃ³n del grÃ¡fico
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(cyl~year, nrow=1, ncol=7)
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(cyl~year, nrow=2, ncol=4)
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(cyl~year, nrow=3, ncol=3)
####---- Graficos de ggplot personalizados ----####

# para personalizar nuestro graficos se debe utilizar
# color = afecta contornos, lÃ­neas y puntos
# fill = afecta rellenos
# alpha = modifica la transparencia de los objetos alpha puede tomar cualquier valor entre 0 y 1
# shape = modifica la forma de un punto (shape in r)
# linetype = modifica el tipo de lÃ­nea (linetype in r)
# Size = modifica el tamaÃ±o de los puntos y el grosor de las lÃ­neas

ggplot(data=iris, mapping = aes(x=Species,y=Petal.Width))+
  geom_boxplot(color="black",fill="green",size=0.1)
ggplot(data = iris,mapping = aes(x=Species,y=Petal.Width, fill=Species))+
  geom_boxplot()


# cuando coloquemos las personalizaciÃ³n fill, color, shape etc, lo va a
# cambiar para todo los geom_(), en cambio cuando se ponen esas personalizaciones
# en los geom_() solo se va a cambiar en esas capas 

ggplot(data = iris,mapping = aes(x=Petal.Length,y=Petal.Width,color=Species))+
  geom_point(size=1.8,alpha=0.8,shape=9)+geom_smooth(method = "lm", color="red")

ggplot(data = iris,mapping = aes(x=Petal.Length,y=Petal.Width,color=Species))+
  geom_point()+geom_smooth(method = "lm")

####---- Scales manuales para preservar y personalizar mÃ¡s ----####

# values
# name
# labels
# limits

ggplot(data = iris,mapping = aes(x=Species,y=Petal.Length, fill=Species))+
  geom_boxplot()+
  scale_fill_manual(values=c("blue4","blueviolet","brown"),labels=c("Setosa","Versicolor","Virginica"),
                    name="Flores")+
  scale_color_manual(values=c("blue4","blueviolet","brown"),labels=c("Setosa","Versicolor","Virginica"),
                     name="Flores")

## Scalar de forma predeterminada

#### para variables discretas, categoricas
# brewer
# grey
#### para variables continuas
# gradient
# viridis
# ggsci

# scale_color_() se puede escoger cualquiera de acuerdo a las variables
# en las coordenadas

ggplot(data = iris,mapping = aes(x=Species,y=Petal.Width,fill=Species))+
  geom_boxplot()+scale_fill_brewer()

ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Petal.Width,color=Sepal.Length))+
  geom_point()+geom_smooth(method = "lm")+scale_color_viridis()

ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Sepal.Width,color=Sepal.Length))+
  geom_point()+geom_smooth(method = "lm")+scale_color_viridis()

####---- PrÃ¡ctica de lo aprendido ----####

# color afecta a los contornos , lineas y puntos de la figura

ggplot(data = iris,mapping = aes(x=Sepal.Length))+geom_density(color="red")
ggplot(data = iris,mapping = aes(x=Sepal.Length,color=Species))+geom_density()

ggplot(data = iris,mapping = aes(x=Species))+geom_bar(color="red")
ggplot(data = iris,mapping = aes(x=Species,color=Species))+geom_bar()

ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Sepal.Width))+
  geom_point(color="red")
ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Sepal.Width,color=Species))+
  geom_point()

ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Sepal.Width))+
  geom_smooth(method = "lm",color="red")
ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Sepal.Width,color=Species))+
  geom_smooth(method = "lm")

ggplot(data = iris,mapping = aes(x=Species,y=Petal.Length))+
  geom_boxplot(color="red")
ggplot(data = iris,mapping = aes(x=Species,y=Petal.Length,color=Species))+
  geom_boxplot()

# fill afecta el relleno de las figuras

ggplot(data = iris,mapping = aes(x=Sepal.Length))+geom_density(fill="red")
ggplot(data = iris,mapping = aes(x=Sepal.Length,fill=Species))+geom_density()

ggplot(data = iris,mapping = aes(x=Species))+geom_bar(fill="red")
ggplot(data = iris,mapping = aes(x=Species,fill=Species))+geom_bar()

ggplot(data = iris,mapping = aes(x=Species,y=Sepal.Length))+
  geom_boxplot(fill=c("red","green","blue"))
ggplot(data = iris,mapping = aes(x=Species,y=Sepal.Length,fill=Species,color=Species))+
  geom_boxplot(color="black")

ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Sepal.Width))+
  geom_point(fill="red")  # no afecta a los puntos fill
ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Sepal.Width,fill=Species))+
  geom_point() # no afecta, color es la que afecta a los puntos

# geom_smooth

ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Sepal.Width))+
  geom_smooth(method = "lm",fill="blue",color="black")
ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Sepal.Width,fill=Species))+
  geom_smooth(color="black")

# shape modifica la forma respecto a la caracteristica de la variable o de la
# que queremos darle

ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Petal.Length,color=Species))+
  geom_jitter(shape=20)
ggplot(data = iris,mapping = aes(x=Sepal.Length,y=Petal.Length,shape=Species))+
  geom_jitter()

# size cambia el tamaÃ±o de las graficas de acuerdo a la variable o que deseemos

ggplot(iris,aes(x=Sepal.Length))+geom_density(size=0.9)
ggplot(iris,aes(x=Sepal.Length,size=Species))+geom_density()

ggplot(iris,aes(x=Species))+geom_bar(color="black",size=0.2)
ggplot(iris,aes(x=Species,size=Species))+geom_bar(color="red")

ggplot(iris,aes(x=Petal.Length,y=Petal.Width))+geom_jitter(size=1.5)
ggplot(iris,aes(x=Petal.Length,y=Petal.Width,size=Species))+geom_point()

ggplot(iris,aes(x=Petal.Length,y=Petal.Width))+
  geom_smooth(method = "lm", color="black",size=2)
ggplot(iris,aes(x=Petal.Length,y=Petal.Width, size=Species))+geom_smooth(method = "lm",color="green")

ggplot(iris,aes(x=Species,y=Petal.Width))+geom_boxplot(size=0.5)
ggplot(iris,aes(x=Species,y=Petal.Width,size=Species))+geom_boxplot()

# Alpha modifica la transparencia de los objetos, puede tomar valores de 0 y 1

ggplot(iris,aes(x=Sepal.Length))+geom_density(fill="blue",alpha=0)
ggplot(iris,aes(x=Sepal.Length))+geom_density(fill="blue",alpha=0.25)
ggplot(iris,aes(x=Sepal.Length))+geom_density(fill="blue",alpha=0.50)
ggplot(iris,aes(x=Sepal.Length))+geom_density(fill="red",alpha=0.75)
ggplot(iris,aes(x=Sepal.Length))+geom_density(fill="red",alpha=1)
ggplot(iris,aes(x=Sepal.Length,alpha=Species))+geom_density(fill="red")

ggplot(iris,aes(x=Species))+geom_bar(fill="red",alpha=c(0.5,0.75,1))
ggplot(iris,aes(x=Species,alpha=Species))+geom_bar(fill="darkgreen")

ggplot(iris,aes(x=Petal.Length,y=Petal.Width))+geom_jitter(size=2,alpha=0.25)
ggplot(iris,aes(x=Petal.Length,y=Petal.Width,color=Species,alpha=Species))+geom_jitter(size=2)

ggplot(iris,aes(x=Petal.Length,y=Petal.Width))+geom_smooth(method = "lm",alpha=1,color="darkred")
ggplot(iris,aes(x=Petal.Length,y=Petal.Width,alpha=Species))+geom_smooth(method = "lm")

ggplot(iris,aes(x=Species,y=Sepal.Length))+geom_boxplot(fill="red",alpha=0.5)
ggplot(iris,aes(x=Species,y=Sepal.Length,alpha=Species))+geom_boxplot(fill="red")

#"Linetype" modifica el tipo de lÃ­nea

ggplot(data = iris, mapping = aes(x=Species)) + geom_bar(color="black", size=2, linetype=2)
ggplot(data = iris, mapping = aes(x=Species, linetype=Species)) + geom_bar(color="black", size=2)

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width)) + geom_smooth(method="lm", color="black", linetype=2)
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, linetype=Species)) + geom_smooth(method="lm", color="black")

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Length)) + geom_boxplot(fill="steelblue2", size=0.75, linetype=2)
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, linetype=Species)) + geom_boxplot(fill="steelblue2", size=0.75)

## Si se especifica en aes, tiene que aisgnarse una variable, que puede ser categÃ³rica o continua
#Si es categÃ³rica, se asigna un color para cada categorÃ­a
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point(size=2)
#Si es continua, se genera un gradiente de colores entre el valor mÃ­nimo y el mÃ¡ximo de la variable
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Length)) + geom_point(size=2)

## Estos atributos pueden combinarse para crear grÃ¡ficos mÃ¡s estÃ©ticos
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(color="midnightblue", size=0.9, linetype=5)

ggplot(data = iris, mapping = aes(x=Species, linetype=Species, fill=Species)) + geom_bar(color="black", alpha=0.5, size=1)

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Length, size=Species)) + geom_point(size=2.5) +
  geom_rug(color="black", size=1) + geom_smooth(method="lm", color="black", size=1)

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Length, size=Species)) + geom_point() +
  geom_rug(color="black", size=1) + geom_smooth(method="lm", color="black", size=1)

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species, color=Species)) +
  geom_point(size=2)+geom_boxplot(color="black", size=0.75)

# graficos personalizados con scale
# Con las funciones scale manual se pueden personalizar manualmente las variables categÃ³ricas
# Se tiene que especificar en la funciÃ³n quÃ© es lo que se quiere cambiar
# scale_fill_manual // scale_color_manual // scale_linetype_manual // etc.

#En las funciones scale manual se pueden especificar los siquientes argumentos: values, labels, name, limits

#Con el argumento values se puede cambiar el relleno de cada una de las categorÃ­as
ggplot(data=iris,mapping = aes(x=Species,y=Petal.Width,fill=Species))+
  geom_boxplot(color="black",size=0.50)+
  scale_fill_manual(values=c("gold4","darkviolet","green4"))

#Con el argumento labels se puede cambiar la etiqueta de las categorÃ­as en las leyendas
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(color="black", size=0.75)+
  scale_fill_manual(values=c("indianred1","red2","red4"), labels = c("F1","F2","F3"))

#Con el argumento labels se puede cambiar la etiqueta de las categorÃ­as en las leyendas
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(color="black", size=0.75)+
  scale_fill_manual(values=c("indianred1","red2","red4"), labels = c("F1","F2","F3"))

#Con el argumento name se puede cambiar el nombre de la leyenda
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(color="black", size=0.75)+
  scale_fill_manual(values=c("indianred1","red2","red4"), labels=c("F1","F2","F3"),
                    name="Tipo \n de \n flor") #"\n" significa salto de lÃ­nea

ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(color="black", size=0.75)+
  scale_fill_manual(values=c("indianred1","red2","red4"), labels=c("F1","F2","F3"),
                    name="Tipo de flor") #"\n" significa salto de lÃ­nea

#Con el argumento limits se puede cambiar el orden de la leyenda
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(color="black", size=0.75)+
  scale_fill_manual(values=c("indianred1","red2","red4"), name="Tipo \n de Flor",
                    limits=c("virginica", "setosa", "versicolor"))

#Si se quiere modificar otro parÃ¡metro se tienen que agregar exactamente los mismos argumentos
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species, color=Species))+
  geom_point(size=2) + geom_boxplot(color="black", size=0.75) + 
  scale_fill_manual(values=c("indianred1","red2","red4"), labels=c("F1","F2","F3"), name="Tipo de Flor") +
  scale_color_manual(values=c("indianred1","red2","red4"), labels=c("F1","F2","F3"), name="Tipo de Flor")

#De otra forma, aparecerÃ¡n dos leyendas diferentes
ggplot(data = iris, mapping = aes(x=Species, y=Petal.Width, fill=Species, color=Species))+
  geom_point(size=2) + geom_boxplot(color="black", size=0.75) + 
  scale_fill_manual(values=c("indianred1","red2","red4"), labels=c("F1","F2","F3"), name="Tipo de Flor")

####---- Escala de colores para variables discretas ----####

#scale brewer
RColorBrewer::display.brewer.all()
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black") + scale_fill_brewer()

ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ scale_fill_brewer(palette = "Spectral")

ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+scale_fill_brewer(palette = "Accent")

ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black") + scale_fill_brewer(palette = "Set3")

#AquÃ­ tambiÃ©n se pueden modificar las etiquetas y nombre de la leyenda
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) + geom_point()+geom_boxplot(color="black") +
  scale_fill_brewer(palette = "Set3", name="Tipo", labels=LETTERS[1:7])

#scale discrete
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ scale_fill_discrete()

#scale grey
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ scale_fill_grey()

#scale ordinal
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+scale_fill_ordinal()

#scale viridis
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+scale_fill_viridis_d()

##PAQUETE GGTHEMES
#scale calc
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_calc()

#scale colorblind
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_colorblind()

#scale economist
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_economist()

#scale excel
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_excel()

#scale gdocs
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_gdocs()

#scale hc
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_hc()

#scale pander
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_pander()

#scale ptol
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_ptol()

#scale stata
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggthemes::scale_fill_stata()

##PAQUETE GGSCI

#scale aaas
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_aaas()

#scale jama
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_jama()

#scale lancet
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_lancet()

#scale nejm
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_nejm(name="GGG",labels=1:7)

#scale futurama
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_futurama()

#scale rickandmorty
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_rickandmorty()

#scale simpson
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_simpsons()

#scale startrek
ggplot(data = mpg, mapping = aes(x=class, y=hwy, fill=class)) +
  geom_point()+geom_boxplot(color="black")+ ggsci::scale_fill_startrek()

####---- Escala de colores para variables continuas ----####

#Scale binned
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + ggplot2::scale_color_binned()

#Scale gradient2
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + ggplot2::scale_color_gradient2()

#Scale gradient2 tableau
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + ggthemes::scale_color_gradient2_tableau()

#Scale viridis
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis() #Default (D)

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "A") #Magma

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "B") #Inferno

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "C") #Plasma

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "E") #Cividis


##ESCALAR LOS EJES DE LAS GRÃFICAS
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + viridis::scale_color_viridis(option = "A")

#Logaritmo
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") + scale_x_log10()

#RaÃ­z cuadrada
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") + scale_y_sqrt()

#Inverso
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") + scale_x_reverse()

#Separar en segmentos
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") + scale_y_binned()

#Convertir en tiempo
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Petal.Width)) +
  geom_point(size=2) + scale_color_viridis(option = "A") + scale_x_time() + scale_y_time()




ggplot(data = iris,mapping = aes(x=Species,y=Sepal.Length))+geom_col()+
  gganimate::shadow_wake(Species)
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point(aes(colour = Species, group = 1L)) + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)


library(gapminder)
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

####---- Graficos titulos, etiquetas y temas ----####
# el agregar "\n" permite dar espacio 
library(ggpubr)
library(ggthemes)

data(iris)

# Primera forma de colocar titulo y etiquetas en los ejes
ggplot(data = iris,mapping = aes(x=Sepal.Length,fill=Species))+
  geom_density(color="black",size=0.5,alpha=0.75)+
  scale_fill_viridis_d(option="C")+ggtitle("DistribuciÃ³n de Densidad")+
  xlab("Longitud de SÃ©palo")+ylab("Densidad")

# Segunda forma de colocar titulo y etiquetas en los ejes
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) +
  geom_density(color="black", size=1, linetype=5, alpha=0.75) +
  scale_fill_viridis_d(option="C",name="Tipo de flor")+
  labs(x="Longitud",y="Densidad de SÃ©palo",title = "DistribuciÃ³n de la Flor")

### los themes_() son para enmarcar el cuadro
ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Width)) +
  geom_point(size=2) + scale_color_viridis_c(option="C",name="Anchura\nde sÃ©palo") +
  geom_smooth(color="black", method="lm") +
  labs(x="Longitud del PÃ©talo",y="Ancho del PÃ©talo",title = "Distribuciones")+
  theme_light()

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Width)) +
  geom_point(size=2) + scale_color_viridis_c(option="C",name="Anchura\nde sÃ©palo") +
  geom_smooth(color="black", method="lm") +
  labs(x="Longitud del PÃ©talo",y="Ancho del PÃ©talo",title = "Distribuciones")+
  theme_linedraw()

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Width)) +
  geom_point(size=2) + scale_color_viridis_c(option="C",name="Anchura\nde sÃ©palo") +
  geom_smooth(color="black", method="lm") +
  labs(x="Longitud del PÃ©talo",y="Ancho del PÃ©talo",title = "Distribuciones")+
  theme_test()

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Width)) +
  geom_point(size=2) + scale_color_viridis_c(option="C",name="Anchura\nde sÃ©palo") +
  geom_smooth(color="black", method="lm") +
  labs(x="Longitud del PÃ©talo",y="Ancho del PÃ©talo",title = "Distribuciones")+
  theme_bw()

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Width)) +
  geom_point(size=2) + scale_color_viridis_c(option="C",name="Anchura\nde sÃ©palo") +
  geom_smooth(color="black", method="lm") +
  labs(x="Longitud del PÃ©talo",y="Ancho del PÃ©talo",title = "Distribuciones")+
  theme_clean()

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Width)) +
  geom_point(size=2) + scale_color_viridis_c(option="C",name="Anchura\nde sÃ©palo") +
  geom_smooth(color="black", method="lm") +
  labs(x="Longitud del PÃ©talo",y="Ancho del PÃ©talo",title = "Distribuciones")+
  theme_pubr()

## GrÃ¡ficas minimalistas 
ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_viridis_d(option = "C") + ggplot2::theme_minimal()

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_viridis_d(option = "C") + ggthemes::theme_calc()

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_viridis_d(option = "C") + ggpubr::theme_pubr()

ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.75) +
  scale_fill_viridis_d(option = "C") + ggpubr::theme_pubclean()

####---- MÃ¡s personalizaciÃ³n ----####

F1<-ggplot(data = iris, mapping = aes(x=Sepal.Length, fill=Species)) +
  geom_density(color="black", size=1, linetype=5, alpha=0.75) +
  scale_fill_viridis_d(option="C",name="Tipo de flor")+
  labs(x="Longitud",y="Densidad de SÃ©palo",title = "DistribuciÃ³n de la Flor")+
  theme_pubr()

F2<-ggplot(data = iris, mapping = aes(x=Petal.Length, y=Petal.Width, color=Sepal.Width)) +
  geom_point(size=2) + scale_color_viridis_c(option="C",name="Anchura\nde sÃ©palo") +
  geom_smooth(color="black", method="lm") +
  labs(x="Longitud del PÃ©talo",y="Ancho del PÃ©talo",title = "Distribuciones")+
  theme_pubr()

F3<-ggplot(data = iris, mapping = aes(x=Species, y=Sepal.Length, fill=Species)) +
  geom_point(size=1.9, shape=21)+ geom_boxplot(color="black") +
  scale_fill_viridis_d(option = "C", name="Tipo \nde flor")+
  scale_color_viridis_d(option = "C", name="Tipo \nde flor")+
  theme_pubr()+
  labs(title ="Distribuciones de Cajas",
       y="Longitud del SÃ©palo",
       x="Especies de Flores")

#FunciÃ³n "theme" para modificar el formato de la grÃ¡fica

#Borrar elementos: element_blank
F1 + theme(plot.title = element_blank()) #TÃ­tulo
F1 + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) #Texto del eje
F1 + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) #TÃ­tulo del eje
F1 + theme(axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) #LÃ­neas del eje
F1 + theme(legend.text = element_blank()) #Texto de la leyenda
F1 + theme(legend.title = element_blank()) #TÃ­tulo de la leyenda

#Cambiar posiciÃ³n de la leyenda
F1 + theme(legend.position = "top")
F1 + theme(legend.position = "bottom")
F1 + theme(legend.position = "right")
F1 + theme(legend.position = "left")
F1 + theme(legend.position = "none")

#Element_text: justificaciÃ³n horizontal para poner en el centro el titulo
F1 + theme(plot.title = element_text(size=11, hjust = 0))
F1 + theme(plot.title = element_text(size=11, hjust = 1))
F1 + theme(plot.title = element_text(size=14, hjust = 0.5))
F2 + theme(plot.title = element_text(size=14, hjust = 0.5))
F3 + theme(plot.title = element_text(size=14, hjust = 0.5))

#Element_text: justificaciÃ³n vertical
F1 + theme(plot.title = element_text(size=11, hjust = 0.5, vjust = 0)) #Hasta abajo
F1 + theme(plot.title = element_text(size=11, hjust = 0.5, vjust = 1)) #Hasta arriba

#### Element_text: fuente ###
F1 + theme(plot.title = element_text(family = "sans")) #Arial
F1 + theme(plot.title = element_text(family = "serif")) #Times New Roman
F1 + theme(plot.title = element_text(family = "mono")) #Monospace

#### Element_text: formato ###
F1 + theme(plot.title = element_text(face = "plain"))
F1 + theme(plot.title = element_text(face = "bold"))
F1 + theme(plot.title = element_text(face = "italic"))
F1 + theme(plot.title = element_text(face = "bold.italic"))

#### Element_text: color ###
F1A + theme(plot.title = element_text(colour = "red4"))
F1A + theme(plot.title = element_text(colour = "midnightblue"))
F1A + theme(plot.title = element_text(colour = "gold4"))

#### Element_text: tamaÃ±o ###
F1A + theme(plot.title = element_text(size = 1))
F1A + theme(plot.title = element_text(size = 10))
F1A + theme(plot.title = element_text(size = 20))

paste0("Cruz","Azul","campeÃ³n", 2021)
paste0("Cruz ","Azul ","campeÃ³n ", 2021)
paste0("La letra nÃºmero ", 1:4, " del abecedario es ", LETTERS[1:4])

F1<-F1 + theme(plot.title = element_text(size=14, hjust = 0.5))
F2<-F2 + theme(plot.title = element_text(size=14, hjust = 0.5))
F3<-F3 + theme(plot.title = element_text(size=14, hjust = 0.5))


#### Juntar y Guardar las figuras ###
Figuras <- ggarrange(F1, F2, F3, ncol=3, nrow=1)

ggsave(filename="Graph1.png", plot = Figuras,
       units=c("cm"), width = 45, height = 20,
       dpi=200, limitsize=FALSE)

### Los comandos sigueintes sirven para poder colocar anotaciones sobre
### algunos datos estadisticos en las figuras

#Paste0 pega los objetos que se le indiquen como caracteres sin dejar espacios
paste0("Cruz","Azul","campeÃ³n", 2021)
paste0("Cruz ","Azul ","campeÃ³n ", 2021)
paste0("La letra nÃºmero ", 1:4, " del abecedario es ", LETTERS[1:4])

#Round redondea un nÃºmero con los decimales que se le indiquen
round(3.14159265359, 4)
round((1:10)/7, 2)

#Combinar round y paste0
paste0(1:10, " entre 7 es igual a ", round((1:10)/7, 2))

## Anotaciones a cada grafica
anotacion<-tapply(iris$Sepal.Length,iris$Species,mean)
anotacion<-round(anotacion,2)
anotacion<-paste0("Media=", anotacion)

F1<-F1+annotate("text",x=c(5.01,5.94,6.59),y=c(1.3,0.86,0.77),label= anotacion,size=4)

### Anotacion para otra grafica 
#Coeficiente de correlaciÃ³n
ann2 <- cor(x = iris$Petal.Length, y = iris$Petal.Width, method = "spearman")
ann2 <- round(ann2, 3)
ann2 <- paste0("Pearson= ", ann2)

F2 + annotate("text", x=2.25, y=2, label=ann2, size=3.5)




####---- Practica con datos peruanos ----####
data<-read_("D:\\SCRIPTS HECHOS EN r\\Fallecidos.xlsx")
data<-data.frame(data)
data<-as.data.frame(data)
file.choose()
datos<-read_csv2("D:\\SCRIPTS HECHOS EN r\\ded.csv")
rm(dat)
library(tidyverse)
library(dplyr)
library(ggplot2)
datos<-select(datos,-FECHA_CORTE,-id_persona)

data<-mutate(datos,FECHA_FALLECIMIENTO=as.character(FECHA_FALLECIMIENTO),
             FECHA_FALLECIMIENTO=as.Date(FECHA_FALLECIMIENTO,format="%Y%m%d"))

dat<-select(data,FECHA) %>% count(FECHA,name = "Fallecidos")
data<-filter(data,SEXO!=".")

ggplot(data = dat,mapping = aes(x=FECHA,y=Fallecidos))+geom_col()+
  labs(x="Fecha por dÃ­a",
       y="Tendencia de Fallecidos",
       title = "DistribuciÃ³n")+theme_test()



ggplot(data = dat)+geom_col(aes(x=FECHA,y=Fallecidos))

datos_sexo<-select(data,FECHA,SEXO) %>% count(FECHA,SEXO,name = "Fallecidos")

ggplot(data = datos_sexo,mapping = aes(x=FECHA,y=Fallecidos,color=SEXO))+
  geom_line()+labs(x="Fecha",
                   y="Fallecidos")

datos_criterio<-select(data,FECHA,SEXO,CLASIFICACION) %>% count(FECHA,SEXO,CLASIFICACION,name = "FALLECIDOS")

ggplot(data = datos_criterio,mapping = aes(x=FECHA,y=FALLECIDOS,fill=CLASIFICACION))+
  geom_col()+theme_classic()+
  scale_y_continuous(breaks = seq(0,1000,100))+
  scale_x_date(date_breaks = "2 months",date_labels = "%y/%m")+
  scale_fill_viridis_d()+theme_light()+
  labs(x="Fecha por Mes",
       y="Fallecidos",
       title = "Grafica de Olas")

# https://www.stat.berkeley.edu/~s133/dates.html
# https://www.google.com/search?q=format+date+and+time+in+r&rlz=1C1ALOY_esPE946PE946&sxsrf=AOaemvLvALi42wWOFi6oGN9mEOJH1HcD9Q%3A1631549385949&ei=yXc_Ye6vOcWOwbkPpbGo6A8&oq=formatted+date+and+time+in+&gs_lcp=Cgdnd3Mtd2l6EAEYBTIGCAAQFhAeMgYIABAWEB4yBggAEBYQHjIGCAAQFhAeMgYIABAWEB4yBggAEBYQHjIGCAAQFhAeMgYIABAWEB4yBggAEBYQHjIGCAAQFhAeOgcIABBHELADOgUIABDLAToICAAQFhAKEB5KBAhBGABQuGxYqntgl4MBaAFwAngAgAHBAYgB5wWSAQMwLjSYAQCgAQHIAQjAAQE&sclient=gws-wiz



















####---- Practica con Libro ----####

data("Migration")
library(mosaicData)
data("Marriage")

ggplot(Marriage,aes(x=race))+geom_bar(fill="green",color="black")+
  labs(x="Raza",y="Frecuencia",title = "Participantes por Raza")

# cuando tenemos una variable categorica y queremos poner en porcentaje
# la frecuencua debemos unsar la funciónaes(x=race,y=..count../sum(..count..)))+
# scale_y_continuous(labels = scales::percent)
#ggplot(Marriage, 
#       aes(x = race, y = ..count.. / sum(..count..)
#           )) + 
#  geom_bar() +
#  labs(x = "Race", 
#       y = "Percent", 
#       title  = "Participants by race")

ggplot(Marriage,aes(x=race,y=..count../sum(..count..)))+
  geom_bar(fill="green",color="black")+
  labs(x="Raza",y="Frecuencia",title = "Participantes por Raza")+
  scale_y_continuous(labels = scales::percent)

# Las siguientes lineas sirven para poner la cantidad de "n" en los
# cuadros, con el 

plotdata<-Marriage %>% count(race,name = "Cantidad")

ggplot(plotdata,aes(x=race,y=Cantidad))+
  geom_bar(stat = "identity")+geom_text(aes(label=Cantidad),vjust=-0.5)

# Aquí geom_textagrega las etiquetas y vjustcontrola la justificación vertical. 
# Consulte Anotaciones para obtener más detalles.

plotdata <- Marriage %>%
  count(race) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

dos <- Marriage %>%
  count(race) %>%
  mutate(pct = n / 100,
         pctlabel = paste0(round(pct*100), "%"))

# Juntando estas ideas, puede crear un gráfico como el que se muestra a 
# continuación. El signo menos reorder(race, -pct)se utiliza para ordenar las 
# barras en orden descendente.

ggplot(plotdata, 
       aes(x = reorder(race, -pct),
           y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels =scales::percent) +
  labs(x = "Race", 
       y = "Percent", 
       title  = "Participants by race")

# Etiquetas superpuestas
#Las etiquetas de categoría pueden superponerse si (1) hay muchas categorías o 
#(2) las etiquetas son largas. Considere la distribución de funcionarios matrimoniales.

ggplot(Marriage, aes(x = officialTitle)) + 
  geom_bar() +
  labs(x = "Officiate",
       y = "Frequency",
       title = "Marriages by officiate")

# En este caso, puede invertir los ejes xey.

ggplot(Marriage,aes(x=officialTitle))+ geom_bar()+coord_flip()+
  labs(y="Frecuencia",x="")

# Alternativamente, puede rotar las etiquetas de los ejes.

ggplot(Marriage,aes(x=officialTitle))+ geom_bar()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(x="",y="Frecuencia")

ggplot(Marriage, aes(x = officialTitle)) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Marriages by officiate") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))



uno <- Marriage %>%
  count(officialTitle)

ggplot(uno, 
       aes(fill = officialTitle, 
           area = n)) +
  geom_treemap() + 
  labs(title = "Marriages by officiate")

#Mapa de árbol
#Una alternativa a un gráfico circular es un mapa de árbol. A diferencia de 
#los gráficos circulares, puede manejar variables categóricas que tienen muchos
#niveles.
library(treemapify)

dos <- Marriage %>%
  count(race) %>%
  mutate(pct = n / 100,
         pctlabel = paste0(round(pct*100), "%"))


ggplot(dos, 
       aes(fill = race, 
           area = n, 
           label = pctlabel)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "Marriages by officiate") +
  theme()



ggplot(Marriage, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 bins = 20) + 
  labs(title="Participants by age", 
       subtitle = "number of bins = 20",
       x = "Age")


ggplot(Marriage, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 10) + 
  labs(title="Participants by age", 
       subtitle = "binwidth = 5 years",
       x = "Age")



file.choose()
rm(datos)
datos<-read_csv("D:\\SCRIPTS HECHOS EN r\\T de student\\Tratamiento.csv")
library(ggplot2)
library(ggridges)

ggplot(datos, 
       aes(x = Group, 
           y = Tratamiento, 
           fill = Tratamiento)) +
  geom_density_ridges() + 
  theme_ridges() +
  labs("Highway mileage by auto class") +
  theme()


library(dplyr)
library(tidyverse)
library(vroom)
casos<-vroom("https://cloud.minsa.gob.pe/s/AC2adyLkHCKjmfm/download")
casos<-na.omit(casos) %>% filter(EDAD>=0)
str(casos)
glimpse(casos)

casos %>% summarise(mean(EDAD),max(EDAD),min(EDAD))
df<-casos %>% group_by(DEPARTAMENTO) %>% 
  summarise(mean(EDAD),min(EDAD),max(EDAD)) %>% 
  ungroup()
df2<-casos %>% count(DEPARTAMENTO, name = "Casos Positivos")
