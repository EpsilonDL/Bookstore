library(plotly)

rm(list = ls(all.names = TRUE))
hola<- .SimBookStore(30)

hola$Dia=1:30
names(hola)<- c("Clientes","BuenosClientes","Libros","LibrosUsados","LibrosComprados",
                "Ganancia","GananciaNeta","GananciaNetaLU","Dia")

#Clientes
plot_ly(data=hola, x= ~Dia, y= ~Clientes, type = "bar", name = "Total Clientes") %>% 
    add_trace(y = ~BuenosClientes, name = "Buenos Clientes") %>% 
    layout(title = "Clientes por Día")

#Libros por cliente
plot_ly(data = hola, x = ~Dia, y = ~(Libros / Clientes), type = "bar") %>%
    layout(title = "Promedio de libros comprados por cada cliente por día",
           yaxis = list(title = "Libros Por Cliente"))

#Venta Libros
hola$LibrosNuevos<- hola$Libros - hola$LibrosUsados
plot_ly(data= hola, x = ~Dia, y = ~Libros, type = "scatter", mode = "lines", name = "Libros Vendidos") %>%
    add_trace(x = ~Dia, y = ~LibrosUsados, name = "Libros Usados") %>%
    add_trace(x = ~Dia, y = ~LibrosNuevos, name = "Libros Nuevos") %>%
    layout(title = "Libros Vendidos")

#Ganancia
hola$GananciaNetaN<- hola$GananciaNeta - hola$GananciaNetaLU
plot_ly(data = hola, x = ~Dia, y = ~GananciaNeta, type = "scatter", mode = "lines", name = "Ganancia Neta") %>%
    add_trace(x = ~Dia, y = ~GananciaNetaLU, name ="Ganancia Libros Usados") %>%
    add_trace(x = ~Dia, y= ~GananciaNetaN, name = "Ganancia Libros Nuevos") %>%
    layout(title = "Ganancia", yaxis = list(title = "Ganancia Neta"))

# Ganancia Porcentual
hola$GananciaP<- hola$GananciaNeta/hola$Libros
hola$GananciaPLN<- hola$GananciaNetaN/hola$LibrosNuevos
hola$GananciaPLU<- hola$GananciaNetaLU/hola$LibrosUsados
plot_ly(data = hola, x = ~Dia, y = ~GananciaPLU, type = "bar", name = "Libros Usados") %>%
    add_trace(x = ~Dia, y = ~GananciaPLN, name = "Libros Nuevos") %>%
    add_trace(x = ~Dia, y = ~GananciaP, name = "Total Libros") %>%
    layout(title = "Ganancia Porcentual", yaxis = list(title = "Porcentaje de ganancia"))
