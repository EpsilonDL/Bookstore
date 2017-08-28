library(ggplot2)

hola$Dia=1:30
names(hola)<- c("Clientes","BuenosClientes","Libros","LibrosUsados","LibrosComprados",
                "Ganancia","GananciaNeta","GananciaNetaLU","dia")

#Clientes
plot_ly(data=hola, x= ~Dia, y= ~Clientes, type = "bar", name = "Total Clientes") %>% 
    add_trace(y = ~BuenosClientes, name = "Buenos Clientes") %>% 
    layout(title = "Clientes por Día")

#Libros por cliente
plot_ly(data = hola, x = ~Dia, y = ~(Libros / Clientes), type = "bar") %>%
    layout(title = "Promedio de libros comprados por cada cliente por día",
           yaxis = list(title = "Libros Por Cliente"))

#Venta Libros
plot_ly(data= hola, x = ~Dia, y = ~Libros, type = "scatter", mode = "lines", name = "Libros Vendidos") %>%
    add_trace(x = ~dia, y = ~LibrosUsados, name = "Libros Usados") %>%
    add_trace(x = ~dia, y = ~(Libros - LibrosUsados), name = "Libros Nuevos") %>%
    layout(title = "Libros Vendidos")

#Ganancia
plot_ly(data = hola, x = ~Dia, y = ~Ganancia, type = "scatter", mode = "lines", name = "Ganancia Neta") %>%
    add_trace(x = ~Dia, y = ~GananciaNetaLU, name ="Ganancia Libros Usados") %>%
    add_trace(x = ~Dia, y= ~(GananciaNeta - GananciaNetaLU), name = "Ganancia Libros Nuevos") %>%
    layout(title = "Ganancia")
