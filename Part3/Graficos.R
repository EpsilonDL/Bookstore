library(plotly)

rm(list = ls(all.names = TRUE))
hola<- .SimBookStore(30)

hola$Dia=1:30

#Clientes
plot_ly(data=hola, x= ~Dia, y= ~Clients, type = "bar", name = "Total Clientes") %>% 
    add_trace(y = ~GoodClients, name = "Buenos Clientes") %>% 
    layout(title = "Clientes por Día", yaxis = list(title = "Clientes"),
           xaxis = list(title = "Días"))

#Libros por cliente
plot_ly(data = hola, x = ~Dia, y = ~(BooksSolds / Clients), type = "bar", name = "Libros vendidos \npor cada cliente") %>%
    add_trace(x = ~Dia, y = ~(BooksSolds / GoodClients), name = "Libros Vendidos \npor cada buen cliente") %>%
    layout(title = "Promedio de libros comprados por cada cliente por día",
           yaxis = list(title = "Libros Por Cliente"),
           xaxis = list(title = "Días"))

#Venta Libros
plot_ly(data= hola, x = ~Dia, y = ~BooksSolds, type = "scatter", mode = "lines", name = "Libros Vendidos") %>%
    add_trace(x = ~Dia, y = ~UsedBooksSold, name = "Libros Usados") %>%
    add_trace(x = ~Dia, y = ~NewBooksSold, name = "Libros Nuevos") %>%
    layout(title = "Libros Vendidos", yaxis= list(title= "Libros"),
           xaxis = list(title = "Días"))

#Ganancia
plot_ly(data = hola, x = ~Dia, y = ~Profit, type = "scatter", mode = "lines", name = "Ganancia Total") %>%
    add_trace(x = ~Dia, y = ~UsedBooksprofit, name ="Ganancia Libros Usados") %>%
    add_trace(x = ~Dia, y= ~NewBooksProfit, name = "Ganancia Libros Nuevos") %>%
    layout(title = "Ganancia", yaxis = list(title = "Ganancia"))

# Ganancia Neta
plot_ly(data = hola, x = ~Dia, y = ~NetProfit, type = "bar", name = "Ganancia Neta") %>%
    add_trace(x = ~Dia, y = ~NewBooksNetProfit, name = "Ganancia Neta de Libros Nuevos") %>%
    add_trace(x = ~Dia, y = ~UsedBooksNetProfit, name = "Ganancia Neta de Libros Usados") %>%
    layout(title = "Ganancia Neta", yaxis = list(title = "Ganancia Neta"),
           xaxis = list(title = "Días"))

# Ganancia por libro
plot_ly(data = hola, x = ~Dia, y = ~(NetProfit/BooksSolds), type = "bar", name = "Total") %>%
    add_trace(x = ~Dia, y = ~(NewBooksNetProfit/NewBooksSold), name = "Libros Nuevos") %>%
    add_trace(x = ~Dia, y = ~(UsedBooksNetProfit/UsedBooksSold), name = "Libros Usados") %>%
    layout(title = "Ganancia Neta por Libro", yaxis = list(title = "Ganancia Neta"),
           xaxis = list(title = "Días"))

# Porcentaje de ganancia
plot_ly(data = hola, y = ~(sum(NetProfit)/sum(TotalSpent))*100, type = "bar", name = "Total") %>%
    add_trace(y = ~(sum(NewBooksNetProfit)/sum(NewBooksTotalSpent))*100, name = "Libros Nuevos") %>%
    add_trace(y = ~(sum(UsedBooksNetProfit)/sum(UsedBooksTotalSpent))*100, name = "Libros Usados") %>%
    layout(title = "Porcentaje de Ganancia", yaxis = list(title = "Porcentaje"),
           xaxis = list(title = "Tipo de Libro"))
