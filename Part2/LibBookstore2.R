# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||                                                            ||
# ||                     Epsilon Data Labs                      ||
# ||   Proyecto: Bookstore                                      ||
# ||   Post: Part2. Disenando y simulando una base de datos.    ||
# ||   Autor: Eloy Chang.                                       ||
# ||   Archivo: LibBookstore2.R                                 ||
# ||                                                            ||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#
# Esta libreria contiene todas las funciones usadas en el post "", 
# del proyecto BookStore creado por Epsilon Data Labs.
# Para mayor informacion referirse al post en la direccion:
# 

.ReadTables<- function(){
    # ----| Help: .ReadTables |----
    # Esta Función lee las tablas simuladas en la parte 1 del proyecto. 
    #
    # Argumentos:
    #
    
    # ----| Procesamiento |----
    
    # Conexion a la base de datos
    require(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = "epsilon_dl",host = "localhost", port = 5432, user = "Epsilon", password = "123456")
    # Bajar tablas
    Books<<- dbGetQuery(con, 'select * from bookstore."Books"')
    Authors<<- dbGetQuery(con, 'select * from bookstore."Author"')
    Inventory<<- dbGetQuery(con, 'select * from bookstore."Inventory"')
    Purchases<<- dbGetQuery(con, 'select * from bookstore."Purchases"')
    Sales<<- dbGetQuery(con, 'select * from bookstore."Sales"')
    Employees<<- dbGetQuery(con, 'select * from bookstore."Employees"')
    dbDisconnect(con)
    dbUnloadDriver(drv)
    return()
}

.GenParametros<- function(Seed){
    # ----| Help: .SimulatePoll |----
    # Esta función simula los parametros poblacionales relacionados a la 
    # aceptación de libros de segunda mano como opcion de compra. 
    #
    # Argumentos:
    # Seed: Numérico. Semilla para las simulaciones. 
    
    # ---| Procesamiento |----
    
    set.seed(Seed)
    # Generamos los parametros poblacionales
    parametros<- data.frame(Tipo=1:4,Media=runif(4),SD=runif(4, max = 0.2),
                            Proporcion=runif(4))
    parametros$Proporcion<- parametros$Proporcion/sum(parametros$Proporcion)
    parametros$Proporcion<- cumsum(parametros$Proporcion)
    return(parametros)
}

.SimulatePoll<- function(N, parametros){
    # ----| Help: .SimulatePoll |----
    # Esta función simula, tanto los parametros poblacionales, como los resultados 
    # de una encuesta sobre la aceptación de compras de libros usados. 
    #
    # Argumentos:
    # N: Numérico. Número de encuestas a realizar.
    # parametros: Dataframe. Salida de la función .GenParametros 
    
    # ---| Procesamiento |----
    
    Poll<- data.frame(Pregunta1=numeric(length = N),Pregunta2=numeric(length = N),
                      Pregunta3=numeric(length = N),Pregunta4=numeric(length = N))
    
    for(i in 1:N){
        p<- which.max(parametros$Proporcion > runif(1))
        Poll[i,]<- rnorm(4,parametros$Media[p],parametros$SD[p])
    }
    p<- which(Poll >= 1) 
    if(length(p)>=1){
        for(i in p){
            Poll[i %% N,ceiling(i/N)]<- 1 - abs(rnorm(1,0,0.1))
        } 
    } 
    p<- which(Poll <= 0)
    if(length(p)>=1){
        for(i in p){
            Poll[i %% N,ceiling(i/N)]<- 0 + abs(rnorm(1,0,0.1))
        }
    }  
    return(ceiling(Poll*5))
}

.GenSellers<- function(N){
    # ----| Help: .GenSellers |----
    # Esta función simulara N vendedores de libros de segunda mano, cada uno con
    # un nivel de ambición según la condición de los libros.
    #
    # Argumentos:
    # - N: Numérico. Número de vendedores a simular. 
    
    # ----| Procesamiento |----
    
    SellerID<- 1:N
    Excellent<- numeric(length = N)
    Good<- numeric(length = N)
    Bad<- numeric(length = N)
    for(i in 1:N){
        Excellent[i]<- runif(1)
        aux<- Excellent[i]*runif(1,min = 0.5)
        Good[i]<- aux
        aux<- aux*runif(1,min = 0.5)
        Bad[i]<- aux
    }
    return(data.frame(SellerID,Excellent,Good,Bad))
}

.GenBooks2<- function(N, Books, Sellers){
    # ----| Help: .GenBooks2 |----
    # Esta función simula una tabla de libros de segunda mano ofrecidos a la tienda
    # los libros son escogidos de forma aleatoria de la tabla Books, al igual que 
    # el estado de los mismos. 
    # 
    # Argumentos:
    # - N:Numérico. Número de libros a simular.
    # - Books. DataFrame. Salida de la función .GenBooks de la parte 1 del proyecto.
    # - Sellers. DataFrame. Salida de la función .GenSellers
    
    # ----| Procesamiento |----
    
    # La tabla de libros de segunda mano ofrecidos tendra las siguientes variables:
    # - BookID: Identificador del libro.
    # - SellerID: Identificador del vendedor.
    # - Condition: Estado fisico del libro.
    # - Price: Precio ofrecido para la compra. 
    
    # Se simulan los libros, los vendedores y las condiciones.
    BookID<- sample(nrow(Books),N,replace = TRUE)
    SellerID<- sample(nrow(Sellers),N,replace = TRUE)
    Condition<- factor(sample(c("Excellent","Good","Bad"),N, replace = TRUE),
                       levels = c("Excellent","Good","Bad"))
    # Calculo del precio
    Price<- numeric(length = N)
    for(i in 1:N){
        Price[i]<- Books$Price[BookID[i]]*Sellers[SellerID[i],
                                                  as.numeric(Condition[i])+1]
    }
    return(data.frame(BookID,SellerID,Condition=as.character(Condition),Price))
}

.SimAll<- function(NSellers, NBooks, NPoll, Seed=8){
    # ---| Help: .SimAll |----
    # Esta función se encarga de realizar todas las simulaciones necesarias para
    # el desarrollo de la parte 2 del proyecto Bookstore.
    #
    # Argumentos:
    # - NSellers: Numérico. Número de vendedores a simular.
    # - NBooks: Numérico. Número de libros usados a simular.
    # - NPoll: Numérico. Número de encuestas a simular.
    # - Seed: Numérico. Semilla para las simulaciones. 
    
    # ----| Procesamiento |----
    
    .ReadTables()
    Sellers<<- .GenSellers(NSellers)
    UsedBooks<<- .GenBooks2(NBooks,Books,Sellers)
    Poll<<- .SimulatePoll(NPoll,Seed)
    return()
}
    
.Analitic<- function(UsedBooks,Books,Poll,ProfitRate=1.1){
    # ----| help: .Analitic |----
    # Esta es la función principal de la parte 2 del proyecto, en la que se 
    # analizaran las tablas simuladas. 
    #
    # Argumentos:
    # - UsedBooks: DataFrame. Salida de la función .GenBooks2
    # - Books: DataFrame. Salida de la función .GenBooks de la parte 1 del proyecto.
    # - Poll: DataFrame. Salida de la función .SimulatePoll
    # - ProfitRate: Numérico. Ganancia por venta de libros, debe de ser el mismo 
    #   usado en la parte 1. 
    
    # ----| Procesamiento |----
    
    # Calculamos el porcentaje de reducción de precio según la aceptación por la
    # condicion de los libros
    Prices<- numeric(length = 3);names(Prices)<- c("Excellent","Good","Bad")
    Prices[1]<- mean(Poll$Pregunta1)/5
    Prices[2]<- mean(Poll$Pregunta2)/5
    Prices[3]<- mean(Poll$Pregunta3)/5
    if(mean(Poll$Pregunta4)<3) print(paste("La aceptación es baja = "
                                           ,round(mean(Poll$Pregunta4)*20,2),
                                           "%",sep = ""))
    Profit<- data.frame(Profit=numeric(length = nrow(UsedBooks)),
                        ProfitRate=numeric(length = nrow(UsedBooks)))
    for(i in 1:nrow(UsedBooks)){
        Profit$Profit[i]<- (Books$Price[Books$BookID==UsedBooks$BookID[i]]*
            Prices[names(Prices)==UsedBooks$Condition[i]]*ProfitRate) - UsedBooks$Price[i] 
    }
    Profit$ProfitRate<- Profit$Profit/UsedBooks$Price
    if(mean(Profit$Profit)<0) {
        print("No es rentable")
        return(Profit)
    }
    print(paste("Es rentable, con una gannacia promedio del ",
                round(mean(Profit$ProfitRate)*100,2),"%", sep = ""))
    return(Profit)
}

.WriteTables<- function(UsedBooks,Poll,Sellers,Profit){
    require(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = "epsilon_dl",host = "localhost", port = 5432, user = "Epsilon", password = "123456")
    dbWriteTable(con, c("bookstore","UsedBooks"), Books, row.names = FALSE)
    dbWriteTable(con, c("bookstore","Sellers"), Sellers, row.names = FALSE)
    dbWriteTable(con, c("bookstore","Poll"), Poll, row.names = FALSE)
    dbWriteTable(con, c("bookstore","Profit"), Profit, row.names = FALSE)
    dbWriteTable(con, c("bookstore","Parametros"), Profit, row.names = FALSE)
    dbDisconnect(con)
    dbUnloadDriver(drv)
    return()
}
