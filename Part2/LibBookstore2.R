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
    dbGetQuery(con, "SET search_path TO bookstore")
    # Bajar tablas
    Books<<- dbGetQuery(con, 'select * from "Books"')
    Authors<<- dbGetQuery(con, 'select * from "Author"')
    Inventory<<- dbGetQuery(con, 'select * from "Inventory"')
    Purchases<<- dbGetQuery(con, 'select * from "Purchases"')
    Sales<<- dbGetQuery(con, 'select * from "Sales"')
    Employees<<- dbGetQuery(con, 'select * from "Employees"')
    dbDisconnect(con)
    dbUnloadDriver(drv)
    return()
}

.SimulatePoll<- function(N,Seed){
    # ----| Help: .SimulatePoll |----
    # Esta función simula, tanto los parametros poblacionales, como los resultados 
    # de una encuesta sobre la aceptación de compras de libros usados. 
    #
    # Argumentos:
    # N: Numérico. Número de encuestas a realizar.
    # Seed: Numérico. Semilla para las simulaciones. 
    
    # ---| Procesamiento |----
    
    set.seed(Seed)
    # Queremos generar valores dependientes entre si para los parametros.
    medias<- runif(1)
    desviacion<- min(medias,1-medias)/3
    for(i in 2:4){
        aux<- mean(medias[1:(i-1)])*rnorm(1,1,0.2)
        aux<- min(runif(1,min = 0.9),aux)
        aux<- max(runif(1,max = 0.1),aux)
        medias<- c(medias,aux)
        aux<- min(aux,1-aux)/3
        desviacion<- c(desviacion,aux)
    }
    # Respondemos las N encuestas
    Poll<- data.frame(Pregunta1=numeric(length = N),Pregunta2=numeric(length = N),
                      Pregunta3=numeric(length = N),Pregunta4=numeric(length = N))
    for(i in 1:4){
        Poll[,i]<- rnorm(N,medias[i],desviacion[i])
    }
    Poll<- ceiling(Poll*5)
    return(Poll)
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
    return(data.frame(BookID,SellerID,as.character(Condition),Price))
}

.SimAll<- function(NSellers, NBooks, NPoll, Seed=10){
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
    
.Analitic<- function(UsedBooks,Poll){
    # ----| help: .Analitic |----
    # Esta es la función 
}
