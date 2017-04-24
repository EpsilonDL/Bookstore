# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||                                                            ||
# ||                     Epsilon Data Labs                      ||
# ||   Proyecto: Bookstore                                      ||
# ||   Post: Part1. Disenando y simulando una base de datos.    ||
# ||   Autor: Eloy Chang.                                       ||
# ||   Archivo: LibBookstore1.R                                 ||
# ||                                                            ||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#
# Esta libreria contiene todas las funciones usadas en el post "Disenando y 
# simulando una base de datos", del proyecto BookStore creado por Epsilon Data
# Labs.
# Para mayor informacion referirse al post en la direccion:
# 
# O al archivo "EDL-ProjectBookStore-Part1.html".

.GenBook<- function(NBooks=100, NAuthors = 20, BasePrice = 10){
    # ----| Help: .GenBook |----
    # Esta Funcion genera la tabla de libros con los que comercia la tienda 
    # simulada.
    #
    # Argumentos:
    # - NBooks: Numerico. Numero de libros a generar. Valor por defecto=100.
    # - NAuthors: Numerico. Numero de autores diferentes. Valor por defecto=20.
    # - BasePrice: Numerico. Precio minimo de un libro.
    #
    # ----| Procesamiento |----
    
    BookID<- 1:NBooks
    AuthorID<- round(runif(NBooks,min = 1, max = NAuthors))
    Rating<- round(rbeta(NBooks,shape1 = 15,shape2 = 10)*5)
    Price<- BasePrice + rgamma(NBooks,shape = BasePrice, scale = 2)
    Price<- Price*(Rating*rnorm(NBooks,mean = 1, sd = 0.1))
    UnitsBought<- round(rgamma(NBooks,shape = 10, scale = 1))*Rating
    UnitsSold<- round(runif(NBooks)*UnitsBought)
    return(data.frame(BookID,AuthorID,Rating,Price,UnitsBought,UnitsSold))
}

.GenAuthor<- function(Books){
    # ----| Help: .GenAuthor |----
    # Esta Funcion genera la tabla de autores de los libros con los que comercia
    # la tienda simulada.
    #
    # Argumentos:
    # - Books: DataFrame. Salida de la funcion .GenBook
    #
    # ----| Procesamiento |----
    
    N<- max(Books$AuthorID)
    AuthorID<- unique(Books$AuthorID)
    MaxRating<- tapply(Books$Rating,Books$AuthorID,max)
    AvgRating<- tapply(Books$Rating,Books$AuthorID,mean)
    PublishedBooks<- tapply(Books$BookID,Books$AuthorID,length)
    UnitsBought<- tapply(Books$UnitsBought,Books$AuthorID,sum)
    UnitsSold<- tapply(Books$UnitsSold,Books$AuthorID,sum)
    return(data.frame(AuthorID,MaxRating,AvgRating,PublishedBooks,UnitsBought,UnitsSold))
}

.GenInventory<- function(Books){
    # ----| Help: .GenInventory |----
    # Esta Funcion genera la tabla de inventario disponible por la tienda 
    # simulada. 
    #
    # Argumentos:
    # - Books: DataFrame. Salida de la funcion .GenBook
    #
    # ----| Procesamiento |----
    
    CopiesByBook<- Books$UnitsBought - Books$UnitsSold
    ProductID<- numeric();BookID<- numeric();AuthorID<- numeric();Price<- numeric()
    PID<- 1
    for(Book in 1:nrow(Books)){
        if(CopiesByBook[Book]==0){
            next
        }
        ProductID<- c(ProductID,PID:(PID+CopiesByBook[Book]-1))
        BookID<- c(BookID,rep(Book,CopiesByBook[Book]))
        AuthorID<- c(AuthorID,rep(Books$AuthorID[Book], CopiesByBook[Book]))
        Price<- c(Price,rep(Books$Price[Book], CopiesByBook[Book]))
        PID<- PID + CopiesByBook[Book]
    }
    return(data.frame(ProductID,BookID,AuthorID,Price))
}

.GenPurchases<- function(Books, Inventory, NEmployee=5){
    # ----| Help: .GenPurchases |----
    # Esta Funcion genera la tabla de compras realizadas por la tienda simulada
    #
    # Argumentos:
    # - Books: DataFrame. Salida de la funcion .GenBook
    # - Inventory. dataFrame. Salida de la funcion .GEnInventory
    #
    # ----| Procesamiento |----
    
    TID<- 1
    PID<- Inventory$ProductID[nrow(Inventory)] + 1
    TransactionID<- numeric()
    ProductID<- numeric()
    EmployeeID<- numeric()
    Price<- numeric()
    rows<- 1
    for(Book in 1:nrow(Books)){
        entries<- rows:(rows + Books$UnitsBought[Book] - 1)
        TransactionID[entries]<- TID
        EmployeeID[entries]<- round(runif(1,min = 1,max = NEmployee))
        Price[entries]<- Books$Price[Book]
        aux<- Inventory$BookID==Books$BookID[Book]
        if(Books$UnitsSold[Book]!=0){
            ProductID[entries] <- c(Inventory$ProductID[aux],PID:(PID + Books$UnitsSold[Book] - 1))
        }
        else{
            ProductID[entries] <- Inventory$ProductID[aux]
        }
        rows<- rows + Books$UnitsBought[Book]
        TID<- TID + 1
        PID<- PID + Books$UnitsSold[Book]
    }
    return(data.frame(ProductID,TransactionID,EmployeeID,Price))
}

.GenSales<- function(Books, Inventory, Purchases, ProfitRate = 1.1){
    # ----| Help: .GenSales |----
    # Esta Funcion genera la tabla de Ventas realizadas por la tienda simulada
    #
    # Argumentos:
    # - Books: DataFrame. Salida de la funcion .GenBook
    # - Inventory: dataFrame. Salida de la funcion .GenInventory
    # - Purchases: DataFrame. salida de la funcion .GenPurchases
    # - ProfitRate: Numerico. Incremento en el precio de compra para la venta.
    #
    # ----| Procesamiento |----
    
    TID<- max(Purchases$TransactionID) + 1
    ProductID<- numeric()
    TransactionID<- numeric()
    EmployeeID<- numeric()
    Price<- numeric()
    ProductSold<- which(Purchases$ProductID %in% Inventory$ProductID)
    ready<- FALSE
    while(!ready){
        Items<- round(runif(1,min = 1,max = min(10,length(ProductSold))))
        TransactionID<- c(TransactionID,rep(TID,Items))
        EmployeeID<- c(EmployeeID,rep(round(runif(1,min = 0.5,max = 4.99)),Items))
        Items<- sample(1:length(ProductSold),Items)
        ProductID<- c(ProductID,Purchases$ProductID[ProductSold[Items]])
        Price<- c(Price,Purchases$Price[ProductSold[Items]]*ProfitRate)
        ProductSold<- ProductSold[-Items]
        TID<- TID + 1
        ready<- length(ProductSold) == 0
    }
    return(data.frame(ProductID,TransactionID,EmployeeID,Price))
}

.GenEmployees<- function(Purchases,Sales){
    # ----| Help: .GenEmployees |----
    # Esta Funcion genera la tabla de los empleados de la tienda simulada.
    #
    # Argumentos:
    # - Purchases: DataFrame. salida de la funcion .GenPurchases
    # - Sales: DataFrame. Salida de la funcion .GenSales
    #
    # ----| Procesamiento |----
    
    NEmployees<- length(unique(Purchases$EmployeeID))
    EmployeesID<- 1:NEmployees
    E.Sales<- numeric(length = NEmployees)
    E.Purchases<- numeric(length = NEmployees)
    GeneratedProfit<- numeric(length = NEmployees)
    for(i in 1:NEmployees){
        E.Sales[i]<- length(unique(Sales$TransactionID[Sales$EmployeeID==i]))
        E.Purchases[i]<- length(unique(Purchases$TransactionID[Purchases$EmployeeID==i]))
        GeneratedProfit[i]<- sum(Sales$Price[Sales$EmployeeID==i]) - sum(Purchases$Price[Purchases$EmployeeID==i])
    }
    return(data.frame(EmployeesID,Sales=E.Sales,Purchases=E.Purchases,GeneratedProfit))
}

.WriteTables<- function(Books,Author,Inventory,Purchases,Sales,Employees){
    require(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = "epsilon_dl",host = "localhost", port = 5432, user = "Epsilon", password = "123456")
    dbWriteTable(con, c("bookstore","Books"), Books, row.names = FALSE)
    dbWriteTable(con, c("bookstore","Author"), Author, row.names = FALSE)
    dbWriteTable(con, c("bookstore","Inventory"), Inventory, row.names = FALSE)
    dbWriteTable(con, c("bookstore","Purchases"), Purchases, row.names = FALSE)
    dbWriteTable(con, c("bookstore","Sales"), Sales, row.names = FALSE)
    dbWriteTable(con, c("bookstore","Employees"), Employees, row.names = FALSE)
    dbDisconnect(con)
    dbUnloadDriver(drv)
    return()
}
