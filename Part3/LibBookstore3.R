# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||                                                            ||
# ||                     Epsilon Data Labs                      ||
# ||   Proyecto: Bookstore                                      ||
# ||   Post: Part3. Simulando los resultados.                   ||
# ||   Autor: Eloy Chang.                                       ||
# ||   Archivo: LibBookstore3.R                                 ||
# ||                                                            ||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


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
    UsedBooks<<- dbGetQuery(con, 'select * from bookstore."UsedBooks"')
    Sellers<<- dbGetQuery(con, 'select * from bookstore."Sellers"')
    Poll<<- dbGetQuery(con, 'select * from bookstore."Poll"')
    Profit<<- dbGetQuery(con, 'select * from bookstore."Profit"')
    Parametros<<- dbGetQuery(con, 'select * from bookstore."Parametros"')
    dbDisconnect(con)
    dbUnloadDriver(drv)
    return()
}

.GenParametros<- function(Seed){
    # ----| Help: .SimulatePoll |----
    # Esta función simula los parametros poblacionales relacionados a la 
    # compra de un libro en general. 
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

.Analitic<- function(UsedBooks,Books,Poll,ProfitRate){
    # ----| help: .Analitic |----
    # Esta es la función se encargara de evaluar las ganancias que generan los
    # libros usados disonibles para la compra. 
    #
    # Argumentos:
    # - UsedBooks: DataFrame. Salida de la función .GenBooks2
    # - Books: DataFrame. Salida de la función .GenBooks de la parte 1 del proyecto.
    # - Poll: DataFrame. Salida de la función .SimulatePoll
    # - ProfitRate: Numérico. Ganancia por venta de libros, debe de ser el mismo 
    #   usado en la parte 1 y 2. 
    
    # ----| Procesamiento |----
    
    # Calculamos el porcentaje de reducción de precio según la aceptación por la
    # condicion de los libros
    Prices<- numeric(length = 3);names(Prices)<- c("Excellent","Good","Bad")
    Prices[1]<- mean(Poll$Pregunta1)/5
    Prices[2]<- mean(Poll$Pregunta2)/5
    Prices[3]<- mean(Poll$Pregunta3)/5
    Profit<- data.frame(Profit=numeric(length = nrow(UsedBooks)),
                        ProfitRate=numeric(length = nrow(UsedBooks)))
    for(i in 1:nrow(UsedBooks)){
        Profit$Profit[i]<- (Books$Price[Books$BookID==UsedBooks$BookID[i]]*
                                Prices[names(Prices)==UsedBooks$Condition[i]]*ProfitRate) - UsedBooks$Price[i] 
    }
    Profit$ProfitRate<- Profit$Profit/UsedBooks$Price
    return(Profit)
}

.SimSale<- function(Operation,Inventory,Sales,Books,Authors,TID,ProfitRate){
    # ----| Help: .UpdateSale |----
    # Esta Función simula las ventas realizadas por la libreria
    #
    # Argumentos:
    # - Operation: Lista. Contiene información detallada de las caracteristicas 
    #     de la operación.
    # - Inventory: Dataframe. Tabla de inventario de libros de la libreria.
    # - Sales: Dataframe. Tabla de ventas generadas por la libreria. 
    # - Books: Dataframe. Tabla con información de los libros disponibles en la 
    #     libreria.
    # - Authors: dataframe. Tabla con información de los autores de los libros
    #     disponibles en la libreria.
    
    # ----| Procesamiento |----
    
    TID<<- TID + 1
    p<- which(Books$BookID %in% Operation$Books)
    Books$UnitsSold[p]<<- Books$UnitsSold[p] + 1
    Authors$UnitsSold[Authors$AuthorID %in% Books$AuthorID[p]]<<- 
        Authors$UnitsSold[Authors$AuthorID %in% Books$AuthorID[p]] + 1
    p<- which(Inventory$ProductID %in% Operation$Books)
    Sales<<- rbind(Sales,data.frame(ProductID=Operation$Books,
                                    TransactionID=rep(TID,length(p)),
                                    EmployeeID=rep(Operation$Employee,length(p)),
                                    Price=Inventory$Price[p]*ProfitRate))
    Inventory<<- Inventory[-which(p),]
    return(TRUE)
}

.SimPurchase<- function(Operation,Inventory,Purchases,Books,Authors,TID,PID){
    # ----| Help: .SimPurchase |----
    # Esta Función simulara la compra de libros por parte de la libreria.
    #
    # Argumentos:
    # - Operation: Lista. Contiene información detallada de las caracteristicas 
    #     de la operación.
    # - Inventory: Dataframe. Tabla de inventario de libros de la libreria.
    # - Purchases: Dataframe. Tabla de compras realizadas por la libreria. 
    # - Books: Dataframe. Tabla con información de los libros disponibles en la 
    #     libreria.
    # - Authors: dataframe. Tabla con información de los autores de los libros
    #     disponibles en la libreria.
    
    # ----| Procesamiento |----
    
    if(Operation$Type == "New"){
        auxbooks<- tapply(Inventory$BookID,Inventory$BookID,length)
        if((sum(auxbooks < 8) >= 3) | (sum(auxbooks < 3) > 0)){
            TID<<- TID + 1;PID<- PID + 1
            auxbooks<- (10 - auxbooks[auxbooks < 10])*3
            auxbooks<- auxbooks[order(names(auxbooks))]
            for(i in 1:length(auxbooks)){
                BookID<- names(auxbooks)[i]
                AuthorID=Books$AuthorID[Books$BookID == BookID]
                Books$UnitsBought[Books$BookID == BookID]<<- Books$UnitsBought[Books$BookID == BookID] + auxbooks[i]
                Authors$UnitsBought[Authors$AuthorID == AuthorID]<<- Authors$UnitsBought[Authors$AuthorID == AuthorID] + auxbooks[i]
                Inventory<<- rbind(Inventory,
                                  data.frame(ProductID=PID:(PID + auxbooks[i] - 1),
                                             BookID=rep(BookID,auxbooks[i]),
                                             AuthorID=rep(AuthorID,auxbooks[i]),
                                             Price=rep(Books$Price[Books$BookID == BookID],auxbooks[i])))
                Purchases<<- rbind(Purchases,
                                  data.frame(ProducID=PID:(PID + auxbooks[i] - 1),
                                             TransactionID=rep(TID,auxbooks[i]),
                                             EmployeeID=rep(Operation$Employee,auxbooks[i]),
                                             Price=rep(Books$Price[Books$BookID == BookID],auxbooks[i])))
                PID<<- PID + auxbooks[i]
            }
        }
    }
    else{
        TID<<- TID + 1;PID<- PID + 1
    }
}

.SimClient<- function(Parametros,ParametrosCompra){
    # ----| Help: .SimClient |----
    # Esta Función simulara a un cliente que llega a la libreria 
    #
    # Argumentos:
    # - Parametros: Dataframe. Parametros caracteristico de los tipos de 
    #    clientes que frecuentan la libreria sobre los libros usados.
    # - Parametros: Dataframe. Parametros caracteristico de los tipos de 
    #    clientes que frecuentan la libreria sobre la compra de libros en general.
    # ----| Procesamiento |----
    
    BuyList<- numeric()
    Profit<- 0
    p<- which.max(ParametrosCompra$Proporcion > runif(1))
    aux<- rnorm(1,ParametrosCompra$Media[p],ParametrosCompra$SD[p])
    ready<- runif(1) > aux
    while(!ready){
        p<- which.max(Parametros$Proporcion > runif(1))
        aux<- rnorm(1,Parametros$Media[p],Parametros$SD[p])
        if(runif(1)<aux){
            p<- ceiling(runif(1)*sum(Inventory$Condition != "New"))
            p<- 1:nrow(Inventory)[Inventory$Condition != "New"][p]
        }
        else{
            p<- ceiling(runif(1)*sum(Inventory$Condition == "New"))
            p<- 1:nrow(Inventory)[Inventory$Condition == "New"][p]
        }
        BuyList<- c(BuyList,Inventory$ProductID[p])
        Profit<- Profit + Inventory$Price[p]
        Books$UnitsSold[Inventory$BookID[p]]<<- Books$UnitsSold[Inventory$BookID[p]] + 1
        Authors$UnitsSold[Inventory$AuthorID[p]]<<- Authors$UnitsSold[Inventory$AuthorID[p]] + 1
        Inventory<<- Inventory[p,]
    }
    aux<- length(BuyList)> 0
    if(aux) Client<- list(Bought=TRUE,Operation=list(Books=BuyList,
                                                     Employee=ceiling(runif(1)*5)),
                          Profit=Profit)
    else Client<- list(Bought=FALSE)
    return(Client)
}

.SimBookStore<- function(Days,UsedSellersFreq=5,ProfitRate=1.1){
    # ----| Help: .SimBookStore |----
    # Esta Función simulara tantos dás de actividad de la Libreria como se
    # indiquen. 
    #
    # Argumentos:
    # - Days: Numérico. Días de actividad a simular de la libreria.  
    # - UsedSellersFreq: Numérico. Frecuencia de ofertas de libros usados. 
    
    # ----| Procesamiento |----
    
    .ReadTables()
    if(!("Condition" %in% names(Inventory))){
        Inventory$Condition<- "New"
        .SimPurchase(list(Type="Used",Books=cbind(UsedBooks,Profit),
                          Employee=ceiling(runif(1)*5)),Inventory,Purchases)
        ParametrosCompra<- .GenParametros(18)
    }  
    Statistics<- data.frame(Clients=numeric(length = Days),
                            GoodClients=numeric(length = Days),
                            BooksSolds=numeric(length = Days),
                            BooksPurchases=numeric(length = Days),
                            Profit=numeric(length = Days),
                            NetProfit=numeric(length = Days))
    TID<- max(c(Sales$TransactionID,Purchases$TransactionID))
    PID<- max(Inventory$ProductID)
    for(day in 1:Days){
        Clients<-GoodClients<-BooksSolds<-BooksPurchases<-Profit<-time<- 0
        ready<- FALSE
        aux<- rpois(1,rnorm(1,60,10))
        while(!ready){
            time<- time + aux
            Client<- .SimClient(Parametros)
            Clients<- Clients + 1
            if(Client$Bought){
                .SimSale(Client$Operation,Inventory,Sales,Books,Authors,TIDProfitRate)
                GoodClients<- GoodClients + 1
                BooksSolds<- BooksSolds + length(Clients$Operation$Books)
                Profit<- Profit + Client$Profit
            }
            aux<- rpois(1,rnorm(1,60,10))
            ready<- time + aux > 28800
        }
        NetProfit<- Profit;aux<- 0
        if(day %% UsedSellersFreq == 0 & sum(Inventory$Condition != "New") < 50){
            UsedBooks<- .GenBooks2(round(runif(1)*100), Books, Sellers)
            Profit<- .Analitic(UsedBooks,Books,Poll,ProfitRate)
            aux<- .SimPurchase(list(Type="Used",Books=cbind(UsedBooks,Profit),
                              Employee=ceiling(runif(1)*5)),Inventory,Purchases,
                              Books,Authors,TID,PID)
        }
        NetProfit<- NetProfit - aux - 
            .SimPurchase(list(Type="New",Employee=ceiling(runif(1)*5)),
                                Inventory,Purchases,Books,Authors,TID,PID)
        Statistics[day,]<- c(Clients,GoodClients,BooksSolds,BooksPurchases,
                             Profit,NetProfit)
    }
    return(Statistics)
}