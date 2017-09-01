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
    # compra de un libro nuevo. 
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
                        SellPrice=numeric(length = nrow(UsedBooks)),
                        ProfitRate=numeric(length = nrow(UsedBooks)))
    for(i in 1:nrow(UsedBooks)){
        Profit$Profit[i]<- (Books$Price[Books$BookID==UsedBooks$BookID[i]]*
                                Prices[names(Prices)==UsedBooks$Condition[i]]*ProfitRate) - UsedBooks$Price[i] 
        Profit$SellPrice[i]<- Books$Price[Books$BookID==UsedBooks$BookID[i]]*
            Prices[names(Prices)==UsedBooks$Condition[i]]*ProfitRate
    }
    Profit$ProfitRate<- Profit$Profit/UsedBooks$Price
    return(Profit)
}

.SimPurchase<- function(Operation,Inventory,Purchases,Books,Authors,TID,PID,ProfitRate){
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
    
    spent<- BooksPurchased<- 0
    if(Operation$Type == "New"){
        auxbooks<- tapply(Inventory$BookID,Inventory$BookID,length)
        if((sum(auxbooks < 8) >= 3) | (sum(auxbooks < 3) > 0)){
            TID<- TID + 1;PID<- PID + 1
            auxbooks<- (10 - auxbooks[auxbooks < 10])*3
            auxbooks<- auxbooks[order(names(auxbooks))]
            for(i in 1:length(auxbooks)){
                BookID<- names(auxbooks)[i]
                spent<- spent + (Books$Price[Books$BookID==BookID]*auxbooks[i])
                AuthorID<-Books$AuthorID[Books$BookID == BookID]
                Books$UnitsBought[Books$BookID == BookID]<- Books$UnitsBought[Books$BookID == BookID] + auxbooks[i]
                Authors$UnitsBought[Authors$AuthorID == AuthorID]<- Authors$UnitsBought[Authors$AuthorID == AuthorID] + auxbooks[i]
                Inventory<- rbind(Inventory,
                                  data.frame(ProductID=PID:(PID + auxbooks[i] - 1),
                                             BookID=rep(BookID,auxbooks[i]),
                                             AuthorID=rep(AuthorID,auxbooks[i]),
                                             Price=rep(Books$Price[Books$BookID == BookID],auxbooks[i]),
                                             Condition=rep("New",auxbooks[i]),
                                             SellPrice=rep(Books$Price[Books$BookID == BookID]*ProfitRate,auxbooks[i])))
                Purchases<- rbind(Purchases,
                                  data.frame(ProductID=PID:(PID + auxbooks[i] - 1),
                                             TransactionID=rep(TID,auxbooks[i]),
                                             EmployeeID=rep(Operation$Employee,auxbooks[i]),
                                             Price=rep(Books$Price[Books$BookID == BookID],auxbooks[i])))
                PID<- PID + auxbooks[i]
            }
            BooksPurchased<- sum(auxbooks)
        }
    }
    else{
        Operation$Books<- Operation$Books[Operation$Books$ProfitRate > 0.2,]
        if(nrow(Operation$Books) == 0) return(list(spent=spent,BooksPurchased=BooksPurchased,Inventory=Inventory,
                                                   Purchases=Purchases,Books=Books,Authors=Authors,TID=TID,PID=PID))
        auxbooks<- table(Inventory$BookID)
        if((sum(auxbooks < 8) >= 3) | (sum(auxbooks < 3) > 0)){
            TID<- TID + 1;PID<- PID + 1
            auxbooks<- (10 - auxbooks[auxbooks < 10])
            auxbooks<- auxbooks[as.numeric(names(auxbooks)) %in% Operation$Books$BookID]
            auxbooks<- auxbooks[order(as.numeric(names(auxbooks)))]
            Operation$Books<- Operation$Books[Operation$Books$BookID %in% as.numeric(names(auxbooks)),]
            Operation$Books$Condition<- factor(as.character(Operation$Books$Condition),
                                               levels = c("Excellent","Good","Bad"))
            Operation$Books<- Operation$Books[order(Operation$Books$BookID,
                                                    Operation$Books$Condition),]
            quantity<- table(Operation$Books$BookID)
            for(i in 1:length(auxbooks)){
                BookID<- as.numeric(names(auxbooks)[i])
                auxbooks[i]<- min(auxbooks[i],quantity[i])
                BooksPurchased<- BooksPurchased + auxbooks[i]
                AuthorID<-Books$AuthorID[Books$BookID == BookID]
                Books$UnitsBought[Books$BookID == BookID]<- Books$UnitsBought[Books$BookID == BookID] + auxbooks[i]
                Authors$UnitsBought[Authors$AuthorID == AuthorID]<- Authors$UnitsBought[Authors$AuthorID == AuthorID] + auxbooks[i]
                Inventory<- rbind(Inventory,
                                   data.frame(ProductID=PID:(PID + auxbooks[i] - 1),
                                              BookID=rep(BookID,auxbooks[i]),
                                              AuthorID=rep(AuthorID,auxbooks[i]),
                                              Price=Operation$Books$Price[Operation$Books$BookID == BookID][1:auxbooks[i]],
                                              Condition=Operation$Books$Condition[Operation$Books$BookID == BookID][1:auxbooks[i]],
                                              SellPrice=Operation$Books$SellPrice[Operation$Books$BookID == BookID][1:auxbooks[i]]))
                Purchases<- rbind(Purchases,
                                   data.frame(ProductID=PID:(PID + auxbooks[i] - 1),
                                              TransactionID=rep(TID,auxbooks[i]),
                                              EmployeeID=rep(Operation$Employee,auxbooks[i]),
                                              Price=Operation$Books$Price[Operation$Books$BookID == BookID][1:auxbooks[i]]))
                PID<- PID + auxbooks[i]
                spent<- spent + sum(Operation$Books$Price[Operation$Books$BookID == BookID][1:auxbooks[i]])
            }
        }
    }
    return(list(spent=spent,BooksPurchased=BooksPurchased,Inventory=Inventory,
                Purchases=Purchases,Books=Books,Authors=Authors,TID=TID,PID=PID))
}

.SimClient<- function(Parametros,ParametrosCompra,Inventory,Sales,TID,ProfitRate,Employees,Books,Authors){
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
    Profit<- NetProfit<- 0
    q<- which.max(ParametrosCompra$Proporcion > runif(1))
    aux<- rnorm(1,ParametrosCompra$Media[q],ParametrosCompra$SD[q])
    EmployeeID<- ceiling(runif(1)*5)
    p<- numeric()
    NewBooksSold<-NewBooksProfit<- NewBooksNetProfit<- 0
    if(runif(1) < aux){
        p<- sample(which(Inventory$Condition == "New"),min(sample(5,1),
                                                           sum(Inventory$Condition == "New")))
        NewBooksSold<- length(p)
        BuyList<- c(BuyList,Inventory$ProductID[p])
        Profit<- NewBooksProfit<- sum(Inventory$SellPrice[p])
        Books$UnitsSold[Books$BookID %in% Inventory$BookID[p]]<- Books$UnitsSold[Books$BookID %in% Inventory$BookID[p]] + 1
        Authors$UnitsSold[Authors$AuthorID %in% Inventory$AuthorID[p]]<- Authors$UnitsSold[Authors$AuthorID %in% Inventory$AuthorID[p]] + 1
        NetProfit<- NewBooksNetProfit<- Profit - sum(Inventory$Price[p])
    } 
    q<- which.max(Parametros$Proporcion > runif(1))
    aux<- rnorm(1,Parametros$Media[q],Parametros$SD[q])
    UsedBooksSold<- UsedBooksProfit<- UsedBooksNetProfit<- 0
    if(runif(1) < aux){
        q<- sample(which(Inventory$Condition != "New"),min(sample(5,1),
                                                           sum(Inventory$Condition != "New")))
        BuyList<- c(BuyList,Inventory$ProductID[q])
        tmp<- sum(Inventory$SellPrice[q])
        Profit<- Profit + tmp
        UsedBooksProfit<- tmp
        Books$UnitsSold[Books$BookID %in% Inventory$BookID[q]]<- Books$UnitsSold[Books$BookID %in% Inventory$BookID[q]] + 1
        Authors$UnitsSold[Authors$AuthorID %in% Inventory$AuthorID[q]]<- Authors$UnitsSold[Authors$AuthorID %in% Inventory$AuthorID[q]] + 1
        p<- c(p,q)
        UsedBooksSold<- length(q)
        UsedBooksNetProfit<- (tmp - sum(Inventory$Price[q])) 
        NetProfit<- NetProfit + UsedBooksNetProfit
    }
    if(length(p) > 0){
        TID<- TID + 1
        Sales<- rbind(Sales,
                      data.frame(ProductID=Inventory$ProductID[p],
                                 TransactionID=rep(TID,length(p)),
                                 EmployeeID=rep(EmployeeID,length(p)),
                                 Price=Inventory$Price[p]*ProfitRate,
                                 Condition=Inventory$Condition[p]))
        Employees[Employees$EmployeesID == EmployeeID,]<- c(
            EmployeeID,Employees$Sales[Employees$EmployeesID == EmployeeID] + 1,
            Employees$Purchases[Employees$EmployeesID == EmployeeID],
            Employees$GeneratedProfit[Employees$EmployeesID == EmployeeID] + Profit
        )
        Inventory<- Inventory[-p,]
        Client<- list(Bought=TRUE,Operation=list(Books=BuyList,
                                                 Employee=EmployeeID),
                      Profit=Profit, NetProfit=NetProfit,NewBooksSold=NewBooksSold,
                      UsedBooksSold=UsedBooksSold,NewBooksProfit=NewBooksProfit,
                      UsedBooksProfit=UsedBooksProfit,NewBooksNetProfit=NewBooksNetProfit,
                      UsedBooksNetProfit=UsedBooksNetProfit,
                      Inventory=Inventory,Sales=Sales,Books=Books,
                      Authors=Authors,Employees=Employees,TID=TID)
    } 
    else Client<- list(Bought=FALSE)
    return(Client)
}

.SimBookStore<- function(Days,UsedSellersFreq=5,ProfitRate=1.1,startingweekday="Lunes",seed=NULL){
    # ----| Help: .SimBookStore |----
    # Esta Función simulara tantos dás de actividad de la Libreria como se
    # indiquen. 
    #
    # Argumentos:
    # - Days: Numérico. Días de actividad a simular de la libreria.  
    # - UsedSellersFreq: Numérico. Frecuencia de ofertas de libros usados. 
    
    # ----| Procesamiento |----
    
    # .ReadTables()
    load("~/UNI/Epsilon/Bookstore/Part2/Datos.RData")
    Authors$MaxRating<- as.numeric(Authors$MaxRating)
    Authors$AvgRating<- as.numeric(Authors$AvgRating)
    Authors$PublishedBooks<- as.numeric(Authors$PublishedBooks)
    Authors$UnitsBought<- as.numeric(Authors$UnitsBought)
    Authors$UnitsSold<- as.numeric(Authors$UnitsSold)
    Profit<- .Analitic(UsedBooks,Books,Poll,ProfitRate)
    TID<- max(c(Sales$TransactionID,Purchases$TransactionID))
    PID<- max(Inventory$ProductID)
    if(!("Condition" %in% names(Inventory))){
        Inventory$Condition<- "New"
        Inventory$SellPrice<- Inventory$Price*ProfitRate
        Sales$Condition<- "New"
        tmp<- .SimPurchase(list(Type="Used",Books=cbind(UsedBooks,Profit),
                          Employee=ceiling(runif(1)*5)),Inventory,Purchases,
                     Books,Authors,TID,PID)
        Inventory<- tmp$Inventory;Purchases<- tmp$Purchases;Books<- tmp$Books
        Authors<- tmp$Authors;TID<- tmp$TID;PID<- tmp$PID
    }  
    ParametrosCompra<- .GenParametros(18)
    Statistics<- data.frame(Clients=numeric(length = Days),
                            GoodClients=numeric(length = Days),
                            BooksSolds=numeric(length = Days),
                            UsedBooksSold=numeric(length = Days),
                            NewBooksSold=numeric(length = Days),
                            BooksPurchased=numeric(length = Days),
                            UsedBooksPurchased=numeric(length = Days),
                            NewBooksPurchased=numeric(length = Days),
                            Profit=numeric(length = Days),
                            NewBooksProfit=numeric(length = Days),
                            UsedBooksprofit=numeric(length = Days),
                            NetProfit=numeric(length = Days),
                            UsedBooksNetProfit=numeric(length = Days),
                            NewBooksNetProfit=numeric(length = Days),
                            TotalSpent=numeric(length = Days),
                            NewBooksTotalSpent=numeric(length = Days),
                            UsedBooksTotalSpent=numeric(length = Days),
                            DiaS=numeric(length = Days))
    if(is.null(seed)){
        seed<- Sys.time()
        save(seed,file = "seed.RData")
    }
    set.seed(seed)
    if(startingweekday=="Lunes") wday<- 1
    else if(startingweekday=="Martes") wday<- 2
    else if(startingweekday=="Miercoles") wday<- 3
    else if(startingweekday=="Jueves") wday<- 4
    else if(startingweekday=="Viernes") wday<- 5
    else if(startingweekday=="Sabado") wday<- 6
    else wday<- 7
    for(day in 1:Days){
        lambda<- c(rep(60,4),50,40,70)
        Clients<-GoodClients<-BooksSolds<-UsedBooksSold<-NewBooksSold<-0
        BooksPurchased<-NewBooksPurchased<- UsedBooksPurchased<- 0
        Profit<-NewBooksProfit<-UsedBooksProfit<- 0
        NetProfit<- UsedBooksNetProfit<-NewBooksNetProfit<-time<- 0
        TotalSpent<- UsedBooksTotalSpent<- NewBooksTotalSpent<- 0
        ready<- FALSE
        aux<- rpois(1,rnorm(1,lambda[wday],10))
        while(!ready){
            time<- time + aux
            Client<- .SimClient(Parametros,ParametrosCompra,Inventory,Sales,TID,ProfitRate, 
                                Employees,Books,Authors)
            Clients<- Clients + 1
            if(Client$Bought){
                GoodClients<- GoodClients + 1
                BooksSolds<- BooksSolds + length(Client$Operation$Books)
                UsedBooksSold<- UsedBooksSold + Client$UsedBooksSold
                NewBooksSold<- NewBooksSold + Client$NewBooksSold
                Profit<- Profit + Client$Profit
                NewBooksProfit<- NewBooksProfit + Client$NewBooksProfit
                UsedBooksProfit<- UsedBooksProfit + Client$UsedBooksProfit
                NetProfit<- NetProfit + Client$NetProfit
                UsedBooksNetProfit<- UsedBooksNetProfit + Client$UsedBooksNetProfit
                NewBooksNetProfit<- NewBooksNetProfit + Client$NewBooksNetProfit
                Inventory<- Client$Inventory;Sales<- Client$Sales;Books<- Client$Books
                Authors<- Client$Authors;Employees<- Client$Employees;TID<- Client$TID
            }
            aux<- rpois(1,rnorm(1,60,10))
            ready<- time + aux > 28800
        }
        if(day %% UsedSellersFreq == 0 & sum(Inventory$Condition != "New") < 50){
            UsedBooks<- .GenBooks2(ceiling(runif(1)*100), Books, Sellers)
            Profit2<- .Analitic(UsedBooks,Books,Poll,ProfitRate)
            tmp<- .SimPurchase(list(Type="Used",Books=cbind(UsedBooks,Profit2),
                              Employee=ceiling(runif(1)*5)),Inventory,Purchases,
                              Books,Authors,TID,PID,ProfitRate)
            BooksPurchased<-UsedBooksPurchased<- tmp$BooksPurchased
            TotalSpent<- UsedBooksTotalSpent<- tmp$spent
            Inventory<- tmp$Inventory;Purchases<- tmp$Purchases;Books<- tmp$Books
            Authors<- tmp$Authors;TID<- tmp$TID;PID<- tmp$PID
        }
        tmp<- .SimPurchase(list(Type="New",Employee=ceiling(runif(1)*5)),
                                Inventory,Purchases,Books,Authors,TID,PID,ProfitRate)
        Inventory<- tmp$Inventory;Purchases<- tmp$Purchases;Books<- tmp$Books
        Authors<- tmp$Authors;TID<- tmp$TID;PID<- tmp$PID
        BooksPurchased<- BooksPurchased + tmp$BooksPurchased
        NewBooksPurchased<- tmp$BooksPurchased
        TotalSpent<- TotalSpent + tmp$spent
        NewBooksTotalSpent<- tmp$spent
        Statistics[day,]<- c(Clients,GoodClients,BooksSolds,UsedBooksSold,NewBooksSold,
                             BooksPurchased,UsedBooksPurchased,NewBooksPurchased,
                             Profit,NewBooksProfit,UsedBooksProfit,NetProfit,
                             UsedBooksNetProfit,NewBooksNetProfit,TotalSpent,
                             NewBooksTotalSpent,UsedBooksTotalSpent,wday)
        wday<- ((wday+1) %% 7) + 1
        print(paste("Total Libros =",nrow(Inventory)))
    }
    save(Authors,Books,Employees,Inventory,Parametros,ParametrosCompra,Poll,
         Purchases,Sales,Sellers,UsedBooks, file = "BookstoreSimulatedTables.RData")
    return(Statistics)
}
