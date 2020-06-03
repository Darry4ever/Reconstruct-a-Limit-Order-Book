book.total_volumes <- function(book) {
    ask <-  sum(book$ask$size)
    bid <-  sum(book$bid$size)
    total_volumes <-data.frame(ask,bid)
    return(total_volumes)
}

book.best_prices <- function(book) {
    ask <-  min(book$ask$price)
    bid <-  max(book$bid$price)
    best_prices <-data.frame(ask,bid)
    return(best_prices)
}

book.midprice <- function(book) {
    ask <-  min(book$ask$price)
    bid <-  max(book$bid$price)
    midprice<-(ask+bid)/2
    return(midprice)
}

book.spread <- function(book) {
    ask <-  min(book$ask$price)
    bid <-  max(book$bid$price)
    spread<-ask-bid
    return(spread)
}


book.add <- function(book, message) {
    book<-book.sort(book)
    deleteBidArray<-'0'
    deleteAskArray<-'0'
    if(message$side=='S'){#message是ask 
        if (message$price<=max(book$bid$price)){
            for (j in 1:nrow(book$bid)){
                if (message$price<=book$bid$price[j]){
                    if (book$bid$size[j]>=message$size){#bid的size大，删掉这个message$size,bid$size减掉message$size
                        book$bid$size[j]<-book$bid$size[j]-message$size
                        message$size<-0
                    }else if (book$bid$size[j]<message$size){#message的size大，删掉这个bid,message$size减掉bid$size
                        deleteBidArray<-c(deleteBidArray,book$bid$oid[j])
                        message$size<-message$size-book$bid$size[j]
                        book$bid$size[j]<-0
                    }
                }
            }
        }
        if (message$size!=0){
            row<-data.frame(oid=message$oid,price=message$price,size=message$size, stringsAsFactors = FALSE)
            book$ask<- rbind(book$ask, row, stringsAsFactors = FALSE)
        }
    }else if(message$side=='B'){#message是bid，bid的price要大于最小ask的price
        if (message$price>=min(book$ask$price)){
            for (j in 1:nrow(book$ask)){
                if (message$price>=book$ask$price[j]){
                    if (book$ask$size[j]>=message$size){#ask的size大，删掉这个message$size,ask$size减掉message$size
                        book$ask$size[j]<-book$ask$size[j]-message$size
                        message$size<-0
                    }else if (book$ask$size[j]<message$size){#message的size大，删掉这个ask,message$size减掉ask$size
                        deleteAskArray<-c(deleteAskArray,book$ask$oid[j])
                        message$size<-message$size-book$ask$size[j]
                        book$ask$size[j]<-0
                    }
                }
                
            }
        }
        if (message$size!=0){
            row<-data.frame(oid=message$oid,price=message$price,size=message$size, stringsAsFactors = FALSE)
            book$bid<- rbind(book$bid, row, stringsAsFactors = FALSE)
        }
    }
    if (length(deleteAskArray)!=1){
        for (i in deleteAskArray)
            if (i!="0")
                #print(i)
                book$ask<-book$ask[book$ask$oid!=i,]
    }
    if (length(deleteBidArray)!=1){
        for (i in deleteBidArray)
            if (i!="0")
                book$bid<-book$bid[book$bid$oid!=i,]
    }
    return(book)
}

book.reduce <- function(book, message) {
    bookReduceOid<-""
    if (nrow(book$ask)!=0){
        for (i in 1:nrow(book$ask)){
            if (message$oid==book$ask$oid[i]){
                if (message$amount>=book$ask$size[i]){
                    bookReduceOid<-message$oid
                }
                else if (message$amount<book$ask$size[i])
                    book$ask$size[i]<-book$ask$size[i]-message$amount
            }
        }
    }
    if (!is.null(bookReduceOid)){
        book$ask<-book$ask[book$ask$oid!=bookReduceOid,]
    }
    bookReduceBidOid<-""
    for (i in 1:nrow(book$bid)){
        if (message$oid==book$bid$oid[i]){
            if (message$amount>=book$bid$size[i]){
                bookReduceBidOid<-message$oid
            }
            else if (message$amount<book$bid$size[i]){
                book$bid$size[i]<-book$bid$size[i]-message$amount
            }
        }
    }
    if (!is.null(bookReduceBidOid)){
        book$bid<-book$bid[book$bid$oid!=bookReduceBidOid,]
    }
    book.sort(book)
    return(book)    
}

book.handle <- function(book, row) {
    if (row$type == 'A')
        return(book.add(book, list(
            oid=row$oid,
            side=row$side,
            price=as.numeric(row$price),
            size=as.numeric(row$size)
        )))
    else if (row$type == 'R')
        return(book.reduce(book, list(
            oid=row$oid,
            amount=as.numeric(row$size)
        )))
    else {
        warn("Unknown row type.")

        return(book)
    }
}

book.load <- function(path) {
    df <- read.table(
        path, fill=NA, stringsAsFactors=FALSE, header=TRUE, sep=','
    )

    book.sort(list(
        ask=df[df$side == "S", c("oid", "price", "size")],
        bid=df[df$side == "B", c("oid", "price", "size")]
    ))
}

book.summarise <- function(book, with_stats=T) {
    if (nrow(book$ask) > 0)
        book$ask <- book$ask[nrow(book$ask):1,]

    print(book)

    if (with_stats) {
        clean <- function(x) { ifelse(is.infinite(x), NA, x) }

        total_volumes <- book.total_volumes(book)
        best_prices <- lapply(book.best_prices(book), clean)
        midprice <- clean(book.midprice(book))
        spread <- clean(book.spread(book))

        cat("Total volume:", total_volumes$bid, total_volumes$ask, "\n")
        cat("Best prices:", best_prices$bid, best_prices$ask, "\n")
        cat("Mid-price:", midprice, "\n")
        cat("Spread:", spread, "\n")
    }
}

book.sort <- function(book, sort_bid=T, sort_ask=T) {
    if (sort_ask && nrow(book$ask) >= 1) {
        book$ask <- book$ask[order(book$ask$price,
                                   nchar(book$ask$oid),
                                   book$ask$oid,
                                   decreasing=F),]
        row.names(book$ask) <- 1:nrow(book$ask)
    }

    if (sort_bid && nrow(book$bid) >= 1) {
        book$bid <- book$bid[order(-book$bid$price,
                                   nchar(book$bid$oid),
                                   book$bid$oid,
                                   decreasing=F),]
        row.names(book$bid) <- 1:nrow(book$bid)
    }
    book
}

book.reconstruct <- function(data, init=NULL, log=F) {

    if (nrow(data) == 0) return(book)
    if (is.null(init)) init <- book.init()

    book <- Reduce(
         function(b, i) {
             new_book <- book.handle(b, data[i,])
             if (log) {
                 cat("Step", i, "\n\n")
                 book.summarise(new_book, with_stats=F)
                 cat("====================\n\n")
             }
             new_book
         },
         1:nrow(data), init,
    )
    book.sort(book)
    #print("reconstructsort")
    #print(book)
}

data.load <- function(data_path, n_rows=-1) {
    data <- read.table(
        data_path,
        fill=NA,
        stringsAsFactors=FALSE,
        col.names=c("type", "oid", "side", "price", "size"),
        nrows=n_rows,
    )

    data[data$type == 'R', "size"] <- data[data$type == 'R', "side"]
    data[data$type == 'R', "side"] <- NA

    data
}

if (!interactive()) {
    options(warn=-1)

    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) != 2) {
        stop("Must provide two arguments: <path_to_book> <path_to_messages>")
    }
    book_path <- args[1]; data_path <- args[2]

    if (!file.exists(data_path) || !file.exists(book_path)) {
        stop("File does not exist at path provided.")
    }

    book <- book.load(book_path)
    book <- book.reconstruct(data.load(data_path), init=book)

    book.summarise(book)
}
