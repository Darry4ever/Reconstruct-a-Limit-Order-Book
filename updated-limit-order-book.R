book.total_volumes <- function(book) {
    ask <-  sum(book$ask$size) #add the size of the ask book together
    bid <-  sum(book$bid$size)
    total_volumes <-data.frame(ask,bid) #create a dataframe total_volumes to store the total volumes
    return(total_volumes)
}

book.best_prices <- function(book) {
    ask <-  min(book$ask$price) #find the smallest price of the ask book as its best price
    bid <-  max(book$bid$price) #find the biggest price of the bid book as its best price
    best_prices <-data.frame(ask,bid)
    return(best_prices)
}

book.midprice <- function(book) {
    ask <-  min(book$ask$price)
    bid <-  max(book$bid$price)
    midprice<-(ask+bid)/2 #find the best price of the ask book and the bid book, add them up and divide it by 2
    return(midprice)
}

book.spread <- function(book) {
    ask <-  min(book$ask$price)
    bid <-  max(book$bid$price)
    spread<-ask-bid #spread is the result of minusing the best price of ask by that of bid
    return(spread)
}


book.add <- function(book, message) {
    book<-book.sort(book)
    deleteBidArray<-'0'
    deleteAskArray<-'0' #initialize deleteAskArray for storing the oid of the ask book to be deleted
    if(message$side=='S'){ #message side is "ask" 
        if (message$price<=max(book$bid$price)){ #compare the message price to the largest bid book price
            for (j in 1:nrow(book$bid)){ #compare the bid book to the 
                if (message$price<=book$bid$price[j]){ #compare the price of the message price and the bid book size
                    if (book$bid$size[j]>=message$size){ #bid's size is larger, set message$size to 0
                        book$bid$size[j]<-book$bid$size[j]-message$size
                        message$size<-0
                    }else if (book$bid$size[j]<message$size){ #message's size is larger, set this bid size to 0
                        deleteBidArray<-c(deleteBidArray,book$bid$oid[j]) #put this oid into the array storing the book$bid to be deleted
                        message$size<-message$size-book$bid$size[j] #subtract the size of the bid book from size of this message
                        book$bid$size[j]<-0 #set the size to 0
                    }
                }
            }
        }
        if (message$size!=0){ #if the message size is not 0, add this message into the ask book
            row<-data.frame(oid=message$oid,price=message$price,size=message$size, stringsAsFactors = FALSE)
            book$ask<- rbind(book$ask, row, stringsAsFactors = FALSE)
        }
    }else if(message$side=='B'){ #message belongs to the bid book
        if (message$price>=min(book$ask$price)){ #compare the message price to the min ask book price
            for (j in 1:nrow(book$ask)){ #iterate the ask book
                if (message$price>=book$ask$price[j]){
                    if (book$ask$size[j]>=message$size){#ask's size is larger 
                        book$ask$size[j]<-book$ask$size[j]-message$size #set message$size to 0, substract message$size from the book$ask$size
                        message$size<-0
                    }else if (book$ask$size[j]<message$size){#message's size is larger,delete this ask,substract ask$size from the message$size 
                        deleteAskArray<-c(deleteAskArray,book$ask$oid[j])
                        message$size<-message$size-book$ask$size[j]
                        book$ask$size[j]<-0
                    }
                }
                
            }
        }
        if (message$size!=0){ #compare message size to 0
            row<-data.frame(oid=message$oid,price=message$price,size=message$size, stringsAsFactors = FALSE)
            book$bid<- rbind(book$bid, row, stringsAsFactors = FALSE)
        }
    }
    if (length(deleteAskArray)!=1){ #if there is oid added into the deleteAskArray
        for (i in deleteAskArray)
            if (i!="0")
                book$ask<-book$ask[book$ask$oid!=i,] #delete this row in ask book according to its oid
    }
    if (length(deleteBidArray)!=1){ #if there is oid added into the deleteBidArray
        for (i in deleteBidArray)
            if (i!="0")
                book$bid<-book$bid[book$bid$oid!=i,] #delete this row in bid book according to its oid
    }
    return(book)
}

book.reduce <- function(book, message) {
    bookReduceOid<-"" #initialize the bookReduceOid for storing the oid of the row to be deleted
    if (nrow(book$ask)!=0){ #make sure the ask book is not null
        for (i in 1:nrow(book$ask)){ #iterate the ask book
            if (message$oid==book$ask$oid[i]){ #compare the oid of the ask book to the message oid
                if (message$amount>=book$ask$size[i]) #compare the amount of the message to the ask book size
                    bookReduceOid<-message$oid #set the bookReduceOid to this message oid
                else if (message$amount<book$ask$size[i])
                    book$ask$size[i]<-book$ask$size[i]-message$amount #substract the message amount from the ask book size
            }
        }
    }
    if (!is.null(bookReduceOid)) 
        book$ask<-book$ask[book$ask$oid!=bookReduceOid,] #delete this row from the ask book
    bookReduceBidOid<-""
    for (i in 1:nrow(book$bid)){
        if (message$oid==book$bid$oid[i]){
            if (message$amount>=book$bid$size[i])
                bookReduceBidOid<-message$oid
            else if (message$amount<book$bid$size[i])
                book$bid$size[i]<-book$bid$size[i]-message$amount
        }
    }
    if (!is.null(bookReduceBidOid))
        book$bid<-book$bid[book$bid$oid!=bookReduceBidOid,] #delete the row from the bid book
    book.sort(book) #sort the book
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
