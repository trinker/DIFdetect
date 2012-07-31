row2col <-
function(dataframe, new.col.name = NULL){
    x <- data.frame(NEW = rownames(dataframe), dataframe, 
        check.names=FALSE)
    if(!is.null(new.col.name)) names(x)[1] <- new.col.name
    rownames(x) <- 1:nrow(x)
    return(x)
}
