easyview <-
function(sigsobject) {
    FUN <- function(x) {
        x <- ifelse(x==FALSE, "-", ifelse(x==TRUE, "WARNING", x)) 
        return(as.factor(x))
    }  
    data.frame(sigsobject[, 1, drop = FALSE], 
        sapply(sigsobject,  FUN)[, -1], check.names=FALSE)
}
