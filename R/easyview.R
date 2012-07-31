easyview <-
function(sigsobject) {
    FUN <- function(x) {
        x <- ifelse(x==FALSE, "-", ifelse(x==TRUE, "WARNING", x)) 
        return(as.factor(x))
    }
    data.frame(sapply(sigsobject,  FUN), check.names=FALSE)
}
