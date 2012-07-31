sigs <-
function(logDIFobject, items=NULL, alpha = .05){ #takes an object from logDIF
    n <- length(logDIFobject)
    sn <- which(sapply(1:n, function(i) logDIFobject[[i]][[1]][["Pr(>Chi)"]][2] < alpha))
    SIG <- lapply(sn, function(i) logDIFobject[[i]])
    names(logDIFobject)
    LOG <- data.frame(item = paste0("ITEM_", 1:length(logDIFobject)))
    LOG$DIF_DETECTED <-sapply(seq_along(logDIFobject), function(i)  
        logDIFobject[[i]][["OMNIBUS BLOCK 1: DIF (INTERACTION to NULL)"]][["Pr(>Chi)"]][2] < .05)
    LOG$UNIFORM_DIF <- sapply(seq_along(logDIFobject), function(i)  
        logDIFobject[[i]][["OMNIBUS BLOCK 2: UNIFORM DIF (ADDIITIVE to NULL)"]][["Pr(>Chi)"]][2] < .05)
    LOG$NONUNIFORM_DIF <- sapply(seq_along(logDIFobject), function(i)  
        logDIFobject[[i]][["OMNIBUS BLOCK 3: NON-UNIFORM DIF (INTERACTION TO ADDITIVE)"]][["Pr(>Chi)"]][2] < .05)
    if (!is.null(items)){
        LOG <- cbind(LOG[, 1, drop=FALSE], left.just(items), LOG[, -1])
    }
    return(LOG)
}
