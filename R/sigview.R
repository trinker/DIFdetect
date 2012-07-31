sigview <-
function(logDIFobject, sigsobject, sigtype='inter.vs.null'){
    ns <- switch(sigtype,
        inter.vs.null = which(sigsobject[, "DIF_DETECTED"]),
        all.sig = {Y <- c("DIF_DETECTED", "UNIFORM_DIF", "NONUNIFORM_DIF")
            sort(unlist(unique(sapply(Y, 
                function(i) which(sigsobject[, i])))))
        }
    )
    names.s <- as.character(sigsobject[ns, "item"])
    sig.mods <- lapply(ns, function(i) logDIFobject[[i]])
    names(sig.mods) <- names.s
    return(sig.mods)
}
