logDIF <-
function(items, group, digits=3, output = "bare"){
    dat <- data.frame(items)
    total <- rowSums(dat)
    MODS <- lapply(seq_along(dat), function(i) logDIFh(dat[, i], total=total, 
        group=group, digits=digits, output = output))
    names(MODS) <- paste0("ITEM_", 1:length(MODS))
    return(MODS)
}
