mhDIF <-
function(items, group.var, digits = 3, alpha = .05, 
    focal.name = 0, item.map = TRUE){
    require(difR)
    get.ci <- function(r, alpha){
        Q <- qnorm(1 - (alpha/2))
        ci.logOR <- cbind(log(r$alphaMH) - Q*sqrt(r$varLambda), 
            log(r$alphaMH) + Q*sqrt(r$varLambda))
        ci.OR <- exp(ci.logOR)
        return(list(logOR = ci.logOR,OR=ci.OR))
    }
    r <- difMH(items, group.var, focal.name = focal.name)
    cis <- get.ci(r, alpha = alpha)
    colnames(cis[[2]]) <- c('lower.CI', 'upper.CI')
    p <- pchisq(r$MH, 1, lower.tail = FALSE)
    x <- r$alphaMH
    y <- cis[[2]][, 1]
    z <- cis[[2]][, 2]
    DF2 <- data.frame(MH_X.sqrd = r$MH, df = rep(1, length(r$MH)), 
        p.val = p, common.OR = r$alphaMH, cis[[2]])
    DF2 <- round(DF2, digits = digits)
    DF2$ETS <- ifelse(x > .65 & x < 1.53, "A",  
         ifelse(y <= 1 & z >= 1, "A", 
         ifelse((y <= .65 & z >= .65) | (y <= 1.53 & z >= 1.53), "B", 
         ifelse(x > .52 & x < 1.89, "B", "C" ))))
    rownames(DF2) <- r$names
    if(item.map) plot(r)
    return(DF2)
}
