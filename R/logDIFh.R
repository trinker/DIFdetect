logDIFh <-
function(item, total, group, digits=3, output = "bare"){
    modn <- glm(item~total, family=binomial)
    mod1 <- glm(item~total + group, family=binomial)
    mod2 <- glm(item~total * group, family=binomial)
    TAB <- function(mod, digits){
        x <- coefficients(summary(mod))
        round(cbind(x,  "exp(B)" = exp(x[, 1]), confint(mod)), digits)
    }
    if (output == "full") {
        OP <- list("omnibus M3 to NULL"=anova(modn, mod2, test="Chisq"),
            "NULL"=summary(modn), "anova NULL" = anova(modn, test="Chisq"),  
            "omnibus M1"=anova(modn, mod1, test="Chisq"), "anova M1" = anova(mod1, test="Chisq"), 
            "betas M1" = TAB(mod1, digits=digits), "omnibus M2"=anova(mod1, mod2, test="Chisq"), 
            "anova M2" = anova(mod2, test="Chisq"), "betas M2" = TAB(mod2, digits=digits))
    } else {
        OP <- list("OMNIBUS BLOCK 1: DIF (INTERACTION to NULL)"=anova(modn, mod2, test="Chisq"), 
            "OMNIBUS BLOCK 2: UNIFORM DIF (ADDIITIVE to NULL)"=anova(modn, mod1, test="Chisq"),  
            "betas BLOCK 2" = TAB(mod1, digits=digits), 
            "OMNIBUS BLOCK 3: NON-UNIFORM DIF (INTERACTION TO ADDITIVE)"=anova(mod1, mod2, test="Chisq"), 
            "betas BLOCK 3" = TAB(mod2, digits=digits))
    } 
    return(OP)
}
