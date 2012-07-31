\name{logDIFh}
\alias{logDIFh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
logDIFh(item, total, group, digits = 3, output = "bare")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{item}{
%%     ~~Describe \code{item} here~~
}
  \item{total}{
%%     ~~Describe \code{total} here~~
}
  \item{group}{
%%     ~~Describe \code{group} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{output}{
%%     ~~Describe \code{output} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (item, total, group, digits = 3, output = "bare") 
{
    modn <- glm(item ~ total, family = binomial)
    mod1 <- glm(item ~ total + group, family = binomial)
    mod2 <- glm(item ~ total * group, family = binomial)
    TAB <- function(mod, digits) {
        x <- coefficients(summary(mod))
        round(cbind(x, `exp(B)` = exp(x[, 1]), confint(mod)), 
            digits)
    }
    if (output == "full") {
        OP <- list(`omnibus M3 to NULL` = anova(modn, mod2, test = "Chisq"), 
            `NULL` = summary(modn), `anova NULL` = anova(modn, 
                test = "Chisq"), `omnibus M1` = anova(modn, mod1, 
                test = "Chisq"), `anova M1` = anova(mod1, test = "Chisq"), 
            `betas M1` = TAB(mod1, digits = digits), `omnibus M2` = anova(mod1, 
                mod2, test = "Chisq"), `anova M2` = anova(mod2, 
                test = "Chisq"), `betas M2` = TAB(mod2, digits = digits))
    }
    else {
        OP <- list(`OMNIBUS BLOCK 1: DIF (INTERACTION to NULL)` = anova(modn, 
            mod2, test = "Chisq"), `OMNIBUS BLOCK 2: UNIFORM DIF (ADDIITIVE to NULL)` = anova(modn, 
            mod1, test = "Chisq"), `betas BLOCK 2` = TAB(mod1, 
            digits = digits), `OMNIBUS BLOCK 3: NON-UNIFORM DIF (INTERACTION TO ADDITIVE)` = anova(mod1, 
            mod2, test = "Chisq"), `betas BLOCK 3` = TAB(mod2, 
            digits = digits))
    }
    return(OP)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
