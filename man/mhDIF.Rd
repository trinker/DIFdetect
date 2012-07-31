\name{mhDIF}
\alias{mhDIF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mhDIF(items, group.var, digits = 3, alpha = 0.05, focal.name = 0, item.map = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{
%%     ~~Describe \code{items} here~~
}
  \item{group.var}{
%%     ~~Describe \code{group.var} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{focal.name}{
%%     ~~Describe \code{focal.name} here~~
}
  \item{item.map}{
%%     ~~Describe \code{item.map} here~~
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
function (items, group.var, digits = 3, alpha = 0.05, focal.name = 0, 
    item.map = TRUE) 
{
    require(difR)
    get.ci <- function(r, alpha) {
        Q <- qnorm(1 - (alpha/2))
        ci.logOR <- cbind(log(r$alphaMH) - Q * sqrt(r$varLambda), 
            log(r$alphaMH) + Q * sqrt(r$varLambda))
        ci.OR <- exp(ci.logOR)
        return(list(logOR = ci.logOR, OR = ci.OR))
    }
    r <- difMH(items, group.var, focal.name = focal.name)
    cis <- get.ci(r, alpha = alpha)
    colnames(cis[[2]]) <- c("lower.CI", "upper.CI")
    p <- pchisq(r$MH, 1, lower.tail = FALSE)
    x <- r$alphaMH
    y <- cis[[2]][, 1]
    z <- cis[[2]][, 2]
    DF2 <- data.frame(MH_X.sqrd = r$MH, df = rep(1, length(r$MH)), 
        p.val = p, common.OR = r$alphaMH, cis[[2]])
    DF2 <- round(DF2, digits = digits)
    DF2$ETS <- ifelse(x > 0.65 & x < 1.53, "A", ifelse(y <= 1 & 
        z >= 1, "A", ifelse((y <= 0.65 & z >= 0.65) | (y <= 1.53 & 
        z >= 1.53), "B", ifelse(x > 0.52 & x < 1.89, "B", "C"))))
    rownames(DF2) <- r$names
    if (item.map) 
        plot(r)
    return(DF2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
