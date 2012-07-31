\name{sigs}
\alias{sigs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Grab significant models
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sigs(logDIFobject, items = NULL, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{logDIFobject}{
%%     ~~Describe \code{logDIFobject} here~~
}
  \item{items}{
%%     ~~Describe \code{items} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
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
function (logDIFobject, items = NULL, alpha = 0.05) 
{
    n <- length(logDIFobject)
    sn <- which(sapply(1:n, function(i) logDIFobject[[i]][[1]][["Pr(>Chi)"]][2] < 
        alpha))
    SIG <- lapply(sn, function(i) logDIFobject[[i]])
    names(logDIFobject)
    LOG <- data.frame(item = paste0("ITEM_", 1:length(logDIFobject)))
    LOG$DIF_DETECTED <- sapply(seq_along(logDIFobject), function(i) logDIFobject[[i]][["OMNIBUS BLOCK 1: DIF (INTERACTION to NULL)"]][["Pr(>Chi)"]][2] < 
        0.05)
    LOG$UNIFORM_DIF <- sapply(seq_along(logDIFobject), function(i) logDIFobject[[i]][["OMNIBUS BLOCK 2: UNIFORM DIF (ADDIITIVE to NULL)"]][["Pr(>Chi)"]][2] < 
        0.05)
    LOG$NONUNIFORM_DIF <- sapply(seq_along(logDIFobject), function(i) logDIFobject[[i]][["OMNIBUS BLOCK 3: NON-UNIFORM DIF (INTERACTION TO ADDITIVE)"]][["Pr(>Chi)"]][2] < 
        0.05)
    if (!is.null(items)) {
        LOG <- cbind(LOG[, 1, drop = FALSE], left.just(items), 
            LOG[, -1])
    }
    return(LOG)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
