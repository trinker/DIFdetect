\name{sigview}
\alias{sigview}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Easy viewing of sig objects~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sigview(logDIFobject, sigsobject, sigtype = "inter.vs.null")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{logDIFobject}{
%%     ~~Describe \code{logDIFobject} here~~
}
  \item{sigsobject}{
%%     ~~Describe \code{sigsobject} here~~
}
  \item{sigtype}{
%%     ~~Describe \code{sigtype} here~~
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
function (logDIFobject, sigsobject, sigtype = "inter.vs.null") 
{
    ns <- switch(sigtype, inter.vs.null = which(sigsobject[, 
        "DIF_DETECTED"]), all.sig = {
        Y <- c("DIF_DETECTED", "UNIFORM_DIF", "NONUNIFORM_DIF")
        sort(unlist(unique(sapply(Y, function(i) which(sigsobject[, 
            i])))))
    })
    names.s <- as.character(sigsobject[ns, "item"])
    sig.mods <- lapply(ns, function(i) logDIFobject[[i]])
    names(sig.mods) <- names.s
    return(sig.mods)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
