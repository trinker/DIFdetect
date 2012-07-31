\name{easyview}
\alias{easyview}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Easy viewing of sig objects
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
easyview(sigsobject)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sigsobject}{
%%     ~~Describe \code{sigsobject} here~~
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
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (sigsobject) 
{
    FUN <- function(x) {
        x <- ifelse(x == FALSE, "-", ifelse(x == TRUE, "WARNING", 
            x))
        return(as.factor(x))
    }
    data.frame(sapply(sigsobject, FUN), check.names = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
