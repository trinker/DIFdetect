\name{mhDIF}
\alias{mhDIF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mantel-Haenszel DIF detection
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
scored test items
}
  \item{group.var}{
grouping variable
}
  \item{digits}{
numbe rof digits in the output
}
  \item{alpha}{
alpha level; default is .05
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
mhDIF(dat[, 2:21], dat[, 1])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
