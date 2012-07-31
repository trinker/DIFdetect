\name{logDIF}
\alias{logDIF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
logistic DIF detection
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
logDIF(items, group, digits = 3, output = "bare")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{
%%     ~~Describe \code{items} here~~
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
MODS <- logDIF(dat[, 2:21], dat$race)
(MODSnsigs <- sigs(MODS))
(MODSnsigs <- sigs(MODS, questions))
easyview(MODSnsigs)
sigview(MODS, MODSnsigs, "all.sig")  #to view any models that have significant values
sigview(MODS, MODSnsigs)
easyview(MODSnsigs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
