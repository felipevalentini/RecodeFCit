\name{checkTypo}
\alias{checkTypo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check for typos in the data
%%  ~~function to do ... ~~
}
\description{
Check for errors in the dataset, likewise typos and whitespaces.
It might happen in the paper and pencil format.
Function for MOLE format only.
}
\usage{
checkTypo(data, RecSpc = T, Cd = c("a", "b", "c"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Row data in a format of least and most.
Data must have two collums for each block: one for the least and one for the most, IN ORDER.
}
  \item{RecSpc}{
Logical argument (\code{T} or \code{F}): Do you want to remove whitespace from the whole data?
}
   \item{Cd}{
A vector with all possible codings for the answers (ex. \code{c(1,2,3)}).
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
Dr. Felipe Valentini & Leonardo Mose

Graduate School of Psychological Assessment

Universidade Sao Francisco, Brazil
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## You must inform the codes for least and most choices into a vector ('Cd')
## Cd changes according to your dataset
## default for Cd is Cd=('a', 'b', 'c')

checkTypo(dataErr, Cd=c('a','b','c'))

# It returns a data.frame with the information of
# the row and the columns with error on coding

# function returns
#   cell_typo
# 1   r1col19
# 2   r2col20
# In this dataset, errors of coding were found for
# subjects 1 (r1) and 2 (r2), columns 19 and 20, respectively.


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
