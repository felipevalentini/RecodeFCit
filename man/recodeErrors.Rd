\name{recodeErrors}
\alias{recodeErrors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Recode typos and ties
%%  ~~function to do ... ~~
}
\description{
Recode typos or ties into missing.
We suggest you carefully check the dataset before running this function.
Function for MOLE and Rank format (triplets only).
}
\usage{
recodeErrors(data, type="mole", RecTie=T, RecTyp=T, Cd=c("a","b","c"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Row data in a format of least and most (MOLE) or rank.
If data is set in MOLE format, it must have two columns for each block: one for the least and one for the most, IN ORDER.
The MOLE format is often used in a paper-and-pencil survey.
If data is set in RANK format, it must have three columns for each block: one for each item rank.
The RANK format is often used in online surveys.
Data must be free from typos and ties.
}
  \item{type}{
Dataset format.
         \code{type="mole"} Dataset is in the MOLE format (Most and Least).If data is set in MOLE format, it must have two columns for each block: one for the least and one for the most, IN ORDER.
         \code{type="rank"} Dataset is in the RANK format. If data is set in RANK format, it must have three columns for each block: one for each item rank.
}
  \item{RecTie}{
Logical argument (\code{T} or \code{F}). Do you want to replace ties for missing?
}
  \item{RecTyp}{
Logical argument (\code{T} or \code{F}). Do you want to replace typos for missing?
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
## You Must inform the codes for least and most choices into a vector ('Cd')
## Cd changes according to your dataset
## default for Cd is Cd=('a', 'b', 'c')

dataClean<-
recodeErrors(dataErr, RecTie=T, RecTyp=T, Cd=c('a','b','c'))

# It returns a new data.frame with ties and typos recoded as missing values (NA)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
