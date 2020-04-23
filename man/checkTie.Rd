\name{checkTie}
\alias{checkTie}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check the data for ties
}
\description{
Check for ties (same answer for the least and the most). It might happen in the paper and pencil format.
Function for MOLE and Rank format (triplets only).
}
\usage{
checkTie(data, type="mole")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  If data is set in MOLE format, it must have two columns for each block: one for the least and one for   the most, IN ORDER.
  The MOLE format is often used in a paper-and-pencil survey.
  If data is set in RANK format, it must have three columns for each block: one for each item rank.
  The RANK format is often used in online surveys.
}
  \item{type}{
Dataset format.
         \code{type="mole"} Dataset is in the MOLE format (Most and Least).If data is set in MOLE format, it must have two columns for each block: one for the least and one for the most, IN ORDER.
         \code{type="rank"} Dataset is in the RANK format. If data is set in RANK format, it must have three columns for each block: one for each item rank.
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

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## You must inform the codes for least and most choices into a vector ('Cd')
## Cd changes according to your dataset
## default for Cd is Cd=('a', 'b', 'c')

checkTie(dataErr, Cd=c('a','b','c'))

# It returns a data.frame with the information of
# the row and the blocks with tied anwer

# function returns
#   block_tied_answer
#1             r1Bl2
#2             r2Bl5
# In this dataset, subjects 1 (r1) and 2 (r2)
# tied their answers for blocks 2 and 5, respectively.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

