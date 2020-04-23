\name{recodeData}
\alias{recodeData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Recode data
%%  ~~function to do ... ~~
}
\description{
Recode the data from the format MOLE (least and most) and RANK INTO binary comparisons.
Dataset must be in binary comparions for running T-IRT model in MPlus or Lavaan.
}
\usage{
recodeData(data, type="mole", Cd=c(1,2,3), na=T, rk=2)
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
  \item{Cd}{
A vector with all possible codings for the answers (ex. \code{c(1,2,3)}).
It is used only on MOLE format (If data is in rank format, \code{Cd} will not be used inside the function)
}
  \item{na}{
  Logical argument. If code{TRUE} dataset contains NA. If code{FALSE} dataset does not contain NA. Function will run faster if the dataset does not contain NA.
}
  \item{rk}{ The ranking direction. If \code{rk=1}, the dataset must have items rank in the direct order (from least=1 to most=3). If \code{rk=2}, the dataset must have items rank in the inverted order (from least=3 to most=1). Online platforms, such as survey monkey, often use the inverted order.
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
## MOLE format
## You must inform the codes for least and most choices into a vector ('Cd')
## Cd changes according to your dataset
## default for Cd is Cd=('a', 'b', 'c')

# Data must be free from typos and ties
# If necessary, run 'recodeErros' function before.

dataBinary<-
recodeData(dataCln, Cd=c('a','b','c'))

# It returns a new data.frame with the binary comparisons.
# Ex. i1i2 = 1, if item1 is preferred over item2, OR
#     I1i2 = 0, if Item2 is preferred over item1.
# Each triplet has three possible comparisons.
# Ex. i1i2=it1 vs it2; i1i3= it1 vs it3; i2i3 = it2 vs it3.


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
