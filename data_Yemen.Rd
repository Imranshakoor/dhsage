\name{Yemen}
\alias{yemen_2013}
\alias{yemen_1991}
\title{The data represent the number of women of reproductive age in Yemen
}

\usage{
yemen_2013
yemen_1991
}

\description{
The function provides the number of women of reproductive age 15-49 in Yemen based on the demographic health survey of 1991 and 2013.
}
\arguments{
\item{yemen_2013}{
A vector of (non-negative) count values.
}
\item{yemen_1991}{
A vector of (non-negative) count values.
}
}
\details{Following demographic health surveys conducted in 1991 and 2013 for Yemen, the function gives information on women between the ages of 15 and 49 who are fertile.
}

\value{
yemen_1991 and yemen_2013 give the number of women of reproductive age 15-49 in Yemen.
}
\references{
Ministry of Public Health and Population (MOPHP), Central Statistical Organization (CSO) [Yemen], Pan Arab
Program for Family Health (PAPFAM), and ICF International. 2015. Yemen National Health and Demographic Survey 2013. Rockville, Maryland, USA: MOPHP, CSO, PAPFAM, and ICF International.

Central Statistical Organization (CSO) [Yemen] and Pan Arab Project for Child Development (PAPCHILD)[Egypt]  and Macro International  Inc.(Ml).  1994.  Yemen Demographic and Maternal and Child Health Survey 1991/1992.
Calverton, Maryland: CSO and MI.
}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{phil_2013}, \link{heaping}}
}


\examples{
x <- yemen_2013
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
