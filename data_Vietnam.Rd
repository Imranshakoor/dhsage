\name{Vietnam}
\alias{viet_2005}
\alias{viet_2002}
\alias{viet_1997}
\title{The data represent the number of women of reproductive age in Vietnam
}

\usage{
viet_2005
viet_2002
viet_1997
}

\description{
The function provides the number of women of reproductive age 15-49 in Vietnam based on the demographic health survey of 2005, 2002 and 1997.
}
\arguments{
\item{viet_2005}{
A vector of (non-negative) count values.
}
\item{viet_2002}{
A vector of (non-negative) count values.
}
\item{viet_1997}{
A vector of (non-negative) count values.
}
}
\details{Given the results of the demographic health surveys from 2005, 2002, and 1997, the function gives the number of Vietnamese women in the reproductive age range of 15 to 49.
}
\value{
viet_1997, viet_2002 and viet_2005 give the number of women of reproductive age 15-49 in Vietnam.
}
\references{
General Statistical Office (GSO), National Institute of Hygiene and Epidemiology (NIHE) [Vietnam] and ORC Macro. 2006. Vietnam Population and AIDS Indicator Survey 2005. Calverton, Maryland, USA: GSO, NIHE, and ORC Macro.

Committee for Population, Family and Children [Vietnam], and ORC Macro. 2003. Vietnam Demographic and Health Survey 2002. Calverton, Maryland, USA: Committee for Population, Family and Children and ORC Macro.

Committee for Population, Family and Children [Vietnam], and ORC Macro. 1999. Vietnam Demographic and Health Survey 1997. Calverton, Maryland, USA: Committee for Population, Family and Children and ORC Macro.
}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{india_2005}, \link{camb_2005}, \link{armen_2005}, \link{heaping}}
}


\examples{
x <- viet_2005
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
