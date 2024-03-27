\name{India}
\alias{india_2015}
\alias{india_2005}
\alias{india_1998}
\alias{india_1992}
\title{The data represent the number of women of reproductive age in India
}

\usage{
india_2015
india_2005
india_1998
india_1992
}

\description{
The function provides the number of women of reproductive age 15-49 in India based on the demographic health survey of 1992, 1998, 2005 and 2015.
}
\arguments{
\item{india_2015}{
A vector of (non-negative) count values.
}
\item{india_2005}{
A vector of (non-negative) count values.
}
\item{india_1998}{
A vector of (non-negative) count values.
}
\item{india_1992}{
A vector of (non-negative) count values.
}
}
\details{Using data from the demographic health surveys conducted in 1992, 1998, 2005, and 2015, the function assesses the number of women in India between the ages of 15 and 49 who are fertile.
}

\value{
india_1992, india_1998, india_2005 and india_2015 give the number of women of reproductive age 15-49 in India.
}

\references{
International Institute for Population Sciences (IIPS) and ICF. 2017. National Family Health Survey (NFHS-4), 2015-16: India. Mumbai: IIPS.

International Institute for Population Sciences (IIPS) and Macro International. 2007. National Family Health Survey (NFHS-3), 2005–06: India: Volume I. Mumbai: IIPS.

International Institute for Population Sciences (IIPS) and ORC Macro. 2000. National Family Health Survey (NFHS-2), 1998–99: India. Mumbai: IIPS.

International Institute for Population Sciences (IlPS). 1995. National Family Health Survey (MCH and Family Planning), India 1992-93. Bombay: IlPS.
}


\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{afgan_2015}, \link{armen_2015}, \link{heaping}}
}


\examples{
x <- india_2015
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
