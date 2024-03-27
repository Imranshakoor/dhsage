\name{Philippines}
\alias{phil_2017}
\alias{phil_2013}
\alias{phil_2008}
\alias{phil_2003}
\alias{phil_1998}
\alias{phil_1993}
\title{The data represent the number of women of reproductive age in the Philippines
}

\usage{
phil_2017
phil_2013
phil_2008
phil_2003
phil_1998
phil_1993
}

\description{
The function provides the number of women of reproductive age 15-49 in the Philippines based on the demographic health survey of 2017, 2013, 2008, 2003, 1998 and 1993.
}
\arguments{
\item{phil_2017}{
A vector of (non-negative) count values.
}
\item{phil_2013}{
A vector of (non-negative) count values.
}
\item{phil_2008}{
A vector of (non-negative) count values.
}
\item{phil_2003}{
A vector of (non-negative) count values.
}
\item{phil_1998}{
A vector of (non-negative) count values.
}
\item{phil_1993}{
A vector of (non-negative) count values.
}
}
\details{The function provides the number of women in the Philippines between the ages of 15 and 49 who are now capable of bearing children, based on the demographic health surveys carried out in 2017, 2013, 2008, 2003, 1998, and 1993.
}

\value{
phil_1993, phil_1998, phil_2003, phil_2008, phil_2013 and phil_2017 give the number of women of reproductive age 15-49 in the Philippines.
}

\references{
Philippine Statistics Authority (PSA) and ICF. 2018. Philippines National Demographic and Health Survey 2017. Quezon City, Philippines, and Rockville, Maryland, USA: PSA and ICF.

Philippine Statistics Authority (PSA) [Philippines], and ICF International. 2014. Philippines National Demographic and Health Survey 2013. Manila, Philippines, and Rockville, Maryland, USA: PSA and ICF International.

National Statistics Office (NSO) [Philippines], and ICF Macro. 2009. National Demographic and Health Survey 2008. Calverton, Maryland: National Statistics Office and ICF Macro.

National Statistics Office (NSO) [Philippines], and ORC Macro. 2004. National Demographic and Health Survey 2003. Calverton, Maryland: NSO and ORC Macro.

National Statistics Office (NSO)], Department of Health (DOH) [Philippines] and Macro International Inc. (MI). 1999. National Demographic and Health Survey 1998. Manila: NSO and MI.

National Statistics Office (NSO) [Philippines] and Macro International Inc. (MI). 1994. National Demographic Survey 1993. Calverton, Maryland: NSO and MI.
}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{pak_2017}, \link{heaping}}
}


\examples{
x <- phil_2017
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
