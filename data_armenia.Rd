\name{Armenia}
\alias{armen_2015}
\alias{armen_2010}
\alias{armen_2005}
\alias{armen_2000}
\title{The data represent the number of women of reproductive age in Armenia
}

\usage{
armen_2015
armen_2010
armen_2005
armen_2000
}

\description{
The function provides the number of women of reproductive age 15-49 in Armenia based on the demographic health survey of 2000, 2005, 2010 and 2015.
}
\arguments{
\item{armen_2015}{
A vector of (non-negative) count values.
}
\item{armen_2010}{
A vector of (non-negative) count values.
}
\item{armen_2005}{
A vector of (non-negative) count values.
}
\item{armen_2000}{
A vector of (non-negative) count values.
}
}
\details{Using data from the demographic health surveys conducted in Armenia in 2000, 2005, 2010, and 2015, the function indicates the number of women in Armenia between the ages of 15 and 49 who are fertile.
}

\value{
armen_2015, armen_2010, armen_2005 and armen_2000 give the number of women of reproductive age 15-49 in Armenia.
}

\references{
National Statistical Service [Armenia], Ministry of Health [Armenia], and ICF. 2017. Armenia Demographic and Health Survey 2015-16. Rockville, Maryland, USA: National Statistical Service, Ministry of Health, and ICF.

National Statistical Service [Armenia], Ministry of Health [Armenia], and ICF International. 2012. Armenia Demographic and Health Survey 2010. Calverton, Maryland: National Statistical Service, Ministry of Health, and ICF International.

National Statistical Service [Armenia], Ministry of Health [Armenia], and ORC Macro. 2006. Armenia Demographic and Health Survey 2005. Calverton, Maryland: National Statistical Service, Ministry of Health, and ORC Macro.

National Statistical Service [Armenia], Ministry of Health [Armenia], and ORC Macro. 2001. Armenia Demographic and Health Survey 2000. Calverton, Maryland: National Statistical Service, Ministry of Health, and ORC Macro.

}


\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{afgan_2015}, \link{india_2015}, \link{heaping}}
}


\examples{
x <- armen_2015
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
