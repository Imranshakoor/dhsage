\name{Timor-Leste}
\alias{timor_2016}
\alias{timor_2009}
\title{The data represent the number of women of reproductive age in Timor-Leste
}

\usage{
timor_2016
timor_2009
}

\description{
The function provides the number of women of reproductive age 15-49 in Timor-Leste based on the demographic health survey of 2009 and 2016.
}
\arguments{
\item{timor_2016}{
A vector of (non-negative) count values.
}
\item{timor_2009}{
A vector of (non-negative) count values.
}
}
\details{According to the demographic health surveys from 2009 and 2016, the function gives the number of women in Timor-Leste between the ages of 15 and 49.
}

\value{
timor_2009 and timor_2016 give the number of women of reproductive age 15-49 in Timor-Leste.
}
\references{
General Directorate of Statistics (GDS), Ministry of Health and ICF. 2018. Timor-Leste Demographic and Health Survey 2016. . Dili, Timor-Leste Rockville, Maryland, USA: GDS and ICF.

National Statistics Directorate (NSD) [Timor-Leste], Ministry of Finance [Timor-Leste], and ICF Macro. 2010. Timor-Leste Demographic and Health Survey 2009-10. Dili, Timor-Leste: NSD [TimorLeste] and ICF Macro.
}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{nep_2016}, \link{mal_2016}, \link{heaping}}
}


\examples{
x <- timor_2016
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
