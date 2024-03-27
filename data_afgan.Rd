\name{Afghanistan}
\alias{afgan_2015}
\alias{afgan_2010}
\title{The data represent the number of women of reproductive age in Afghanistan
}

\usage{
afgan_2015
afgan_2010
}

\description{
The function provides the number of women of reproductive age 15-49 in Afghanistan based on the demographic health survey of 2010 and 2015.
}
\arguments{
\item{afgan_2015}{
A vector of (non-negative) count values.
}
\item{afgan_2010}{
A vector of (non-negative) count values.
}
}
\details{Based on the demographic health surveys from 2010 and 2015, the function gives the number of Afghan women in the reproductive age range of 15 to 49.
}

\value{
afgan_2015 and afgan_2010 give the number of women of reproductive age 15-49 in Afghanistan.
}

\references{
Central Statistics Organization (CSO), Ministry of Public Health (MoPH), and ICF. 2017. Afghanistan Demographic and Health Survey 2015. Kabul, Afghanistan: Central Statistics Organization.

Central Statistics Organization (CSO), Ministry of Public Health (MoPH), and ICF. 2010. Afghanistan Demographic and Health Survey 2010. Kabul, Afghanistan: Central Statistics Organization.
}


\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{afgan_2010}, \link{armen_2010}, \link{camb_2010}, \link{heaping}}
}


\examples{
x <- afgan_2010
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
