\name{Cambodia}
\alias{camb_2014}
\alias{camb_2010}
\alias{camb_2005}
\alias{camb_2000}
\title{The data represent the number of women of reproductive age in Cambodia
}

\usage{
camb_2014
camb_2010
camb_2005
camb_2000
}

\description{
The function provides the number of women of reproductive age 15-49 in Cambodia based on the demographic health survey of 2000, 2005, 2010 and 2014.
}
\arguments{
\item{camb_2014}{
A vector of (non-negative) count values.
}
\item{camb_2010}{
A vector of (non-negative) count values.
}
\item{camb_2005}{
A vector of (non-negative) count values.
}
\item{camb_2000}{
A vector of (non-negative) count values.
}
}
\details{Based on the demographic health surveys conducted in 2000, 2005, 2010, and 2014, the function gives data on the number of Cambodian women in the reproductive age range of 15 to 49.
}

\value{
camb_2000, camb_2005, camb_2010 and camb_2014 give the number of women of reproductive age 15-49 in Cambodia.
}

\references{
National Institute of Statistics, Directorate General for Health, and ICF International, 2015. Cambodia Demographic and Health Survey 2014. Phnom Penh, Cambodia, and Rockville, Maryland, USA: National Institute of Statistics, Directorate General for Health, and ICF International.

National Institute of Statistics, Directorate General for Health, and ICF Macro, 2011. Cambodia Demographic and Health Survey 2010. Phnom Penh, Cambodia and Calverton, Maryland, USA: National Institute of Statistics, Directorate General for Health, and ICF Macro.

National Institute of Public Health, National Institute of Statistics [Cambodia] and ORC Macro. 2006. Cambodia Demographic and Health Survey 2005. Phnom Penh, Cambodia and Calverton, Maryland, USA: National Institute of Public Health, National Institute of Statistics and ORC Macro.

National Institute of Statistics, Directorate General for Health [Cambodia], and ORC Macro. 2001. Cambodia Demographic and Health Survey 2000. Phnom Penh, Cambodia, and Calverton, Maryland USA: National Institute of Statistics, Directorate General for Health, and ORC Macro.

}


\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{afgan_2010}, \link{armen_2010}, \link{heaping}}
}


\examples{
x <- camb_2010
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
