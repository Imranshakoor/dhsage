\name{Pakistan}
\alias{pak_2017}
\alias{pak_2012}
\alias{pak_2006}
\alias{pak_1990}
\title{The data represent the number of women of reproductive age in Pakistan
}

\usage{
pak_2017
pak_2012
pak_2006
pak_1990
}

\description{
The function provides the number of women of reproductive age 15-49 in Pakistan based on the demographic health survey of 2017, 2012, 2006 and 1990.
}
\arguments{
\item{pak_2017}{
A vector of (non-negative) count values.
}
\item{pak_2012}{
A vector of (non-negative) count values.
}
\item{pak_2006}{
A vector of (non-negative) count values.
}
\item{pak_1990}{
A vector of (non-negative) count values.
}
}
\details{The function provides information on the number of women in Pakistan who are 15–49 years old and of reproductive age based on the demographic health surveys from 2017, 2012, 2006, and 1990.
}

\value{
pak_1990, pak_2006, pak_2012 and pak_2017 give the number of women of reproductive age 15-49 in Pakistan.
}

\references{
National Institute of Population Studies (NIPS) [Pakistan] and ICF. 2019. Pakistan Demographic and Health Survey 2017-18. Islamabad, Pakistan, and Rockville, Maryland, USA: NIPS and ICF.

National Institute of Population Studies (NIPS) [Pakistan] and ICF International. 2013. Pakistan Demographic and Health Survey 2012-13. Islamabad, Pakistan, and Calverton, Maryland, USA: NIPS and ICF International.

National Institute of Population Studies (NIPS) [Pakistan], and Macro International Inc. 2008. Pakistan Demographic and Health Survey 2006-07. Islamabad, Pakistan: National Institute of Population Studies and Macro International Inc.

National Institute of Population Studies - NIPS/Pakistan and Institute for Resource Development - IRD/Macro International. 1992. Pakistan Demographic and Health Survey 1990/1991. Islamabad, Pakistan: NIPS/Pakistan and IRD/Macro International.

}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{jord_2017}, \link{phil_2017}, \link{heaping}}
}


\examples{
x <- pak_2017
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
