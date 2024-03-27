\name{Sri Lanka}
\alias{sri_1987}
\title{The data represent the number of women of reproductive age in the Sri Lanka
}

\usage{
sri_1987
}

\description{
The function provides the number of women of reproductive age 15-49 in Sri Lanka based on the demographic health survey of 1987.
}
\arguments{
\item{sri_1987}{
A vector of (non-negative) count values.
}
}
\details{According to the 1987 demographic health census, the function gives Sri Lankan women between the ages of 15 and 49 who are fertile.
}

\value{
sri_1987 gives the number of women of reproductive age 15-49 in Sri Lanka.
}

\references{
Department of Census and Statistics Ministry of Plan Implementation/Sri Lanka 	and Institute for Resource Development/Westinghouse. 1988. Sri Lanka Demographic and Health Survey 1987. Columbia, Maryland, USA: Institute for Resources Development/Westinghouse.
}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{thai_1987}, \link{heaping}}
}


\examples{
x <- sri_1987
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
