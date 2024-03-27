\name{Thailand}
\alias{thai_1987}
\title{The data represent the number of women of reproductive age in Thailand
}

\usage{
thai_1987
}

\description{
The function provides the number of women of reproductive age 15-49 in Thailand based on the demographic health survey of 1987.
}
\arguments{
\item{thai_1987}{
A vector of (non-negative) count values.
}
}
\details{Based on the demographic health survey of 1987, the function gives the number of Thai women in the reproductive age range of 15 to 49.
}

\value{
sri_1987 gives gives the number of women of reproductive age 15-49 in Thailand.
}

\references{
Chayovan, Napaporn, Peerasit Kamnuansilpa,and John Knodel. 1988. Thailand Demographic and Health Survey 1987. Columbia, Maryland, USA: Institute of Population Studies/Thailand and Institute for Resource Development- IRD/Westinghouse.
}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{sri_1987}, \link{heaping}}
}


\examples{
x <- thai_1987
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
