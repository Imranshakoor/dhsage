\name{Azerbaijan}
\alias{azer_2006}
\title{The data represent the number of women of reproductive age in Azerbaijan
}

\usage{
azer_2006
}

\description{
The function provides the number of women of reproductive age 15-49 in Azerbaijan based on the demographic health survey of 2006.
}
\arguments{
\item{azer_2006}{
A vector of (non-negative) count values.
}
}
\details{Based on the 2006 demographic health survey, the function gives the number of women in Azerbaijan between the ages of 15 and 49.
}

\value{
azer_2006 gives the number of women of reproductive age 15-49 in Azerbaijan.
}

\references{
State Statistical Committee (SSC) [Azerbaijan] and Macro International Inc. 2008. Azerbaijan Demographic and Health Survey 2006. Calverton, Maryland, USA: State Statistical Committee and Macro International Inc.
}


\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{nep_2006}, \link{pak_2006}, \link{heaping}}
}


\examples{
x <- azer_2006
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
