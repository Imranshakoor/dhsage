\name{Myanmar}
\alias{myan_2016}
\title{The data represent the number of women of reproductive age in Myanmar
}

\usage{
myan_2016
}

\description{
The function provides the number of women of reproductive age 15-49 in Myanmar based on the demographic health survey of 2016.
}
\arguments{
\item{myan_2016}{
A vector of (non-negative) count values.
}
}
\details{According to the 2016 Demographic Health Survey, the function gives the number of women in Myanmar who are between the ages of 15 and 49 and in the reproductive age range.
}

\value{
myan_2016 gives the number of women of reproductive age 15-49 in Myanmar.
}

\references{
Ministry of Health and Sports (MoHS) and ICF. 2017. Myanmar Demographic and Health Survey 2015-16. Nay Pyi Taw, Myanmar, and Rockville, Maryland USA: Ministry of Health and Sports and ICF.
}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{nep_2016}, \link{timor_2016}, \link{mal_2016}, \link{heaping}}
}


\examples{
x <- myan_2016
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
