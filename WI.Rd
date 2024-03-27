\name{Whipple's index}
\alias{heaping}
\title{Whipple's index calculation for reproductive-age females
}

\usage{
heaping(x, a, b)
}

\description{
The function provides the Whipple's index of females 15 to 49 years old who are in reproductive age with data quality.
}

\arguments{
\item{x}{
a data vector of females, aged 15 to 49 year.
}
\item{a}{
Choose the starting age for the calculation of Whipple's index.
}
\item{b}{
Choose the ending age for the calculation of Whipple's index.
}
}
\details{Whipple's indes can be computed by the following formula
\deqn{WI=\left(\frac{P_{25}+P_{30}+P_{35}+\cdots\cdots+P_{60}}{P_{23}+P_{24}+P_{25}+\cdots\cdots+P_{62}}\right)\times500}
where P represents the population of reported age in completed year. There are 5 categories for the index value, ranging from very rough to highly accurate. Data is classified as being very rough (if WI > 175), rough (125 < WI < 175), approximate (110 < WI < 125), accurate (105 < WI < 110) and highly accurate (WI < 105).
}
\value{
heaping computes the Whipple's index with the quality of age reporting.
}
\references{
Yadav, A., Vishwakarma, M., and Chauhan, S. (2020). The quality of age data: Comparison between two recent Indian censuses 2001–2011. Clinical Epidemiology and Global Health, 8(2), 371-376.
}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{afgan_2015}, \link{india_2015}}
}


\examples{
x <- armen_2015
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 23, 62)
}
