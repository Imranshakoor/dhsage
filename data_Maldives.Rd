\name{Maldives}
\alias{mal_2016}
\alias{mal_2009}
\title{The data represent the number of women of reproductive age in Maldives
}

\usage{
mal_2016
mal_2009
}

\description{
The function provides the number of women of reproductive age 15-49 in Maldives based on the demographic health survey of 2009 and 2016.
}
\arguments{
\item{mal_2016}{
A vector of (non-negative) count values.
}
\item{mal_2009}{
A vector of (non-negative) count values.
}
}
\details{Based on the demographic health surveys conducted in 2009 and 2016, the function gives the number of Maldivian women in the reproductive age range of 15 to 49.
}

\value{
mal_2009 and mal_2016  give the number of women of reproductive age 15-49 in Maldives.
}

\references{
Ministry of Health (MOH) [Maldives] and ICF. 2018. Maldives Demographic and Health Survey 2016-17. Male, Maldives, and Rockville, Maryland, USA: MOH and ICF.

Ministry of Health and Family (MOHF) [Maldives] and ICF Macro. 2010. Maldives Demographic and Health Survey 2009. Calverton, Maryland: MOHF and ICF Macro.
}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{timor_2009}, \link{jord_2009}, \link{heaping}}
}


\examples{
x <- mal_2009
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
