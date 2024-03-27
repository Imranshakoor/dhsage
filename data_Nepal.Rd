\name{Nepal}
\alias{nep_2016}
\alias{nep_2011}
\alias{nep_2006}
\alias{nep_2001}
\alias{nep_1996}
\title{The data represent the number of women of reproductive age in Nepal
}

\usage{
nep_2016
nep_2011
nep_2006
nep_2001
nep_1996
}

\description{
The function provides the number of women of reproductive age 15-49 in Nepal based on the demographic health survey of 2016, 2011, 2006, 2001 and 1996.
}
\arguments{
\item{nep_2016}{
A vector of (non-negative) count values.
}
\item{nep_2011}{
A vector of (non-negative) count values.
}
\item{nep_2006}{
A vector of (non-negative) count values.
}
\item{nep_2001}{
A vector of (non-negative) count values.
}
\item{nep_1996}{
A vector of (non-negative) count values.
}
}
\details{Based on the demographic health surveys conducted in 2016, 2011, 2006, 2001, and 1996, the function gives the number of Nepalese women between the ages of 15 and 49.
}

\value{
nep_1996, nep_2001, nep_2006, nep_2011 and nep_2016  give the number of women of reproductive age 15-49 in Nepal.
}

\references{
Ministry of Health, Nepal; New ERA, Nepal; Nepal Health Sector Support Program (NHSSP); and ICF. 2017. Nepal Health Facility Survey 2015. Kathmandu, Nepal: Ministry of Health, Nepal.

Ministry of Health and Population (MOHP) [Nepal], New ERA, and ICF International Inc. 2012. Nepal Demographic and Health Survey 2011. Kathmandu, Nepal: Ministry of Health and Population, New ERA, and ICF International, Calverton, Maryland.

Ministry of Health and Population (MOHP) [Nepal], New ERA, and Macro International Inc. 2007. Nepal Demographic and Health Survey 2006. Kathmandu, Nepal: Ministry of Health and Population, New ERA, and Macro International Inc.

Ministry of Health [Nepal], New ERA, and ORC Macro. 2002. Nepal Demographic and Health Survey 2001. Calverton, Maryland, USA: Family Health Division, Ministry of Health; New ERA; and ORC Macro.

Pradhan, Ajit, Ram Hari Aryal, Gokarna Regmi, Bharat Ban, and Pavalavalli Govindasamy. 1997. Nepal Family Health Survey 1996. Kathmandu, Nepal and Calverton, Maryland: Ministry of Health [Nepal], New ERA, and Macro International Inc.


}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{myan_2016}, \link{timor_2016}, \link{mal_2016}, \link{heaping}}
}


\examples{
x <- nep_2016
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
