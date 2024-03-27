\name{Turkey}
\alias{turkey_2013}
\alias{turkey_2008}
\alias{turkey_2003}
\alias{turkey_1998}
\alias{turkey_1993}
\title{The data represent the number of women of reproductive age in Turkey
}

\usage{
turkey_2013
turkey_2008
turkey_2003
turkey_1998
turkey_1993
}

\description{
The function provides the number of women of reproductive age 15-49 in Turkey based on the demographic health survey of 2013, 2008, 2003, 1998 and 1993.
}
\arguments{
\item{turkey_2013}{
A vector of (non-negative) count values.
}
\item{turkey_2008}{
A vector of (non-negative) count values.
}
\item{turkey_2003}{
A vector of (non-negative) count values.
}
\item{turkey_1998}{
A vector of (non-negative) count values.
}
\item{turkey_1993}{
A vector of (non-negative) count values.
}
}
\details{In accordance with the demographic health surveys from 2013, 2008, 2003, 1998, and 1993, the function gives the number of Turkish women in the reproductive age range of 15 to 49.
}

\value{
turkey_1993, turkey_1998, turkey_2003, turkey_2008 and turkey_2013 the number of women of reproductive age 15-49 in Turkey
}
\references{
Hacettepe University Institute of Population Studies (2014), “2013 Turkey Demographic and Health Survey”. Hacettepe University Institute of Population Studies, T.R. Ministry of Development and TÜBİTAK, Ankara, Turkey.

Hacettepe University Institute of Population Studies (2009) Turkey Demographic and Health Survey, 2008. Hacettepe University Institute of Population Studies, Ministry of Health General Directorate of Mother and Child Health and Family Planning, T.R. Prime Ministry Undersecretary of State Planning Organization and TUBITAK, Ankara, Turkey.

Hacettepe University Institute of Population Studies, Turkey Demographic and Health Survey, 2003. Hacettepe University Institute of Population Studies, Ministry of Health General Directorate of Mother and Child Health and Family Planning, State Planning Organization and European Union. Ankara, Turkey.

Hacettepe University Institute of Population Studies, Turkey Demographic and Health Survey, 1998. Hacettepe University Institute of Population Studies, Ministry of Health General Directorate of Mother and Child Health and Family Planning, State Planning Organization and European Union. Ankara, Turkey.

Ministry of Health [Turkey], Hacettepe University Institute of Population Studies, and Macro International Inc. 1994. Turkish Demographic and Health Survey 1993. Ankara, Turkey.

}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{phil_2013}, \link{yemen_2013}, \link{heaping}}
}


\examples{
x <- turkey_2013
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
