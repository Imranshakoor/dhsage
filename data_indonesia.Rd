\name{Indonesia}
\alias{indo_2017}
\alias{indo_2012}
\alias{indo_2007}
\alias{indo_2002}
\alias{indo_1997}
\alias{indo_1994}
\alias{indo_1991}
\alias{indo_1987}
\title{The data represent the number of women of reproductive age in Indonesia
}

\usage{
indo_2017
indo_2012
indo_2007
indo_2002
indo_1997
indo_1994
indo_1991
indo_1987
}

\description{
The function provides the number of women of reproductive age 15-49 in Indonesia based on the demographic health survey of 1987, 1991, 1994, 1997, 2002, 2007, 2012 and 2017.
}
\arguments{
\item{indo_2017}{
A vector of (non-negative) count values.
}
\item{indo_2012}{
A vector of (non-negative) count values.
}
\item{indo_2007}{
A vector of (non-negative) count values.
}
\item{indo_2002}{
A vector of (non-negative) count values.
}
\item{indo_1997}{
A vector of (non-negative) count values.
}
\item{indo_1994}{
A vector of (non-negative) count values.
}
\item{indo_1991}{
A vector of (non-negative) count values.
}
\item{indo_1987}{
A vector of (non-negative) count values.
}
}
\details{Based on demographic health surveys conducted in Indonesia in 1987, 1991, 1994, 1997, 2002, 2007, 2012, and 2017, the function gives data on the number of women in reproductive age (15–49).
}

\value{
indo_1987, indo_1991, indo_1994, indo_1997, indo_2002, indo_2007, indo_2012 and indo_2017 give the number of women of reproductive age 15-49 in Indonesia.
}

\references{
National Population and Family Planning Board (BKKBN), Statistics Indonesia (BPS), Ministry of Health (Kemenkes), and ICF. 2018. Indonesia Demographic and Health Survey 2017. Jakarta, Indonesia: BKKBN, BPS, Kemenkes, and ICF.

Statistics Indonesia (Badan Pusat Statistik—BPS), National Population and Family Planning Board (BKKBN), and Kementerian Kesehatan (Kemenkes—MOH), and ICF International. 2013. Indonesia Demographic and Health Survey 2012. Jakarta, Indonesia: BPS, BKKBN, Kemenkes, and ICF International.

Statistics Indonesia (Badan Pusat Statistik—BPS) and Macro International. 2008. Indonesia Demographic and Health Survey 2007. Calverton, Maryland, USA: BPS and Macro International.

Badan Pusat Statistik-Statistics Indonesia (BPS) and ORC Macro. 2003. Indonesia Demographic and Health Survey 2002-2003. Calverton, Maryland, USA: BPS and ORC Macro

Central Bureau of Statistics (CBS) [Indonesia] and State Ministry o f Population/National Family Planning Coordinating Board (NFPCB) and Ministry of Health (MOH) and Macro Intemational Inc. (MI). 1998. Indonesia Demographic and Health Survey 1997. Calverton, Maryland: CBS and MI.

Central Bureau of Statistics (CBS) [Indonesia] and State Ministry of Population/National Family Planning Coordinating Board (NFPCB) and Ministry of Health (MOH) and Macro International Inc. (MI). 1995. Indonesia Demographic and Health Survey 1994. Calverton, Maryland: CBS and MI

Central Bureau of Statistics- CBS/Indonesia and Ministry of Health- MOH/Indonesia and Macro International. 1992. Indonesia Demographic and Health Survey 1991. Calverton, Maryland, USA: CBS and Macro International.

Central Bureau of Statistics- CBS/Indonesia and State Ministry of Population/National Family Planning Coordinating Board-NFPCB/Indonesia and Institute for Resources Development/Westinghouse. 1989. National Indonesia Contraceptive Prevalence Survey 1987. Calverton, Maryland, USA: CBS and Institute for Resource Development/Westinghouse.


}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{jord_2017}, \link{pak_2017}, \link{phil_2017}, \link{heaping}}
}


\examples{
x <- indo_2017
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
