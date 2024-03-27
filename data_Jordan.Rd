\name{Jordan}
\alias{jord_2017}
\alias{jord_2012}
\alias{jord_2009}
\alias{jord_2007}
\alias{jord_2002}
\alias{jord_1997}
\alias{jord_1990}
\title{The data represent the number of women of reproductive age in Jordan
}

\usage{
jord_2017
jord_2012
jord_2009
jord_2007
jord_2002
jord_1997
jord_1990
}

\description{
The function provides the number of women of reproductive age 15-49 in Jordan based on the demographic health survey of 1990, 1997, 2002, 2007, 2009, 2012 and 2017.
}
\arguments{
\item{jord_2017}{
A vector of (non-negative) count values.
}
\item{jord_2012}{
A vector of (non-negative) count values.
}
\item{jord_2009}{
A vector of (non-negative) count values.
}
\item{jord_2007}{
A vector of (non-negative) count values.
}
\item{jord_2002}{
A vector of (non-negative) count values.
}
\item{jord_1997}{
A vector of (non-negative) count values.
}
\item{jord_1990}{
A vector of (non-negative) count values.
}
}
\details{According to the 1990, 1997, 2002, 2007, 2009, 2012, and 2017 demographic health surveys, the function provides the Jordanian number of women between the ages of 15 and 49.
}

\value{
jord_1990, jord_1997, jord_2002, jord_2007, jord_2009, jord_2012 and jord_2017 give the number of women of reproductive age 15-49 in Jordan.
}

\references{
Department of Statistics (DOS) and ICF. 2019. Jordan Population and Family and Health Survey 2017-18. Amman, Jordan, and Rockville, Maryland, USA: DOS and ICF.

Department of Statistics [Jordan] and ICF International. 2013. Jordan Population and Family Health Survey 2012. Calverton, Maryland, USA: Department of Statistics and ICF International.

Department of Statistics [Jordan] and ICF Macro. 2010. Jordan Population and Family Health Survey 2009. Calverton, Maryland, USA: Department of Statistics and ICF Macro.

Department of Statistics [Jordan] and Macro International Inc. 2008. Jordan Population and Family Health Survey 2007. Calverton, Maryland, USA: Department of Statistics and Macro International Inc.

Department of Statistics [Jordan] and ORC Macro. 2003. Jordan Population and Family Health Survey 2002. Calverton, Maryland, USA: Department of Statistics and ORC Macro.

Department of Statistics (DOS) [Jordan] and Macro International Inc. (MI). 1998. Jordan Population and Family Health Survey 1997. Calverton, Maryland: DOS and MI.

Abdel Aziz Zoubi, Abdallah, Sri Poedjastoeti, and Mohamed Ayad. 1992. Jordan Population and Family Health Survey 1990. Columbia, Maryland, USA: Department of Statistics/Jordan, Ministry of Health/Jordan, and IRD/Macro International.


}

\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{pak_2017}, \link{phil_2017}, \link{heaping}}
}


\examples{
x <- jord_2017
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
