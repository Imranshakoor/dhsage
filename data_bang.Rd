\name{Bangladesh}
\alias{bang_2014}
\alias{bang_2011}
\alias{bang_2007}
\alias{bang_2004}
\alias{bang_2001}
\alias{bang_1999}
\alias{bang_1996}
\alias{bang_1993}
\title{The data represent the number of women of reproductive age in Bangladesh
}

\usage{
bang_2014
bang_2011
bang_2007
bang_2004
bang_2001
bang_1999
bang_1996
bang_1993
}

\description{
The function provides the number of women of reproductive age 15-49 in Bangladesh based on the demographic health survey of 1993, 1996, 1999, 2001, 2004, 2007, 2011 and 2014.
}
\arguments{
\item{bang_2014}{
A vector of (non-negative) count values.
}
\item{bang_2011}{
A vector of (non-negative) count values.
}
\item{bang_2007}{
A vector of (non-negative) count values.
}
\item{bang_2004}{
A vector of (non-negative) count values.
}
\item{bang_2001}{
A vector of (non-negative) count values.
}
\item{bang_1999}{
A vector of (non-negative) count values.
}
\item{bang_1996}{
A vector of (non-negative) count values.
}
\item{bang_1993}{
A vector of (non-negative) count values.
}
}
\details{Based on the demographic health surveys from 1993, 1996, 1999, 2001, 2004, 2007, 2011, and 2014, the function gives the number of Bangladeshi women of reproductive age (15–49).
}

\value{
bang_2014, bang_2011, bang_2007, bang_2004, bang_2001, bang_1999, bang_1996 and bang_1993    give the frequency distribution of Bangladesh women of reproductive age 15 to 49.
}

\references{
National Institute of Population Research and Training (NIPORT), Mitra and Associates, and ICF International. 2016. Bangladesh Demographic and Health Survey 2014. Dhaka, Bangladesh, and Rockville, Maryland, USA: NIPORT, Mitra and Associates, and ICF International.

National Institute of Population Research and Training (NIPORT), Mitra and Associates, and ICF International. 2013. Bangladesh Demographic and Health Survey 2011. Dhaka, Bangladesh and Calverton, Maryland, USA: NIPORT, Mitra and Associates, and ICF International.

National Institute of Population Research and Training (NIPORT), Mitra and Associates, and Macro International. 2009. Bangladesh Demographic and Health Survey 2007. Dhaka, Bangladesh and Calverton, Maryland, USA: National Institute of Population Research and Training, Mitra and Associates, and Macro International.

National Institute of Population Research and Training (NIPORT), Mitra and Associates, and ORC Macro. 2005. Bangladesh Demographic and Health Survey 2004. Dhaka, Bangladesh and Calverton, Maryland [USA]: National Institute of Population Research and Training, Mitra and Associates, and ORC Macro.

National Institute of Population Research and Training (NIPORT), Mitra and Associates (MA), and ORC Macro (ORCM). 2001. Bangladesh Demographic and Health Survey 2001. Dhaka, Bangladesh and Calverton, Maryland [USA]: National Institute of Population Research and Training, Mitra and Associates, and ORC Macro.

National Institute of Population Research and Training (NIPORT), Mitra and Associates (MA), and ORC Macro (ORCM). 2001. Bangladesh Demographic and Health Survey 1999-2000. Dhaka, Bangladesh and Calverton, Maryland [USA]: National Institute of Population Research and Training, Mitra and Associates, and ORC Macro.

Mitra, S.N., Ahmed A1-Sabir, Anne R. Cross, and Kanta Jamil. 1997. Bangladesh Demographic and Health Survey, 1996-1997. Dhaka and Calverton, Maryland: National Institute of Population Research and Training (NIPORT), Mitra and Associates, and Macro International Inc.

Mitra, S.N., M. Nawab Ali, Shahidul Islam, Anne R. Cross, and Tulshi Saha. 1994. Bangladesh Demographic and Health Survey, 1993-1994. Calverton, Maryland: National Institute of Population Research and Training (NIPORT), Mitra and Associates, and Macro International Inc.
}


\author{
Muhammad Imran

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{camb_2014}, \link{heaping}}
}


\examples{
x <- bang_2014
# starting age 23 and ending age 62
heaping(x, 23, 62)
# starting age 15 and ending age 49
heaping(x, 15, 49)
}
