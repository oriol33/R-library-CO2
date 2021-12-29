\name{decadechangeplots}
\alias{decadechangeplots}
\title{
Graphical repreentation of the differences of means between periods.
}
\description{
The decade mean variations are represented for each gas and the confidence interval of these are provided as well. A different graph is represented for each country.
}
\usage{
decadechangeplots(countries="World")
}
\arguments{
  \item{countries}{
The argument must be a country or a set of countries from the ones used in the Greenhouse Dataset. It must be used a character object or a character vector. The default argument is "world", which plots the total gas emissions by year in the earth planet.
}
}
\value{
It returns a graphic or a set of graphics.
}
\author{
Adri Perez & Oriol Gairin
}
\note{
Function n from is used in the code of this function.
}

\examples{
## decadechangeplots()
## decadechangeplots("United States")
## decadechangeplots(c("United States", "United Kingdom"))
