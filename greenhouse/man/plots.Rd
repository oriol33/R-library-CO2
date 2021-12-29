\name{plots}
\alias{plots}
\title{
Graphical representation of gas emission through time.
}
\description{
This function plots the time series of the three gases provided by the Greenhouse_Gas_Emissions data set representing each country in a different graphic.
}
\usage{
plots(countries="World")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{countries}{
    The argument must be a country or a set of countries from the ones used in the Greenhouse Dataset. It must be used a character object or a character vector. If "all" is used, it is represented a plot for every country, region or organization. The default argument is "world", which plots the total gas emissions by year in the earth planet.
}
}
\value{
  It returns a graphic or a set of graphics.
}
\author{
Adri Perez & Oriol Gairin
}

\examples{
## plots()
## plots("United States")
## plots(c("United States", "Spain", "Germany"))

