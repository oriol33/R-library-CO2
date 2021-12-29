\name{Decades_Increase}
\alias{Decades_Increase}

\title{ Percentage increase of emissions between consecutive decades}
\description{
The function returns the percentage increase of CO2, Methane and Nitrous Oxide emissions between two consecutive decades, displaying both a numerical value and a chart showing the increase. Additionaly, it returns the anual total average emissions between the periods considered.
}
\usage{
Decades_Increase(Country="United States", DecadeStart=1990)
}
\arguments{
  \item{Country}{The argument must be a country from the ones appearing in the Greenhouse Dataset. It must be used a character object. The default argument is the country "United States".
  \item{DecadeStart}{The argument must be the decade of start of your desired comparison. It must be a numerical value. If the Greenhouse Dataset does not have observations about the decade introduced, it will return NA.}
}

\value{
\item{p_increase}{The percentage increase of the anual average emissions in "Country" between the "DecadeStart" and "ConsecutiveDecade"" decade.}
\item{total_emissions}{The anual total average emissions in "Country" between the years considered.}
\item{chart}{Graphical comparison chart.}
}

\author{
Adri Perez & Oriol Gairin
}
\note{
Greenhouse Dataset has to be loaded first and with a given appropiate format of the variable decade.
}

\examples{
## Decades_Increase("World", 1990)
## Decades_Increase("World", 2000)
## Decades_Increase("China", 1980)
## Decades_Increase("China", 1990)
## Decades_Increase("China", 2000)
## Decades_Increase("Spain", 1980)
## Decades_Increase("Japan", 1930)
}
