\name{decadepredict}
\alias{decadepredict}
\title{Projection of the emission of gases for next decades}
\description{
The function returns a prediction of the anual average emission of gasses CO2, Methane and Nitrous Oxide for a given country and decade.
}
\usage{
decadepredict(Country="World",Decade=2030)
}
\arguments{
  \item{Country}{The argument must be a country from the ones appearing in the df Dataset. It must be used a character object. The default argument is the country "World".
  \item{Decade}{The argument must be the decade you want the prediction to be made. It must be a numerical value.}
}
\value{
Prediction for average anual emissions of CO2, Methane and Nitrous Oxide in "Country" for a given decade (numerical value).
}
\author{
Adri Perez & Oriol Gairin
}
\note{
df Dataset has to be loaded first and with a given appropiate format of the variable decade.
}

\examples{
## decadepredict()
## decadepredict("Spain",2050)
## decadepredict("India", 2060)
}
