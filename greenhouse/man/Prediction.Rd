\name{Prediction}
\alias{Prediction}
\title{Projection of the emission of gases for next years}
\description{
The function returns a prediction of the anual emission of gasses CO2, Methane and Nitrous Oxide for a given country and year.
}
\usage{
Prediction(Country="World", predictyear=2030)
}
\arguments{
  \item{Country}{The argument must be a country from the ones appearing in the df Dataset. It must be used a character object. The default argument is the country "World".
  \item{Decade}{The argument must be the year you want the prediction to be made. It must be a numerical value.}
}
\value{
\item{Prediction for anual emissions of CO2, Methane and Nitrous Oxide in "Country" for a given year (numerical value).}
\item{Summary of the regression models performed to do the predicition.}
}
\author{
Adri Perez & Oriol Gairin
}
\note{
df Dataset has to be loaded first and with a given appropiate format of the variable decade.
}

\examples{
## Prediction()
## Prediction("China", 2035)
## Prediction("Russia", 2042)
}