setwd("C:/Users/USUARI/Desktop/MESIO/1r Quadri/Software estadístic R i SAS/R/Project")

## Exercise 1
#a)
library(readxl)
read_excel(url("https://github.com/owid/co2-data/raw/master/owid-co2-data.xlsx"))


Greenhouse_Gas_Emissions <- read_excel("owid-co2-data.xlsx")
Greenhouse_Gas_Emissions <- data.frame(Greenhouse_Gas_Emissions)

#b) 
library(Hmisc)
var.labels <- c(iso_code = "ISO 3166-1 alpha-3 - three-letter country codes",
                country =	"Geographic location",
                year = "Year of observation",
                co2 = "Annual production-based emissions of carbon dioxide (CO2), measured in million tonnes",
                co2_per_capita = "Annual production-based emissions of carbon dioxide (CO2), measured in tonnes per person",
                trade_co2 = "Annual net carbon dioxide (CO2) emissions embedded in trade, measured in million tonnes",
                cement_co2 = "Annual production-based emissions of carbon dioxide (CO2) from cement, measured in million tonnes",
                cement_co2_per_capita	= "Annual production-based emissions of carbon dioxide (CO2) from cement, measured in tonnes per person",
                coal_co2 = "Annual production-based emissions of carbon dioxide (CO2) from coal, measured in million tonnes",
                coal_co2_per_capita =	"Annual production-based emissions of carbon dioxide (CO2) from coal, measured in tonnes per person",
                flaring_co2 = "Annual production-based emissions of carbon dioxide (CO2) from flaring, measured in million tonnes",
                flaring_co2_per_capita = "Annual production-based emissions of carbon dioxide (CO2) from flaring, measured in tonnes per person",
                gas_co2 = "Annual production-based emissions of carbon dioxide (CO2) from gas, measured in million tonnes",
                gas_co2_per_capita = "Annual production-based emissions of carbon dioxide (CO2) from gas, measured in tonnes per person",
                oil_co2 = "Annual production-based emissions of carbon dioxide (CO2) from oil, measured in million tonnes",
                oil_co2_per_capita = "Annual production-based emissions of carbon dioxide (CO2) from oil, measured in tonnes per person",
                other_industry_co2 = "Annual production-based emissions of carbon dioxide (CO2) from other industry sources, measured in million tonnes",
                other_co2_per_capita = "Annual production-based emissions of carbon dioxide (CO2) from other industry sources, measured in tonnes per person",
                co2_growth_prct = "Annual percentage growth in production-based emissions of carbon dioxide (CO2)",
                co2_growth_abs = "Annual growth in production-based emissions of carbon dioxide (CO2), measured in million tonnes",
                co2_per_gdp = "Annual production-based emissions of carbon dioxide (CO2), measured in kilograms per dollar of GDP (2011 international-$)",
                co2_per_unit_energy = "Annual production-based emissions of carbon dioxide (CO2), measured in kilograms per kilowatt-hour of primary energy consumption",
                consumption_co2 = "Annual consumption-based emissions of carbon dioxide (CO2), measured in million tonnes",
                consumption_co2_per_capita = "Annual consumption-based emissions of carbon dioxide (CO2), measured in tonnes per person",
                consumption_co2_per_gdp = "Annual consumption-based emissions of carbon dioxide (CO2), measured in kilograms per dollar of GDP (2011 international-$)",
                cumulative_co2 = "Cumulative production-based emissions of carbon dioxide (CO2) since the first year of data availability, measured in million tonnes)",
                cumulative_cement_co2 = "Cumulative production-based emissions of carbon dioxide (CO2) from cement since the first year of data availability, measured in million tonnes",
                cumulative_coal_co2 = "Cumulative production-based emissions of carbon dioxide (CO2) from coal since the first year of data availability, measured in million tonnes",
                cumulative_flaring_co2 = "Cumulative production-based emissions of carbon dioxide (CO2) from flaring since the first year of data availability, measured in million tonnes",
                cumulative_gas_co2 = "Cumulative production-based emissions of carbon dioxide (CO2) from gas since the first year of data availability, measured in million tonnes",
                cumulative_oil_co2= "Cumulative production-based emissions of carbon dioxide (CO2) from oil since the first year of data availability, measured in million tonnes",
                cumulative_other_co2 = "Cumulative production-based emissions of carbon dioxide (CO2) from other industry sources since the first year of data availability, measured in million tonnes",
                trade_co2_share = "Annual net carbon dioxide (CO2) emissions embedded in trade, measured as a percentage of production-based emissions of CO2",
                share_global_co2 = "Annual production-based emissions of carbon dioxide (CO2), measured as a percentage of global production-based emissions of CO2 in the same year",
                share_global_cement_co2 = "Annual production-based emissions of carbon dioxide (CO2) from cement, measured as a percentage of global production-based emissions of CO2 from cement in the same year",
                share_global_coal_co2 = "Annual production-based emissions of carbon dioxide (CO2) from coal, measured as a percentage of global production-based emissions of CO2 from coal in the same year",
                share_global_flaring_co2 = "Annual production-based emissions of carbon dioxide (CO2) from flaring, measured as a percentage of global production-based emissions of CO2 from flaring in the same year",
                share_global_gas_co2 = "Annual production-based emissions of carbon dioxide (CO2) from gas, measured as a percentage of global production-based emissions of CO2 from gas in the same year",
                share_global_oil_co2 = "Annual production-based emissions of carbon dioxide (CO2) from oil, measured as a percentage of global production-based emissions of CO2 from oil in the same year",
                share_global_other_co2 = "Annual production-based emissions of carbon dioxide (CO2) from other industry sources, measured as a percentage of global production-based emissions of CO2 from other industry sources in the same year",
                share_global_cumulative_co2 = "Cumulative production-based emissions of carbon dioxide (CO2) since the first year of data availability, measured as a percentage of global cumulative production-based emissions of CO2 since the first year of data availability",
                share_global_cumulative_cement_co2 = "Cumulative production-based emissions of carbon dioxide (CO2) from cement since the first year of data availability, measured as a percentage of global cumulative production-based emissions of CO2 from cement since the first year of data availability",
                share_global_cumulative_coal_co2 = "Cumulative production-based emissions of carbon dioxide (CO2) from coal since the first year of data availability, measured as a percentage of global cumulative production-based emissions of CO2 from coal since the first year of data availability",
                share_global_cumulative_flaring_co2 = "Cumulative production-based emissions of carbon dioxide (CO2) from flaring since the first year of data availability, measured as a percentage of global cumulative production-based emissions of CO2 from flaring since the first year of data availability",
                share_global_cumulative_gas_co2 =	"Cumulative production-based emissions of carbon dioxide (CO2) from gas since the first year of data availability, measured as a percentage of global cumulative production-based emissions of CO2 from gas since the first year of data availability",
                share_global_cumulative_oil_co2 = "Cumulative production-based emissions of carbon dioxide (CO2) from oil since the first year of data availability, measured as a percentage of global cumulative production-based emissions of CO2 from oil since the first year of data availability",
                share_global_cumulative_other_co2 = "Cumulative production-based emissions of carbon dioxide (CO2) from other industry sources since the first year of data availability, measured as a percentage of global cumulative production-based emissions of CO2 from other industry sources since the first year of data availability",
                total_ghg = "Total greenhouse gas emissions including land use change and forestry, measured in million tonnes of carbon dioxide-equivalents",
                ghg_per_capita = "Total greenhouse gas emissions including land use change and forestry, measured in tonnes of carbon dioxide-equivalents per capita",
                methane = "Total methane emissions including land use change and forestry, measured in million tonnes of carbon dioxide-equivalents",
                methane_per_capita = "Total methane emissions including land use change and forestry, measured in tonnes of carbon dioxide-equivalents per capita",
                nitrous_oxide = "Total nitrous oxide emissions including land use change and forestry, measured in million tonnes of carbon dioxide-equivalents",
                nitrous_oxide_per_capita = "Total nitrous oxide emissions including land use change and forestry, measured in tonnes of carbon dioxide-equivalents per capita",
                population = "Population by country, available from 1800 to 2021 based on Gapminder data, HYDE, and UN Population Division (2019) estimates",
                gdp = "Gross domestic product measured in international-$ using 2011 prices to adjust for price changes over time (inflation) and price differences between countries",
                primary_energy_consumption = "Primary energy consumption, measured in terawatt-hours per year",
                energy_per_capita = "Primary energy consumption per capita, measured in kilowatt-hours per year",
                energy_per_gdp = "Primary energy consumption per unit of gross domestic product, measured in kilowatt-hours per international-$")

label(Greenhouse_Gas_Emissions) = as.list(var.labels[match(names(Greenhouse_Gas_Emissions), names(var.labels))])

# Checking that the previous command worked correctly:
label(Greenhouse_Gas_Emissions)

#c)
dim(Greenhouse_Gas_Emissions)

# Univariate Description
numericvar <- vector()
for(i in colnames(Greenhouse_Gas_Emissions)){
  if(is.numeric(Greenhouse_Gas_Emissions[,i])){
    numericvar <- append(numericvar, i)
  }
}

rm(i)

charactervar <- vector()
for(i in colnames(Greenhouse_Gas_Emissions)){
  if(is.character(Greenhouse_Gas_Emissions[,i])|is.factor(Greenhouse_Gas_Emissions[,i])|is.logical(Greenhouse_Gas_Emissions[,i])){
    charactervar <- append(charactervar, i)
  }
}

rm(i) 

library(vtable)
sumtable(Greenhouse_Gas_Emissions, vars = numericvar,
         summ = c('mean(x)', 'median(x)', 'sd(x)', 'min(x)','pctile(x)[25]', 'pctile(x)[75]','max(x)'), 
         summ.names = c('Mean','Median','Std. Dev.','Min','Pctl. 25','Pctl. 75','Max'),
         out="return", factor.numeric = T)

sumtable(Greenhouse_Gas_Emissions, vars = charactervar[3:length(charactervar)], out="return")

Greenhouse_Gas_Emissions <-  data.frame(Greenhouse_Gas_Emissions)
for(i in colnames(Greenhouse_Gas_Emissions)){
  if(is.numeric(Greenhouse_Gas_Emissions[,i])){
    hist(Greenhouse_Gas_Emissions[,i], xlab = i, main = paste("Histogram for ", i), col = "blue")
  }
  else{
    barplot(table(Greenhouse_Gas_Emissions[,i]), main = paste("Barplot for ",i))
  }
}


rm(i)  


# Bivariate Description of Numerical Variables
cormat <- round(cor(Greenhouse_Gas_Emissions[, numericvar], use = "complete"),2)
cormat

library(reshape2)
library(ggplot2)
melted_cormat <- melt(cormat)

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)

### Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=1,
                                   size = 8))+
  coord_fixed()
#d)
# The following command excludes the country that gives problems:
plots <- function(countries="World"){
  if(length(countries)==1){
    if(countries=="all"){
      for(i in unique(Greenhouse_Gas_Emissions$country)[c(1:142, 144:200)]){
        with(subset(Greenhouse_Gas_Emissions, Greenhouse_Gas_Emissions$country==i), plot(year, co2, type = 'l', main = paste("Gas emissions by",i), col="green", xlab="Year", ylab="Gas Emission (in millions of tonnes)"))
        with(subset(Greenhouse_Gas_Emissions, Greenhouse_Gas_Emissions$country==i), lines(year, methane, col="blue"))
        with(subset(Greenhouse_Gas_Emissions, Greenhouse_Gas_Emissions$country==i), lines(year, nitrous_oxide, col="orange"))
        legend("topleft", legend=c("CO2", "Methane", "Nitrous Oxide"),
               col=c("green", "blue", "orange"), cex=0.8, lty=1,
               title="Gases", text.font=4, bg='lightgrey')
      }
    } else if(countries=="World"){
      with(subset(Greenhouse_Gas_Emissions, Greenhouse_Gas_Emissions$country=="World"), plot(year, co2, type = 'l', main = "Total Gas Emissions", col="green", xlab="Year", ylab="Gas Emission (in millions of tonnes)"))
      with(subset(Greenhouse_Gas_Emissions, Greenhouse_Gas_Emissions$country=="World"), lines(year, methane, col="blue"))
      with(subset(Greenhouse_Gas_Emissions, Greenhouse_Gas_Emissions$country=="World"), lines(year, nitrous_oxide, col="orange"))
      legend("topleft", legend=c("CO2", "Methane", "Nitrous Oxide"),
             col=c("green", "blue", "orange"), cex=0.8, lty=1,
             title="Gases", text.font=4, bg='lightgrey')
    }
    else{
      with(subset(Greenhouse_Gas_Emissions, Greenhouse_Gas_Emissions$country==countries), plot(year, co2, type = 'l', main = paste("Total Gas Emissions by", countries), col="green", xlab="Year", ylab="Gas Emission (in millions of tonnes)"))
      with(subset(Greenhouse_Gas_Emissions, Greenhouse_Gas_Emissions$country==countries), lines(year, methane, col="blue"))
      with(subset(Greenhouse_Gas_Emissions, Greenhouse_Gas_Emissions$country==countries), lines(year, nitrous_oxide, col="orange"))
      legend("topleft", legend=c("CO2", "Methane", "Nitrous Oxide"),
             col=c("green", "blue", "orange"), cex=0.8, lty=1,
             title="Gases", text.font=4, bg='lightgrey')
    }
  } else{
    for(i in countries){
      with(subset(Greenhouse_Gas_Emissions, Greenhouse_Gas_Emissions$country==i), plot(year, co2, type = 'l', main = paste("Gas emissions by",i), col="green"), xlab="Year", ylab="Gas Emission (in millions of tonnes)")
      with(subset(Greenhouse_Gas_Emissions, Greenhouse_Gas_Emissions$country==i), lines(year, methane, col="blue"))
      with(subset(Greenhouse_Gas_Emissions, Greenhouse_Gas_Emissions$country==i), lines(year, nitrous_oxide, col="orange"))
      legend("topleft", legend=c("CO2", "Methane", "Nitrous Oxide"),
             col=c("green", "blue", "orange"), cex=0.8, lty=1,
             title="Gases", text.font=4, bg='lightgrey')
    }
  }
}

# Running the function:
plots()
plots("Spain")
plots(c("United States", "China"))
#plots("all")
      
# Country that gives problems is:
unique(Greenhouse_Gas_Emissions$country)[143] #Micronesia is the one that gives problems, not Micronesia country

#e)
min(Greenhouse_Gas_Emissions$year)
max(Greenhouse_Gas_Emissions$year)
Greenhouse_Gas_Emissions$decade <- cut2(Greenhouse_Gas_Emissions$year, c(seq(from=1750, to=2020, by=10)))
df<-data.frame(subset(Greenhouse_Gas_Emissions, decade=="[1990,2000)"|decade=="[2000,2010)"|decade=="[2010,2020]"))  

#f)
# The following should work, but does not:
library(doBy)
for(i in unique(numericvar)){
    #i <- as.name(i)
    summaryBy(i ~ decade, data = df, FUN = c(mean, sd, median, IQR), na.rm=T)
}
rm(i)

# The following works and provides the same result:
summarystatistics <- summaryBy(df[,3:58] ~ decade, data = df, FUN = c(mean, sd, median, IQR), na.rm=T)

#g)
# Function:
decadechangeplots <- function(countries="World"){
  for(c in countries){
    avgco2 <- with(subset(Greenhouse_Gas_Emissions, country == c),tapply(co2, decade, mean, na.rm=T))
    sdco2 <- with(subset(Greenhouse_Gas_Emissions, country == c),tapply(co2, decade, sd, na.rm=T))
    nco2 <- with(subset(Greenhouse_Gas_Emissions, country == c & co2>0),tapply(co2, decade, n))
    
    avgmethane <- with(subset(Greenhouse_Gas_Emissions, country == c),tapply(methane, decade, mean, na.rm=T))
    sdmet <- with(subset(Greenhouse_Gas_Emissions, country == c),tapply(methane, decade, sd, na.rm=T))
    nmet <- with(subset(Greenhouse_Gas_Emissions, country == c & methane>0),tapply(methane, decade, n))
    
    avgnitrous <- with(subset(Greenhouse_Gas_Emissions, country == c),tapply(nitrous_oxide, decade, mean, na.rm=T))
    sdnit <- with(subset(Greenhouse_Gas_Emissions, country == c),tapply(nitrous_oxide, decade, sd, na.rm=T))
    nnit <- with(subset(Greenhouse_Gas_Emissions, country == c & nitrous_oxide>0),tapply(nitrous_oxide, decade, n))
    
    # Computing the differences in means
    avgco2dif <- diff(avgco2)
    avgmethanedif <- diff(avgmethane)
    avgnitrousdif <- diff(avgnitrous)
    
    # Computing the standard deviation for differences in means from a period to the previous period
    sddif <- vector(length = length(avgco2dif))
    for(i in 1:length(avgco2dif)){
      sddif[i] = sqrt(sdco2[i]^2/nco2[i]+sdco2[i+1]^2/nco2[i+1])
    }
    
    sddifmet <- vector(length = length(avgmethanedif))
    for(i in 1:length(avgmethanedif)){
      sddifmet[i] = sqrt(sdmet[i]^2/nmet[i]+sdmet[i+1]^2/nmet[i+1])
    }
    
    sddifnit <- vector(length = length(avgnitrousdif))
    for(i in 1:length(avgnitrousdif)){
      sddifnit[i] = sqrt(sdnit[i]^2/nnit[i]+sdnit[i+1]^2/nnit[i+1])
    }
    # Computing the degrees of freedom for the t-student distribution
    g <- vector(length = length(sddif))
    for(i in 1:length(avgco2dif)){
      g[i] = (((sdco2[i]^2/nco2[i])+(sdco2[i+1]^2/nco2[i+1]))^2)/(((sdco2[i]^2/nco2[i])^2)*(1/(nco2[i]-1))+((sdco2[i+1]^2/nco2[i+1])^2)*(1/((nco2[i+1]-1))))
    }
    
    gmet <- vector(length = length(sddifmet))
    for(i in 1:length(avgco2dif)){
      gmet[i] = (((sdmet[i]^2/nmet[i])+(sdmet[i+1]^2/nmet[i+1]))^2)/(((sdmet[i]^2/nmet[i])^2)*(1/(nmet[i]-1))+((sdmet[i+1]^2/nmet[i+1])^2)*(1/(nmet[i+1]-1)))
    }
    
    gnit <- vector(length = length(sddifnit))
    for(i in 1:length(avgnitrousdif)){
      gnit[i] = (((sdnit[i]^2/nnit[i])+(sdnit[i+1]^2/nnit[i+1]))^2)/(((nnit[i]^2/nnit[i])^2)*(1/(nnit[i]-1))+((sdnit[i+1]^2/nnit[i+1])^2)*(1/(nnit[i+1]-1)))
    }
    
    # Computing the confidence intervals
    avgco2difup <- vector(length = length(avgco2dif))
    for(i in 1:length(avgco2dif)){
      avgco2difup[i] <- avgco2dif[i] + qt(0.025, g[i], lower.tail = F)*sddif[i]
    }
    avgco2diflow <- vector(length = length(avgco2dif))
    for(i in 1:length(avgco2dif)){
      avgco2diflow[i] <- avgco2dif[i] - qt(0.025, g[i], lower.tail = F)*sddif[i]
    }
    
    avgmethanedifup <- vector(length = length(avgmethanedif))
    for(i in 1:length(avgmethanedif)){
      avgmethanedifup[i] <- avgmethanedif[i] +  qt(0.025, gmet[i], lower.tail = F)*sddifmet[i]
    }
    avgmethanediflow <- vector(length = length(avgmethanedif))
    for(i in 1:length(avgmethanedif)){
      avgmethanediflow[i] <- avgmethanedif[i] -  qt(0.025, gmet[i], lower.tail = F)*sddifmet[i]
    }
    
    avgnitrousdifup <- vector(length = length(avgnitrousdif))
    for(i in 1:length(avgnitrousdif)){
      avgnitrousdifup[i] <- avgnitrousdif[i] +  qt(0.025, gnit[i], lower.tail = F)*sddifnit[i]
    }
    avgnitrousdiflow <- vector(length = length(avgnitrousdif))
    for(i in 1:length(avgnitrousdif)){
      avgnitrousdiflow[i] <- avgnitrousdif[i] -  qt(0.025, gnit[i], lower.tail = F)*sddifnit[i]
    }
    
    
    # Plot:
    plot(1:26, avgco2dif, type='l', ylim=c(min(c(min(avgco2diflow[!is.na(avgco2diflow)]),min(avgmethanediflow[!is.na(avgmethanediflow)]), min(avgnitrousdiflow[!is.na(avgnitrousdiflow)]))),max(c(max(avgco2difup[!is.na(avgco2difup)])),max(avgmethanedifup[!is.na(avgmethanedif)]), max(avgnitrousdifup[!is.na(avgnitrousdifup)]))), col="green",
         xlab="", ylab="Variation in the decade means", main=paste("Mean decade variations in ",c))
    lines(1:26, avgco2diflow, type='l', col='red', lty=2)
    lines(1:26, avgco2difup, type='l', col='red', lty=2)
    lines(1:26, avgmethanedif, type='l', col='orange')
    lines(1:26, avgmethanedifup, type='l', col='red', lty=2)
    lines(1:26, avgmethanediflow, type='l', col='red', lty=2)
    lines(1:26, avgnitrousdif, type='l', col='blue')
    lines(1:26, avgnitrousdifup, type='l', col='red', lty=2)
    lines(1:26, avgnitrousdiflow, type='l', col='red', lty=2)
    axis(1, at=1:26, labels = sort(unique(Greenhouse_Gas_Emissions$decade))[2:27], las=3)
    abline(h=0, lty=2)
    legend("topleft", legend=c("CO2", "Methane", "Nitrous Oxide", "CI(95%)"),
           col=c("green", "blue", "orange","red"), cex=0.8, lty=c(1,1,1,2),
           title="Gases", text.font=4, bg='lightgrey')
  }
}


#Section 1: For all countries ("world")
decadechangeplots() #funciona

#Section 2: Plots for the top 10 co2 producers
library(doBy)
avgco2_bycandd <- data.frame(summaryBy(co2 ~ country + decade, Greenhouse_Gas_Emissions, FUN = mean, na.rm=T))
avgmethane_bycandd <- data.frame(summaryBy(methane ~ country + decade, Greenhouse_Gas_Emissions, FUN = mean, na.rm=T))
avgnitrous_bycandd <- data.frame(summaryBy(nitrous_oxide ~ country + decade, Greenhouse_Gas_Emissions, FUN = mean, na.rm=T))

# Which are the countries with the highest co2 production on average in the last decades?
library(data.table)
co2production <- setorder(data.frame(summaryBy(co2 ~ country, Greenhouse_Gas_Emissions, FUN = mean, na.rm=T)), -co2.mean, na.last = T)
co2production[1:30,]
# We do not consider the groups or regions, juts countries. So we have:
countries <- c("United States", "China", "Russia", "Japan", "Germany", "India", "United Kingdom", "Saudi Arabia", "France", "Ukraine")

decadechangeplots("United States") # it works well
decadechangeplots("China") # it works well
decadechangeplots("Russia") # it works well
decadechangeplots("Japan") # it does NOT work well
decadechangeplots("Germany") # it does NOT work well
decadechangeplots("India") # it works well
decadechangeplots("United Kingdom") # it does NOT work well
decadechangeplots("Saudi Arabia") # it does NOT work well
decadechangeplots("France") # it does NOT work well
decadechangeplots("Ukraine") # it does NOT work well

#h)
aggco2 <- aggregate(df[,numericvar], by=list(df$country), FUN = mean, na.rm=T)
library("writexl")
write_xlsx(aggco2,"aggco2.xlsx")

## Exercise 2

# GGPLOT LIBRARY PLOT: 2 numerical variables
ggplot(df, aes(x = gdp, y = co2, shape=decade, color=decade)) + geom_point() + geom_smooth(method=lm, fullrange=T)

# Alternative: a log-log model
ggplot(df, aes(x = log(gdp), y = log(co2), shape=decade, color=decade)) + geom_point() + geom_smooth(method=lm, fullrange=T)


# Pie Chart for Study group Country in the last Decade [2010,2020] (package plotrix)
install.packages("plotrix")
library(plotrix)

#Top 10 countries co2 emissions in the last decade

df2 <- subset(Greenhouse_Gas_Emissions, decade == "[2010,2020]")

library(data.table)
library(doBy)
co2production <- setorder(data.frame(summaryBy(co2 ~ country, df2, FUN = mean, na.rm=T)), -co2.mean, na.last = T)
co2production[1:30,]

#Top 5 + World
countries <-c("China","United States", "India" ,"Rusia", "Japan", "World")

df3<-data.frame(subset(df2, country=="United States" | country=="World"
                       | country=="China" | country=="Russia"| country=="Japan"
                       | country=="India" )) 

sumco2 <- with(df3,tapply(co2, country, sum, na.rm=T)) 
Others <- sumco2[6]-sum(sumco2[1:5])
sumco2[6] <- Others
sumco2

pie.labels <-  c("China", "India", "Japan", "Russia", "USA", "Others")
sumco2 <- as.vector(sumco2)
lp<-pie3D(sumco2,radius=0.9,labels=pie.labels,explode=0.1,main="Main CO2 emitters in the last decade",
          col=c("brown","aliceblue","pink","coral1", "grey", "cadetblue"))

                                    #### Exercise 3 ####

# a)
#Let's take the data frame with the variable "decade" that groups the observations by the decade they are.

Greenhouse_Gas_Emissions

#We'll do first for one specific country and decade and after we generalize with a function.

# i.e. USA between 1900 decade and 2000 decade.

subset1 <- data.frame(subset(Greenhouse_Gas_Emissions, decade=="[1990,2000)" & country=="United States"))
subset2 <- data.frame(subset(Greenhouse_Gas_Emissions, decade=="[2000,2010)" & country=="United States"))

mean(subset1[,"co2"])
mean(subset1[,"methane"])
mean(subset1[,"nitrous_oxide"])
mean(subset2[,"co2"])
mean(subset2[,"methane"])
mean(subset2[,"nitrous_oxide"])

inc_co2 <- 100*(mean(subset2[,"co2"])-mean(subset1[,"co2"]))/mean(subset1[,"co2"])
inc_meth <- 100*(mean(subset2[,"methane"])-mean(subset1[,"methane"]))/mean(subset1[,"methane"])
inc_nitox <- 100*(mean(subset2[,"nitrous_oxide"])-mean(subset1[,"nitrous_oxide"]))/mean(subset1[,"nitrous_oxide"])

subsettot <- rbind(subset1,subset2)

totavg_co2 <- mean(subsettot[,"co2"])
totavg_met <- mean(subsettot[,"methane"])
totavg_nitox <- mean(subsettot[,"nitrous_oxide"])

with(subsettot, plot(year, co2, type = 'l', main = paste("Gas emissions by USA between 1990 and 2010"), col="green", xlab="Year", ylab="Gas Emission", ylim=c(0,max(subsettot[,"co2"]+50)) ))
with(subsettot, lines(year, methane, col="blue"))
with(subsettot, lines(year, nitrous_oxide, col="orange"))
legend("topleft",legend=c("CO2", "Methane", "Nitrous Oxide"),
      col=c("green", "blue", "orange"), cex=0.8, lty=1,
     title="Gases", text.font=4, bg='lightgrey')

cat("Percentage increase in USA between the 1900 decade and 2000 decade is: ")
cat("CO2:",inc_co2, "%")
cat("Methane:",inc_meth, "%")
cat("Nitrous Oxide:",inc_nitox, "%")
cat("The average emission in USA between 1990 and 2010 is: ")
cat("CO2:",totavg_co2, "million tonnes")
cat("Methane:",totavg_met, "million tonnes")
cat("Nitrous Oxide:",totavg_nitox, "million tonnes")


# Stacked Bar Plot with Colors and Legend
table <- data.frame(c(mean(subset1[,"co2"]),mean(subset1[,"methane"]),mean(subset1[,"nitrous_oxide"])),
                    c(mean(subset2[,"co2"]),mean(subset2[,"methane"]),mean(subset2[,"nitrous_oxide"])),
                    row.names=c("CO2","Methane","Nitrous Oxide"))
names(table) <- c("[1990,2000)","[2000,2010)")

table <- as.matrix(table)

par(mar=c(5.1, 4.1, 4.1, 10.1), xpd=TRUE)
barplot(table, main="Mean gas emissions by USA in decades 1990 and 2000",
        xlab="Decades", col=c("darkblue","red", "orange")) 
legend("topright", inset = c(- 0.5, 0.5), xpd=T, legend = rownames(table),fill = c("darkblue", "red","orange"))


#We first recode the the last level of factor decade so that the function works for all decades:

library(dplyr)
Greenhouse_Gas_Emissions$decade <- recode_factor(Greenhouse_Gas_Emissions$decade, "[2010,2020]"  = "[2010,2020)")

#Now we define the function with parameters Country & Decade:

Decades_Increase <- function(Country="United States", DecadeStart=1990) {
  
  #Calculations
  
  Decadelim1 <- DecadeStart+10
  Decadelim2 <- DecadeStart+20
  Decade1 <- paste("[",DecadeStart,",",DecadeStart+10,")",sep = "")
  Decade2 <- paste("[",DecadeStart+10,",",DecadeStart+20,")",sep = "")
  
  subset1 <- data.frame(subset(Greenhouse_Gas_Emissions, decade==Decade1 & country==Country))
  subset2 <- data.frame(subset(Greenhouse_Gas_Emissions, decade==Decade2 & country==Country))
  
  inc_co2 <- 100*( mean(subset2[,"co2"],na.rm=T)-mean(subset1[,"co2"],na.rm=T) )/mean(subset1[,"co2"],na.rm=T)
  inc_meth <- 100*( mean(subset2[,"methane"],na.rm=T)-mean(subset1[,"methane"],na.rm=T) )/mean(subset1[,"methane"],na.rm=T)
  inc_nitox <- 100*( mean(subset2[,"nitrous_oxide"],na.rm=T)-mean(subset1[,"nitrous_oxide"],na.rm=T) )/mean(subset1[,"nitrous_oxide"],na.rm=T)
  
  subsettot <- rbind(subset1,subset2)
  
  totavg_co2 <- mean(subsettot[,"co2"],na.rm=T)
  totavg_met <- mean(subsettot[,"methane"],na.rm=T)
  totavg_nitox <- mean(subsettot[,"nitrous_oxide"],na.rm=T)
  
  # Printing the results of percentage increase between decades and anual total average of emission
  
  cat("The percentage increase of the anual average emissions in", Country, "between the", DecadeStart, "decade and", Decadelim1, "decade is:", "\n")
  cat("CO2:",inc_co2, "%   ; ")
  cat("Methane:",inc_meth, "%   ; ")
  cat("Nitrous Oxide:",inc_nitox, "%", "\n", "\n")
  cat("The anual total average emission in", Country, "between", DecadeStart, "and", Decadelim2, "is:", "\n")
  cat("CO2:",totavg_co2, "million tonnes   ; ")
  cat("Methane:",totavg_met, "million tonnes   ; ")
  cat("Nitrous Oxide:",totavg_nitox, "million tonnes.")
  
  # Stacked Bar Plot with Colors and Legend
  
  table <- data.frame(c(mean(subset1[,"co2"],na.rm=T),mean(subset1[,"methane"],na.rm=T),mean(subset1[,"nitrous_oxide"],na.rm=T)),
                      c(mean(subset2[,"co2"],na.rm=T),mean(subset2[,"methane"],na.rm=T),mean(subset2[,"nitrous_oxide"],na.rm=T)),
                      row.names=c("CO2","Methane","Nitrous Oxide"))
  names(table) <- c(Decade1,Decade2)
  
  table <- as.matrix(table)
  
  par(mar=c(5.1, 4.1, 4.1, 10.1), xpd=TRUE)
  barplot(table, main=paste("Anual mean gas emissions in", Country, "in the decades of", DecadeStart, "and", Decadelim1),
          xlab="Decades", ylab="Gas emissions (million tones)", col=c("darkblue","red", "orange")) 
  legend("topright", inset=c(-0.5, 0.5), xpd=T, legend = rownames(table),fill = c("darkblue", "red","orange"))
  
}

Decades_Increase()
Decades_Increase("Spain", 1980) #No data of Methane an Nitrous Oxide available
Decades_Increase("United Kingdom", 1990)
Decades_Increase("China", 1980)
Decades_Increase("China", 1990)
Decades_Increase("China", 2000)
Decades_Increase("World", 1990)
Decades_Increase("World", 2000)

#b)

# We do the projection based on the last 3 decades because they are the ones...
#... that contain information about Methane and Nitrous Oxide.

# Hence, we take the dataframe "df".

Greenhouse_Gas_Emissions$decade <- cut2(Greenhouse_Gas_Emissions$year, c(seq(from=1750, to=2020, by=10)))
df<-data.frame(subset(Greenhouse_Gas_Emissions, decade=="[1990,2000)"|decade=="[2000,2010)"|decade=="[2010,2020]"))  

#We recode the levels so that they are numeric and we can do the linear regression:

df$decadenum <- df$decade
df$decadenum <- recode_factor(df$decadenum, "[1990,2000)"=1, "[2000,2010)"=2 ,"[2010,2020]" = 3)
df$decadenum <- as.numeric(df$decadenum)

is.numeric(df$decadenum)

#Linear model

#First for one specific country  (i.e. United States)

subset1 <- subset(df, decadenum==1 & country=="World")
subset2 <- subset(df, decadenum==2 & country=="World")
subset3 <- subset(df, decadenum==3 & country=="World")
 
m1co2 <- mean(subset1[,"co2"],na.rm=T)
m2co2 <- mean(subset2[,"co2"],na.rm=T)
m3co2 <- mean(subset3[,"co2"],na.rm=T)

datatoadjust <- data.frame(c(m1co2,m2co2,m3co2),c(1,2,3))
names(datatoadjust) <- c("y","x")
model <- lm(y~x, data=datatoadjust)
summary(model)

new <- data.frame(x = 4)  # For the 2020 decade
new1 <- data.frame(x = 5)  # For the 2030 decade
predict(model,new)
predict(model, new1)

# Does not make any sense to make a lm with only 3 points!
#Let's do a lm accounting for all years.

is.numeric(Greenhouse_Gas_Emissions$year)

subset1 <- subset(Greenhouse_Gas_Emissions, country=="World")

model <- lm(co2~year, data=subset1)
summary(model)

x <- 2030  # For the 2030 year
predict <- -184624.08+101.27*x
predict

plot(co2~year, data=subset1)

#We can see it would better fit an exponential type function

# Anyways, we do a function that does a linear model with the last 3 decades and then...
# ... predicts the average anual emission in the next decades.


decadepredict <- function(Country="World",Decade=2030) {
  
  gases <- c("co2","methane", "nitrous_oxide")
  
  for (i in 1:3) {
  subset1 <- subset(df, decadenum==1 & country==Country)
  subset2 <- subset(df, decadenum==2 & country==Country)
  subset3 <- subset(df, decadenum==3 & country==Country)
  
  m1co2 <- mean(subset1[,gases[i]],na.rm=T)
  m2co2 <- mean(subset2[,gases[i]],na.rm=T)
  m3co2 <- mean(subset3[,gases[i]],na.rm=T)
  
  datatoadjust <- data.frame(c(m1co2,m2co2,m3co2),c(1,2,3))
  names(datatoadjust) <- c("y","x")
  model <- lm(y~x, data=datatoadjust)
  summary(model)
  
  new <- data.frame(x = (Decade-1990)/10)
  cat("Prediction for average anual emissions of", gases[i], "in", Country, "in the decade", 
      Decade, "is:", predict(model,new), "million tones", "\n")
  }
}

decadepredict()
decadepredict("Spain",2050)


# Also, we do a function that does a linear model with the last 30 years and then...
# ... predicts for future years.

Greenhouse_Gas_Emissions$decade <- cut2(Greenhouse_Gas_Emissions$year, c(seq(from=1750, to=2020, by=10)))
df<-data.frame(subset(Greenhouse_Gas_Emissions, decade=="[1990,2000)"|decade=="[2000,2010)"|decade=="[2010,2020]"))  


Prediction <- function(Country="World", predictyear=2030) {
  
  subset1 <- data.frame(subset(df, country==Country))
  modelco2 <- lm(co2~year, data=subset1)
  modelmethane <- lm(methane~year , data=subset1)
  modelnitrous <- lm(nitrous_oxide~year, data=subset1)
  new <- data.frame(year = predictyear)
  cat("Projection of gas emissions for country", Country, "in", 
      predictyear, ":" , "\n" , "CO2: " , as.vector(predict(modelco2,new)) , "million tones","\n" ,
      "Methane: " ,as.vector(predict(modelmethane,new)) ,"million tones", "\n" ,
      "Nitrous Oxide: " ,as.vector(predict(modelnitrous,new)) ,"million tones", "\n")
  cat("The summary of the regression models obtained are displayed below:","\n","\n")
  print(summary(modelco2))
  print(summary(modelmethane))
  print(summary(modelnitrous))
}

Prediction()
Prediction("China", 2035)


#c) The results found, especially when looking for the entire world or overall...
#... regions that concentrate big part of the population, suggest that gas emissions are...
#... increasing with time and will continue to increase in the next upcoming years.
# Therefore, we can argue that during the next years, if the gas emissions increase as...
#... suggested from the data, they will have a significant impact on the climate change.

# For the entire World, all of the gas emissions studied present a linear trend with positive slope that is...
#... statistically significant.
# However, when modelling the CO2 emissions taking into account previous decades, one can...
#... realize that rather a linear trend, the CO2 emissions present exponentially increasing values and...
#.. the linear model is no longer appropriate to make predictions.
# Some models that would probably be better for fitting the data we have:
# One could probably better model the gas emissions over years with a time series approach, ...
# ... i.e. , fitting an arima model.
# Or it could be also used and approach of a panel data linear model for accounting for...
# ... the individual heterogeneity across countries.


#Also: A package has been created and uploaded to Github with some of the functions and data from this script.
# To download the package and use it go to: https://github.com/oriol33/R-library-CO2



avgco2 <- with(subset(Greenhouse_Gas_Emissions, country == "World"),tapply(co2, decade, mean, na.rm=T))
avgmethane <- with(subset(Greenhouse_Gas_Emissions, country == "World"),tapply(methane, decade, mean, na.rm=T))
avgnitrous <- with(subset(Greenhouse_Gas_Emissions, country == "World"),tapply(nitrous_oxide, decade, mean, na.rm=T))

#Using barplots:
barplot(avgco2)
barplot(avgmethane)
barplot(avgnitrous)


#d)

#Webpage for climate change data

#https://climatedata.imf.org/pages/climatechange-data/#cc5

#World Monthly Atmospheric Carbon Dioxide concentrations
#This indicator presents the concentration of carbon dioxide in the atmosphere, on a monthly and yearly basis, dating back to 1958.
#The source data for these visualizations comes from the National Oceanic and Atmospheric Association Global Monitoring Laboratory.

AtmosCO2conc <- read.csv2("Atmospheric_CO2_Concentrations.csv",header = T,sep = ",")


#Climate related disasters frequency
#The links between climate change and natural disasters. 
#This dataset depicts the climate-related disasters over time.

Climatedisasters <- read.csv2("Climate_related_disasters_frequency.csv",header = T,sep = ",")

s2 <- matrix(subset(Climatedisasters, ISO2=="AQ"&Indicator=="Climate related disasters frequency, Number of Disasters: TOTAL")[,8:ncol(Climatedisasters)],ncol=1)
disasters <- data.frame("year"=1980:2020, "Total number of disasters"=as.numeric(s2))

meanglobaltemp <- read.csv2("Mean_Global_Surface_Temperature.csv",header = T,sep = ",")
s1 <- matrix(subset(meanglobaltemp, Country=="World")[,8:ncol(meanglobaltemp)],ncol=1)
avg_change_temp_world <- data.frame("year"=1961:2020, "Avg. Change in temperature"=as.numeric(s1))


dataframe2 <- merge(disasters, avg_change_temp_world, by='year')

# Correlation matrix
cormat3 <- round(cor(dataframe2[,2:3], use = "complete"),2)
cormat3

# Plot
par(mar=c(5, 4, 4, 6) + 0.1)
plot(dataframe2$year, dataframe2$Avg..Change.in.temperature, axes=F,type='l',xlab="",ylab="")
axis(2,col="black",las=1)
mtext("Avg. Temperature change with respect to a baseline",side=2,line=2.5)
par(new=T)
plot(dataframe2$year, dataframe2$Total.number.of.disasters, axes=F, type='l',xlab="",ylab="", col="red")
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
mtext("Number of disasters",side=4,col="red",line=4) 
axis(1,pretty(range(dataframe2$year),10))
mtext("Years",side=1,col="black",line=2.5) 

#Annual Mean Global Surface Temperature

#This indicator presents the mean surface temperature change during the period 1961-2019, 
# using temperatures between 1951 and 1980 as a baseline.
#This data is provided by the Food and Agriculture Organization Corporate Statistical Database (FAOSTAT)
#and is based on publicly available GISTEMP data from the National Aeronautics and Space Administration Goddard Institute for Space Studies (NASA GISS).

meanglobaltemp <- read.csv2("Mean_Global_Surface_Temperature.csv",header = T,sep = ",")
s1 <- matrix(subset(meanglobaltemp, Country=="World")[,8:ncol(meanglobaltemp)],ncol=1)
avg_change_temp_world <- data.frame("year"=1961:2020, "Avg. Change in temperature"=as.numeric(s1))

dataframe <- merge(avg_change_temp_world, data.frame(subset(Greenhouse_Gas_Emissions, country=="World")[,c("year","co2","methane","nitrous_oxide")]), by="year")

# Correlations matrix
cormat2 <- round(cor(dataframe[2:5], use = "complete"),2)
cormat2

# Time series of temperature change and Co2 together
par(mar=c(5, 4, 4, 6) + 0.1)
plot(dataframe$year, dataframe$Avg..Change.in.temperature, axes=F,type='l',xlab="",ylab="")
axis(2,col="black",las=1)
mtext("Avg. Temperature change with respect to a baseline",side=2,line=2.5)
par(new=T)
plot(dataframe$year, dataframe$co2, axes=F, type='l',xlab="",ylab="", col="green")
axis(4, ylim=c(0,7000), col="green",col.axis="green",las=1)
mtext("Co2 emissions in millions of tonnes",side=4,col="green",line=4) 
axis(1,pretty(range(dataframe$year),10))
mtext("Years",side=1,col="black",line=2.5) 

# Time series of temperature change and methane together
par(mar=c(5, 4, 4, 6) + 0.1)
plot(dataframe$year, dataframe$Avg..Change.in.temperature, axes=F,type='l',xlab="",ylab="")
axis(2,col="black",las=1)
mtext("Avg. Temperature change with respect to a baseline",side=2,line=2.5)
par(new=T)
plot(dataframe$year, dataframe$methane, axes=F, type='l',xlab="",ylab="", col="orange")
axis(4, ylim=c(0,7000), col="orange",col.axis="orange",las=1)
mtext("Methane emissions in tonnes",side=4,col="orange",line=4) 
axis(1,pretty(range(dataframe$year),10))
mtext("Years",side=1,col="black",line=2.5) 

# Time series of temperature change and nitrous oxide together
par(mar=c(5, 4, 4, 6) + 0.1)
plot(dataframe$year, dataframe$Avg..Change.in.temperature, axes=F,type='l',xlab="",ylab="")
axis(2,col="black",las=1)
mtext("Avg. Temperature change with respect to a baseline",side=2,line=2.5)
par(new=T)
plot(dataframe$year, dataframe$nitrous_oxide, axes=F, type='l',xlab="",ylab="", col="blue")
axis(4, ylim=c(0,7000), col="blue",col.axis="blue",las=1)
mtext("Nitrous oxide emissions in tonnes",side=4,col="blue",line=4) 
axis(1,pretty(range(dataframe$year),10))
mtext("Years",side=1,col="black",line=2.5) 


#e) A library has been created and uploaded to Github with all of the functions and data from this script.
# To download the package and use it go to: https://github.com/oriol33/R-library-CO2







