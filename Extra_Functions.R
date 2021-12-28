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

n <- function(x){length(x)}

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
      sddif[i] = sqrt((sdco2[i]^2)/nco2[i]+(sdco2[i+1]^2)/nco2[i+1])
    }
    
    sddifmet <- vector(length = length(avgmethanedif))
    for(i in 1:length(avgmethanedif)){
      sddifmet[i] = sqrt((sdmet[i]^2)/nmet[i]+(sdmet[i+1]^2)/nmet[i+1])
    }
    
    sddifnit <- vector(length = length(avgnitrousdif))
    for(i in 1:length(avgnitrousdif)){
      sddifnit[i] = sqrt((sdnit[i]^2)/nnit[i]+(sdnit[i+1]^2)/nnit[i+1])
    }
    # Computing the degrees of freedom for the t-student distribution
    g <- vector(length = length(sddif))
    for(i in 1:length(avgco2dif)){
      g[i] = (((sdco2[i]^2/nco2[i])+(sdco2[i+1]^2/nco2[i+1]))^2)/(((sdco2[i]^2/nco2[i])^2)*(1/(nco2[i]+1))+((sdco2[i+1]^2/nco2[i+1])^2)*(1/((nco2[i+1]+1))))
    }
    
    gmet <- vector(length = length(sddifmet))
    for(i in 1:length(avgco2dif)){
      gmet[i] = (((sdmet[i]^2/nmet[i])+(sdmet[i+1]^2/nmet[i+1]))^2)/(((sdmet[i]^2/nmet[i])^2)*(1/(nmet[i]+1))+((sdmet[i+1]^2/nmet[i+1])^2)*(1/(nmet[i+1]+1)))
    }
    
    gnit <- vector(length = length(sddifnit))
    for(i in 1:length(avgnitrousdif)){
      gnit[i] = (((sdnit[i]^2/nnit[i])+(sdnit[i+1]^2/nnit[i+1]))^2)/(((sdnit[i]^2/nnit[i])^2)*(1/(nnit[i]+1))+((sdnit[i+1]^2/nnit[i+1])^2)*(1/(nnit[i+1]+1)))
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
