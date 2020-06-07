#install dplyr library
#install.packages("dplyr")
library("dplyr")
library(magrittr)

#install the psc1 package
#install.packages("tidyr")
library("tidyr")

#install ggplot package
#install.packages("ggplot2")
library("ggplot2")

sensex_data <- read.csv("data/market_analysis.csv",
                        stringsAsFactors = FALSE
                        )

#find out the Fundamental Returs (i.e EPS and percent returns)
#Find out the EPS Return (Sensex value per PE ratio)

# a function which will spill out percent growth of each year
percen_growth_in_value <- function(value) {
  # reversing the order of clsoing values
  rev_value = rev(value)
  # prev_value will hold last used variable of the closing value
  prev_value = NA
  x=1
  # this will return the growth figures each year
  growth <- NA
  # looping through closing values that will determine the growth per year
  for (n in rev_value) {
    # precentage growth in two values
    growth[x] <- round(((prev_value/n)-1)*100, digits=2)
    x = x+1
    #changing the prev_value
    prev_value = n
  }
  return(growth)
}

# The function will find out EPS of each year.
eps <- function(pe_ratio, stock_value){
  earnings <- round(stock_value / pe_ratio, digits = 2)  
  return(earnings)
}

# function will determine the speculative return
# speculative return is EPS of current year into difference of PE ration
# between the current and previous year wholly divided by current sensex value
speculative_return <- function(eps, pe_ratio, stock_value){
  #reverseing all the values
  rev_eps = rev(eps)
  rev_pe_ratio = rev(pe_ratio)
  rev_value = rev(stock_value)
  # a holding value for last year pe ratio
  prev_pe = 0
  # return holder for speculative return
  spec_return = numeric(length(eps))
  for (n in 1:length(eps)){
    x <- (rev_eps[n] * (rev_pe_ratio[n] - prev_pe)*100)/rev_value[n]
    spec_return[n] <- round(x, digits = 2)
    prev_pe = rev_pe_ratio[n]
  }
  return(rev(spec_return))
}
#select the close and year coloumn
#closing_value <- sensex_data %>% select(ï..year, Close,PE.Ratios) 

# function will return total return in each year
# which is the addition of dividend yield, speculative return and fundamental return  
total_return = function(div_yield, spec_re, fund_re){
  return(round(div_yield + spec_re + fund_re, digits = 2))
}

# Fundamental Return each year
fund_return <- function(eps){
  rev_eps = rev(eps)
  prev_eps = 0
  fun_re = NA
  for (n in 1:length(eps)) {
    fun_re[n] <- round(((rev_eps[n]/prev_eps)-1)*100, digits = 2)
    prev_eps = rev_eps[n]
  }
  return(rev(fun_re))
}

market_analysis <- sensex_data %>% 
  select(Year = "ï..Year", Sensex.Value = Close,PE.Ratios, Dividend.Yield) %>%
  mutate(EPS = eps(PE.Ratios, Sensex.Value),
         Fundamental.Return = fund_return(EPS),
         Speculative_Return = speculative_return(EPS, PE.Ratios, Sensex.Value),
         Total_Return = total_return(Dividend.Yield,
                                     Speculative_Return,
                                     Fundamental.Return)
         )  
  
