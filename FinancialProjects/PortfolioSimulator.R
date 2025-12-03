#!/usr/bin/env Rscript
#Above line allows code to be run using ./PortfolioSimulator.R in terminal

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(tidyr)#drop_na function
library(insight)#print_color function
library(ggplot2)#plot related
library(reshape2)#melt function
library(argparser)#argument parser stuff
library(lubridate)#time stuff
library(yfR)#collect data from yahoo finance

#Code allows one to compare multiple accounts assuming they all get renewed with the same terms

#Gathers both the length of contract and interest rate
#Can also change starting value if you want more realistic projection 
parser <- arg_parser('Account Inputs')
parser <- add_argument(parser, '--initial', help = 'initial account deposit: default is 10000',default=10000)
parser <- add_argument(parser, '--nyrs', help = 'number of years to simulate: default is 10',default=10)
parser <- add_argument(parser, '--nsims', help = 'number of simulations: default is 1000',default=1000)
parser <- add_argument(parser, '--tickers', help = 'tickers to use: default are Vangaurd ETFs that I plan to hold',default=c('default'))
arg <- parse_args(parser)

initial <- as.numeric(arg$initial)
simlength <- as.numeric(arg$nyrs)
nsims <- as.numeric(arg$nsims)

delT <- 251#251 trading days per year on average
today <- today()

#Setting parameters
if (arg$tickers == 'default'){
	TICKERS <- c('VOO','VOOG','VONG','MGK','VGT')
	PORTFOLIO.WEIGHTS <- c(.1,.15,.25,.25,.2)#leaving 5% of the portfolio for cash/trading
}else {
	TICKERS <- strsplit(arg$tickers, ',')[[1]]
	PORTFOLIO.WEIGHTS <- rep(1/length(TICKERS), length(TICKERS))#Assuming equal weights for now but will add more flexibility later
}

holdings <- data.frame(Ticker = TICKERS, Weight = PORTFOLIO.WEIGHTS)

##############################################################################################################
#################################################FUNCTIONS####################################################
##############################################################################################################
#Calculate historical daily gains
Gains <- function(data){
        #Separate dates to calculate gains 
        dates <- data$Date
        gainvec <- c()
        for (i in 1:(length(dates)-1)){
                gain <- data[data$Date == dates[i+1],]$Close / data[data$Date == dates[i],]$Close - 1
                gainvec <- c(gainvec,gain)
        }
        return(gainvec)
}

#Simulate gains 
SimGain <- function(tickers, holdings, correlations){
	h <- holdings
	corr <- correlations
	#Assign random gains to each "die"
	for (t in tickers){
		assign(paste0('r',t,'gain'), rnorm(1, mean = h[h$Ticker == t,'Gain.mn'], sd = h[h$Ticker == t,'Gain.sd']))
	}
	
	#Normalize correlations to produce weights 
	for (t in tickers){
		corr[[t]] <- corr[[t]] / sum(corr[[t]])
	}
	
	#Calculate gain for each ticker in the portfolio
	gain <- list()
	for (t in tickers){
		value <- 0
		for (i in tickers){
			value <- value + get(paste0('r',i,'gain'))*corr[t,i]
		}
		gain[[t]] <- list(value)
	}
	return(gain)
}

##############################################################################################################
#############################################BASE CALCULATIONS################################################
##############################################################################################################
print_color(paste0('================================================================================\n'),'bcyan')
print_color(paste0('================Gathering Information To Be Used In Simulations=================\n'),'bcyan')
print_color(paste0('================================================================================\n'),'bcyan')

#Collecting data from yahoo finance
df <- yf_get(TICKERS, first_date = today - years(20), last_date = today, freq_data = 'daily', thresh_bad_data = .5)#Allowing for tickers that have at least 10 years worth of data over a 20 year period
df <- df %>%
	select(c('ticker','ref_date','price_open','price_high','price_low','price_close','volume')) %>%
	rename(Tickers = ticker, Date = ref_date, Open = price_open, High = price_high, Low = price_low, Close = price_close, Volume = volume) %>%
	print()

#Add more portfolio specific things to holdings dataframe
tstart <- c()
tgainmn <- c()
tgainsd <- c()
gainsdata <- list()
for (t in TICKERS){
	price <- df[df$Tickers == t,]
	tstart <- c(tstart, as.numeric(price[price$Date == last(price$Date), 'Close']))
	
	gainsall <- Gains(price)	
	gains10y <- Gains(price[price$Date >= last(price$Date) - years(10),])	
	gains5y <- Gains(price[price$Date >= last(price$Date) - years(5),])	
	gains3y <- Gains(price[price$Date >= last(price$Date) - years(3),])	

	#Will average over yearly gains from each of the periods to create quasi-weighted gains
	tgainmn <- c(tgainmn, (.25*mean(gains3y) + .25*mean(gains5y) + .25*mean(gains10y) + .25*mean(gainsall)))
	tgainsd <- c(tgainsd, (.25*sd(gains3y) + .25*sd(gains5y) + .25*sd(gains10y) + .25*sd(gainsall)))

	#Will save gains so I can calculate correlations
	gains <- data.frame(Dates = price$Date[-1])
	gains[[t]] <- gainsall
	gainsdata <- append(gainsdata, list(gains))
}
holdings$Ticker.Start <- tstart
holdings$Gain.mn <- tgainmn
holdings$Gain.sd <- tgainsd
print(as_tibble(holdings))

#Merging gains data so that I can get correlations between gains
allgains <- Reduce(function(x,y) merge(x,y,all = TRUE), gainsdata)
allgains <- allgains %>% drop_na()
corr <- as.data.frame(cor(allgains[,TICKERS]))
print(corr)

##############################################################################################################
################################################SIMULATIONS###################################################
##############################################################################################################
#Calculate starting portfolio values using whole shares that can be purchased
sharesvec <- c()
for (t in TICKERS){
	shares <- (initial*holdings[holdings$Ticker == t,]$Weight)/(holdings[holdings$Ticker == t,]$Ticker.Start)
	sharesvec <- c(sharesvec, round(shares,0))
}
holdings$Shares <- sharesvec
print(holdings)

#Check that rounding shares didn't exceed/udershot initial value and if so then remove/add smallest priced ticker 
total <- sum(holdings$Shares*holdings$Ticker.Start)
minhold <- holdings[holdings$Ticker.Start == min(holdings$Ticker.Start),'Ticker']
if (total > initial*.95){
	print_color(paste0('!!!!!!!!!!!INITIAL VALUE SURPASSED: REMOVING SMALLEST PRICED HOLDING!!!!!!!!!!!!\n'),'bred')
	while (total > initial*.95){
		holdings[holdings$Ticker == minhold,'Shares'] <- holdings[holdings$Ticker == minhold,'Shares'] - 1
		total <- sum(holdings$Shares*holdings$Ticker.Start)
	}
}else {
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!INITIAL VALUE UNDERSHOT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	while (total <= (initial*.95 - min(holdings$Ticker.Start))){
		print_color(paste0('!!!!!!!!!!!INITIAL VALUE UNDERSHOT: ADDING SMALLEST PRICED HOLDING!!!!!!!!!!!!!!\n'),'bgreen')
		holdings[holdings$Ticker == minhold,'Shares'] <- holdings[holdings$Ticker == minhold,'Shares'] + 1
		total <- sum(holdings$Shares*holdings$Ticker.Start)
	}
}

#Build starting portfolio dataframe
print_color(paste0('================================================================================\n'),'bcyan')
print_color(paste0('================Starting Portfolio Values For Every Simulation==================\n'),'bcyan')
print_color(paste0('================================================================================\n'),'bcyan')
print(holdings)
portfolio <- data.frame(Day = c(0), Balance = c(total))
for (t in TICKERS){
	portfolio[[t]] <- holdings[holdings$Ticker == t,'Shares']*holdings[holdings$Ticker == t,'Ticker.Start']
}
print(portfolio)
ndays <- simlength*delT

#Simulating the portfolio
print_color(paste0('================================================================================\n'),'bgreen')
print_color(paste0('====================Simulating Possible Portfolio Futures=======================\n'),'bgreen')
print_color(paste0('================================================================================\n'),'bgreen')
#Saving data of interest
finalbalance <- c()
balancedata <- list()
for (simnum in 1:nsims){
	#Simulate a single possible future
	simportfolio <- portfolio
	names(simportfolio)[names(simportfolio) == 'Balance'] <- paste0('Sim',simnum,'.Balance')

	for (d in 1:ndays){
		#Simulate the gains for each position
		newgains <- SimGain(tickers = TICKERS, holdings = holdings, correlations = corr)
		newgains <- unlist(newgains)
		temp <- c()
		for (t in TICKERS){
			temp <- c(temp, simportfolio[simportfolio$Day == d - 1, t]*(1 + newgains[t]))
		}
		newtotal <- sum(temp)
		newrow <- c(d,newtotal,temp)
		simportfolio <- rbind(simportfolio,newrow)
	}
	finalbalance <- c(finalbalance,last(simportfolio[[paste0('Sim',simnum,'.Balance')]]))
	balancedata <- append(balancedata, list(simportfolio[,c('Day',paste0('Sim',simnum,'.Balance'))])) 
	print(tail(simportfolio,10))
}

#Summarizing portfolio results and plotting all simulations together
print_color(paste0('================================================================================\n'),'bviolet')
print_color(paste0('=========================Simulated Portfolio Results============================\n'),'bviolet')
print_color(paste0('================================================================================\n'),'bviolet')
print_color(paste0('Final Portfolio Balance Quantiles:\n'),'bold')
quantiles <- quantile(finalbalance)
print(quantiles)
median <- quantiles['50%']
upperquart <- quantiles['75%']
lowerquart <- quantiles['25%']


#Gathering all plotting data together
allportfolios <- Reduce(function(x,y) merge(x,y,all = TRUE), balancedata)
plotdf <- melt(allportfolios, id = 'Day')
plotends <- plotdf %>% filter(Day == ndays)
print(plotends)

#Calculate proportion of portfolios that exceed 20% per year avg
baseline <- total
for (i in 1:arg$nyrs){
	baseline <- baseline*1.2
}
propabove <- nrow(plotends[plotends$value >= baseline,])/nrow(plotends) 

ggplot(data=plotdf, mapping=aes(x=Day,y=value,group=variable))+geom_line(color = 'blue', alpha=.5)+labs(title=paste0('Simulated Portfolio Balances'))+scale_x_continuous(name='Day', n.breaks=10)+scale_y_continuous(name='Balance', n.breaks=10)+geom_hline(yintercept=median, linetype='dashed', color = 'slategrey', alpha=.5)+annotate('text', size=3, hjust='inward', vjust=-1, x=1, y=median, label=paste('50%: ',round(median,2)), color='slategrey')+geom_hline(yintercept=upperquart, linetype='dashed', color = 'green3', alpha=.5)+annotate('text', size=3, hjust='inward', vjust=-1, x=1, y=upperquart, label=paste0('75%: ',round(upperquart,2)), color='green3')+geom_hline(yintercept=lowerquart, linetype='dashed', color = 'red', alpha=.5)+annotate('text', size=3, hjust='inward', vjust=-1, x=1, y=lowerquart, label=paste0('25%: ',round(lowerquart,2)), color='red')+geom_hline(yintercept=baseline, linetype='dashed', color = 'black', alpha=.5)+annotate('text', size=3, hjust='inward', vjust=-1, x=ndays, y=baseline, label=paste0('Proportion of simulations above 20% per year avg: ',round(propabove,2)), color='black')
ggsave(file=paste0('PortfolioSimulator-',simlength,'years-',nsims,'sims.pdf'), path=paste0('plots/'))
