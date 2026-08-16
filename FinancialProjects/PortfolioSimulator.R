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
parser <- add_argument(parser, '--nsims', help = 'number of simulations: default is 20',default=20)
parser <- add_argument(parser, '--tickers', help = 'tickers to use: default are Vangaurd ETFs that I plan to hold',default=c('default'))
parser <- add_argument(parser, '--weights', help = 'portfoloio weights for each ticker: default is equal',default=c('eq'))
parser <- add_argument(parser, '--divs', help = 'whether to include dividends in analysis: default is TRUE',default=TRUE)
arg <- parse_args(parser)

initial <- as.numeric(arg$initial)
simlength <- as.numeric(arg$nyrs)
nsims <- as.numeric(arg$nsims)

delT <- 251#251 trading days per year on average
today <- today()

#Setting parameters
if (arg$tickers == 'default'){
	TICKERS <- c('VOO','VOOG','VONG','MGK','VGT')
	PORTFOLIO.WEIGHTS <- c(.15,.15,.25,.25,.2)
}else {
	TICKERS <- strsplit(arg$tickers, ',')[[1]]
	if (grepl('eq',arg$weights)){
		PORTFOLIO.WEIGHTS <- rep(1/length(TICKERS), length(TICKERS))
	}else {
		PORTFOLIO.WEIGHTS <- sapply(strsplit(arg$weights,',')[[1]], function(x) as.numeric(x))
		#Check that weights do not exceed 1
		if (sum(PORTFOLIO.WEIGHTS) != 1){
			print_color(paste0('!!!!!!!!!!!!!!!!!!WEIGHTS DO NOT SUM TO 1: INPUT NEW WEIGHTS!!!!!!!!!!!!!!!!!!!!\n'),'bred')
			break
		}
	}
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
	select(c('ticker','ref_date','price_open','price_high','price_low','price_close','price_adjusted','volume')) %>%
	rename(Tickers = ticker, Date = ref_date, Open = price_open, High = price_high, Low = price_low, Close = price_close, Adj.Close = price_adjusted, Volume = volume) %>%
	print()

if (arg$divs){
	#Collect dividend information
	divdata <- data.frame(Tickers = TICKERS, Yield = rep(0,length(TICKERS)), Frequency = rep(0,length(TICKERS)), Frequency.Label = rep('Irregular',length(TICKERS)))
	for (t in TICKERS){
		#TryCatch for tickers that do not have dividends
		tryCatch(
			expr = {
				divid <- yf_get_dividends(t, first_date = today - years(5), last_date = today)#Getting dividends in last 5 years for tickers if applicable
				print(divid)

				#Categorize common frequencies for simulations
				divfreq <- round(length(divid$dividend) / 5)
				divdata[divdata$Tickers == t, 'Frequency'] <- divfreq

				if (divfreq == 1){
					divdata[divdata$Tickers == t, 'Frequency.Label'] <- 'Annually'
				}else if (divfreq == 2){
					divdata[divdata$Tickers == t, 'Frequency.Label'] <- 'Semi-Annually'
				}else if (divfreq == 4){
					divdata[divdata$Tickers == t, 'Frequency.Label'] <- 'Quarterly'
				}else if (divfreq == 12){
					divdata[divdata$Tickers == t, 'Frequency.Label'] <- 'Monthly'
				}else {
					divdata[divdata$Tickers == t, 'Frequency.Label'] <- 'Irregular'
				}
				
				#Calculate forward dividend yield for the last five years and trailing dividend yield for the last three years
				forwyield <- c()
				trailyield <- c()
				for (date in divid$ref_date){
					paydiv <- as.numeric(divid[divid$ref_date == date,'dividend'])
					dayclose <- as.numeric(df[(df$Tickers == t & df$Date == date),'Close'])
					forwyield <- c(forwyield, (paydiv * divfreq)/dayclose)
					if (date > (today - years(3))){
						traildata <- divid %>%
								filter(ref_date <= date) 
						traildata <- tail(traildata,4)
						trailyield <- c(trailyield, sum(traildata$dividend)/dayclose)
					}
				}
				print(mean(forwyield))
				print(mean(trailyield))
				divdata[divdata$Tickers == t, 'Yield'] <- mean(c(mean(forwyield),mean(trailyield)))
				
			},
			error = function(e){
				print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!',t,' DOES NOT HAVE DIVIDENDS!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
			}
		)
	}
	print(divdata)
}

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
if (length(TICKERS) > 1){
	corr <- as.data.frame(cor(allgains[,TICKERS]))
}else {
	corr <- data.frame('TEMP' = 1)
	colnames(corr) <- TICKERS[[1]]
	row.names(corr) <- TICKERS[[1]]
}
print(corr)

##############################################################################################################
################################################SIMULATIONS###################################################
##############################################################################################################
#Calculate starting portfolio values using whole shares that can be purchased
sharesvec <- c()
for (t in TICKERS){
	shares <- (initial*holdings[holdings$Ticker == t,]$Weight)/(holdings[holdings$Ticker == t,]$Ticker.Start)
	sharesvec <- c(sharesvec, round(shares))
}
holdings$Shares <- sharesvec
print(holdings)

#Check that rounding shares didn't exceed/udershot initial value and if so then remove/add smallest priced ticker 
total <- sum(holdings$Shares*holdings$Ticker.Start)
temp <- holdings[order(holdings$Ticker.Start),]

if (total > initial){
	print_color(paste0('!!!!!!INITIAL VALUE SURPASSED: REMOVING SMALLEST PRICED HOLDINGS IN ORDER!!!!!!!\n'),'bred')
	count <- 1
	while (total > initial){
		trimhold <- temp[count,'Ticker']
		holdings[holdings$Ticker == trimhold,'Shares'] <- holdings[holdings$Ticker == trimhold,'Shares'] - 1
		total <- sum(holdings$Shares*holdings$Ticker.Start)
		count <- count + 1
	}
}else {
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!INITIAL VALUE UNDERSHOT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	diff <- initial - total
	while (min(temp$Ticker.Start) <= diff){
		print_color(paste0('!!!!!!INITIAL VALUE UNDERSHOT: ADDING SMALLEST PRICED HOLDINGS IN ORDER!!!!!!!!!\n'),'bgreen')
		addholds <- c(temp[1,'Ticker'])
		for (i in 2:length(temp$Ticker)){
			if (sum(temp$Ticker.Start[1:i]) <= diff){
				addholds <- c(addholds, temp[i,'Ticker'])
			}
		}
		for (t in addholds){
			holdings[holdings$Ticker == t,'Shares'] <- holdings[holdings$Ticker == t,'Shares'] + 1
		}
		total <- sum(holdings$Shares*holdings$Ticker.Start)
		diff <- initial - total
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

#Starting dividends information
if (arg$divs){
	dividends <- data.frame(Day = c(0), Total = c(0))
	for (t in TICKERS){
		dividends[[t]] <- c(0)
	}
	print(dividends)
}

#Simulating the portfolio
print_color(paste0('================================================================================\n'),'bgreen')
print_color(paste0('====================Simulating Possible Portfolio Futures=======================\n'),'bgreen')
print_color(paste0('================================================================================\n'),'bgreen')
#Saving data of interest
ndays <- simlength*delT
finalbalance <- c()
balancedata <- list()

if (arg$divs){
	finaltotal <- c()
	dividendsdata <- list()
}

for (simnum in 1:nsims){
	#Simulate a single possible future
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!!Simulating Portfolio ',simnum,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bblue')
	simportfolio <- portfolio
	names(simportfolio)[names(simportfolio) == 'Balance'] <- paste0('Sim',simnum,'.Balance')

	#Find days when dividends will be paid out for regular schedules and it may differ between simulations to get more realistic simulations 
	if (arg$divs){
		simdividends <- dividends
		names(simdividends)[names(simdividends) == 'Total'] <- paste0('Sim',simnum,'.Total')
		
		annual.divdays <- seq(from = 1, to = ndays, by = 251)
		annual.divdays <- round(annual.divdays)
		shift <- sample(0:250,1)
		annual.divdays <- annual.divdays + shift

		semiannual.divdays <- seq(from = 1, to = ndays, by = 251/2)
		semiannual.divdays <- round(semiannual.divdays)
		shift <- sample(0:floor(250/2),1)
		semiannual.divdays <- semiannual.divdays + shift
		
		quart.divdays <- seq(from = 1, to = ndays, by = 251/4)
		quart.divdays <- round(quart.divdays)
		shift <- sample(0:floor(250/4),1)
		quart.divdays <- quart.divdays + shift
		
		month.divdays <- seq(from = 1, to = ndays, by = 251/12)
		month.divdays <- round(month.divdays)
		shift <- sample(0:floor(250/12),1)
		month.divdays <- month.divdays + shift
	}
	
	for (d in 1:ndays){
		#Simulate the gains for each position
		newgains <- SimGain(tickers = TICKERS, holdings = holdings, correlations = corr)
		newgains <- unlist(newgains)
		temp <- c()
		divpayvec <- c()
		for (t in TICKERS){
			if (arg$divs){
				if (divdata[divdata$Tickers == t,'Frequency.Label'] == 'Annually'){
					checkdiv <- annual.divdays
				}else if (divdata[divdata$Tickers == t,'Frequency.Label'] == 'Semi-Annually'){
					checkdiv <- semiannual.divdays
				}else if (divdata[divdata$Tickers == t,'Frequency.Label'] == 'Quarterly'){
					checkdiv <- quart.divdays
				}else if (divdata[divdata$Tickers == t,'Frequency.Label'] == 'Monthly'){
					checkdiv <- month.divdays
				}else {
					checkdiv <- c(0)
					divpayvec <- c(divpayvec,0)
				}
				if (d %in% checkdiv){
					divpay <- (simportfolio[simportfolio$Day == d - 1, t] * (divdata[divdata$Tickers == t,'Yield'] / divdata[divdata$Tickers == t,'Frequency']))
					divpayvec <- c(divpayvec,divpay)
					temp <- c(temp, (simportfolio[simportfolio$Day == d - 1, t]*(1 + newgains[t]) + divpay))
				}else {
					temp <- c(temp, simportfolio[simportfolio$Day == d - 1, t]*(1 + newgains[t]))
					divpayvec <- c(divpayvec,0)
				}
			}else {
				temp <- c(temp, simportfolio[simportfolio$Day == d - 1, t]*(1 + newgains[t]))
			}
		}
		newbalance <- sum(temp)
		newrow <- c(d,newbalance,temp)
		simportfolio <- rbind(simportfolio,newrow)
		if (arg$divs){
			newtotal <- sum(divpayvec)
			newrow <- c(d,newtotal,divpayvec)
			simdividends <- rbind(simdividends,newrow)
		}
	}
	finalbalance <- c(finalbalance,last(simportfolio[[paste0('Sim',simnum,'.Balance')]]))
	balancedata <- append(balancedata, list(simportfolio)) 
	print(tail(simportfolio,10))
	if (arg$divs){
		simdividends <- simdividends[simdividends[[paste0('Sim',simnum,'.Total')]] > 0,]
		finaltotal <- c(finaltotal,sum(simdividends[[paste0('Sim',simnum,'.Total')]]))
		dividendsdata <- append(dividendsdata, list(simdividends)) 
		print(tail(simdividends,10))
	}
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
meltcols <- colnames(allportfolios[,!(names(allportfolios) %in% c('Day',TICKERS))])
plotdf <- melt(allportfolios, id = 'Day', measure.vars = meltcols)
plotdf <- plotdf %>% na.omit(plotdf)
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

if (arg$divs){
	print_color(paste0('================================================================================\n'),'bviolet')
	print_color(paste0('=========================Simulated Dividend Results=============================\n'),'bviolet')
	print_color(paste0('================================================================================\n'),'bviolet')
	#Looking at dividend results
	print_color(paste0('Final Dividend Total Quantiles:\n'),'bold')
	divquant <- quantile(finaltotal)
	print(divquant)

	#Gathering all dividend data together
	alldividends <- Reduce(function(x,y) merge(x,y,all = TRUE), dividendsdata)
}
