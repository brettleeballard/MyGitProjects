#!/usr/bin/env Rscript
#Above line allows code to be run using ./CDCompare.R in terminal

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(insight)#print_color function
library(ggplot2)#plot related
library(argparser)#argument parser stuff
library(lubridate)#time stuff

#Code allows one to compare multiple CD's assuming they all get renewed with the same terms

#Gathers both the length of contract and interest rate
#Can also change starting value if you want more realistic projection 
parser <- arg_parser('CD Inputs')
parser <- add_argument(parser, '--start', help = 'initial CD deposit: default is 1000',nargs='*',default=1000)
parser <- add_argument(parser, '--len', help = 'length of CD contract: put d,m,y at end of number to indicate whether it is in days,months,years',nargs='*')
parser <- add_argument(parser, '--rate', help = 'CD interest rate',nargs='*')
arg <- parse_args(parser)

initial <- as.numeric(arg$start)

#Dataframe containing all of the CD values
arg$len <- c(unlist(strsplit(arg$len, ',')))
arg$rate <- c(unlist(strsplit(arg$rate, ',')))
if (length(arg$len) != length(arg$rate)){
	print_color(paste0('DID NOT INPUT SAME NUMBER OF TERM LENGTHS AND RATES\n'),'bred')
	break
}
arg$len <- gsub('y',' Years',arg$len)
arg$len <- gsub('m',' Months',arg$len)
arg$len <- gsub('d',' Days',arg$len)
labels <- c()
for (i in 1:length(arg$len)){
	label <- paste0(arg$len[i],' @',arg$rate[i],'%')
	labels <- c(labels,label)
}	
cd <- data.frame(Label = labels, Term.Length = arg$len, Term.Rate = as.numeric(arg$rate))
print(as_tibble(cd))

#Timeframe for CD simulation
startdate <- Sys.Date()
print_color(paste0('Beginning of analysis: ',startdate,'\n'),'bgreen')
enddate <- startdate + years(40)
print_color(paste0('End of analysis: ',enddate,'\n'),'bred')

#Define vectors that will store everything to be plotted
valuevec <- c()
timevec <- c()
labelvec <- c()

#Simulate CD growth
for (i in cd$Label){
	value <- initial
	term <- cd[cd$Label == i,]$Term.Length
	rate <- cd[cd$Label == i,]$Term.Rate
	time <- startdate
	#Run time steps
	while (time < enddate){
		valuevec <- c(valuevec, value)
		timevec <- c(timevec, time)
		labelvec <- c(labelvec, i)
		#Vary time steps based on term length
		if (grepl('Years', i)){
			t <- as.numeric(sub(' Years','',term))
			time <- time + years(t)
		}else if (grepl('Months', i)){
			t <- as.numeric(sub(' Months','',term))
			time <- time %m+% months(t)
		}else {
			t <- as.numeric(sub(' Days','',term))
			time <- time + days(t)
		}
		value <- value + value*(rate/100)
	}
}
print_color(paste0('====================CD Values====================\n'),'bcyan')
pldf <- data.frame(CD = labelvec, Time = timevec, CD.Value = valuevec)
print(pldf)

#Plot stuff
ggplot(data = pldf, aes(x = Time, y = CD.Value))+geom_point(aes(colour=CD))+geom_vline(xintercept=(startdate+years(10)))+geom_vline(xintercept=(startdate+years(20)))+geom_vline(xintercept=(startdate+years(30)))
ggplot(data = pldf, aes(x = Time, y = CD.Value))+geom_point(aes(shape=CD))+geom_vline(xintercept=(startdate+years(10)))+geom_vline(xintercept=(startdate+years(20)))+geom_vline(xintercept=(startdate+years(30)))#this plot is easier for someone who is colorblind to interpret
