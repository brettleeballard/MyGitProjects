#!/usr/bin/env Rscript
#Above line allows code to be run in terminal if desired using ./DataCleaner.R

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(insight)#print_color function
library(argparser)#argument parser stuff
library(hash)#used to emulate python dictionaries in R
library(lubridate)#used for datetime stuff
source('teams/Dictionary.R')

#Adding parsers to help with coding environment
parser <- arg_parser('Cleaning Data for Different Sports')
parser <- add_argument(parser, '--s', help = 'sports currently supported: default is NFL',nargs='*',default=c('NFL'))
arg <- parse_args(parser)

#Making sure the sports argparser is properly formatted for the loop below
arg$s <- strsplit(arg$s,',')[[1]]

#Will loop over each sport given so multiple can be run with the same command
for (sport in arg$s){
	if (sport == 'NFL'){
		years <- seq(from = 2000, to = 2025, by = 1)
	}

	#Get team names dictionary from teams/Dictionary.R
	teams <- TeamNames(sport)

	#Save datasets to a list for merge later
	datasets <- list()
	for (y in years){

		#Collect data and start cleaning
		df <- read.csv(paste0('DATA/',sport,'/',sport,'-',y,'.csv'))
		print_color(paste0('================================================================================\n'),'bold')
		print_color(paste0('================================PRE-CLEANED DATA================================\n'),'bold')
		print_color(paste0('================================================================================\n'),'bold')
		print(head(df))

		#Cleaning and formatting data based on the sport
		if (sport == 'NFL'){
			cols <- c('Season','Week','DateTime','Away','Away.Pts','Away.Yds','Away.TOs','Home','Home.Pts','Home.Yds','Home.TOs')
			df <- df %>%
				filter(!(Date %in% c('Date','Playoffs'))) %>%
				mutate(Season = y) %>%
				mutate(Week = replace(Week, Week == 'WildCard', 101)) %>%
				mutate(Week = replace(Week, Week == 'Division', 102)) %>%
				mutate(Week = replace(Week, Week == 'ConfChamp', 103)) %>%
				mutate(Week = replace(Week, Week == 'SuperBowl', 104)) %>%
				mutate(Week = as.numeric(Week)) %>%
				mutate(DateTime = paste0(Date,' ',Time)) %>%
				mutate(DateTime = parse_date_time(DateTime, '%Y-%m-%d %H:%M:%p')) %>%
				mutate(Away = ifelse(Unnamed..5 == '@',Winner.tie,Loser.tie)) %>% 
				mutate(Away.Yds = as.numeric(ifelse(Unnamed..5 == '@',YdsW,YdsL))) %>% 
				mutate(Away.TOs = as.numeric(ifelse(Unnamed..5 == '@',TOW,TOL))) %>% 
				mutate(Home = ifelse(Unnamed..5 == '@',Loser.tie,Winner.tie)) %>%
				mutate(Home.Yds = as.numeric(ifelse(Unnamed..5 == '@',YdsL,YdsW))) %>%
				mutate(Home.TOs = as.numeric(ifelse(Unnamed..5 == '@',TOL,TOW)))
		
			#Format seems to be different this year compared to past years
			if (y == 2025){
				df <- df %>%
					mutate(Away.Pts = as.numeric(ifelse(Unnamed..5 == '@',PtsW,PtsL))) %>% 
					mutate(Home.Pts = as.numeric(ifelse(Unnamed..5 == '@',PtsL,PtsW))) %>%
					select(all_of(cols)) 
			}else {
				df <- df %>%
					mutate(Away.Pts = as.numeric(ifelse(Unnamed..5 == '@',Pts,Pts.1))) %>% 
					mutate(Home.Pts = as.numeric(ifelse(Unnamed..5 == '@',Pts.1,Pts))) %>%
					select(all_of(cols)) 
			}

		}

		print_color(paste0('================================================================================\n'),'bgreen')
		print_color(paste0('==================================CLEANED DATA==================================\n'),'bgreen')
		print_color(paste0('================================================================================\n'),'bgreen')
		print(head(df))
		datasets <- append(datasets, list(df))
	}
	print_color(paste0('================================================================================\n'),'bviolet')
	print_color(paste0('==================================MERGED DATA===================================\n'),'bviolet')
	print_color(paste0('================================================================================\n'),'bviolet')
	#Merge data from all years into a single dataframe
	data <- Reduce(function(x,y) merge(x,y,all = TRUE), datasets)
	print(as_tibble(data))
	write.csv(data, paste0('DATA/',sport,'.csv'), row.names = FALSE)
}

