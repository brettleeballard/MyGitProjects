#!/usr/bin/env Rscript
#Above line allows code to be run in terminal if desired using ./Dictionary.R

#Libraries and what they are used for commented next to them
library(hash)#used to emulate python dictionaries in R

#Defining team dictionaries
TeamNames <- function(sport){
	if (sport == 'NFL'){
		#Going to create a dictionary of franchise names since 2000 and a team shortened code that reflects current name
		teams <- hash()
		teams[['ARI']] <- c('Arizona Cardinals')
		teams[['ATL']] <- c('Atlanta Falcons')
		teams[['BAL']] <- c('Baltimore Ravens')
		teams[['BUF']] <- c('Buffalo Bills')
		teams[['CAR']] <- c('Carolina Panthers')
		teams[['CHI']] <- c('Chicago Bears')
		teams[['CIN']] <- c('Cincinati Bengals')
		teams[['DAL']] <- c('Dallas Cowboys')
		teams[['DEN']] <- c('Denver Broncos')
		teams[['DET']] <- c('Detroit Lions')
		teams[['GB']] <- c('Green Bay Packers')
		teams[['HOU']] <- c('Houston Texans')
		teams[['IND']] <- c('Indianapolis Colts')
		teams[['JAC']] <- c('Jacksonville Jaguars')
		teams[['KC']] <- c('Kansas City Chiefs')
		teams[['LV']] <- c('Oakland Raiders', 'Las Vegas Raiders')
		teams[['LAC']] <- c('San Diego Chargers', 'Los Angeles Chargers')
		teams[['LAR']] <- c('St. Louis Rams', 'Los Angeles Rams')
		teams[['MIA']] <- c('Miami Dolphins')
		teams[['MIN']] <- c('Minnesota Vikings')
		teams[['NE']] <- c('New England Patriots')
		teams[['NO']] <- c('New Orleans Saints')
		teams[['NYG']] <- c('New York Giants')
		teams[['NYJ']] <- c('New York Jets')
		teams[['PHI']] <- c('Philadelphia Eagles')
		teams[['PIT']] <- c('Pittsburgh Steelers')
		teams[['SF']] <- c('San Francisco 49ers')
		teams[['SEA']] <- c('Seattle Seahawks')
		teams[['TB']] <- c('Tampa Bay Buccaneers')
		teams[['TEN']] <- c('Tennessee Titans')
		teams[['WAS']] <- c('Washington Redskins', 'Washington Football Team', 'Washington Commanders')
	}
	return(teams)
}
