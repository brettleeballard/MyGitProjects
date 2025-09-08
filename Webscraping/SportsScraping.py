#!/usr/bin/env python
#Above line allows code to be run in terminal if desired using ./SportsScraping.py

import numpy as np
import pandas as pd
import requests
import argparse
import time
import random as rand

#Adding parsers to help with coding environment
parser = argparse.ArgumentParser(description = 'Cleaning Data For Different Sports With the Same Code')
parser.add_argument('--s',metavar ='SPORT',nargs = '*',choices = ['nfl'], default = ['nfl'],help = 'sports currently supported: nfl')
args = parser.parse_args()

headers = {'User-Agent': 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:142.0) Gecko/20100101 Firefox/142.0', 'Accept-Language': 'en-US,en;q=.5'}

for sport in args.s:
    #Change years for each sport
    if sport == 'nfl':
        years = [x for x in range(2000,2026)]
        print(years)

    #Start parsing the sports reference scores page
    tables = []
    for y in years:
        print(y)
        if sport == 'nfl':
            response = requests.get('https://www.pro-football-reference.com/years/'+str(y)+'/games.htm', headers = headers)
        
        #Save tables to be merged afterwards
        table = pd.read_html(response.text)
        tables.append(table[0])
        print(table[0])
        table[0].to_csv('../SportsRatings/DATA/NFL-'+str(y)+'.csv', index=False)
        time.sleep(rand.randint(60,120))#adding delay to limit number of requests per minute
