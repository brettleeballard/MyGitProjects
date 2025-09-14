#!/usr/bin/env python
#Above line allows code to be run in terminal if desired using ./SportsScraping.py

import numpy as np
import pandas as pd
import requests
import argparse
import time
import random as rand

#Adding parsers to help with coding environment
parser = argparse.ArgumentParser(description = 'Collecting Data For Different Sports With the Same Code')
parser.add_argument('--s',metavar ='SPORT',nargs = '*',choices = ['NFL'], default = ['NFL'],help = 'sports currently supported: NFL')
args = parser.parse_args()

headers = {'User-Agent': 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:142.0) Gecko/20100101 Firefox/142.0', 'Accept-Language': 'en-US,en;q=.5'}

for sport in args.s:
    #Change years for each sport
    if sport == 'NFL':
        years = [x for x in range(2025,2026)]#only updating current year right now
        print(years)

    #Start parsing the sports reference scores page
    tables = []
    for y in years:
        print(y)
        if sport == 'NFL':
            response = requests.get('https://www.pro-football-reference.com/years/'+str(y)+'/games.htm', headers = headers)
        
        #Save tables to be merged afterwards
        table = pd.read_html(response.text)
        tables.append(table[0])
        print(table[0])
        table[0].to_csv('../SportsRatings/DATA/'+sport+'/'+sport+'-'+str(y)+'.csv', index=False)
        time.sleep(rand.randint(60,120))#adding delay to limit number of requests per minute
