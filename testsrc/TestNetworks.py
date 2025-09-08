#!/usr/bin/env python
#Above line allows code to be run in terminal if desired using ./TestNetworks.py

import pandas as pd
import numpy as np

#import Networks file from src
import sys
sys.path.append('/home/bballard/Personal/Projects/MyGitProjects/src')
import Networks as net

testnodes = ['A','B','C','D','E','F']
testedges = pd.read_csv('testedges.csv')

#Initialize adjancency matrix using class from Networks code
net.testfunct()
test = net.AdjMatrix(testnodes, testedges)
print(test.nodes)
print(test.edges)
print(test.Adf)

for week in testedges['Weeks'].unique():
    print('========================'+week+'========================')
    print(testedges[testedges['Weeks'] == week])
    newedges = testedges[testedges['Weeks'] == week]
    test.Update(edges = newedges)
    print('================Make Stochastic Matrix================')
    test.Stochastic()
    print('==================Make Google Matrix==================')
    test.Google(alpha = .85)
    print('====================Make Team Rank====================')
    test.PageRank()
    print('=====================End of '+week+'====================')

print('======================================================')
print('=====================FINAL RESULTS====================')
print('======================================================')

print('===================Adjancency Matrix==================')
print(test.Adf)
print('===================Stochastic Matrix==================')
print(test.Sdf)
print('=====================Google Matrix====================')
print(test.Gdf)
print('=======================Team Rank======================')
print(test.Rank)

print('======================================================')
print('=====================TEST TRANSPOSE===================')
print('======================================================')
tedges = testedges.rename(columns = {'From':'To', 'To':'From'})
print(tedges)
t = net.AdjMatrix(testnodes, tedges)
t.Update(edges = tedges)
t.Stochastic()
t.Google(alpha = .85)
t.PageRank()

print('======================================================')
print('====================COMPARE RATINGS===================')
print('======================================================')
print('Winner -> Loser')
testrank = test.Rank
testrating = testrank.apply(lambda x: -(x - np.mean(testrank))/np.std(testrank))
print(testrating.sort_values(ascending = False))
print('Loser -> Winner')
trank = t.Rank
trating = trank.apply(lambda x: (x - np.mean(trank))/np.std(trank))
print(trating.sort_values(ascending = False))
print('Combined Rating')
crating = .6 * testrating + .4 * trating
print(crating.sort_values(ascending = False))

print('======================================================')
print('=====================UNIT WEIGHTS=====================')
print('======================================================')
unitedges = testedges
unitedges['Weight'] = 1
print(unitedges)
unit = net.AdjMatrix(testnodes, unitedges)
unit.Update(edges = unitedges)
unit.Stochastic()
unit.Google(alpha = .85)
unit.PageRank()
unitrank = unit.Rank
unitrating = unitrank.apply(lambda x: -(x - np.mean(unitrank))/np.std(unitrank))
print(unitrating.sort_values(ascending = False))

print('======================================================')
print('=====================UNIT TRANSPOSE===================')
print('======================================================')
tunitedges = tedges
tunitedges['Weight'] = 1
print(tunitedges)
tunit = net.AdjMatrix(testnodes, tunitedges)
tunit.Update(edges = tunitedges)
tunit.Stochastic()
tunit.Google(alpha = .85)
tunit.PageRank()
tunitrank = tunit.Rank
tunitrating = tunitrank.apply(lambda x: (x - np.mean(tunitrank))/np.std(tunitrank))
print(tunitrating.sort_values(ascending = False))
cunitrating = .6 * unitrating + .4 * tunitrating
print(cunitrating.sort_values(ascending = False))

print('======================================================')
print('====================COMPARE RATINGS===================')
print('======================================================')
print('Winner -> Loser')
print(testrating.sort_values(ascending = False))
print('Loser -> Winner')
print(trating.sort_values(ascending = False))
print('Combined Rating')
print(crating.sort_values(ascending = False))
print('Unit: Winner -> Loser')
print(unitrating.sort_values(ascending = False))
print('Unit: Loser -> Winner')
print(tunitrating.sort_values(ascending = False))
print('Unit: Combined Rating')
print(cunitrating.sort_values(ascending = False))
