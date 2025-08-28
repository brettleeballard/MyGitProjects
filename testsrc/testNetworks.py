#!/usr/bin/env python
#Above line allows code to be run in terminal if desired using ./testNetworks.py

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
