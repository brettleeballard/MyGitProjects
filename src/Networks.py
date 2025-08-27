#!/usr/bin/env python
#Above line allows code to be run in terminal if desired using ./Networks.py

#Can be imported using below
#import sys
#sys.path.append('/home/bballard/Personal/Projects/MyGitProjects/src')
#import Networks as net

import pandas as pd
import numpy as np

#Test function to make sure import is successful
def testfunct():
    print('Test Success')

#Defining Adjancency Matrix class
class AdjMatrix:
    
    #Defining attributes
    def __init__(self, nodes, edges):
        #Nodes are a list
        self.nodes = nodes
        #Edges are a dataframe
        self.edges = edges
        #Define adjancency matrix
        adjmat = np.zeros((len(nodes),len(nodes)))
        self.Adf = pd.DataFrame(adjmat, columns=nodes, index=nodes)
        self.Sdf = self.Adf.copy()
        self.Gdf = self.Adf.copy()
        self.Rank = pd.Series(1/len(self.nodes), index = self.nodes)
    
    #Defining Update method
    def Update(self, edges):
        for index, row in edges.iterrows():
            self.Adf.loc[row['To']][row['From']] = self.Adf.loc[row['To']][row['From']] + row['Weight']
            print(self.Adf)
        return self.Adf

    #Defining convert to stochastic matrix method
    def Stochastic(self):
        self.Sdf = self.Adf.copy()
        for series_name, series in self.Sdf.items():
            sum(series)
            if (sum(series) == 0):
                self.Sdf[series_name] = 1/len(self.nodes)
            else:
                self.Sdf[series_name] = self.Sdf[series_name].apply(lambda x: x/sum(series))
        return self.Sdf

    #Defining convert to google matrix method
    def Google(self, alpha):
        self.Gdf = self.Sdf.copy()
        normvec = pd.Series(1/len(self.nodes), index = self.nodes)
        for series_name, series in self.Gdf.items():
            self.Gdf[series_name] = alpha * self.Gdf[series_name] + (1 - alpha) * normvec
        return self.Gdf

    #Defining PageRank method to get rank for each team
    def PageRank(self):
        Gmat = self.Gdf.copy().to_numpy()
        self.Rank = Gmat.dot(self.Rank)
        return self.Rank






