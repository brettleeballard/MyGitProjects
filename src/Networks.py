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
            self.Adf.loc[row['To']][row['From']] += row['Weight']
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
        print(self.Sdf)
        return self.Sdf

    #Defining convert to google matrix method
    def Google(self, alpha):
        self.Gdf = self.Sdf.copy()
        normvec = pd.Series(1/len(self.nodes), index = self.nodes)
        for series_name, series in self.Gdf.items():
            self.Gdf[series_name] = alpha * self.Gdf[series_name] + (1 - alpha) * normvec
        print(self.Gdf)
        return self.Gdf

    #Defining PageRank method to get rank for each team
    def PageRank(self, maxiter = 100):
        Gmat = self.Gdf.copy().to_numpy()
        niter = 0
        while niter < maxiter:
            temp = pd.Series(Gmat.dot(self.Rank.copy()), index = self.nodes)
            if temp.round(6).equals(self.Rank.round(6)):
                self.Rank == temp
                break
            else:
                self.Rank = temp
                niter += 1
        print('Number of iterations needed: '+str(niter))
        print(self.Rank)
        return self.Rank






