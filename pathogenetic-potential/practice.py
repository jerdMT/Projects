#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  4 10:19:09 2021

@author: jarodw
"""
#LSTM with Keras

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import math
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
k=28

data = pd.read_csv ("/Users/jarodw/Documents/pathogenetic-potential/DATA.csv")

df = pd.DataFrame(data, columns = ['strain', 'seq', 'clade'])

clades = pd.unique(df['clade'])
print(clades)

clade_count = df["clade"].value_counts()
clade_count.plot(kind='barh')

def Kmers_funct(seq, size):
    return [seq[x:x+size].lower() for x in range(len(seq) - size + 1)]

sentences = []
for j in range(len(df)) :
    sequence = str(df.loc[j, "seq"])
    sentence = ' '.join(Kmers_funct(sequence, size=k))
    sentences.append(sentence)
    
    
df['sentence'] = sentences    
    
from sklearn.feature_extraction.text import CountVectorizer
cv = CountVectorizer(ngram_range=(28,28))

print(df.head())
