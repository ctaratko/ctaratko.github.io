#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr  2 10:57:05 2025

@author: christophertaratko
"""



# %%
# =============================================================================
# Setup
# =============================================================================
import pandas as pd
import os
import time
import random
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions import NoSuchElementException # try / except

options = Options()
driver = webdriver.Chrome(options=options)

# =============================================================================
# Question 1
# =============================================================================
#url = 'https://www.imdb.com/search/title/?genres=family&sort=popularity,desc&count=200'
#driver.get(url)

# %%


# =============================================================================
# BROKEN, I put the Try: Except: statements in the wrong area :skull:
# =============================================================================



# import pandas as pd
# import os
# import time
# import random
# from selenium import webdriver
# from selenium.webdriver.common.by import By
# from selenium.webdriver.chrome.options import Options
# from selenium.common.exceptions import NoSuchElementException # try / except

# options = Options()
# options.add_argument("window-size=1400,1200")
# driver = webdriver.Chrome(options=options)

# url = 'https://www.imdb.com/search/title/?genres=family&sort=popularity,desc&count=200'
# driver.get(url)
# df = pd.DataFrame()
# df2 = pd.DataFrame()
# while True:
#     RankingTitle = driver.find_elements(By.CLASS_NAME,'ipc-title__text')
#     imdb_rating = driver.find_elements(By.CLASS_NAME,'ipc-rating-star--rating')
#     vote = driver.find_elements(By.CLASS_NAME,'ipc-rating-star--voteCount')
#     plot = driver.find_elements(By.CLASS_NAME,'ipc-html-content-inner-div')
#     for i in range(1,len(RankingTitle)):
#         try:
#             year        = f'/html/body/div[2]/main/div[2]/div[3]/section/section/div/section/section/div[2]/div/section/div[2]/div[2]/ul/li[{i}]/div/div/div/div[1]/div[2]/div[2]/span[1]'
#         except: 
#             year        = 'NA'
#         try:
#             runtime     = f'/html/body/div[2]/main/div[2]/div[3]/section/section/div/section/section/div[2]/div/section/div[2]/div[2]/ul/li[{i}]/div/div/div/div[1]/div[2]/div[2]/span[2]'
#         except:
#             runtime     = 'NA'
#         try:
#             rating      = f'/html/body/div[2]/main/div[2]/div[3]/section/section/div/section/section/div[2]/div/section/div[2]/div[2]/ul/li[{i}]/div/div/div/div[1]/div[2]/div[2]/span[3]'
#         except: 
#             rating      = 'NA'
#         try:
#             metaScore   = f'/html/body/div[2]/main/div[2]/div[3]/section/section/div/section/section/div[2]/div/section/div[2]/div[2]/ul/li[{i}]/div/div/div/div[1]/div[2]/div[2]/span[4]'
#         except:
#             metaScore   = 'NA'
#         yearVal = driver.find_element(By.XPATH,year).text
#         runtimeVal = driver.find_element(By.XPATH,runtime).text
#         ratingVal = driver.find_element(By.XPATH,rating).text
#         metaScoreVal = driver.find_element(By.XPATH,metaScore).text
#         RankingTitleVal = RankingTitle[i].text
#         imdbVal = imdb_rating[i].text 
#         voteVal = vote[i].text
#         plotVal = plot[i].text
#         obs = [RankingTitleVal, yearVal, runtimeVal, ratingVal, metaScoreVal,imdbVal, voteVal, plotVal]
#         obs = pd.DataFrame([obs])
#         df = pd.concat([df, obs], ignore_index= True)
#     break


#df[['ranking', 'title']] = df['RankingTitleVal'].str.split('.', n=1, expand=True)
#df = df.drop(columns=['ranking_title'])

#pd.merge(df, df2, how="left")


# %%

import pandas as pd
import os
import time
import random
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions import NoSuchElementException # try / except

options = Options()
options.add_argument("window-size=1400,1200")
driver = webdriver.Chrome(options=options)

url = 'https://www.imdb.com/search/title/?genres=family&sort=popularity,desc&count=200'
driver.get(url)
df = pd.DataFrame()
df2 = pd.DataFrame()


RankingTitle = driver.find_elements(By.CLASS_NAME,'ipc-title__text')
imdb_rating = driver.find_elements(By.CLASS_NAME,'ipc-rating-star--rating')
vote = driver.find_elements(By.CLASS_NAME,'ipc-rating-star--voteCount')
plot = driver.find_elements(By.CLASS_NAME,'ipc-html-content-inner-div')
    
for i in range(1, len(RankingTitle)):
    year_xpath = f'/html/body/div[2]/main/div[2]/div[3]/section/section/div/section/section/div[2]/div/section/div[2]/div[2]/ul/li[{i}]/div/div/div/div[1]/div[2]/div[2]/span[1]'
    runtime_xpath = f'/html/body/div[2]/main/div[2]/div[3]/section/section/div/section/section/div[2]/div/section/div[2]/div[2]/ul/li[{i}]/div/div/div/div[1]/div[2]/div[2]/span[2]'
    rating_xpath = f'/html/body/div[2]/main/div[2]/div[3]/section/section/div/section/section/div[2]/div/section/div[2]/div[2]/ul/li[{i}]/div/div/div/div[1]/div[2]/div[2]/span[3]'
    metaScore_xpath = f'/html/body/div[2]/main/div[2]/div[3]/section/section/div/section/section/div[2]/div/section/div[2]/div[2]/ul/li[{i}]/div/div/div/div[1]/div[2]/div[2]/span[4]'
    try:
        yearVal = driver.find_element(By.XPATH, year_xpath).text
    except:
        yearVal = 'NA'
    try:
        runtimeVal = driver.find_element(By.XPATH, runtime_xpath).text
    except:
        runtimeVal = 'NA'
    try:
        ratingVal = driver.find_element(By.XPATH, rating_xpath).text
    except:
        ratingVal = 'NA'
    try:
        metaScoreVal = driver.find_element(By.XPATH, metaScore_xpath).text
    except:
        metaScoreVal = 'NA' # These were the only ones that needed the try & except statements ^^^

    try:
        RankingTitleVal = RankingTitle[i].text
    except:
        RankingTitleVal = 'NA'
    try:
        imdbVal = imdb_rating[i].text 
    except:
        imdbVal = 'NA'
    try:
        voteVal = vote[i].text
    except:
        voteVal = 'NA'
    try:
        plotVal = plot[i].text
    except:
        plotVal = 'NA'

    obs = [RankingTitleVal, yearVal, runtimeVal, ratingVal, metaScoreVal, imdbVal, voteVal, plotVal]
    obs = pd.DataFrame([obs])
    df = pd.concat([df, obs], ignore_index=True)


# %%
# =============================================================================
# Remaining Questions
# =============================================================================
# Setup
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt # also probably won't need (because we haven't done any graphing)
import seaborn as sns # probably won't need

spotify = pd.read_csv('https://bcdanl.github.io/data/spotify_all.csv')


# Q2

spotify['track_name'].value_counts(ascending = False).head(5)

# Q3

S_Counter = spotify['track_name'].value_counts(ascending = False)
S_Counter.head(int(round(S_Counter.nunique()*(5/100),0))) # I'm so goated, there's definitely an easier method, but I came up with that on my own, zero references

# Q4

Q4 = spotify[['artist_name', 'track_name']]
Q4 = Q4.groupby('artist_name').nunique().sort_values('track_name', ascending = False).query('track_name > 50')
Q4 # Doesn't include 50 because it asks for 'more than', rather than '50 or more' or 'greater than or equal to 50'

# Q5

Q5 = spotify.query('track_name == "One Dance" and artist_name == "Drake"')
Q5['album_name'].value_counts()

# Q6

Q6_max = spotify[['artist_name','track_name','duration_ms']].groupby('artist_name').max().sort_values('duration_ms', ascending = False)
Q6_min = spotify[['artist_name','track_name','duration_ms']].groupby('artist_name').min().sort_values('duration_ms', ascending = False)
Q6_max
MergeTable = pd.merge(Q6_max, Q6_min, on = 'artist_name', suffixes = ('_max', '_min'))
MergeTable

# Q7

spotify.groupby(['playlist_name', 'track_name']).value_counts(ascending = False) # First Attempt
spotify.groupby(['playlist_name', 'track_name']).size().sort_values(ascending = False) # Had to get some help on this, don't entirely understand the size() func, but it works

# Q8

# Filter for songs that appear in more than 100 different albums
filtered_songs = spotify['track_name'].value_counts()[spotify['track_name'].value_counts() > 100].index
filtered_songs
# Getting the number of different album_name's each track is on
Q8 = spotify[spotify['track_name'].isin(filtered_songs)].groupby('track_name')['playlist_name'].nunique().sort_values(ascending = False)
Q8 = pd.DataFrame(Q8)
Q8.query('playlist_name > 100')
# HELL YEAH :eagle: :eagle: :eagle: :eagle: (I kept using 'album name this whole time and it threw off my entire code, finally ∆'d to playlist name)

 # %%













