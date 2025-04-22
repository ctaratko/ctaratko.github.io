#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 16 22:48:30 2025

@author: christophertaratko
"""

# Homework bitches!!!!!!!

#"//*[@id="1411686949968"]/div/div/div[2]/div/table[1]/tbody/tr[3]/td[3]/a"

# "/html/body/div[1]/div/div/div[1]/div/div[2]/div/div[1]/div/div[2]/div/div/div/div[2]/div/table[1]/tbody/tr[3]/td[3]/a"
# "/html/body/div[1]/div/div/div[1]/div/div[2]/div/div[1]/div/div[2]/div/div/div/div[2]/div/table[1]/tbody/tr[4]/td[3]/a"

# tbody "/html/body/div[1]/div/div/div[1]/div/div[2]/div/div[1]/div/div[2]/div/div/div/div[2]/div/table[1]/tbody"
# %%
import time
import random
import pandas as pd
import os
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

options = Options()
options.add_argument("window-size=1400,1200")  # Set window size
options.add_argument('--disable-blink-features=AutomationControlled')  # Prevent detection of automation by disabling blink features
options.page_load_strategy = 'eager'
driver = webdriver.Chrome(options=options)

url = 'https://www.nyc.gov/site/finance/property/property-annualized-sales-update.page#'
driver.get(url)

table = driver.find_elements(By.TAG_NAME,"tbody")
driver.close()
driver.quit()

table

# %%

# =============================================================================
# *****************************************************************************
# I needed a lot of help with the first question and didn't end up understanding it 
# but I threw it into chatgpt to help me, and I understood the prep for the API & making a dataframe
# but taking the data and merging it with the fred data completely lost me, sorry for not completing this
# question on the homework
#
# I'm writing this at 7:58am on Friday, April 18th :skull: I'm all out of possibilities
# and any other work that I initially had was essentially too close to what the chatgpt writeup was
# *****************************************************************************
# =============================================================================

# import pandas as pd
# import requests

# # Load the metadata
# url = 'https://bcdanl.github.io/data/fred_api_series_housing_price.csv'
# series_housing_price = pd.read_csv(url)

# # FRED API key (replace with your own)
# api_key = 'YOUR_API_KEY'

# # Empty list to store dataframes
# all_data = []

# # Loop through each row of the metadata
# for i in range(len(series_housing_price)):
#     series_id = series_housing_price.loc[i, 'id']
#     title = series_housing_price.loc[i, 'title']
    
#     # Clean title to extract tier and city/state
#     title_clean = title.replace('Home Price Index (', '')
#     split_title = title_clean.split(' Tier) for ')
#     tier = split_title[0]
#     city_state = split_title[1]
#     city, state = city_state.rsplit(', ', 1)

#     # Construct API request
#     base_url = 'https://api.stlouisfed.org/fred/series/observations'
#     params = {
#         'series_id': series_id,
#         'api_key': api_key,
#         'file_type': 'json'
#     }
    
#     response = requests.get(base_url, params=params)
#     data = response.json()['observations']

#     # Convert to DataFrame
#     df = pd.DataFrame(data)
#     df = df[['date', 'value']]
#     df.rename(columns={'value': 'home_price_index'}, inplace=True)
    
#     # Convert value to numeric
#     df['home_price_index'] = pd.to_numeric(df['home_price_index'], errors='coerce')
    
#     # Add city, state, tier
#     df['city'] = city
#     df['state'] = state
#     df['tier'] = tier

#     # Add to list
#     all_data.append(df)

# # Combine all the results
# df_all = pd.concat(all_data, ignore_index=True)

# # Reorder columns
# df_all = df_all[['city', 'state', 'tier', 'date', 'home_price_index']]

# # Sorting
# df_all['tier'] = pd.Categorical(df_all['tier'], categories=['Low', 'Middle', 'High'], ordered=True)
# df_all['date'] = pd.to_datetime(df_all['date'])
# df_all = df_all.sort_values(['state', 'city', 'date', 'tier'], ascending=[True, True, False, True])

# # Reset index
# df_all.reset_index(drop=True, inplace=True)

# Display a preview
# =============================================================================
#  END OF CHAT GPT OUTPUT
# =============================================================================

# %%
import pandas as pd 
cms_ny_2022 = pd.read_csv('https://bcdanl.github.io/data/cms-2022-cities-all.zip')
# %%
Q3 = str(cms_ny_2022['Physician_or_NPP'])
type(Q3)

# %%
Q4 = (
      cms_ny_2022['Specialty_Detail']
      .fillna(cms_ny_2022['Specialty'])
      )
# %% 
# Q5
threshold = cms_ny_2022['Amount_of_Payment'].quantile(0.9)

top_10_percent_count = (cms_ny_2022['Amount_of_Payment'] > threshold).sum()
top_10_percent_count
# %%
# Q6 *******************************************************
# Q6 *******************************************************
# Q6 *******************************************************
# Q6 *******************************************************
city_specialty_counts = cms_ny_2022.dropna(subset=['Specialty_Detail']).groupby('City').size()
max_count = city_specialty_counts.max()
top_cities = city_specialty_counts[city_specialty_counts == max_count]
top_cities
# %%
Q7 = cms_ny_2022.groupby(['Taxonomy', 'Specialty_Detail']).size().reset_index(name='n')
print(Q7[Q7['Taxonomy'] == 'Dental Providers'])
# %%
Q8 = cms_ny_2022[cms_ny_2022['Taxonomy'] == 'Dental Providers']['Specialty_Detail'].nunique()
Q8
# %%
Q9 = cms_ny_2022[cms_ny_2022['Specialty'] == 'Dermatology']['Amount_of_Payment']
summary = Q9.describe()
missing = Q9.isna().sum()
print("Missing: ",missing)
print(summary)
# %%
Q10 = (
       cms_ny_2022.groupby(['City', 'Physician_or_NPP'])
       .size()
       .reset_index(name='count')
       )
total = (
    Q10
    .groupby('City')['count']
    .transform('sum')
    )
Q10['Total_Count'] = total
Q10['Percent'] = (Q10['count'] / total * 100).round(2)
grouped = Q10.sort_values(by=['Total_Count', 'City'], ascending=[False, True])
print(grouped)
# %%
Q11 = (
    cms_ny_2022
    .query("Product_Manufacturer == 'AbbVie Inc.' and City in ['Rochester', 'Buffalo', 'Syracuse']")
    )
total_PMT = Q11['Amount_of_Payment'].sum()
PMT_count = Q11.shape[0]
print({'Total_Amount_of_Payment': total_PMT, 'count': PMT_count})
# %%
cms_ny_2022_npi = pd.read_csv('https://bcdanl.github.io/data/cms-2022-cities-npi.csv')
cms_ny_2022_records = pd.read_csv('https://bcdanl.github.io/data/cms-2022-cities-records.csv')

cms_ny_2022 = pd.merge(cms_ny_2022_records, cms_ny_2022_npi,on='NPI',how='left') # I thought you needed brackets for the 2 dfs, or is that just for pd.concat?

# %%
Q13 = (
    cms_ny_2022
    .groupby(['City', 'Physician_or_NPP'])
    .size()
    .unstack(fill_value=0)
    .reset_index()
    )
print(Q13)





