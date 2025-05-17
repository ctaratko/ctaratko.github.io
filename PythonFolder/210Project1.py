#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 14 11:28:28 2025

@author: christophertaratko
"""
# %%
import pandas as pd
url_2024 = "https://bcdanl.github.io/data/esg_proj_2024_data.csv"
esg_proj_2024_data = pd.read_csv(url_2024)

url_2025 = "https://bcdanl.github.io/data/esg_proj_2025.csv"
esg_proj_2025 = pd.read_csv(url_2025)

del url_2024 # new favorite thing to use, clean explorer
del url_2025

esg_proj_2025_filtered = esg_proj_2025[esg_proj_2025['Symbol'].isin(esg_proj_2024_data['Symbol'])].drop_duplicates().copy().reset_index(drop=True)
# someone cooked here, and that person's name.... is me :sunglasses emoji:
del esg_proj_2025 #I'm keeping this explorer looking absotootly beautiful
# %%
# Time Calc
#TotalTime = round(4 * 3139/60/60,2)
#NewTotalTime = round(4 * 625/60/60,2)
# Data collection comparison
#print(TotalTime,"hours. vs.",NewTotalTime,"hours.")
#del TotalTime
#del NewTotalTime
# %%
# =============================================================================
# Setup working directory
# =============================================================================
wd_path = 'PATHNAME_OF_YOUR_WORKING_DIRECTORY' 
os.chdir(wd_path)
del wd_path
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

df = pd.DataFrame()
for Ticker in esg_proj_2025_filtered['Symbol']:
    url = f'https://finance.yahoo.com/quote/{Ticker}/sustainability/'
    driver.get(url)
    time.sleep(random.uniform(3,5))
    Data = driver.find_elements(By.TAG_NAME,'h4')
    try:
        All_ESG = Data[0].text
    except:
        All_ESG = "NA"
    try:
        EnvRisk = Data[1].text
    except:
        EnvRisk = "NA"
    try:
        SocRisk = Data[2].text
    except:
        SocRisk = "NA"
    try:
        GovRisk = Data[3].text
    except:
        GovRisk = "NA"
    try:
        Controversy = driver.find_element(By.XPATH,"/html/body/div[2]/main/section/section/section/article/section[3]/section[1]/div/div[2]/span[1]").text
    except:
        Controversy = "NA"
    
    obs = [All_ESG, EnvRisk, SocRisk, GovRisk, Controversy]
    obs = pd.DataFrame([obs])
    df = pd.concat([df, obs], ignore_index= True)
driver.quit()
driver.close()
# %%
del All_ESG
del EnvRisk
del SocRisk
del Controversy
del Ticker
del obs
del url
del GovRisk
del Data
# %%
ColumnHeaders = esg_proj_2024_data.columns
Merge = pd.merge(esg_proj_2025_filtered, df, left_index=True, right_index=True)
Merge.columns = ColumnHeaders # COOKED RIGHT HERE, WHO DID THIS AND HAS TWO THUMBS? This guy
del ColumnHeaders # get this GARBAGE outta here (it was useful for the one line, too much clutter gang)

# %%

# Stock Data
url = "https://bcdanl.github.io/data/stock_history_2023.csv"
stock_history_2025 = pd.read_csv(url)

# %%
newDf = esg_proj_2025_filtered['Sector'].value_counts()
newDf = pd.DataFrame(newDf).reset_index()

Consumer = esg_proj_2025_filtered[esg_proj_2025_filtered['Sector'] == 'Consumer Discretionary']
ConsumerCo = Consumer['Symbol']


Industials = esg_proj_2025_filtered[esg_proj_2025_filtered['Sector'] == 'Industrials']
IndustialsCo =Industials['Symbol']

Finance = esg_proj_2025_filtered[esg_proj_2025_filtered['Sector'] == 'Finance']
FinanceCo = Finance['Symbol']

Technology = esg_proj_2025_filtered[esg_proj_2025_filtered['Sector'] == 'Health Care']
TechnologyCo = Technology['Symbol']

Health = esg_proj_2025_filtered[esg_proj_2025_filtered['Sector'] == 'Industrials']
HealthCo = Health['Symbol']

Technology = esg_proj_2025_filtered[esg_proj_2025_filtered['Sector'] == 'Technology']
TechnologyCo = Technology['Symbol']

Utilities = esg_proj_2025_filtered[esg_proj_2025_filtered['Sector'] == 'Utilities']
UtilitiesCo = Utilities['Symbol']

Estate = esg_proj_2025_filtered[esg_proj_2025_filtered['Sector'] == 'Real Estate']
EstateCo = Estate['Symbol']

Energy = esg_proj_2025_filtered[esg_proj_2025_filtered['Sector'] == 'Energy']
EnergyCo = Energy['Symbol']

Staples = esg_proj_2025_filtered[esg_proj_2025_filtered['Sector'] == 'Consumer Staples']
StaplesCo = Staples['Symbol']

Telecommunications = esg_proj_2025_filtered[esg_proj_2025_filtered['Sector'] == 'Telecommunications']
TelecommunicationsCo = Telecommunications['Symbol']

Materials = esg_proj_2025_filtered[esg_proj_2025_filtered['Sector'] == 'Basic Materials']
MaterialsCo = Materials['Symbol']

#	Sector
#0	Consumer Discretionary
#1	Industrials
#2	Finance
#3	Technology
#4	Health Care
#5	Utilities
#6	Real Estate
#7	Energy
#8	Consumer Staples
#9	Telecommunications
#10	Basic Materials

# %%

SandESG = pd.merge(Merge, stock_history_2025, on='Symbol', how = 'inner')
SandESG = SandESG.groupby('Sector')
SandESG['Change'] = 0 
SandESG['Close_Lag1'] = SandESG['Close'].shift(1)
SandESG['Change'] = SandESG['Close'] - SandESG['Close_Lag1']
Values = SandESG[['Symbol', 'Change']].groupby('Symbol').mean().sort_values('Change',ascending = False)
SandESG = pd.merge(Merge, Values, on='Symbol', how = 'inner')

# %%
import seaborn as sns
import matplotlib.pyplot as plt





# %%
Output = SandESG[['Sector','Change']].groupby('Sector').describe()
Output = pd.DataFrame(Output).reset_index()
EndVals = Output.sort_values(by=('Change', 'mean'), ascending=False).reset_index()












