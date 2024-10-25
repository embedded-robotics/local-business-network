# %%
import pickle
import pandas as pd
import numpy as np
from tqdm import tqdm
import datetime
from geopy.distance import geodesic

# %% [markdown]
# Reading all the brands

# %%
brands_visits = pd.read_csv('data/revision_visits_revenue_2019.csv')
brands_visits['brand_standard'] = brands_visits['brand'].apply(lambda x: x.strip().lower()) # For comparison with catalog.tsv
brands_visits['date'] = brands_visits['date'].apply(lambda x: datetime.datetime.strptime(x, '%Y-%m-%d').date())
brands_visits = brands_visits.rename(columns={'brand': 'brand_visitation'})
brands_visits.head()

# %% [markdown]
# Extracting all the unique brand placekeys

# %%
unique_brand_placekeys = brands_visits[['PLACEKEY', 'lat', 'lon']].groupby(['PLACEKEY'])[['lat', 'lon']].apply(lambda x: x.iloc[0]).reset_index()

# %%
# Function to calculate the distance between two coordinates
def calculate_distance_km(df_row):
    global lat_src
    global lon_src
    global placekey_src
    lat_dst = df_row['lat']
    lon_dst = df_row['lon']
    placekey_dst = df_row['PLACEKEY']
    
    dist_km = geodesic((lat_src, lon_src), (lat_dst, lon_dst)).km
    
    return pd.Series([placekey_src, placekey_dst, dist_km])

# %%
global lat_src
global lon_src
global placekey_src
neib_distance_km_df = pd.DataFrame(columns=['SRC_PLACEKEY', 'DST_PLACEKEY', 'Distance_Km'])

for i in tqdm(range(len(unique_brand_placekeys))):
    row_src = unique_brand_placekeys.iloc[i]
    placekey_src = row_src['PLACEKEY']
    lat_src = row_src['lat']
    lon_src = row_src['lon']

    temp_df = unique_brand_placekeys.apply(calculate_distance_km, axis=1).rename(columns={0:'SRC_PLACEKEY', 1: 'DST_PLACEKEY', 2: 'Distance_Km'})
        
    neib_distance_km_df = pd.concat([neib_distance_km_df, temp_df], axis=0).reset_index(drop=True)

# %%
with open('neib_distance_km.pkl', 'wb') as file:
    pickle.dump(neib_distance_km_df, file)

# %%



