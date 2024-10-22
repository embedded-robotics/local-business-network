# %%
import pickle
import pandas as pd
import numpy as np
from tqdm import tqdm
import datetime
from geopy.distance import geodesic

# %% [markdown]
# Reading all the focal brands

# %%
with open('top_brands.pickle', 'rb') as file:
    focal_brands = pickle.load(file)

# %% [markdown]
# Reading the visitation data for all brands

# %%
brands_visits = pd.read_csv('data/revision_visits_revenue_2019.csv')
brands_visits['brand_standard'] = brands_visits['brand'].apply(lambda x: x.strip().lower()) # For comparison with catalog.tsv
brands_visits['date'] = brands_visits['date'].apply(lambda x: datetime.datetime.strptime(x, '%Y-%m-%d').date())
brands_visits = brands_visits.rename(columns={'brand': 'brand_visitation'})
brands_visits.head()

# %% [markdown]
# Reading the data for all brands having local reviews

# %%
with open('brand_visit_local_reviews.pickle', 'rb') as file:
    brand_visit_local_reviews = pickle.load(file)

brand_visit_local_reviews.head()
brand_visit_local_reviews_list = brand_visit_local_reviews['brand_visitation'].unique().tolist()

# %% [markdown]
# Reading Spatial Distance

# %%
distance_results = pd.read_csv('data/distance_results.csv')

# %% [markdown]
# Reading Travel Time

# %%
with open('data/travel_time.pkl', 'rb') as file:
    travel_time_dict = pickle.load(file)
    
travel_time_keys = list(travel_time_dict.keys())
from_keys = [key[0] for key in travel_time_keys]
to_keys = [key[1] for key in travel_time_keys]
time_minutes = list(travel_time_dict.values())
time_minutes = [int(time_inst.split(' ')[0]) for time_inst in time_minutes]

travel_time = pd.DataFrame({'From_PLACEKEY': from_keys, 'To_PLACEKEY': to_keys, 'Time_mins': time_minutes})

# %% [markdown]
# Functions to calculate first neighbor metrics

# %%
def calculate_first_neib_mean_reviews_visits(group_df):
    
    global foc_store_first_degree_neibs
    global foc_store_time
    global foc_store
    
    inv_visits = 0
    inv_visits_exp = 0
    
    num_reviews_fb_neibmean = 0
    num_reviews_ig_neibmean = 0
    num_reviews_tw_neibmean = 0

    num_reviews_fb_neibmean_exp = 0
    num_reviews_ig_neibmean_exp = 0
    num_reviews_tw_neibmean_exp = 0
    
    
    for neib_store in foc_store_first_degree_neibs:
        neib_store_time = foc_store_time[(foc_store_time['From_PLACEKEY'] == foc_store) & (foc_store_time['To_PLACEKEY'] == neib_store)]['Time_mins'].values[0]
        
        fb_reviews = group_df[group_df['PLACEKEY'] == neib_store]['localized_fb_reviews_60_days']
        if (len(fb_reviews) != 0) and (np.isnan(fb_reviews.values[0]) != True):
            num_reviews_fb_neibmean += (1/neib_store_time) * fb_reviews.values[0]
            num_reviews_fb_neibmean_exp += (1/np.exp(neib_store_time)) * fb_reviews.values[0]
        
        ig_reviews = group_df[group_df['PLACEKEY'] == neib_store]['localized_ig_reviews_60_days']
        if (len(ig_reviews) != 0) and (np.isnan(ig_reviews.values[0]) != True):
            num_reviews_ig_neibmean += (1/neib_store_time) * ig_reviews.values[0]
            num_reviews_ig_neibmean_exp += (1/np.exp(neib_store_time)) * ig_reviews.values[0]
        
        tw_reviews = group_df[group_df['PLACEKEY'] == neib_store]['localized_tw_reviews_60_days']
        if (len(tw_reviews) != 0) and (np.isnan(tw_reviews.values[0]) != True):
            num_reviews_tw_neibmean += (1/neib_store_time) * tw_reviews.values[0]
            num_reviews_tw_neibmean_exp += (1/np.exp(neib_store_time)) * tw_reviews.values[0]
        
        visits = group_df[group_df['PLACEKEY'] == neib_store]['visits_by_day']
        if (len(visits) != 0) and (np.isnan(visits.values[0]) != True):
            inv_visits += (1/neib_store_time) * visits.values[0]
            inv_visits_exp += (1/np.exp(neib_store_time)) * visits.values[0]
    
    return pd.Series([foc_store, inv_visits, num_reviews_fb_neibmean, num_reviews_ig_neibmean, num_reviews_tw_neibmean,
                      inv_visits_exp, num_reviews_fb_neibmean_exp, num_reviews_ig_neibmean_exp, num_reviews_tw_neibmean_exp])

# %%
def calculate_first_neib_visits(group_df):
    
    global foc_store_first_degree_neibs
    global foc_store_time
    global foc_store
    
    inv_visits = 0
    inv_visits_exp = 0
    
    num_reviews_fb_neibmean = 0
    num_reviews_ig_neibmean = 0
    num_reviews_tw_neibmean = 0

    num_reviews_fb_neibmean_exp = 0
    num_reviews_ig_neibmean_exp = 0
    num_reviews_tw_neibmean_exp = 0
    
    
    for neib_store in foc_store_first_degree_neibs:
        neib_store_time = foc_store_time[(foc_store_time['From_PLACEKEY'] == foc_store) & (foc_store_time['To_PLACEKEY'] == neib_store)]['Time_mins'].values[0]
                
        visits = group_df[group_df['PLACEKEY'] == neib_store]['visits_by_day']
        if (len(visits) != 0) and (np.isnan(visits.values[0]) != True):
            inv_visits += (1/neib_store_time) * visits.values[0]
            inv_visits_exp += (1/np.exp(neib_store_time)) * visits.values[0]
    
    return pd.Series([foc_store, inv_visits, num_reviews_fb_neibmean, num_reviews_ig_neibmean, num_reviews_tw_neibmean,
                      inv_visits_exp, num_reviews_fb_neibmean_exp, num_reviews_ig_neibmean_exp, num_reviews_tw_neibmean_exp])

# %% [markdown]
# Functions to calculate second degree neighbor metrics

# %%
# Function to calculate the distance between two coordinates
def calculate_distance_km(df_row):
    global lat_src
    global lon_src
    global first_deg_neib
    
    lat_dst = df_row['lat']
    lon_dst = df_row['lon']
    placekey_dst = df_row['PLACEKEY']
    dst_brand = df_row['brand_visitation']
    
    dist_km = geodesic((lat_src, lon_src), (lat_dst, lon_dst)).km
    
    return pd.Series([first_deg_neib, placekey_dst, dst_brand, dist_km])

# %%
def calculate_second_neib_visits(group_df):
    
    global first_deg_stores
    global foc_store_distance
    global second_neib_df
    global foc_store
    
    inv_visits_secondneibmean = 0
    inv_visits_secondneibmean_exp = 0
    
    for first_deg_store in first_deg_stores:
        inv_visits = 0
        inv_visits_exp = 0
        first_deg_store_dist = foc_store_distance[(foc_store_distance['From_PLACEKEY'] == foc_store) & (foc_store_distance['To_PLACEKEY'] == first_deg_store)]['Distance_km'].values[0]
        second_deg_neighbors = second_neib_df[second_neib_df['SRC_PLACEKEY'] == first_deg_store][['DST_PLACEKEY', 'Distance_Km']]
        
        for _, row in second_deg_neighbors.iterrows():
            
            second_neib_store = row['DST_PLACEKEY']
            second_neib_store_dist = row['Distance_Km']
                
            visits = group_df[group_df['PLACEKEY'] == second_neib_store]['visits_by_day']
            if (len(visits) != 0) and (np.isnan(visits.values[0]) != True):
                inv_visits += (1/second_neib_store_dist) * visits.values[0]
                inv_visits_exp += (1/np.exp(second_neib_store_dist)) * visits.values[0]
        
        inv_visits_secondneibmean += (1/first_deg_store_dist) * inv_visits
        inv_visits_secondneibmean_exp += (1/np.exp(first_deg_store_dist)) * inv_visits_exp
    
    return pd.Series([foc_store, inv_visits_secondneibmean, inv_visits_secondneibmean_exp])

# %%
def calculate_second_neib_mean_reviews(group_df):

    global first_deg_stores_local_reviews
    global foc_store_distance
    global second_neib_df_local_reviews
    global foc_store
    
    num_reviews_fb_secondneibmean = 0
    num_reviews_ig_secondneibmean = 0
    num_reviews_tw_secondneibmean = 0
    num_reviews_fb_secondneibmean_exp = 0
    num_reviews_ig_secondneibmean_exp = 0
    num_reviews_tw_secondneibmean_exp = 0
    
    for first_deg_store in first_deg_stores_local_reviews:
        
        num_reviews_fb_neibmean = 0
        num_reviews_ig_neibmean = 0
        num_reviews_tw_neibmean = 0
        num_reviews_fb_neibmean_exp = 0
        num_reviews_ig_neibmean_exp = 0
        num_reviews_tw_neibmean_exp = 0
        
        first_deg_store_dist = foc_store_distance[(foc_store_distance['From_PLACEKEY'] == foc_store) & (foc_store_distance['To_PLACEKEY'] == first_deg_store)]['Distance_km'].values[0]
        second_deg_neighbors = second_neib_df_local_reviews[second_neib_df_local_reviews['SRC_PLACEKEY'] == first_deg_store][['DST_PLACEKEY', 'Distance_Km']]
        
        for _, row in second_deg_neighbors.iterrows():
            
            second_neib_store = row['DST_PLACEKEY']
            second_neib_store_dist = row['Distance_Km']
            
            fb_reviews = group_df[group_df['PLACEKEY'] == second_neib_store]['localized_fb_reviews_60_days']
            if (len(fb_reviews) != 0) and (np.isnan(fb_reviews.values[0]) != True):
                num_reviews_fb_neibmean += (1/second_neib_store_dist) * fb_reviews.values[0]
                num_reviews_fb_neibmean_exp += (1/np.exp(second_neib_store_dist)) * fb_reviews.values[0]
            
            ig_reviews = group_df[group_df['PLACEKEY'] == second_neib_store]['localized_ig_reviews_60_days']
            if (len(ig_reviews) != 0) and (np.isnan(ig_reviews.values[0]) != True):
                num_reviews_ig_neibmean += (1/second_neib_store_dist) * ig_reviews.values[0]
                num_reviews_ig_neibmean_exp += (1/np.exp(second_neib_store_dist)) * ig_reviews.values[0]
            
            tw_reviews = group_df[group_df['PLACEKEY'] == second_neib_store]['localized_tw_reviews_60_days']
            if (len(tw_reviews) != 0) and (np.isnan(tw_reviews.values[0]) != True):
                num_reviews_tw_neibmean += (1/second_neib_store_dist) * tw_reviews.values[0]
                num_reviews_tw_neibmean_exp += (1/np.exp(second_neib_store_dist)) * tw_reviews.values[0]
        
        num_reviews_fb_secondneibmean += (1/first_deg_store_dist) * num_reviews_fb_neibmean
        num_reviews_ig_secondneibmean += (1/first_deg_store_dist) * num_reviews_ig_neibmean
        num_reviews_tw_secondneibmean += (1/first_deg_store_dist) * num_reviews_tw_neibmean
        
        num_reviews_fb_secondneibmean_exp += (1/np.exp(first_deg_store_dist)) * num_reviews_fb_neibmean_exp
        num_reviews_ig_secondneibmean_exp += (1/np.exp(first_deg_store_dist)) * num_reviews_ig_neibmean_exp
        num_reviews_tw_secondneibmean_exp += (1/np.exp(first_deg_store_dist)) * num_reviews_tw_neibmean_exp
    
    return pd.Series([foc_store, num_reviews_fb_secondneibmean, num_reviews_ig_secondneibmean, num_reviews_tw_secondneibmean, 
                      num_reviews_fb_secondneibmean_exp, num_reviews_ig_secondneibmean_exp, num_reviews_tw_secondneibmean_exp])

# %% [markdown]
# Reading the focal brands list

# %%
focal_brands_list = focal_brands['BRANDS'].tolist()

# %%
# Change this line to for loop to change the focal brand
foc_brand = focal_brands_list[0]
store_keys_foc_brand = brand_visit_local_reviews[brand_visit_local_reviews['brand_visitation'] == foc_brand]['PLACEKEY'].unique().tolist()
all_neib_placekey = distance_results[distance_results['From_PLACEKEY'].isin(store_keys_foc_brand)]['To_PLACEKEY'].unique().tolist()
unique_neib_brands_foc = brands_visits[brands_visits['PLACEKEY'].isin(all_neib_placekey)]['brand_visitation'].unique().tolist()

# %%
# Change this line to for loop for each Neighboring brand of the specific focal brand
unique_neib = unique_neib_brands_foc[0]
unique_neib_placekeys = brands_visits[brands_visits['brand_visitation'] == unique_neib]['PLACEKEY'].unique().tolist()
focal_stores_first_degree_neib = distance_results[(distance_results['From_PLACEKEY'].isin(store_keys_foc_brand)) &
                                                (distance_results['To_PLACEKEY'].isin(unique_neib_placekeys)) &
                                                (distance_results['Distance_km']<=16.0934)]
store_keys_foc_brand = focal_stores_first_degree_neib['From_PLACEKEY'].unique().tolist()

# %%
# Change this line to for loop for each store of focal brand
focal_store_information_final = None

for i in tqdm(range(len(store_keys_foc_brand))):
    foc_store = store_keys_foc_brand[i]
    # Calculating all the brands for the stores which can be categorized as the first degreen neighbors. All the stores of such brands need to be excluded from second degree neighbor calculation
    foc_store_all_first_degree_neibs = distance_results[(distance_results['From_PLACEKEY'] == foc_store) & (distance_results['Distance_km'] <= 16.0934)]['To_PLACEKEY'].to_list()
    foc_store_all_first_degree_neibs_brands = brands_visits[brands_visits['PLACEKEY'].isin(foc_store_all_first_degree_neibs)]['brand_visitation'].unique().tolist()
    
    foc_store_first_degree_neibs = focal_stores_first_degree_neib[focal_stores_first_degree_neib['From_PLACEKEY'] == foc_store]['To_PLACEKEY'].to_list()
    foc_store_distance = distance_results[(distance_results['From_PLACEKEY'] == foc_store) & (distance_results['To_PLACEKEY'].isin(foc_store_first_degree_neibs))]
    foc_store_time = travel_time[(travel_time['From_PLACEKEY'] == foc_store) & (travel_time['To_PLACEKEY'].isin(foc_store_first_degree_neibs))]

    # Calculating distance metrics
    spatial_distance_avg = np.average(foc_store_distance['Distance_km'].to_list())
    travel_distance_avg = np.average(foc_store_time['Time_mins'].to_list())

    # Calculating first neighbor metrics
    if unique_neib in brand_visit_local_reviews_list:
        first_neib_metrics = brand_visit_local_reviews[brand_visit_local_reviews['PLACEKEY'].isin(foc_store_first_degree_neibs)][['date', 'PLACEKEY', 'visits_by_day','localized_fb_reviews_60_days','localized_ig_reviews_60_days', 
                                                                                                                                    'localized_tw_reviews_60_days']].groupby('date').apply(calculate_first_neib_mean_reviews_visits)
    else:
        first_neib_metrics = brands_visits[brands_visits['PLACEKEY'].isin(foc_store_first_degree_neibs)][['date', 'PLACEKEY', 'visits_by_day']].groupby('date').apply(calculate_first_neib_visits)

    first_neib_metrics = first_neib_metrics.rename(columns={0:'focal_store', 1:'inv_visits', 2: 'num_reviews_fb_neibmean', 3:'num_reviews_ig_neibmean', 4:'num_reviews_tw_neibmean',
                                                            5:'inv_visits_exp', 6:'num_reviews_fb_neibmean_exp', 7: 'num_reviews_ig_neibmean_exp', 8: 'num_reviews_tw_neibmean_exp'})

    # Calculating second neighbor metrics
    unique_brand_placekeys = brands_visits[['PLACEKEY', 'lat', 'lon', 'brand_visitation']].groupby(['PLACEKEY'])[['lat', 'lon','brand_visitation']].apply(lambda x: x.iloc[0]).reset_index()

    second_neib_df = None

    for i in range(len(foc_store_first_degree_neibs)):
        first_deg_neib = foc_store_first_degree_neibs[i]
        lat_src, lon_src = unique_brand_placekeys[unique_brand_placekeys['PLACEKEY'] == first_deg_neib][['lat', 'lon']].values[0]
        temp_df = unique_brand_placekeys.apply(calculate_distance_km, axis=1).rename(columns={0:'SRC_PLACEKEY', 1: 'DST_PLACEKEY', 2:'DST_BRAND', 3: 'Distance_Km'})
        
        if i == 0:
            second_neib_df = temp_df
        else:
            second_neib_df = pd.concat([second_neib_df, temp_df], axis=0)

    second_neib_df = second_neib_df[(~second_neib_df['DST_BRAND'].isin(foc_store_all_first_degree_neibs_brands)) & (second_neib_df['DST_BRAND'] != unique_neib) & (second_neib_df['Distance_Km']<=16.0934)]
    second_neib_df_local_reviews = second_neib_df[second_neib_df['DST_BRAND'].isin(brand_visit_local_reviews_list)]

    first_deg_stores = second_neib_df['SRC_PLACEKEY'].unique().tolist()
    second_deg_stores = second_neib_df['DST_PLACEKEY'].unique().tolist()

    first_deg_stores_local_reviews = second_neib_df_local_reviews['SRC_PLACEKEY'].unique().tolist()
    second_deg_stores_local_reviews = second_neib_df_local_reviews['DST_PLACEKEY'].unique().tolist()

    second_neib_metrics_visits = brands_visits[brands_visits['PLACEKEY'].isin(second_deg_stores)][['date', 'PLACEKEY', 'visits_by_day']].groupby('date').apply(calculate_second_neib_visits)
    second_neib_metrics_visits = second_neib_metrics_visits.rename(columns={0: 'focal_store', 1:'inv_visits_secondneibmean', 2:'inv_visits_secondneibmean_exp'}).reset_index()

    second_neib_metrics_local_reviews = brand_visit_local_reviews[brand_visit_local_reviews['PLACEKEY'].isin(second_deg_stores_local_reviews)][['date', 'PLACEKEY', 'localized_fb_reviews_60_days','localized_ig_reviews_60_days', 
                                                                                                'localized_tw_reviews_60_days']].groupby('date').apply(calculate_second_neib_mean_reviews)

    second_neib_metrics_local_reviews = second_neib_metrics_local_reviews.rename(columns={0: 'focal_store', 1:'num_reviews_fb_secondneibmean', 2:'num_reviews_ig_secondneibmean',
                                                                                        3: 'num_reviews_tw_secondneibmean', 4: 'num_reviews_fb_secondneibmean_exp',
                                                                                        5: 'num_reviews_ig_secondneibmean_exp', 6: 'num_reviews_tw_secondneibmean_exp'}).reset_index()

    second_neib_metrics = pd.merge(left=second_neib_metrics_visits, right=second_neib_metrics_local_reviews, how='inner', on=['date', 'focal_store'])
    
    # Extracting Local Review Information for the focal store
    focal_store_information = brand_visit_local_reviews[brand_visit_local_reviews['PLACEKEY'] == foc_store][['date', 'PLACEKEY', 'brand_visitation', 'visits_by_day', 'visits_past_60_days',
                                                                                                             'localized_fb_reviews_60_days', 'localized_ig_reviews_60_days', 'localized_tw_reviews_60_days']]
    # Combining distance metrics with the focal store info
    focal_store_information['spatial_distance_km'] = spatial_distance_avg
    focal_store_information['travel_distance_min'] = travel_distance_avg

    # Combining first and second degree neighbor metrics with the focal store info
    focal_store_information = pd.merge(left=focal_store_information, right=first_neib_metrics, how='inner', on=['date']).drop('focal_store', axis=1)
    focal_store_information = pd.merge(left=focal_store_information, right=second_neib_metrics, how='inner', on=['date']).drop('focal_store', axis=1)
    focal_store_information = focal_store_information.sort_values('date').fillna(0)
    
    if i == 0:
        focal_store_information_final = focal_store_information
    else:
        focal_store_information_final = pd.concat([focal_store_information_final, focal_store_information], axis=0)

# %%
focal_store_information_final.to_csv(unique_neib + '.csv', index=False)


