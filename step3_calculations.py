# %%
import logging
import pickle
import pandas as pd
import numpy as np
from tqdm import tqdm
import datetime
from geopy.distance import geodesic
import os
import sys

global start_range
global end_range

global foc_store_first_degree_neibs
global foc_store_time
global foc_store    
global first_deg_stores_local_reviews
global foc_store_distance
global second_neib_df_local_reviews
global foc_store_index
global store_keys_foc_brand_filtered
global date_count
global first_deg_stores
global second_neib_df

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
    
    foc_store_first_degree_neibs_time = foc_store_time['To_PLACEKEY'].to_list()
    
    for neib_store in foc_store_first_degree_neibs_time:
        neib_store_time = foc_store_time[(foc_store_time['From_PLACEKEY'] == foc_store) & (foc_store_time['To_PLACEKEY'] == neib_store)]['Time_mins'].values[0]
        neib_store_time = neib_store_time/60 #Converting from minutes to hours
        
        neib_store_reviews = group_df[group_df['PLACEKEY'] == neib_store][['localized_fb_reviews_60_days',
                                                                            'localized_ig_reviews_60_days',
                                                                            'localized_tw_reviews_60_days',
                                                                            'visits_by_day']]
        
        fb_reviews = neib_store_reviews['localized_fb_reviews_60_days']
        if (len(fb_reviews) != 0):
            num_reviews_fb_neibmean += (1/neib_store_time) * fb_reviews.values[0]
            num_reviews_fb_neibmean_exp += (1/np.exp(neib_store_time)) * fb_reviews.values[0]
        
        ig_reviews = neib_store_reviews['localized_ig_reviews_60_days']
        if (len(ig_reviews) != 0):
            num_reviews_ig_neibmean += (1/neib_store_time) * ig_reviews.values[0]
            num_reviews_ig_neibmean_exp += (1/np.exp(neib_store_time)) * ig_reviews.values[0]
        
        tw_reviews = neib_store_reviews['localized_tw_reviews_60_days']
        if (len(tw_reviews) != 0):
            num_reviews_tw_neibmean += (1/neib_store_time) * tw_reviews.values[0]
            num_reviews_tw_neibmean_exp += (1/np.exp(neib_store_time)) * tw_reviews.values[0]
        
        visits = neib_store_reviews['visits_by_day']
        if (len(visits) != 0):
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
    
    foc_store_first_degree_neibs_time = foc_store_time['To_PLACEKEY'].to_list()
    
    for neib_store in foc_store_first_degree_neibs_time:
        neib_store_time = foc_store_time[(foc_store_time['From_PLACEKEY'] == foc_store) & (foc_store_time['To_PLACEKEY'] == neib_store)]['Time_mins'].values[0]
        neib_store_time = neib_store_time/60 #Converting from minutes to hours
        
        visits = group_df[group_df['PLACEKEY'] == neib_store]['visits_by_day']
        if (len(visits) != 0):
            inv_visits += (1/neib_store_time) * visits.values[0]
            inv_visits_exp += (1/np.exp(neib_store_time)) * visits.values[0]
    
    return pd.Series([foc_store, inv_visits, num_reviews_fb_neibmean, num_reviews_ig_neibmean, num_reviews_tw_neibmean,
                    inv_visits_exp, num_reviews_fb_neibmean_exp, num_reviews_ig_neibmean_exp, num_reviews_tw_neibmean_exp])

def calculate_second_neib_visits(group_df):
            
    global first_deg_stores
    global foc_store_distance
    global second_neib_df
    global foc_store
    global foc_store_index
    global store_keys_foc_brand_filtered
    global date_count

    inv_visits_secondneibmean = 0
    inv_visits_secondneibmean_exp = 0
    date_count = date_count + 1

    # Return these zeros in case there are no second degree neighboring stores which have reviews
    num_reviews_fb_secondneibmean = 0
    num_reviews_ig_secondneibmean = 0
    num_reviews_tw_secondneibmean = 0
    num_reviews_fb_secondneibmean_exp = 0
    num_reviews_ig_secondneibmean_exp = 0
    num_reviews_tw_secondneibmean_exp = 0

    if date_count%30 == 0:
        logging.info("Focal Store [{}/{}]: Calculating Second Neighbor Visits Data...[First Degree Stores: {}, Date Count: {}]".format(foc_store_index+1, len(store_keys_foc_brand_filtered),
                                                                                                                                len(first_deg_stores), date_count))
    
    for first_deg_store_index in range(len(first_deg_stores)):
        first_deg_store = first_deg_stores[first_deg_store_index]
        inv_visits = 0
        inv_visits_exp = 0
        first_deg_store_dist = foc_store_distance[(foc_store_distance['From_PLACEKEY'] == foc_store) & (foc_store_distance['To_PLACEKEY'] == first_deg_store)]['Distance_km'].values[0]
        first_deg_store_dist = first_deg_store_dist/16.0934 #Divide by 10 miles to keep the value of distance between 0 and 1
        
        second_deg_store_placekey = second_neib_df[second_neib_df['SRC_PLACEKEY'] == first_deg_store]['DST_PLACEKEY'].to_list()
        second_deg_store_distance = second_neib_df[second_neib_df['SRC_PLACEKEY'] == first_deg_store]['Distance_Km'].to_list()        
        
        for i in range(len(second_deg_store_placekey)):
            visits = group_df[group_df['PLACEKEY'] == second_deg_store_placekey[i]]['visits_by_day']
            if (len(visits) != 0):
                inv_visits += (1/second_deg_store_distance[i]) * visits.values[0]
                inv_visits_exp += (1/np.exp(second_deg_store_distance[i])) * visits.values[0]
        
        inv_visits_secondneibmean += (1/first_deg_store_dist) * inv_visits
        inv_visits_secondneibmean_exp += (1/np.exp(first_deg_store_dist)) * inv_visits_exp
    
    return pd.Series([foc_store, inv_visits_secondneibmean, inv_visits_secondneibmean_exp,
                      num_reviews_fb_secondneibmean, num_reviews_ig_secondneibmean, num_reviews_tw_secondneibmean,
                      num_reviews_fb_secondneibmean_exp, num_reviews_ig_secondneibmean_exp, num_reviews_tw_secondneibmean_exp])

def calculate_second_neib_mean_reviews(group_df):
    
    global first_deg_stores_local_reviews
    global foc_store_distance
    global second_neib_df_local_reviews
    global foc_store
    global foc_store_index
    global store_keys_foc_brand_filtered
    global date_count
        
    num_reviews_fb_secondneibmean = 0
    num_reviews_ig_secondneibmean = 0
    num_reviews_tw_secondneibmean = 0
    num_reviews_fb_secondneibmean_exp = 0
    num_reviews_ig_secondneibmean_exp = 0
    num_reviews_tw_secondneibmean_exp = 0
    
    date_count = date_count + 1
    
    if date_count%30 == 0:
        logging.info("Focal Store [{}/{}]: Calculating Second Neighbor Local Reviews Data...[First Degree Stores: {}, Date Count: {}]".format(foc_store_index+1,
                                                                                                                                        len(store_keys_foc_brand_filtered),
                                                                                                                                        len(first_deg_stores_local_reviews),
                                                                                                                                        date_count))
    for first_deg_store_index in range(len(first_deg_stores_local_reviews)):
        
        first_deg_store = first_deg_stores_local_reviews[first_deg_store_index]
        num_reviews_fb_neibmean = 0
        num_reviews_ig_neibmean = 0
        num_reviews_tw_neibmean = 0
        num_reviews_fb_neibmean_exp = 0
        num_reviews_ig_neibmean_exp = 0
        num_reviews_tw_neibmean_exp = 0
        
        first_deg_store_dist = foc_store_distance[(foc_store_distance['From_PLACEKEY'] == foc_store) & (foc_store_distance['To_PLACEKEY'] == first_deg_store)]['Distance_km'].values[0]
        first_deg_store_dist = first_deg_store_dist/16.0934 #Divide by 10 miles to keep the value of distance between 0 and 1
        
        second_deg_store_placekey = second_neib_df_local_reviews[second_neib_df_local_reviews['SRC_PLACEKEY'] == first_deg_store]['DST_PLACEKEY'].to_list()
        second_deg_store_distance = second_neib_df_local_reviews[second_neib_df_local_reviews['SRC_PLACEKEY'] == first_deg_store]['Distance_Km'].to_list()
        
        for i in range(len(second_deg_store_placekey)):  
            second_neib_brand_stores_local_reviews = group_df[group_df['PLACEKEY'] == second_deg_store_placekey[i]][['localized_fb_reviews_60_days',
                                                                                                                'localized_ig_reviews_60_days',
                                                                                                                'localized_tw_reviews_60_days']]
            fb_reviews = second_neib_brand_stores_local_reviews['localized_fb_reviews_60_days']
            if (len(fb_reviews) != 0):
                num_reviews_fb_neibmean += (1/second_deg_store_distance[i]) * fb_reviews.values[0]
                num_reviews_fb_neibmean_exp += (1/np.exp(second_deg_store_distance[i])) * fb_reviews.values[0]
            
            ig_reviews = second_neib_brand_stores_local_reviews['localized_ig_reviews_60_days']
            if (len(ig_reviews) != 0):
                num_reviews_ig_neibmean += (1/second_deg_store_distance[i]) * ig_reviews.values[0]
                num_reviews_ig_neibmean_exp += (1/np.exp(second_deg_store_distance[i])) * ig_reviews.values[0]
            
            tw_reviews = second_neib_brand_stores_local_reviews['localized_tw_reviews_60_days']
            if (len(tw_reviews) != 0):
                num_reviews_tw_neibmean += (1/second_deg_store_distance[i]) * tw_reviews.values[0]
                num_reviews_tw_neibmean_exp += (1/np.exp(second_deg_store_distance[i])) * tw_reviews.values[0]

        num_reviews_fb_secondneibmean += (1/first_deg_store_dist) * num_reviews_fb_neibmean
        num_reviews_ig_secondneibmean += (1/first_deg_store_dist) * num_reviews_ig_neibmean
        num_reviews_tw_secondneibmean += (1/first_deg_store_dist) * num_reviews_tw_neibmean
        
        num_reviews_fb_secondneibmean_exp += (1/np.exp(first_deg_store_dist)) * num_reviews_fb_neibmean_exp
        num_reviews_ig_secondneibmean_exp += (1/np.exp(first_deg_store_dist)) * num_reviews_ig_neibmean_exp
        num_reviews_tw_secondneibmean_exp += (1/np.exp(first_deg_store_dist)) * num_reviews_tw_neibmean_exp
    
    return pd.Series([foc_store, num_reviews_fb_secondneibmean, num_reviews_ig_secondneibmean, num_reviews_tw_secondneibmean, 
                    num_reviews_fb_secondneibmean_exp, num_reviews_ig_secondneibmean_exp, num_reviews_tw_secondneibmean_exp])


def main():
    
    global start_range
    global end_range

    global foc_store_first_degree_neibs
    global foc_store_time
    global foc_store    
    global first_deg_stores_local_reviews
    global foc_store_distance
    global second_neib_df_local_reviews
    global foc_store_index
    global date_count
    global first_deg_stores
    global second_neib_df
    global store_keys_foc_brand_filtered
    
    # ### Reading all the focal brands
    # %%
    logging.info("Reading Top Brands")

    with open('top_brands.pickle', 'rb') as file:
        focal_brands = pickle.load(file)

    # %% [markdown]
    # ### Reading the visitation data for all the brands

    # %%
    logging.info("Reading Visitation Data")
    brands_visits = pd.read_csv('data/revision_visits_revenue_2019.csv')
    brands_visits['brand_standard'] = brands_visits['brand'].apply(lambda x: x.strip().lower()) # For comparison with catalog.tsv
    brands_visits['date'] = brands_visits['date'].apply(lambda x: datetime.datetime.strptime(x, '%Y-%m-%d').date())
    brands_visits = brands_visits.rename(columns={'brand': 'brand_visitation'})
    # Dropping unused columns
    brands_visits = brands_visits.drop(columns=['spend_by_day', 'lat', 'lon', 'brand_standard'])
    # Setting date as the index since we will group by date for calculation of metrics
    brands_visits = brands_visits.sort_values('date').set_index('date')

    # %% [markdown]
    # ### Reading the Data for Local Reviews of all brands having social data

    # %%
    logging.info("Reading Brand Local Reviews Data")
    with open('brand_visit_local_reviews.pickle', 'rb') as file:
        brand_visit_local_reviews = pickle.load(file)

    brand_visit_local_reviews = brand_visit_local_reviews.drop(columns=['spend_by_day', 'lat', 'lon', 'brand_standard', 'ID', 'Name',
                                                                        'Genre', 'Type', 'Classification', 'Status', 'Name_Standard'])

    brand_visit_local_reviews = brand_visit_local_reviews.sort_values('date').set_index('date')
    brand_visit_local_reviews = brand_visit_local_reviews.fillna(0)
    brand_visit_local_reviews.head()

    # %%
    brand_visit_local_reviews_list = brand_visit_local_reviews['brand_visitation'].unique().tolist()

    # %% [markdown]
    # ### Reading the neib distance for all the neighbors

    # %%
    logging.info("Reading the neib distance for all the neighbors")
    with open('neib_distance_km_brand.pkl', 'rb') as file:
        neib_distance_km = pickle.load(file)

    # Excluding all the brands having more than 1 mile distance since we don't need them anyways for second-degree neighbor calculations
    neib_distance_km = neib_distance_km[neib_distance_km['Distance_Km']<=16.0934]
    neib_distance_km.head()

    # %% [markdown]
    # ### Reading Spatial Distance

    # %%
    logging.info("Reading Spatial Distance")
    distance_results = pd.read_csv('data/distance_results.csv')
    distance_results.head()

    # %% [markdown]
    # Checking if only the focal brands are present in the FROM_Placekey

    # %%
    from_place_key_distance_brands = brands_visits[brands_visits['PLACEKEY'].isin(distance_results['From_PLACEKEY'].to_list())]['brand_visitation'].unique()

    # %% [markdown]
    # Checking the unique brands present in TO_Placekey

    # %%
    to_place_key_distance_brands = brands_visits[brands_visits['PLACEKEY'].isin(distance_results['To_PLACEKEY'].to_list())]['brand_visitation'].unique()

    # %% [markdown]
    # ### Reading Travel Time

    # %%
    logging.info("Reading Travel Time")
    with open('data/travel_time.pkl', 'rb') as file:
        travel_time_dict = pickle.load(file)
        
    travel_time_keys = list(travel_time_dict.keys())
    from_keys = [key[0] for key in travel_time_keys]
    to_keys = [key[1] for key in travel_time_keys]
    time_minutes = list(travel_time_dict.values())
    time_minutes = [int(time_inst.split(' ')[0]) for time_inst in time_minutes]

    travel_time = pd.DataFrame({'From_PLACEKEY': from_keys, 'To_PLACEKEY': to_keys, 'Time_mins': time_minutes})

    # %% [markdown]
    # Checking if only the focal brands are present in the FROM_Placekey

    # %%
    from_place_key_time_brands = brands_visits[brands_visits['PLACEKEY'].isin(travel_time['From_PLACEKEY'].to_list())]['brand_visitation'].unique()

    # %% [markdown]
    # Checking the unique brands present in TO_Placekey

    # %%
    to_place_key_time_brands = brands_visits[brands_visits['PLACEKEY'].isin(travel_time['To_PLACEKEY'].to_list())]['brand_visitation'].unique()

    # %%
    focal_brands_list = focal_brands['BRANDS'].tolist()

    # %% [markdown]
    # Select a focal brand and then extract all the PlaceKeys for this focal brand

    # %%


    # %%
    foc_brand = focal_brands_list[4]

    logging.info("Performing Calculations for Focal Brand: " + str(foc_brand))
    # Create a directory with the name of focal brand if it does not exist
    if not os.path.exists(foc_brand):
        os.makedirs(foc_brand)
    
    global start_range
    global end_range
    
    store_keys_foc_brand = brand_visit_local_reviews[brand_visit_local_reviews['brand_visitation'] == foc_brand]['PLACEKEY'].unique().tolist()
    all_neib_placekey = distance_results[distance_results['From_PLACEKEY'].isin(store_keys_foc_brand)]['To_PLACEKEY'].unique().tolist()
    unique_neib_brands_foc = brands_visits[brands_visits['PLACEKEY'].isin(all_neib_placekey)]['brand_visitation'].unique().tolist()

    # for unique_neib_index in range(len(unique_neib_brands_foc)):
    for unique_neib_index in range(start_range, end_range):

        unique_neib = unique_neib_brands_foc[unique_neib_index]
        
        # Skip the calculations if neighboring brand is equal to focal brand
        if unique_neib == foc_brand:
            logging.info("-----------------Skipping Calculations for Neighboring Brand [{}/{}]: {} ------------------------".format(unique_neib_index+1,
                                                                                                                                  len(unique_neib_brands_foc),
                                                                                                                                  unique_neib))
            continue
        
        logging.info("-----------------Performing Calculations for Neighboring Brand [{}/{}]: {} ------------------------".format(unique_neib_index+1,
                                                                                                                                  len(unique_neib_brands_foc),
                                                                                                                                  unique_neib))
    
        unique_neib_placekeys = brands_visits[brands_visits['brand_visitation'] == unique_neib]['PLACEKEY'].unique().tolist()
        focal_stores_first_degree_neib = distance_results[(distance_results['From_PLACEKEY'].isin(store_keys_foc_brand)) &
                                                        (distance_results['To_PLACEKEY'].isin(unique_neib_placekeys)) &
                                                        (distance_results['Distance_km']<=16.0934)]
        
        store_keys_foc_brand_filtered = focal_stores_first_degree_neib['From_PLACEKEY'].unique().tolist()
        
        focal_store_information_final = None
        date_count = None
        
        logging.info("Neighboring Brand [{}/{}]: {} - Unique Focal Stores having First Degree Neighboring Stores -> {}".format(unique_neib_index+1,
                                                                                                                            len(unique_neib_brands_foc),
                                                                                                                            unique_neib,
                                                                                                                            len(store_keys_foc_brand_filtered)))
        
        for foc_store_index in range(len(store_keys_foc_brand_filtered)):
            foc_store = store_keys_foc_brand_filtered[foc_store_index]
            logging.info("############ Focal Store [{}/{}]: {} ######################".format(foc_store_index+1, len(store_keys_foc_brand_filtered), foc_store))
            foc_store_all_first_degree_neibs = distance_results[(distance_results['From_PLACEKEY'] == foc_store) & (distance_results['Distance_km'] <= 16.0934)]['To_PLACEKEY'].to_list()
            foc_store_all_first_degree_neibs_brands = brands_visits[brands_visits['PLACEKEY'].isin(foc_store_all_first_degree_neibs)]['brand_visitation'].unique().tolist()
            foc_store_first_degree_neibs = focal_stores_first_degree_neib[focal_stores_first_degree_neib['From_PLACEKEY'] == foc_store]['To_PLACEKEY'].to_list()
            foc_store_distance = distance_results[(distance_results['From_PLACEKEY'] == foc_store) & (distance_results['To_PLACEKEY'].isin(foc_store_first_degree_neibs))]
            spatial_distance_avg = np.average(foc_store_distance['Distance_km'].to_list())
            foc_store_time = travel_time[(travel_time['From_PLACEKEY'] == foc_store) & (travel_time['To_PLACEKEY'].isin(foc_store_first_degree_neibs))]
            travel_distance_avg = np.average(foc_store_time['Time_mins'].to_list())

            logging.info("Unique Neib [{}/{}], Focal Store [{}/{}]: Calculating First Neighbor Metrics...".format(unique_neib_index+1,
                                                                                                                len(unique_neib_brands_foc),
                                                                                                                foc_store_index+1,
                                                                                                                len(store_keys_foc_brand_filtered)))
            
            if unique_neib in brand_visit_local_reviews_list:
                first_neib_metrics = brand_visit_local_reviews[brand_visit_local_reviews['PLACEKEY'].isin(foc_store_first_degree_neibs)][['PLACEKEY', 'visits_by_day','localized_fb_reviews_60_days','localized_ig_reviews_60_days', 
                                                                                                                                            'localized_tw_reviews_60_days']].groupby('date').apply(calculate_first_neib_mean_reviews_visits)
            else:
                first_neib_metrics = brands_visits[brands_visits['PLACEKEY'].isin(foc_store_first_degree_neibs)][['PLACEKEY', 'visits_by_day']].groupby('date').apply(calculate_first_neib_visits)

            first_neib_metrics = first_neib_metrics.rename(columns={0:'focal_store', 1:'inv_visits', 2: 'num_reviews_fb_neibmean', 3:'num_reviews_ig_neibmean', 4:'num_reviews_tw_neibmean',
                                                                    5:'inv_visits_exp', 6:'num_reviews_fb_neibmean_exp', 7: 'num_reviews_ig_neibmean_exp', 8: 'num_reviews_tw_neibmean_exp'})

            logging.info("Unique Neib [{}/{}], Focal Store [{}/{}]: First Neighbor Metrics...Done!".format(unique_neib_index+1,
                                                                                                        len(unique_neib_brands_foc),
                                                                                                        foc_store_index+1,
                                                                                                        len(store_keys_foc_brand_filtered)))

            logging.info("Unique Neib [{}/{}], Focal Store [{}/{}]: Calculating Second Neighbor Metrics...".format(unique_neib_index+1,
                                                                                                                len(unique_neib_brands_foc),
                                                                                                                foc_store_index+1,
                                                                                                                len(store_keys_foc_brand_filtered)))

            second_neib_df = neib_distance_km[neib_distance_km['SRC_PLACEKEY'].isin(foc_store_first_degree_neibs) &
                                            (~neib_distance_km['DST_BRAND'].isin(foc_store_all_first_degree_neibs_brands)) &
                                            (neib_distance_km['DST_BRAND'] != unique_neib)]

            second_neib_df_local_reviews = second_neib_df[second_neib_df['DST_BRAND'].isin(brand_visit_local_reviews_list)]
            first_deg_stores = second_neib_df['SRC_PLACEKEY'].unique().tolist()
            second_deg_stores = second_neib_df['DST_PLACEKEY'].unique().tolist()
            first_deg_stores_local_reviews = second_neib_df_local_reviews['SRC_PLACEKEY'].unique().tolist()
            second_deg_stores_local_reviews = second_neib_df_local_reviews['DST_PLACEKEY'].unique().tolist()

            logging.info("Unique Neib [{}/{}], Focal Store [{}/{}]: Calculating Second Neighbor Visits Data...".format(unique_neib_index+1,
                                                                                                                    len(unique_neib_brands_foc),
                                                                                                                    foc_store_index+1,
                                                                                                                    len(store_keys_foc_brand_filtered)))
            date_count = 0
            second_neib_brand_stores = brands_visits[brands_visits['PLACEKEY'].isin(second_deg_stores)][['PLACEKEY', 'visits_by_day']]
            second_neib_metrics_visits = second_neib_brand_stores.groupby('date').apply(calculate_second_neib_visits)            
            second_neib_metrics_visits = second_neib_metrics_visits.rename(columns={0: 'focal_store', 1:'inv_visits_secondneibmean', 2:'inv_visits_secondneibmean_exp',
                                                                        3:'num_reviews_fb_secondneibmean', 4:'num_reviews_ig_secondneibmean',
                                                                        5: 'num_reviews_tw_secondneibmean', 6: 'num_reviews_fb_secondneibmean_exp',
                                                                        7: 'num_reviews_ig_secondneibmean_exp', 8: 'num_reviews_tw_secondneibmean_exp'}).reset_index()

            if len(second_neib_df_local_reviews) != 0: # only take the first 4 columns if second neighbors exist having local reviews
                second_neib_metrics_visits = second_neib_metrics_visits.iloc[:,0:4]

            logging.info("Unique Neib [{}/{}], Focal Store [{}/{}]: Calculating Second Neighbor Local Reviews Data...".format(unique_neib_index+1,
                                                                                                                            len(unique_neib_brands_foc),
                                                                                                                            foc_store_index+1,
                                                                                                                            len(store_keys_foc_brand_filtered)))
            date_count = 0
            second_neib_brand_stores_local_reviews = brand_visit_local_reviews[brand_visit_local_reviews['PLACEKEY'].isin(second_deg_stores_local_reviews)][['PLACEKEY',
                                                                                                                                                            'localized_fb_reviews_60_days',
                                                                                                                                                            'localized_ig_reviews_60_days', 
                                                                                                                                                            'localized_tw_reviews_60_days']]
            second_neib_metrics_local_reviews = second_neib_brand_stores_local_reviews.groupby('date').apply(calculate_second_neib_mean_reviews)

            second_neib_metrics_local_reviews = second_neib_metrics_local_reviews.rename(columns={0: 'focal_store', 1:'num_reviews_fb_secondneibmean', 2:'num_reviews_ig_secondneibmean',
                                                                                                3: 'num_reviews_tw_secondneibmean', 4: 'num_reviews_fb_secondneibmean_exp',
                                                                                                5: 'num_reviews_ig_secondneibmean_exp', 6: 'num_reviews_tw_secondneibmean_exp'}).reset_index()
            
            logging.info("Unique Neib [{}/{}], Focal Store [{}/{}]: Second Neighbor Metrics...Done!".format(unique_neib_index+1,
                                                                                                            len(unique_neib_brands_foc),
                                                                                                            foc_store_index+1,
                                                                                                            len(store_keys_foc_brand_filtered)))
            
            logging.info("Unique Neib [{}/{}], Focal Store [{}/{}]: Compiling Information".format(unique_neib_index+1,
                                                                                                len(unique_neib_brands_foc),
                                                                                                foc_store_index+1,
                                                                                                len(store_keys_foc_brand_filtered)))
        
            if len(second_neib_df_local_reviews) != 0: # merge both the visits if local reviews data exist, else, just take the values with zero reviews stored in second_neib_metrics_visits
                second_neib_metrics = pd.merge(left=second_neib_metrics_visits, right=second_neib_metrics_local_reviews, how='inner', on=['date', 'focal_store'])
            else:
                second_neib_metrics = second_neib_metrics_visits
            
            focal_store_information = brand_visit_local_reviews[brand_visit_local_reviews['PLACEKEY'] == foc_store][['PLACEKEY', 'brand_visitation', 'visits_by_day', 'visits_past_60_days',
                                                                                                                    'localized_fb_reviews_60_days', 'localized_ig_reviews_60_days', 'localized_tw_reviews_60_days']]
            focal_store_information['spatial_distance_km'] = spatial_distance_avg
            focal_store_information['travel_distance_min'] = travel_distance_avg
            # %%
            # Combining first and second degree neighbor metrics with the focal store info
            focal_store_information = pd.merge(left=focal_store_information, right=first_neib_metrics, how='inner', on=['date']).drop('focal_store', axis=1)
            focal_store_information = pd.merge(left=focal_store_information, right=second_neib_metrics, how='inner', on=['date']).drop('focal_store', axis=1)
            focal_store_information = focal_store_information.sort_values('date').fillna(0)

            if foc_store_index == 0:
                focal_store_information_final = focal_store_information
            else:
                focal_store_information_final = pd.concat([focal_store_information_final, focal_store_information], axis=0)

            logging.info("Unique Neib [{}/{}], Focal Store [{}/{}]: Compilation Done".format(unique_neib_index+1,
                                                                                            len(unique_neib_brands_foc),
                                                                                            foc_store_index+1,
                                                                                            len(store_keys_foc_brand_filtered)))
        
        if len(store_keys_foc_brand_filtered) != 0:
            file_path = os.path.join(foc_brand, unique_neib.replace('/', '_') + '.csv')
            focal_store_information_final.to_csv(file_path, index=False)
            logging.info("-----------------Completed calculations for Neighboring Brand [{}/{}]: {} ------------------------".format(unique_neib_index+1, len(unique_neib_brands_foc), unique_neib))
            logging.info("Neighboring Brand [{}/{}]: {} - Storing Information -> {}".format(unique_neib_index+1, len(unique_neib_brands_foc), unique_neib, file_path))
        else:
            logging.info("Neighboring Brand [{}/{}]: {} - No Focal Stores having First Degree Neighboring Stores - No Calculations/Storage".format(unique_neib_index+1, len(unique_neib_brands_foc), unique_neib))


if __name__ == "__main__":

    logging.basicConfig(format='%(asctime)s - %(levelname)s - %(message)s', level=logging.INFO)

    if (len(sys.argv) < 3) or (len(sys.argv) > 3):
        logging.info("Wrong Number of Input Arguments: ", len(sys.argv) - 1)
        logging.info("Exiting Program!!!")
    else:
        start_range = int(sys.argv[1])
        end_range = int(sys.argv[2])
        logging.info("Start Range: " + str(start_range))
        logging.info("End Range: " + str(end_range))
        
        if start_range >= end_range:
            logging.info("Start Range should be less than end range. Exiting!!!")
        else:
            logging.info("Starting the Step 3 Calculations")
            main()
