# %%
import pickle
import pandas as pd
import numpy as np
from tqdm import tqdm

# %% [markdown]
# ### Reading all the focal brands

# %%
with open('top_brands.pickle', 'rb') as file:
    focal_brands = pickle.load(file)

focal_brands

# %% [markdown]
# ### Reading the social data

# %%
with open('social_data.pickle', 'rb') as file:
    social_data = pickle.load(file)

social_data.head()

# %% [markdown]
# ### Reading the Data for Local Reviews of all brands having social data

# %%
with open('brand_visit_local_reviews.pickle', 'rb') as file:
    brand_visit_local_reviews = pickle.load(file)

brand_visit_local_reviews.head()

# %% [markdown]
# ### Read the travel times

# %%
with open('data/travel_time.pkl', 'rb') as file:
    travel_time_dict = pickle.load(file)

# %%
travel_time_keys = list(travel_time_dict.keys())
from_keys = [key[0] for key in travel_time_keys]
to_keys = [key[1] for key in travel_time_keys]
time_minutes = list(travel_time_dict.values())
time_minutes = [int(time_inst.split(' ')[0]) for time_inst in time_minutes]

# %%
travel_time = pd.DataFrame({'From_PLACEKEY': from_keys, 'To_PLACEKEY': to_keys, 'Time_mins': time_minutes})
travel_time.head()

# %% [markdown]
# ### Read the distance results

# %%
distance_results = pd.read_csv('data/distance_results.csv')
distance_results.head()

# %% [markdown]
# ### Read the first and second neighbors for each store of all focal brands

# %%
with open('focal_brands_first_second_degree_neighbors.pickle', 'rb') as file:
    focal_stores_first_second_neib = pickle.load(file)
    
focal_stores_first_second_neib = focal_stores_first_second_neib.rename(columns={0:'foc_brand_name', 1: 'first_neighbor', 2: 'second_neighbor'})
focal_stores_first_second_neib.head()

# %% [markdown]
# ### Measure the time for first neighbors

# %%
neighbor_count = 0

def get_first_neighbor_time(neighbor_row):
    global neighbor_count
    neighbor_count += 1
    print('First Neighbor:', neighbor_count)
    first_neighbors_time = []
    
    focal_store = neighbor_row.name
    first_neighbors_list = neighbor_row['first_neighbor']
    
    for i in tqdm(range(len(first_neighbors_list))):
        try:
            first_neighbors_time.append(travel_time[(travel_time['From_PLACEKEY'] == focal_store) & (travel_time['To_PLACEKEY'] == first_neighbors_list[i])]['Time_mins'].iloc[0])
        except:
            first_neighbors_time.append(np.nan)
    
    return first_neighbors_time

# %%
focal_stores_first_second_neib.loc[:,'first_neighbor_time'] = focal_stores_first_second_neib.apply(get_first_neighbor_time, axis=1)

print('*****************************Done with First Neighbor********************************')
# %% [markdown]
# ### Measure the time for second neighbors

# %%
neighbor_count = 0
def get_second_neighbor_time(neighbor_row):
    global neighbor_count
    neighbor_count += 1
    print('Second Neighbor:', neighbor_count)

    second_neib_time_dict = {}
    
    second_neib_dict = neighbor_row['second_neighbor']
    first_degree_neib = list(second_neib_dict.keys())

    for i in tqdm(range(len(first_degree_neib))):
        second_neib_time = []
        first_neib = first_degree_neib[i]
        second_neib_list = second_neib_dict[first_neib]
        
        for second_neib in second_neib_list:
            try:
                second_neib_time.append(travel_time[(travel_time['From_PLACEKEY'] == first_neib) & (travel_time['To_PLACEKEY'] == second_neib)]['Time_mins'].iloc[0])
            except:
                second_neib_time.append(np.nan)
        
        second_neib_time_dict[first_neib] = second_neib_time
    
    return second_neib_time_dict

# %%
focal_stores_first_second_neib.loc[:,'second_neighbor_time'] = focal_stores_first_second_neib.apply(get_second_neighbor_time, axis=1)

# %%
with open('focal_brands_first_second_degree_neighbors_time.pickle', 'wb') as file:
    pickle.dump(focal_stores_first_second_neib, file)


print('*****************************Done with Second Neighbor********************************')