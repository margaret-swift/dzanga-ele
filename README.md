# dzanga-ele
Collaborative project focused on space use by forest elephants in Dzanga-Sangha.

Big questions: ?

Mechanical questions: How can we use intermittent canopy-interrupted data from GPS-collared elephants, in conjunction with a grid of passive acoustic monitoring data, to map space use by African forest elephants (_Loxodonta cyclotis_) in Dzanga-Sangha?

Data: 

1. Passive acoustic monitoring data, YYYY(?)
1. Elephant GPS data (4 hour fixes), 2018-2023
1. Canopy cover raster, 2019 (100m) 
- Citation: Reiner, F., Brandt, M., Tong, X. et al. More than one quarter of Africa’s tree cover is found outside areas previously classified as forest. Nat Commun 14, 2258 (2023). https://doi.org/10.1038/s41467-023-37880-4
- Data located: https://zenodo.org/records/7764460

Output: Create three mapping layers.

1. Map of **positive elephant presence density** using _passive acoustic monitoring_ data.
1. Map of **likely canopy obstruction** of GPS data (high values = high likelihood of ele presence being obscured) using the _canopy cover raster_.
1. Map of **likely elephant locations** using _elephant GPS_ data.
