# geographicData.R
# Created 06 November 2024
# Margaret Swift <margaret.swift@cornell.edu>

# ******************************************************************************
#                             DATA & LIBRARY LOADING
# ******************************************************************************

here::i_am('02_scripts/1_cleaning/geographicData.R')
source(here::here('02_scripts','utilities.R'))

## Loading geographic data
setDataPaths('rivers')
rivers.sm <- st_read(rawpath('Petits cours d`eau.shp'))
rivers.big <- st_read(rawpath('Réseau_hydrographique.shp'))

setDataPaths('bais')
bais <- st_read(rawpath("Salines.shp"))

setDataPaths('villages')
villages <- st_read(rawpath("Villages_PDS.shp"))

setDataPaths('roads')
roads <- st_read(rawpath('big roads only.shp'))
roads <- st_set_crs(roads, 4326)

setDataPaths('boundaries')
tns_bounds <- st_read(rawpath('lim_parc tns.shp'))
tns_bounds <- st_set_crs(tns_bounds, 4326)
cc_zone <- st_read(rawpath('lim_secteur zcc.shp'))
apds <- st_read(rawpath('APDS_complet.shp'))
logging <- st_read(rawpath('190_Assiette de coupe provisoire_V3.shp'))
special_reserve <- st_read(rawpath('lim_reserve_speciale.shp'))

save(pa,
     tns_bounds,
     rivers,
     roads,
     cc_zone,
     apds,
     bais,
     villages,
     logging,
     file=file.path(datadir, "geographicdata.rdata"))
