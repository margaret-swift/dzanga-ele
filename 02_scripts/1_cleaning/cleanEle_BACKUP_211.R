# cleanEle.R
# Created 01 November 2024
# Margaret Swift <margaret.swift@cornell.edu>

## TODO: FIND OUT WHICH 48NN FROM METADATA ARE WHICH COLLARS
##    We are guessing that the lone male in the 48NN dataset is probaby #4831


# ******************************************************************************
#                             DATA & LIBRARY LOADING
# ******************************************************************************

here::i_am('02_scripts/1_cleaning/cleanEle.R')
source(here::here('02_scripts', 'utilities.R'))
setDataPaths('elephant')
files <- list.files(rawpath(), pattern="csv")
data <- NULL
for (i in seq_along(files) ) {
  csv <- read.csv(rawpath( files[i]))
  if (is.null(data)) data <- csv
  else data <- rbind(data, csv)
}
<<<<<<< HEAD
<<<<<<< HEAD
metadata <- read.csv(metapath('ele_collared_dates.csv')) %>% 
  mutate(Date.Collared = as.Date(Date.Collared))
=======
=======

>>>>>>> cbda14cd2049c3f3cda35e7ea2e883eae0d1fce7


>>>>>>> cbda14cd2049c3f3cda35e7ea2e883eae0d1fce7

# ******************************************************************************
#                             Initial looks
# ******************************************************************************
names(data) <- gsub('\\.| |`', '', toupper(names(data)))
ele <- data %>% 
  arrange(TAG, TIMESTAMP) %>% 
  mutate( 
    # indexing
    INX = NA,
    TAG.INDEX=INDEX,
    
    # setting up date-time stuff
    DATE.TIME = as.POSIXlt(TIMESTAMP, tz="UTC"),
    MONTH = month(DATE.TIME),
    YEAR = year(DATE.TIME),
    
    # fixing lat lontemp
    LON = as.n(gsub('°', '', LONGITUDE)),
    LAT = as.n(gsub('°', '', LATITUDE)),
    LON = ifelse(LON == 0, NA, LON),
    LON = ifelse(LON == 180, NA, LON),
    LAT = ifelse(LAT == 0, NA, LAT),
    LAT = ifelse(LAT == 90, NA, LAT),
    MISSING = is.na(LON),
    
    TEMPC = as.n(gsub('°C', '', TEMPERATURE)),
    
    # renaming IDs
    ID = as.numeric(gsub('African Elephant: ', '', TAG)),
    
    DATE = date(DATE.TIME),
    DIFF = DATE - lag(DATE),
    DIFF = ifelse(is.na(DIFF), 1, DIFF),
    BURST = ID*1000 + (cumsum(!DIFF %in% c(0,1))),
  ) %>% 
  arrange(ID) %>% 
<<<<<<< HEAD
<<<<<<< HEAD
  dplyr::select(INX, TAG.INDEX, ID, MISSING, TYPE, LOGINTERVAL, BURST,
         DATE.TIME, DATE, MONTH, YEAR, TEMPC,
         LON, LAT, ACCELEROMETER, MOVEMENT,
         DIFF, SPEED)

=======
  select(INX, TAG.INDEX, ID, BURST, START.COUNT,
         DATE.TIME, DATE, MONTH, YEAR, TEMPC,
         LON, LAT, ACCELEROMETER, MOVEMENT,
         DIFF, SPEED)
=======
  select(INX, TAG.INDEX, ID, BURST, START.COUNT,
         DATE.TIME, DATE, MONTH, YEAR, TEMPC,
         LON, LAT, ACCELEROMETER, MOVEMENT,
         DIFF, SPEED)
>>>>>>> cbda14cd2049c3f3cda35e7ea2e883eae0d1fce7
ele$INX <- 1:nrow(ele)
>>>>>>> cbda14cd2049c3f3cda35e7ea2e883eae0d1fce7

## Filter out all collar data before the elephant was collared
## for the collars labeled 48NN, we don't know exactly when they were collared
## so we're going to set the date to 2021-07-09 and then manually set the start point

# grab unique ids
ids <- unique(ele$ID)
ele.new <- ele[0,]
for (i in seq_along(ids)) {
  id = ids[i]
  inx <- which(metadata$Tag.ID == id)
  if (length(inx)){ start.date = metadata[inx,'Date.Collared']; sex=metadata[inx,'Sex']
  } else { start.date = as.Date('2021-07-14'); sex="UNK"}
  x <- ele %>% filter(ID == id, DATE > (start.date) )
  x$SEX = sex
  ele.new <- rbind(ele.new, x)
}
ele.df <- ele.new %>% 
  mutate(
    START.COUNT = BURST != lag(BURST),
    START.COUNT = ifelse(is.na(START.COUNT), TRUE, START.COUNT)
  )
ele.df$INX <- 1:nrow(ele.df)


## add an end count as well.
END.INX = which(ele.df$START.COUNT) - 1
END.INX = c(END.INX[-1], nrow(ele.df))
ele.df$END.COUNT = FALSE
ele.df$END.COUNT[END.INX] <- TRUE


# set SEASON based on first rainfall from precipitation.rdata
ele.df$SEASON = "DRY"
ele.df$SEASON[ele.df$MONTH %in% 5:11] <- "WET"

# save to ele.df
ele.sf <- st_as_sf(ele.df %>% filter(!MISSING), coords=c("LON", "LAT"), crs=4326, remove=FALSE)


# ******************************************************************************
<<<<<<< HEAD
=======
#                                     STATS
# ******************************************************************************
makeHist <- function(i) {
  data <- ele.df %>%
    filter(ID == i,
           # DATE.TIME > as.Date('2010-12-01'),
           # DATE.TIME < as.Date('2011-01-30')
           ) %>%
    mutate(DATE = date(DATE.TIME))
  hist.data <- data %>%
    group_by(DATE, .drop=FALSE) %>%
    summarize(n=n()) %>%
    mutate(FLAG = ifelse(n > 8, "HIGH", ifelse(n<4, "LOW", "AVG")))
  title = paste0('Elephant ', data$ID[1])
  cols <- list(AVG='darkgray', HIGH='#08c952', LOW='#f2055c')[unique(hist.data$FLAG)]
  ggplot() +
    geom_bar(data=hist.data,
             mapping=aes(x=DATE, y=n, fill=FLAG),
             stat="identity") +
    ggtitle(title) + theme(axis.title.x=element_blank()) +
    scale_fill_manual(values=cols) + ylab('daily # GPS fixes') +
    scale_x_date(breaks='4 months', date_labels = "%b-%Y") + theme(text=element_text(size=20))
}
makeHist(ids[1])

setOutPath("elephant_data_histograms")
for (id in ids) {
  p <- makeHist(id)
  fname = paste0('hist_gps_', id, '.png')
  ggsave(p, file=outpath(fname))
}

# ******************************************************************************
>>>>>>> cbda14cd2049c3f3cda35e7ea2e883eae0d1fce7
#                                       STS
# ******************************************************************************

setDataPaths('elephant')
save(ele.df, ele.sf, file=procpath('elephant.rdata'))


