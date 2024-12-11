# cleanEle.R
# Created 26 Sept 2023
# Margaret Swift <margaret.swift@cornell.edu>

# ******************************************************************************
#                             DATA & LIBRARY LOADING
# ******************************************************************************

here::i_am('02_scripts/1_cleaning/cleanEle.R')
source(here::here('02_scripts', 'utilities.R'))
quickload()
setDataPaths('elephant')
files <- list.files(rawpath(), pattern="csv")
data <- NULL
for (i in seq_along(files) ) {
  csv <- read.csv(rawpath( files[i]))
  if (is.null(data)) data <- csv
  else data <- rbind(data, csv)
}



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
    TEMPC = as.n(gsub('°C', '', TEMPERATURE)),
    
    # renaming IDs
    ID = as.numeric(gsub('African Elephant: ', '', TAG)),
    
    DATE = date(DATE.TIME),
    DIFF = DATE - lag(DATE),
    DIFF = ifelse(is.na(DIFF), 1, DIFF),
    BURST = ID*1000 + (cumsum(!DIFF %in% c(0,1))),
    
    # indexing start.count on bursts
    START.COUNT = BURST != lag(BURST),
    START.COUNT = ifelse(is.na(START.COUNT), TRUE, START.COUNT)
  ) %>% 
  arrange(ID) %>% 
  select(INX, TAG.INDEX, ID, BURST, START.COUNT,
         DATE.TIME, DATE, MONTH, YEAR, TEMPC,
         LON, LAT, ACCELEROMETER, MOVEMENT,
         DIFF, SPEED)
ele$INX <- 1:nrow(ele)


## add an end count as well.
END.INX = which(ele$START.COUNT) - 1
END.INX = c(END.INX[-1], nrow(ele))
ele$END.COUNT = FALSE
ele$END.COUNT[END.INX] <- TRUE

# grab unique ids
ids <- unique(ele$ID)

# set SEASON based on first rainfall from precipitation.rdata
ele$SEASON = "NORMAL"
ele$SEASON[ele$MONTH %in% c(10, 11)] <- "LONGRAIN"
ele$SEASON[ele$MONTH %in% c(5, 6)] <- "SHORTRAIN"

# save to ele.df
ele.df <- ele
ele.sf <- st_as_sf(ele, coords=c("LON", "LAT"), crs=4326, remove=FALSE)


# ******************************************************************************
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
#                                       STS
# ******************************************************************************

setDataPaths('elephant')
save(ele.df, file=procpath('elephant.rdata'))
st_write(ele.sf, dsn=procpath('elephants.shp'))


