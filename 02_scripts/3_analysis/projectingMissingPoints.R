# projectingMissingPoints.R
# Created 07 November 2024
# Margaret Swift <margaret.swift@cornell.edu>
# 
# ## This file is going to project the possible locations of missing data points
# and graph them all on top of each other so we can maybe see where the GPS is failing. :) 

# ******************************************************************************
#                             DATA & LIBRARY LOADING
# ******************************************************************************

here::i_am('02_scripts/3_analysis/projectingMissingPoints.R')
source(here::here('02_scripts', 'utilities.R'))
setDataPaths('elephant')
quickload('elephant')

table(ele.df$MISSING)



# ******************************************************************************
#                             DATA & LIBRARY LOADING
# ******************************************************************************

inx.missing <- which(ele.df$MISSING)
inx.start <- which(ele.df$START.COUNT)
inx.end <- which(ele.df$END.COUNT)


inx.rm <- match(c(inx.start[inx.start %in% inx.missing],
            inx.end[inx.end %in% inx.missing]), inx.missing)
to.check <- inx.missing[-inx.rm]
test.pts.df <- data.frame(A=NA, B=NA)[0,]
newrow.inx <- 1
for (i in 1:length(to.check)) {
  if ((i %% 1000) == 0) message(i)
  a <- b <- to.check[i]
  
  if (is.na(a)) { 
    message('skipping ', i)
  } else { 
    stop = FALSE 
    count = 0
    while (!stop) {
      b = b + 1
      flag = (b %in% to.check)
      if (flag) {
        # if you find the index, remove it from to.check
        inx.find = which(to.check == b)
        to.check[inx.find] = NA
      } else {
        stop = TRUE
      }
      
      # exit condition
      count = count + 1
      if (count > 10) stop = TRUE
    }
    ptA.inx = a-1
    ptB.inx = b
    test.pts.df[newrow.inx,] <- c(ptA.inx, ptB.inx)
    newrow.inx = newrow.inx + 1
  }
}
test.pts.df$n.miss = (test.pts.df$B - test.pts.df$A)-1
hist(test.pts.df$n.miss, breaks=30)


## AMT STEP PROJECTION CODE FROM KAZA PROJ :) 
NP = nrow(test.pts.df)
bursts = 1:NP
st.pts = cbind(ele.df[test.pts.df$A,c("LON", "LAT", 'DATE.TIME')],
               n=test.pts.df$n.miss) %>% 
  rename(x_=LON, y_=LAT, t_=DATE.TIME)
ed.pts = cbind(ele.df[test.pts.df$B,c("LON", "LAT", 'DATE.TIME')], 
               n=test.pts.df$n.miss) %>% 
  rename(x_=LON, y_=LAT, t_=DATE.TIME)
st.pts$burst_ = ed.pts$burst_ = bursts
st.pts$type = "start"
ed.pts$type = "end"

# nrep=100
# x1 = start.points[1:nrep,]
# x2 = end.points[1:nrep,]
# ggplot() + 
#   geom_point(data=x1, aes(x=LON, y=LAT, size=n), color='blue')+
#   geom_point(data=x2, aes(x=LON, y=LAT, size=n), color='red') + 
#   plot.theme

# # create tracks from xy data
dat <- rbind(st.pts, ed.pts) %>% arrange(burst_)
ssdf <- NULL
for (b in bursts) {
  # messages
  if ((b %% 1000) == 0) message(b)
  
  # pull data for each burst and make a track
  xyt <- dat %>% filter(burst_ == b)
  track <- as_track(xyt, all_cols=TRUE) %>%
    track_resample(rate = hours(1), tolerance = minutes(10))
  track$burst_ = b
  
  # save track to ssdf object
  if (is.null(ssdf)) ssdf <- track
  else ssdf <- rbind(ssdf, track)
}




##################################################################################
##################################################################################

mydata = ele.sf %>% 
  # filter(ID == 4833) %>% 
  st_transform(crs=32635)
xy = st_coordinates(mydata)
mydata <- cbind(nog(mydata), xy) %>% 
  dplyr::select(ID, BURST, DATE.TIME, X, Y)

tic()
crwOut <-momentuHMM::crawlWrap(obsData  = mydata,
                   timeStep = "hour",
                   Time.name= "DATE.TIME",
                   retryFits = 100,
                   drift=TRUE,
                   attempts=2,
                   coord=c('X', 'Y'),
                   ncore=3,
                   fillCols=TRUE,
                   theta=c(20, 15) #this is from the tutorial, not sure how to choose
                   # theta=c(6.855, -0.007) #this is from the tutorial, not sure how to choose
)

crwOut <-crawl::crwMLE(data  = mydata,
                               timeStep = "hour",
                               Time.name= "DATE.TIME",
                               retryFits = 2,
                               drift=TRUE,
                               attempts=2,
                               coord=c('X', 'Y'),
                               ncore=3,
                               fillCols=TRUE,
                               theta=c(6.855, -0.007) #this is from the tutorial, not sure how to choose
)
toc()