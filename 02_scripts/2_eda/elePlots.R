# elePlots.R
# Created 26 Sept 2023
# Margaret Swift <margaret.swift@cornell.edu>

# ******************************************************************************
#                             DATA & LIBRARY LOADING
# ******************************************************************************

here::i_am('02_scripts/2_eda/elePlots.R')
source(here::here('02_scripts','utilities.R'))
pacman::p_load(sp, reshape2,
               spData, ggspatial, move, terra)
quickload('elephant')
ids <- unique(ele.df$ID)

# ******************************************************************************
#                               PLOT HISTOGRAMS
# ******************************************************************************
makeHist <- function(i) {
  data <- ele.df %>%
    filter(ID == i, !MISSING) %>% 
    mutate(DATE = date(DATE.TIME))
  hibar = 8; lobar = 4
  hist.data <- data %>%
    group_by(DATE, .drop=FALSE) %>%
    summarize(n=n()) %>%
    mutate(FLAG = ifelse(n > hibar, "HIGH", ifelse(n<lobar, "LOW", "AVG")))
  title = paste0('Elephant ', data$ID[1])
  cols <- list(AVG='darkgray', HIGH='#08c952', LOW='#f2055c')[unique(hist.data$FLAG)]
  ggplot() +
    geom_bar(data=hist.data,
             mapping=aes(x=DATE, y=n, fill=FLAG),
             stat="identity") +
    ggtitle(title) + theme(axis.title.x=element_blank()) +
    scale_fill_manual(values=cols) + ylab('daily # GPS fixes') +
    scale_x_date(breaks='6 months', date_labels = "%b-%Y") + theme(text=element_text(size=20))
}

setOutPath("elephant_eda", "histograms_of_fixrates")
for (id in ids) {
  p <- makeHist(id)
  fname = paste0('hist_gps_', id, '.png')
  ggsave(p, file=outpath(fname))
}


# ******************************************************************************
#                               PLOT GPS PATHS
# ******************************************************************************


## PLOT POINTS
plotPoints <- function(id) {
  pts <- ele.sf %>% filter(ID==id) %>% st_transform(crs=4326)
  lines = pts %>% 
    summarize(do_union=FALSE, n=n()) %>%
    st_cast("LINESTRING")
  bb <- st_bbox(pts)
  p = base + 
    geom_sf(data=lines, alpha=0.2, inherit.aes=FALSE) +
    geom_sf(data = pts, 
            col = "red", fill = NA, size=2, alpha=0.2,
            inherit.aes = FALSE) + 
    ggtitle(paste("id = ", id))
  zoomTo(p, bb)
}
setOutPath(c("elephant_eda", "GPS_maps"))
for (id in ids) {
  p <- plotPoints(id)
  fname = paste0('gps_path_', id, '.png')
  ggsave(p, file=outpath(fname))
}


## PLOT LINES
plotLines <- function(id) {
  dat <-  ele.sf %>% filter(ID==id, DATE.TIME < as.Date("2018-11-01"))
  first <- dat[1,]
  lines <- dat %>% 
    st_transform(crs=4326) %>% 
    group_by(DATE) %>% 
    summarize(do_union=FALSE, n=n()) %>%
    st_cast("LINESTRING") %>% 
    filter(n>2)
  p = base + 
    geom_sf(data=first, color='red', size=4) +
    geom_sf(data=lines, alpha=0.3, color='white', inherit.aes=FALSE) +
    ggtitle(paste('ele id = ', id))
  zoomTo(p, st_bbox(dat))
}
setOutPath(c("elephant_eda", "GPS_maps_white"))
for (id in ids) {
  p <- plotLines(id)
  fname = paste0('gps_path_zoomout', id, '.png')
  ggsave(p, file=outpath(fname))
}


## PLOTTING ALL ON TOP OF EACH OTHER
lines = ele.sf %>% st_transform(crs=4326) %>% 
  group_by(ID, DATE) %>% 
  summarize(do_union=FALSE, n=n()) %>%
  st_cast("LINESTRING") %>% 
  filter(n>5)
base + 
  geom_sf(data=lines, color='white', alpha=0.3, inherit.aes=FALSE)
ggsave(outpath('elephant_lines_all_white.png'))


# ******************************************************************************


# Plot tracks
library(amt)
library(moveVis) #devtools::install_github("16EAGLE/moveVis")

i=20
id = ids[i]
plotLines(id)

# video
move_data <- ele.sf %>% 
  mutate(time = as.POSIXct(DATE.TIME),
         YEAR = year(DATE.TIME)) %>% 
  filter(ID==id, 
         DATE.TIME < as.Date('2020-09-15'),
         DATE.TIME > as.Date('2020-09-01')) %>% 
  df2move( proj="EPSG:4326",
           x="LON", y="LAT", 
           time="time", 
           track_id="ID"
          ) %>% 
  align_move(res=1, unit="hours")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(move_data, 
                         map_service = "carto", 
                         map_type="light",
                         alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()

# animate frames
fname = here("03_output", "movies", "eletest.gif")
animate_frames(frames, out_file=fname)


# plot fence crossing
df <- data.frame(sex=c('female', 'male'),
                 river = c(10.1, 14.5),
                 road = c(15.3, 25.8),
                 fence = c(0, 3.5)
                 ) %>% 
  reshape2::melt(id.var='sex')

ggplot(data=df, aes(x=variable, y=value*0.01, fill=sex)) + 
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c('black', 'darkgray')) +
  theme(text=element_text(size=24)) + 
  xlab('linear feature') + ylab('chance of crossing')






# ******************************************************************************
#                               PLOTTING HMM OUTPUTS
# ******************************************************************************

ids <- unique(ele.khau$ID)
data <- ele.khau %>% 
  mutate(YEAR = year(DATE),
         is_new = STATE != lag(STATE)) %>% 
  filter(ID == 2, 
         DATE > as.Date("2015-01-01"), 
         DATE < as.Date("2015-12-31")) %>% 
  st_as_sf(coords=c("LON", "LAT"), crs=4326) %>% 
  ungroup()
data$NEWBURST <- cumsum( data$is_new )
inx.keep <- which(rle(data$NEWBURST)$lengths > 1)
lines <- data %>% 
  group_by(BURST) %>% 
  dplyr::summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING")
lines_states <- data %>% 
  filter(NEWBURST %in% inx.keep) %>% 
  group_by(NEWBURST, STATE) %>% 
  dplyr::summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING")

ggplot() + 
  geom_sf(data=lines) +
  # geom_sf(data=lines_states, mapping=aes(color=STATE)) +
  # geom_sf(data=data, size=2, alpha=0.75, pch=1) +
  geom_sf(data=data, size=3, mapping=aes(color=STATE)) +
  # geom_sf(data=waters_art) +
  annotation_scale(text_cex=1) +
  theme(text=element_text(size=18)) + 
  guides(color="none") + 
  scale_color_manual(values=c('orange', 'purple', 'green'))

ggplot(data) + 
  geom_histogram(aes(fill=STATE, x=DIST)) + 
  facet_wrap(~STATE, ncol=1, scales="free_y") +
  scale_fill_manual(values=c('orange', 'purple', 'green')) + 
  theme(text=element_text(size=18)) + 
  ylab('frequency') + xlab('step length (m)')

ggplot(data) + 
  geom_histogram(aes(fill=STATE, x=REL.ANGLE)) + 
  facet_wrap(~STATE, ncol=1, scales="free_y") +
  scale_fill_manual(values=c('orange', 'purple', 'green')) + 
  theme(text=element_text(size=18)) + 
  ylab('frequency') + xlab('turning angle')




# ******************************************************************************

# measuring displacement


# moving window

i=1; j=25;
data <- ele.df %>% filter(ID == i)
data.sp <- data %>%
  nog() %>% 
  dplyr::select(X, Y) %>% 
  SpatialPoints()
kernel.ref <- kernelUD(data.sp, h='href')
max = findmax(kernel.ref)
max.pts <- max %>% 
  st_as_sf(crs=32734) %>% 
  st_set_crs(32734) %>% 
  st_transform(crs=st_crs(data)) %>% 
  mutate(SINK = factor(1:length(max)))

whichminList <- function(df) {
  apply(df, 1, which.min)
}
maxdists <- spDists(data.sp, max)
data$SINK <- factor( whichminList(maxdists) )
data$SINK.DIST <- unlist(
  lapply(1:nrow(data), function(e) maxdists[e,data$SINK[e]])
)

# plot distance from sinks
ggplot() + 
  geom_bar(data=data, stat='identity',
           mapping=aes(x=DATE, y=SINK.DIST, 
                       color=SINK, fill=SINK)) 
  # scale_x_datetime(date_breaks="6 months")


# plot with sinks
ggplot() + 
  geom_sf(data=data, mapping=aes(color=SINK), alpha=0.2) +
  geom_sf(data=max.pts, color='black', size=3) +
  geom_sf(data=max.pts, mapping=aes(color=SINK), size=1.5)

# plot UD kernel
plot(kernel.ref)
points(max, pch=19, col='black', cex=1)
points(max, pch=19, col='white', cex=0.5)

#Wayne Getz