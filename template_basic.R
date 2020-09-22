# to do 

# template basic   

# steps 
# upload csv to google maps  
# select layer and add driving directions. fix any errors here. 
# download klm > download only directions data > download only kml  
# read in klm data into r

plot_final_pdf <- F

# user info ---------------------------------------------------------------

fh <- "eu"
user <- "testbbox"
date <- Sys.Date()
sites <- c(
  "Zurich",
  "Munich",
  "Berlin",
  "Leipzig",
  "Dresden",
  "Prague"
)
countries <- c("Switzerland","Germany","Czech Republic")
country_label_colour <- "red"

# point params 
city_size <- 4
path_size <- 0.8

# map title params    
map_title <- "MY VACATION PLANS"  
map_title_colour <- "black"  
map_title_size <- 20
map_subtitle <- "April, 2019"

# country params 
plot_country_labels <- T
  
# col params 
bg <- cp[1]# "#f2f0ee" # "#c1e4f5" 
fg <- cp[2] #"#7e3030" #10163c"
border_col <- cp[3] #"#c7d2f5" 
path_col <- cp[4] # "#fbdbee" "#7e3030" #f4d29f"

# font params  
country_label_font <- fontlib[4]   
city_label_font <- fontlib[4]  
map_title_font <- fontlib[4]  

# print params 
width <- 9
height <- 15
units = "in" # "cm" "mm"


# packages  ---------------------------------------------------------------

require(here)
# here::set_here("/Users/malishev/Documents/ecolab/")

# pcks
pacman::p_load(here,sf,RColorBrewer,dplyr,ggmap,RgoogleMaps,sp,maptools,scales,rgdal,ggplot2,jsonlite,readr,devtools,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,reshape2,grid,rnaturalearth,rnaturalearthdata,ggtext)
# install RgoogleMaps and OpenStreetMap separately, for some reason  
# install.packages("RgoogleMaps"); library(RgoogleMaps)
# install.packages("OpenStreetMap"); library(OpenStreetMap)
# install.packages("googleway") ; library(googleway)
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# 
# # install geojsonio from github and source
# devtools::install_github("ropensci/geojsonio"); library(geojsonio)

# high res map version 
# https://hecate.hakai.org/rguide/mapping-in-r.html
require(mapdata) # high res data
require(ggsn) # north symbols and scale bars


# load world latlon, region, country names --------------------------------

# get city names
data(world.cities) # /maps
world_cities <- world.cities

# get country names  
world <- ne_countries(scale = "large", returnclass = "sf") # /rnaturalearth /rnaturalearthdata
world_points<- st_centroid(world) # /sf
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

# data(us.cities) # option 2 for us cities  


# load data  --------------------------------------------------------------

city_df <- world_cities %>% 
  filter(country.etc %in% countries, name %in% sites) %>% 
  select(lat = lat,
         lon = long,
         city = name
  ) %>% 
  mutate(nudge_x = rep(0,length(sites)), # add xy nudge for labels  
         nudge_y = rep(0.1,length(sites))) %>% 
  arrange(match(city,sites)) # reorder to match route path  
readr::write_csv(city_df,here::here(paste0(fh,".csv")))

# read in data ------------------------------------------------------------

# read in kml data from my google maps (drive route)
# https://www.google.com/maps/d/u/0/?hl=en
path <- sf::st_read(paste0(fh,".kml"))

# high res map
d <- map_data("worldHires"
              # , countries # select which countries to plot  
              )

# plot --------------------------------------------------------------------
dput(par(no.readonly=T)) # reset graphical params (doesn't work for ggplot)
par()
require(grid)

# set bbox
xybuff <- 3
bbox <- city_df %>% summarise(min(lon)-xybuff,max(lon)+xybuff, # add 5 degree buffer  
                              min(lat)-xybuff,max(lat)+xybuff,
                              median(c(lat,lon))) %>% unlist
# color palette 
# https://studio.mapbox.com/styles/darwinanddavis/cjx3sqa8obnpg1ctdcx5mxan8/edit/#11.15/46.2378/6.1383

p <- ggplot() + 
  geom_polygon(data=d,aes(x=long, y=lat, group = group), fill = fg, 
               col=border_col,
               size=0.1) +
  # geom_hline(yintercept = 0,linetype="dotted",color=border_col,size=0.5) + # add equator
  geom_sf(data=path,color=path_col,size=path_size) + # add ziggy path
  geom_point(data=city_df,aes(lon,lat),col=path_col,size=city_size) + # add cities
  geom_text(data=city_df,aes(lon,lat,label=city,family=city_label_font,size = 6),nudge_y = 0.5) + # city labels 
  annotate(geom = "text", # map title
           x = bbox[1] + 5, y = bbox[4] - 1,
           label = map_title, family = map_title_font, fontface = "bold", color = map_title_colour, size = map_title_size) +
  coord_sf(xlim = c(bbox[1],bbox[2]), ylim = c(bbox[3],bbox[4])) + # bbox 
  # coord_sf(xlim=c(-75.0,-81.0),ylim=c(8,2),expand=F) #+ # zoom window
  # coord_sf(xlim=c(-75,-81),ylim=c(-2,8)) + # zoom window  
  # theme_tufte(ticks=F) +
  theme_nothing() +
  theme(
    panel.grid.major = element_line(colour = bg),
    plot.background = element_rect(fill = bg), 
    panel.ontop = F # make panel grid invisible  
  )
if(plot_country_labels==T){
  p <- p + 
    geom_text(data= world_points,
              aes(x=X, y=Y, label=name, family=country_label_font),
              color = "black", alpha = 0.3,
              fontface = "bold", size = 5, check_overlap = T)
}

# theme(plot.margin=unit(c(0,0,0,0),"mm")) +
# coord_map("mercator",xlim=c(-75,-81),ylim=c(-2,8))
# geom_polygon(col = "blue", fill = NA) # get country borders back
# coord_fixed(1.5)
# dev.off()
ggsave(paste0(paste(fh,user,sep="_"),".png"), p, width = 9, height = 15)
if(plot_final_pdf==T){
  ggsave(paste0(paste(fh,user,sep="_"),".pdf"), p, width = width, height = height, dpi = "retina", limitsize = F)
}


sites <- c("Houston",
           "Austin",
           "New Orleans",
           "Jackson",
           "Memphis",
           "Nashville",
           "Atlanta",
           "Jacksonville",
           "Melbourne",
           "Miami"
)


# for "conic"
d <- map_data("worldHires", countries) 
ggplot(d) + 
  geom_polygon(aes(x=long, y = lat, group = group)) + 
  theme_bw() +
  coord_map("", lat0 = 18, xlim=c(210, 237), ylim=c(46,62)) # +
  # scale_shape_identity()
# coord_map("", lat0 = bbox[5], xlim=c(bbox[1],bbox[2]), ylim=c(bbox[3],bbox[4])) +


############################ city/country text for ggplot ############################
graphics.off()
# works with annotate (need to save to dir to see custom font)    
g <- ggplot() + 
  geom_polygon(data=d,aes(x=long, y=lat, group = group), fill = fg, col=border_col,size=0.1) +
  geom_point(data=city_df,aes(lon,lat),col=path_col,size=city_size) + # add cities
  geom_text(data=city_df,aes(x=lon,y=lat,label=city,family=fontlib[4],size = 6)) + # city labels 
  annotate(geom = "text", # map title
           x = bbox[2] - 1, y = bbox[4] - 1, 
           label = map_title, fontface = "bold", color = map_title_colour, size = map_title_size) +
  coord_sf(xlim = c(bbox[1],bbox[2]), ylim = c(bbox[3],bbox[4])) # bbox 
# add country labels
if(plot_country_labels==T){
  g <- g + 
    geom_text(data= world_points,
              aes(x=X, y=Y, label=name, family=fontlib[3]),
              color = "black", fontface = "bold", size = 10, check_overlap = T)
}
g
ggsave("eu_font_annotate.png", g, width = 15, height = 9)


# sf
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

# gg text  
gg <- ggplot(data = world) +
  geom_sf() +
  # country labels  
  geom_text(data= world_points,
            aes(x=X, y=Y, label=name, family=fontlib[3]),
            color = "black", fontface = "bold", size = 10, check_overlap = T) +
  # map title
  annotate(geom = "text", x = bbox[2] - 1, y = bbox[4] - 1, 
           label = map_title, fontface = "bold", color = map_title_colour, size = map_title_size) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = F)
ggsave("eu_font_gGtext.png", gg, width = 15, height = 9)


########################################################################



## example 10
# http://dangoldin.com/2014/02/05/visualizing-gps-data-in-r/
library(plotKML)
library(maps)



# errors june 2020   ------------------------------------------------------

#  Error in st_normalize.sfc(x, c(x_range[1], y_range[1], x_range[2], y_range[2])) : 
# domain must have a positive range
# https://stackoverflow.com/questions/62182238/error-in-st-normalize-sfcx-cx-range1-y-range1-x-range2-y-range2
# https://github.com/hadley/ggplot2-book/issues/177