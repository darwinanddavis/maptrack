# to do 
# bbox xylims need to be the same ratio as output dims, e.g. 9*15 in = 1.67
# set bbox xylims to same as paper size  
# set expand = F in coord_sf and retest above and good grom bbox 
# add US state border data (https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html)
# check googleway option for getting driving directions (https://stackoverflow.com/questions/40077798/r-plotting-multiple-decoded-polylines-using-leaflet)
# try enormous width and height or pdf() and ggsave()  
# try ragg package for removing margins https://github.com/r-lib/ragg

# template minimal 

# steps 
# upload csv to google maps  # https://www.google.com/maps/d/u/0/?hl=en
# select layer and add driving directions. fix any errors here. 
# download klm > download only directions data > download only kml  
# read in klm data into r

plot_final_pdf <- F
source("funcs/user_read_func.R") # load user and map details
source("funcs/page_print_func.R") # load page print func 
source("funcs/add_font_func.R") # load font lib func 

if(!is.null(paste0(here::here(month_current,user),"/",user,".Rda"))){
  cat("Reading in final city_df from dir\n")
  city_df <- readRDS(paste0(here::here(month_current,user),"/",user,".Rda")) 
} 


# packages  ---------------------------------------------------------------

# pcks
pacman::p_load(here,sf,RColorBrewer,dplyr,ggmap,RgoogleMaps,sp,maptools,scales,rgdal,ggplot2,jsonlite,readr,devtools,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,reshape2,grid,rnaturalearth,rnaturalearthdata,ggtext,purrr)
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

world %>% class
st_centroid(world$geometry) 
# data(us.cities) # option 2 for us cities  

# find missing cities

find_city <- NULL
if(!is.null(find_city)){
  find_city_save <- world_cities[str_which(world_cities$name,c(find_city)),"name"] 
  find_city_save
  sites[5] <- find_city_save[2] # replace missing city in df
}

# load data  --------------------------------------------------------------

city_df <- world_cities %>% 
  filter(country.etc %in% countries, name %in% sites) %>% 
  select(lat = lat,
         lon = long,
         city = name
  ) %>% 
  arrange(match(city,sites))   # reorder to match route path 

# remove extra cities if necessary
# city_df <- city_df[-c(4,10),]

write_csv(city_df,here::here(month_current,user,paste0(fh,".csv")))

# add global label nudge position to df -------------------------------------------------------------------
label_xy <- data.frame( # global xy label positions, positions 0 to 7 (uses ggtext positioning)
  "hjust" = c(0.5,0,0,0,0.5,1,1,1),
  "vjust" = c(0,0,0.5,1,1,1,0.5,0)
)
label_nudge <- data.frame( # global xy label nudge positions, positions 0 to 7 (uses ggtext positioning)
  "nudge_x" = rep(0,city_df %>% nrow),
  "nudge_y" = rep(0.1,city_df %>% nrow)
)

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------
# read in kml data ------------------------------------------------------------

# read in kml data from my google maps (drive route)
path <- sf::st_read(here::here(month_current,user,paste0(fh,".kml")))

# high res map
d <- map_data("worldHires"
              # , countries # select which countries to plot  
)
usa <- map_data("worldHires",
              c("USA","Canada","Mexico")
              # , countries # select which countries to plot  
)

# nudge labels ------------------------------------------------------------
add_label_box = F

# add text label positioning columns
cat(rep("\n",3), "Total no. of cities = ",city_df %>% nrow)
hv_just <- c(1,1,2,7,1,3,3,6,7,7) # set city label xy
city_df[,c("hjust","vjust")] <- label_xy[hv_just,] # add hust/vjust
city_df[,c("nudge_x","nudge_y")] <- label_nudge  # add nudge_x/nudge_y


# city_df[9,"city"] <- "Ho Chi Minh"

# check what labels need to be nudged  
require(ggtext) 
text_test <- ggplot() + geom_sf(data=path,color=path_col,size=path_size) + geom_point(data=city_df,aes(lon,lat),col=path_col,size=city_size) + 
  if(add_label_box==F){geom_richtext(data=city_df,aes(lon,lat,label=city,family=city_label_font,hjust=hjust,vjust=vjust), color=path_col, size = city_label_size, fill = NA, label.color = NA, nudge_x = city_df$nudge_x,nudge_y = city_df$nudge_y)
    # add label box 
  }else{
    geom_richtext(data=city_df,aes(lon,lat,label=city,family=city_label_font,size=city_label_size, color=path_col,hjust=hjust,vjust=vjust), size = city_label_size, fill = "white", color = path_col, label.color = path_col, label.padding = grid::unit(rep(7.5, 4), "pt"),nudge_x = city_df$nudge_x,nudge_y = city_df$nudge_y)
  } 
ggsave(here::here(month_current,user,paste0(paste(fh,user,"labels_to_nudge",sep="_"),".png")), text_test, width = 21, height = 29.7, units = units)

# adjust labels xy pos as needed
city_to_nudge <- c(2,3,4,6,7,8,9,10)

# nudge labels in df
city_df <- city_df %>% 
  mutate(nudge_x = nudge_x %>% 
           replace(city_to_nudge,
                   c(0.5,-0.5,-0.2,0.2,0.2,0.2,-0.2,-0.2)),
         nudge_y = nudge_y %>% 
           replace(city_to_nudge,
                   c(0,0,0,0,0,-0.2,0,0.2)))
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# plot --------------------------------------------------------------------
# save final city_df to dir
saveRDS(city_df,paste0(here::here(month_current,user),"/",user,".Rda")) 

source("funcs/user_read_func.R") # load user and map details
source("funcs/page_print_dims.R") # load user and map details
source("funcs/title_xy.R") # load user and map details

graphics.off()
# bbox[1:2] %>% diff
# bbox[3:4] %>% diff

print_func(paper_size,orientation) # print func

# set bbox
# xybuff <- 0 # xybuff for page width/length
# if(orientation=="l"){
#   bbox <- city_df %>% summarise(min(lon)-(xybuff),max(lon)+(xybuff), # addbuffer
#                                 min(lat)-xybuff,max(lat)+xybuff,
#                                 median(c(lat,lon))) %>% unlist
# }else{
#   bbox <- city_df %>% summarise(min(lon)-(xybuff*2),max(lon)+(xybuff * 2), # add buffer
#                                 min(lat)-(xybuff),max(lat)+(xybuff),
#                                 median(c(lat,lon))) %>% unlist
# }

if(orientation=="l"){
  bbox <- city_df %>% summarise(mean(lon)-(height/2), # x axis
                                mean(lon)+(height/2), 
                                mean(lat)-(width/2),
                                mean(lat)+(width/2)) %>% unlist 
}else{
  bbox <- city_df %>% summarise(mean(lon)-(width/2), # x axis
                                mean(lon)+(width/2), 
                                mean(lat)-(height/2),
                                mean(lat)+(height/2)) %>% unlist 
}

# title and subtitle position // title_xy.R
map_title_final <- title_xy_func(title_xy,0,2)

# color palette 
# https://studio.mapbox.com/styles/darwinanddavis/cjx3sqa8obnpg1ctdcx5mxan8/edit/#11.15/46.2378/6.1383
# recover default plot margins 
theme_nothing()$plot.margin

p <- ggplot() + 
  geom_polygon(data=usa,aes(x=long, y=lat, group = group), fill = fg, col=border_col,size=0.1) +
  geom_polygon(data=d,aes(x=long, y=lat, group = group), fill = fg, col=border_col,size=0.1) +
  geom_sf(data=path,color=path_col,size=path_size) + # add path
  geom_point(data=city_df,aes(lon,lat),col=path_col,size=city_size) + # add cities
  # geom_richtext(data=map_title_final,aes(tx,ty,label=title,hjust=hjust,vjust=vjust),color=map_title_colour, family=map_title_font, size = map_title_size, fill = NA, label.color = NA) + # map title 
  # geom_richtext(data=map_title_final,aes(tx,ty,label=subtitle,hjust=hjust,vjust=vjust),color=map_title_colour, family=map_title_font, size = map_title_size-10, fill = NA, label.color = NA,nudge_y = -1.5) + # map subtitle
  # coord_map("mollweide",xlim = c(bbox[1],bbox[2]), ylim = c(bbox[3],bbox[4])) +
  coord_sf(xlim = c(bbox[1],bbox[2]), ylim = c(bbox[3],bbox[4])) + # bbox
  # coord_sf(xlim=c(-75,-81),ylim=c(-2,8)) + # zoom window  
  # geom_hline(yintercept = 0,linetype="dotted",color=border_col,size=0.5) + # add equator
  theme_nothing() +
  theme(panel.grid.major = element_line(colour = bg),
        plot.background = element_rect(fill = bg),
        axis.text = element_blank(), 
        axis.ticks.length=unit(0, "null"),
        # plot.margin=unit(c(-10,-10,-10,-10),NULL),
        panel.ontop = F
        ) +
  labs(x = NULL, y = NULL) + 
  # no label box
  if(add_label_box==F){geom_richtext(data=city_df,aes(lon,lat,label=city,family=city_label_font,hjust=hjust,vjust=vjust), color=path_col, size = city_label_size, fill = NA, label.color = NA, nudge_x = city_df$nudge_x,nudge_y = city_df$nudge_y)
    # add label box 
  }else{
    geom_richtext(data=city_df,aes(lon,lat,label=city,family=city_label_font,size=city_label_size, color=path_col,hjust=hjust,vjust=vjust), size = city_label_size, fill = "white", color = path_col, label.color = path_col, label.padding = grid::unit(rep(7.5, 4), "pt"),nudge_x = city_df$nudge_x,nudge_y = city_df$nudge_y)
    } 
if(plot_country_labels==T){ # add country labels 
  p <- p + geom_text(data= world_points,aes(x=X, y=Y, label=name, family=country_label_font),color = country_label_colour, size = country_label_size, check_overlap = T)
}
p
dev.off()

# theme(plot.margin=unit(c(0,0,0,0),"mm")) +
# coord_map("mercator",xlim=c(-75,-81),ylim=c(-2,8))
# geom_polygon(col = "blue", fill = NA) # get country borders back
# coord_fixed(1.5)
# dev.off()



# cowplot::save_plot(paste0(here::here(month_current,user),"/",paste(fh,user,sep="_"),"_cow.pdf"),p, ncol=1,nrow=1,base_width = 16.5, base_height = NULL, base_asp = 2.4)
ggsave(here::here(month_current,user) %>% paste0("/",paste(fh,user,sep="_"),".pdf"), p, width = height, height = width,limitsize = F,units = units)
if(plot_final_pdf==T){
  if(orientation=="p"){
    ggsave(here::here(month_current,user) %>% paste0("/",paste(user,"final",sep="_"),".pdf"), p, width = width, height = height, dpi = "retina", units = units, limitsize = F)
  }else{
    ggsave(here::here(month_current,user) %>% paste0("/",paste(user,"final",sep="_"),".pdf"), p, width = height, height = width, dpi = "retina", units = units, limitsize = F)
  }
  write_lines(c(width,height,orientation,paper_size),paste0(here::here(month_current,user),"/",paste(fh),".txt"),append = T)
}

# change paper orientation
# https://stackoverflow.com/questions/16318396/ggplot2-plot-fill-page-in-landscape-pdf






?mapproj::mapproject()



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