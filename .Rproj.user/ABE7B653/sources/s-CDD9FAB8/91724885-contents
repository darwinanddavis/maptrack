# col palettes  
require(scales)
require(dplyr)
require(ggtext)
require(ggplot2)
require(ggthemes)
require(ggimage)
require(here)
require(scales)

# https://colour.adobe.com/trends
"http://c.sm.mapstack.stamen.com/(positron,(mapbox-water,$3BCDD5[hsl-color]),(parks,$291C9C[hsl-color]))/{z}/{x}/{y}.png"

cp <- paste0("#",c(
  "AAD3D2",  # bg
  "f2e6d8", # fg
  "9acaac" , # border
  "001785"# points/path
))
show_col(cp)

# map
set.seed(1)
mm <- map_data("world",region = "Canada") # test map 
cpmap <- function(bg,fg,border_col,point_col){
  md <- data.frame("x"=sample(-125:-100,5,T),
                   "y" = sample(50:65,5,T)
  )
  cl <- paste0("City no. ",1:length(md$x))
  ggplot() + 
    geom_polygon(data=mm,aes(x=long, y=lat, group = group), fill = fg, col=border_col,size=0.1) + 
    geom_point(data=md,aes(x=x,y=y),col=point_col,size=5) +
    theme_nothing() +
    theme(panel.grid.major = element_line(colour = bg),plot.background = element_rect(fill = bg), panel.ontop = F) +
    geom_richtext(data=md,aes(x,y,label=cl,size = 3),color=point_col,fill = NA, label.color = NA, nudge_x = 2, nudge_y = 2) +
    geom_richtext(aes(x = mm$long %>% min + 10, y = mm$lat %>% max-5,label= paste(cp,collapse = "<br>"),
                      size = 3),color="black")
  ggsave(paste0(here(),"/col_templates/",paste(cp,collapse = "_"),".jpeg"),width = 10,height = 7,units="cm")
  
}
cpmap(cp[1],cp[2],cp[3],cp[4])

# beyonce 
library(beyonce)
beyonce_palette(7) %>% cat


