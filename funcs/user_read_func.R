pacman::p_load(readr,purrr,stringr,lubridate)
source("funcs/col_pal_view.R") # load user and map details
# user info ---------------------------------------------------------------

user <- "mim"

# create dirs -------------------------------------------------------------

# month
month_current <- "templates"
if(!is.null(list.files(pattern=month_current))){
  dir.create(month_current)
} 

# user
if(!is.null(paste0(here::here(month_current,user)))){
  dir.create(paste0(here::here(month_current,user))) # create user folder  
}

# read in user data  
fr <- "mim.txt"  
ww <- readLines(paste0(here::here(month_current,user,fr))) %>% str_trim("both") # read in user data 
ww
fh <- ww[1]; fh
user <- ww[2]; user
sites <- ww[3] %>% str_split(",") %>% unlist %>% str_to_title() %>% str_trim("both"); sites
countries <- ww[4] %>% str_split(",") %>% unlist %>% str_trim("both"); countries
# countries <- ww[4] %>% str_to_title(); countries
 
# map data ----------------------------------------------------------------

# point params 
city_size <- 7
path_size <- 3

# map title params    
map_title <- ""
map_title_colour <- "white"  
map_title_size <- 35 
map_subtitle <- ""
title_xy <- "br"

# country params 
plot_country_labels <- T

# col params 
bg <- "#17242E"
fg <- "#FFFFFF"
border_col <- "#A8AE9C"
path_col <- "#F90F40"


# font params  
country_label_font <- fontlib[1]   
city_label_font <- fontlib[1]  
map_title_font <- fontlib[1]  

# label params
country_label_colour <- "#d3d3d3"
city_label_size <- 18
country_label_size <- 12
add_label_box <- F

# page params 
orientation <- "l"
units = "cm" # "cm" "mm"
paper_size <- "a3"

c("a1","a2","a3","a4","letter","legal")




