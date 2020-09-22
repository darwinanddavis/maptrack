# import new fonts  
pacman::p_load(ggplot2,ggthemes,dplyr)

# get file name  from fontbook.app 
newfonts <- c("A DAY WITHOUT SUN.otf",
              "MUNICH SANS.ttf",
              "Moon Flower.ttf",
              "DK Lemon Yellow Sun.otf",
              "beacon.ttf",
              "BREVE2.ttf",
              "AHundredMiles.ttf",
              "Billy Ohio.ttf"
              ) 

# font stocks 
fontlib <- c("adaywithoutsun",
             "munich",
             "moonflower",
             "dklemon",
             "beacon",
             "breve",
             "ahundredmiles",
             "billyohio"
)
fontname <- fontlib #%>% tail(1) # get last added font  

# load font in r
tt <- "MY IDEAL VACATION" # title
library(showtext)
## Add the font with the corresponding font faces
n <- c()
for(n in 1:length(newfonts)){
font_add(fontname[n],
         regular = newfonts[n],
         bold = newfonts[n]
         )
}
## automatically use showtext to render plots
showtext_auto(enable = T)

# works with annotate()  
font_func <- function(f){
  p <- ggplot(NULL, aes(x = 0, y = 10)) + xlim(-5, 10) + ylim(-5, 10)
  for(i in 1:length(fontlib)){ 
    p <- p + annotate("text", 1, i, label = paste0(tt," (",fontlib[i],")"),
                      family = fontlib[i], size = 15) + 
      theme_classic()
  }
  ggsave("font_lib.png", p, width = 15, height = 9)
}
font_func(fontlib)


