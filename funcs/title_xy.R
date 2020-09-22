title_xy_func <- function(title_xy,tx_buff,ty_buff){
  tx <- c();ty <- c()
  db <- 1 # default xy buff from page edge
  if(title_xy=="tl"){
    tx <-  bbox[1] + db + tx_buff
    ty <-  bbox[4] - db - ty_buff
    hjust <- 0
    vjust <- 1
  }
  if(title_xy=="tr"){
    tx <-  bbox[2] - db - tx_buff
    ty <-  bbox[4] - db - ty_buff
    hjust <- 1
    vjust <- 1
  }
  if(title_xy=="br"){
    tx <-  bbox[2] - db - tx_buff
    ty <-  bbox[3] + db + ty_buff
    hjust <- 1
    vjust <- 1
  }
  if(title_xy=="bl"){
    tx <-  bbox[1] + db + tx_buff
    ty <-  bbox[3] + db + ty_buff
    hjust <- 0
    vjust <- 0
  }
  if(title_xy=="free"){
    tx <-  city_df %>% pull(lon) %>% mean
    ty <-  city_df %>% pull(lat) %>% mean
    hjust <- 0
    vjust <- 1
  }
  return(
    tibble("tx"=tx,"ty"=ty,"title"=map_title,"subtitle"=map_subtitle,"hjust"=hjust,"vjust"=vjust)
    )
}


       