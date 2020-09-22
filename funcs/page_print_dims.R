if(paper_size == "a4"){
  width <- 21
  height <- 29.7
}
if(paper_size == "a3"){
  width <- 29.7
  height <- 42
}
if(paper_size == "a2"){
  width <- 42
  height <- 59.4
}
if(paper_size == "a1"){
  width <- 59.4
  height <- 84.1
}
if(paper_size == "letter"){
  width <- 21.6
  height <- 27.9
}
if(paper_size == "legal"){
  width <- 21.6
  height <- 35.6
} 
cat("paper =", paper_size,"\nwidth = ", width, "\nheight = ", height)
# read in page dims list 
page_dims <- readRDS(paste0(here("funcs"),"/page_dims.Rda")) 

