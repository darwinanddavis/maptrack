# printing special with us letter dims in preview adds correct white border  

print_func <- function(paper_size,orientation){
  width <- c(); height <- c()
    if(paper_size == "a4"){
      width <- 21
      height <- 29.7
      units <- "cm"
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
  units <- "cm"
  orientation <<- orientation
  # landscape
  if(orientation=="p"){
    pdf(paste0(here::here(month_current,user),"/",paste(user,paper_size,orientation,sep="_"),".pdf"), width=width, height=height, paper = "special",onefile = T)
  }else{
    pdf(paste0(here::here(month_current,user),"/",paste(user,paper_size,orientation,sep="_"),".pdf"), width=height, height=width, paper = "special", onefile = T)
  }
  
}




