library(plumber)
library(jsonlite)

# mpg  disp  hp drat    wt  qsec EngConf cylinders gears carbs
#19.2 400.0 175 3.08 3.845 17.05       V         8     3     2
#--------------------------------------------------
# Read in model 
#--------------------------------------------------
model <- readr::read_rds("mtcarslm.rds")

#* @apiTitle Model to predict mpg 

#* @param disp     
#* @param hp       
#* @param drat     
#* @param wt       
#* @param qsec     
#* @param EngConf  
#* @param cylinders
#* @param gears    
#* @param carbs    
#* @post /predict

predictions <- function(
  disp     ,
  hp       ,
  drat     ,
  wt       ,
  qsec     ,
  EngConf  ,
  cylinders,
  gears,    
  carbs )
  
{
  
  disp      <-   as.numeric(disp)
  hp        <-   as.numeric(hp)
  drat      <-   as.numeric(drat)
  wt        <-   as.numeric(wt)
  qsec      <-   as.numeric(qsec)
  EngConf   <-   as.character(EngConf)
  cylinders <-   as.character(cylinders)
  gears     <-   as.character(gears)
  carbs     <-   as.character(carbs)

#Pre-processing the data
X.new <- data.frame(  
                              
  disp      =   disp,
  hp        =   hp,
  drat      =   drat,
  wt        =   wt,
  qsec      =   qsec,
  EngConf   =   as.factor(EngConf),
  cylinders =   as.factor(cylinders),
  gears     =   as.factor(gears),
  carbs     =   as.factor(carbs))
                            
                            
 #predict based on input
 
 y.pred <- predict(model, newdata = X.new)
 return(y.pred)
                            
}


