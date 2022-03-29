library(plumber)
library(caret)
library(jsonlite)
library(ipred)
library(e1071)
library(yaml)

# Utilise post method to send JSON unseen data, in the same 
# format as our dataset

#--------------------------------------------------
# Read in model 
#--------------------------------------------------
model <- readr::read_rds("glmmodel.rds")
model$modelInfo

#* Test connection
#* @get /connection-status

function(){
  list(status = "Connection to Stranded Patient API successful", 
       time = Sys.time(),
       username = Sys.getenv("USERNAME"))
}

## Lets make the predictions

#* @param supplemental_oxygen
#* @param ICPEVDRAIN
#* @param ICPPARENCH                   
#* @param spinal_cord_injury           
#* @param fracture_pelvis              
#* @param fracture_spinal_vertebra     
#* @param neurosurg_tbi                
#* @param major_orthopedic_surgery     
#* @param major_thoracic_surgery       
#* @param major_vascular_surgery       
#* @param intubation                   
#* @param transfusion                  
#* @param AGEYEARS                     
#* @param totalgcs                     
#* @get /predict
predictions <- function(supplemental_oxygen          ,
                        ICPEVDRAIN                   ,
                        ICPPARENCH                   ,
                        spinal_cord_injury           ,
                        fracture_pelvis              ,
                        fracture_spinal_vertebra     ,
                        neurosurg_tbi                ,
                        major_orthopedic_surgery     ,
                        major_thoracic_surgery       ,
                        major_vascular_surgery       ,
                        intubation                   ,
                        transfusion                  ,
                        AGEYEARS                     ,
                        totalgcs                     )
{
  supplemental_oxygen           <- as.character(supplemental_oxygen          )
  ICPEVDRAIN                    <- as.character(ICPEVDRAIN                   )
  ICPPARENCH                    <- as.character(ICPPARENCH                   )
  spinal_cord_injury            <- as.character(spinal_cord_injury           )
  fracture_pelvis               <- as.character(fracture_pelvis              )
  fracture_spinal_vertebra      <- as.character(fracture_spinal_vertebra     )
  neurosurg_tbi                 <- as.character(neurosurg_tbi                )
  major_orthopedic_surgery      <- as.character(major_orthopedic_surgery     )
  major_thoracic_surgery        <- as.character(major_thoracic_surgery       )
  major_vascular_surgery        <- as.character(major_vascular_surgery       )
  intubation                    <- as.character(intubation                   )
  transfusion                   <- as.character(transfusion                  )
  AGEYEARS                      <- as.integer(AGEYEARS                       )
  totalgcs                      <- as.integer(totalgcs                       )
  
  X.new <- data.frame(  supplemental_oxygen           =  as.factor(supplemental_oxygen      ),
                        ICPEVDRAIN                    =  as.factor(ICPEVDRAIN               ),
                        ICPPARENCH                    =  as.factor(ICPPARENCH               ),
                        spinal_cord_injury            =  as.factor(spinal_cord_injury       ),
                        fracture_pelvis               =  as.factor(fracture_pelvis          ),
                        fracture_spinal_vertebra      =  as.factor(fracture_spinal_vertebra ),
                        neurosurg_tbi                 =  as.factor(neurosurg_tbi            ),
                        major_orthopedic_surgery      =  as.factor(major_orthopedic_surgery ),
                        major_thoracic_surgery        =  as.factor(major_thoracic_surgery   ),
                        major_vascular_surgery        =  as.factor(major_vascular_surgery   ),
                        intubation                    =  as.factor(intubation               ),
                        transfusion                   =  as.factor(transfusion              ),
                        AGEYEARS                      =  as.integer(AGEYEARS                ),
                        totalgcs                      =  as.integer(totalgcs                ))
  #predict based on input
  
  
  ##predict(iris_rf, new_data= X.new, type ="class")
  y.pred <- model$NewPredictions(model = modellist$modelobject, newdata = X.new)
  
  response <- ifelse(y.pred$Yes >= 0.001587182, "High-risk", "Low-risk")
  
  return(response)
  
}


