##################################################################################################
# Select the best hybrid partition using silhouete                                               #
# Copyright (C) 2021                                                                             #
#                                                                                                #
# This code is free software: you can redistribute it and/or modify it under the terms of the    #
# GNU General Public License as published by the Free Software Foundation, either version 3 of   #
# the License, or (at your option) any later version. This code is distributed in the hope       #
# that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for    #
# more details.                                                                                  #
#                                                                                                #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
#                                                                                                #
##################################################################################################


##################################################################################################
# Script 
##################################################################################################


##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
sistema = c(Sys.info())
shm = 0
FolderRoot = ""
if (sistema[1] == "Linux"){
  shm = 1
  FolderRoot = paste("/home/", sistema[7], "/Select-Partition-Silhouete", sep="")
} else {
  shm = 0
  FolderRoot = paste("C:/Users/", sistema[7], "/Select-Partition-Silhouete", sep="")
}
FolderScripts = paste(FolderRoot, "/scripts", sep="")



##################################################################################################
# LOAD INTERNAL LIBRARIES                                                                        #
##################################################################################################

FolderScripts = paste(FolderRoot, "/scripts/", sep="")

setwd(FolderScripts)
source("libraries.R")

setwd(FolderScripts)
source("utils.R")

setwd(FolderScripts)
source("bestPartition.R")


##################################################################################################
# Runs for all datasets listed in the "datasets.csv" file                                        #
# n_dataset: number of the dataset in the "datasets.csv"                                         #
# number_cores: number of cores to paralell                                                      #
# number_folds: number of folds for cross validation                                             # 
# delete: if you want, or not, to delete all folders and files generated                         #
##################################################################################################
sps <- function(number_dataset, number_cores, number_folds, folderResults){
  
  diretorios = directories(dataset_name, folderResults)
  
  if(number_cores == 0){
    cat("\n\n################################################################################################")
    cat("\n#Zero is a disallowed value for number_cores. Please choose a value greater than or equal to 1.#")
    cat("\n##################################################################################################\n\n") 
  } else {
    cl <- parallel::makeCluster(number_cores)
    doParallel::registerDoParallel(cl)
    print(cl)
    
    if(number_cores==1){
      cat("\n\n################################################################################################")
      cat("\n#Running Sequentially!                                                                          #")
      cat("\n##################################################################################################\n\n") 
    } else {
      cat("\n\n################################################################################################")
      cat("\n#Running in parallel with ", number_cores, " cores!                                             #")
      cat("\n##################################################################################################\n\n") 
    }
  }
  
  retorno = list()
  
  cat("\n\n################################################################################################")
  cat("\n#Run: Get dataset information: ", number_dataset, "                                                  #")
  ds = datasets[number_dataset,]
  names(ds)[1] = "Id"
  info = infoDataSet(ds)
  dataset_name = toString(ds$Name)
  cat("\n#Dataset: ", dataset_name)   
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n#Run: Get the names labels                                                                          #")
  setwd(diretorios$folderNamesLabels)
  namesLabels = data.frame(read.csv(paste(dataset_name, "-NamesLabels.csv", sep="")))
  namesLabels = c(namesLabels$x)
  cat("\n##################################################################################################\n\n") 
  
  cat("\n##################################################################################################\n\n") 
  cat("\n#Run: Get the label space                                                                       #")
  timeLabelSpace = system.time(resLS <- labelSpace(ds, dataset_name, number_folds, folderResults))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n##################################################################################################\n\n")   
  cat("\n#Run: Get partitions kohonen                                                                         #")
  timeBP  = system.time(resGPK <- bestPartition(ds, resLS, dataset_name, number_folds, folderResults))
  cat("\n##################################################################################################\n\n") 

  cat("\n\n################################################################################################")
  cat("\nRuntime")
  timesExecute = rbind(timeLabelSpace, timeBP)
  setwd(diretorios$folderReportsDataset)
  write.csv(timesExecute, "RunTime-BestPartition.csv")
  cat("\n##################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\n#Stop Parallel")
  on.exit(stopCluster(cl))
  cat("\n##################################################################################################")
  
  gc()
  cat("\n##################################################################################################")
  cat("\n#END OF executeGPKH                                                                              #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n") 
  
  if(interactive()==TRUE){ flush.console() }
  gc()
}

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################