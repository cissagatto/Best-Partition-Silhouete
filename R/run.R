##############################################################################
# BEST PARTITION SILHOUETTE ECC                                              #
# Copyright (C) 2022                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri           #
# Ferrandin | Federal University of Sao Carlos                               #
# (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos | Computer Department  #
# (DC: https://site.dc.ufscar.br/) | Program of Post Graduation in Computer  #
# Science (PPG-CC: http://ppgcc.dc.ufscar.br/) | Bioinformatics and Machine  #
# Learning Group (BIOMAL: http://www.biomal.ufscar.br/)                      #
#                                                                            #
##############################################################################


###########################################################################
#
###########################################################################
FolderRoot = "~/Best-Partition-Silhouette"
FolderScripts = "~/Best-Partition-Silhouette/R"



###############################################################################
# Runs for all datasets listed in the "datasets.csv" file
# n_dataset: number of the dataset in the "datasets.csv"
# number_cores: number of cores to paralell
# number_folds: number of folds for cross validation
# delete: if you want, or not, to delete all folders and files generated
########################################################################
executaBPS <- function(parameters){


  ##########################################################################
  FolderRoot = "~/Best-Partition-Silhouette"
  FolderScripts = "~/Best-Partition-Silhouette/R"

  # LOAD LIBRARIES
  setwd(FolderScripts)
  source("libraries.R")

  setwd(FolderScripts)
  source("utils.R")

  setwd(FolderScripts)
  source("validation.R")

  diretorios <- directories(parameters$Dataset.Name, 
                            parameters$Folder.Results,
                            parameters$Similarity)

  if(parameters$Number.Cores == 0){

    cat("\n\n##########################################################")
    cat("\n# Zero is a disallowed value for number_cores. Please      #")
    cat("\n# choose a value greater than or equal to 1.               #")
    cat("\n############################################################\n\n")

  } else {

    cl <- parallel::makeCluster(parameters$Number.Cores)
    doParallel::registerDoParallel(cl)
    print(cl)

    if(number_cores==1){
      cat("\n\n##########################################################")
      cat("\n# Running Sequentially!                                    #")
      cat("\n############################################################\n\n")
    } else {
      cat("\n\n############################################################")
      cat("\n# Running in parallel with ", parameters$Number.Cores, " cores!         #")
      cat("\n##############################################################\n\n")
    }
  }
  cl = cl

  retorno = list()


  cat("\n\n##########################################################")
  cat("\n# RUN: get labels                                          #")
  cat("\n############################################################\n\n")
  arquivo = paste(parameters$Folders$folderNamesLabels, "/" ,
                  dataset_name, "-NamesLabels.csv", sep="")
  namesLabels = data.frame(read.csv(arquivo))
  colnames(namesLabels) = c("id", "labels")
  namesLabels = c(namesLabels$labels)
  
  
  cat("\n\n###################################################################")
  cat("\n# ====> RUN: Get the label space                                    #")
  cat("\n#####################################################################\n\n")
  timeLabelSpace = system.time(resLS <- labelSpace(parameters))
  parameters$resLS = resLS
  

  cat("\n\n##########################################################")
  cat("\n# RUN: validate                                             #")
  cat("\n############################################################\n\n")
  timeVAl = system.time(resVal <- validate(parameters))
  
  
  cat("\n\n##########################################################")
  cat("\n# RUN: separando as partições                              #")
  cat("\n############################################################\n\n")
  timeBP = system.time(resBP <- bestPartitions(parameters))
  

  cat("\n\n##########################################################")
  cat("\n# RUN: Statistics:                                         #")
  cat("\n############################################################\n\n")
  timeASD = system.time(resASD <- asd(parameters))


  cat("\n\n##########################################################")
  cat("\n# RUN: Save Runtime                                        #")
  cat("\n############################################################\n\n")
  Runtime = rbind(timeVAl, timeBP, timeASD)
  setwd(diretorios$folderOutputDataset)
  name2 = paste(dataset_name, "Runtime-BPS-ECC.csv", sep="")
  write.csv(Runtime, name2)


  cat("\n\n##########################################################")
  cat("\n# RUN: Stop Parallel                                       #")
  cat("\n############################################################\n\n")
  parallel::stopCluster(cl)


  gc()
  cat("\n\n##########################################################")
  cat("\n# RUN: END                                                 #")
  cat("\n############################################################\n\n")

}


##########################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com           #
# Thank you very much!                                                   #
##########################################################################
