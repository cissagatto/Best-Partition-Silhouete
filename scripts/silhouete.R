cat("\n\n################################################################################################")
cat("\n# START SELECT BEST PARTITION WITH SILHOUETE                                                     #")
cat("\n##################################################################################################\n\n") 

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
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/Best-Partition-Silhouete", sep="")
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/Best-Partition-Silhouete", sep="")
}
FolderScripts = paste(FolderRoot, "/scripts", sep="")


##################################################################################################
# Options Configuration                                                                          #
##################################################################################################
options(java.parameters = "-Xmx32g")
options(show.error.messages = TRUE)
options(scipen=30)



##################################################################################################
# Read the dataset file with the information for each dataset                                    #
##################################################################################################
setwd(FolderRoot)
datasets <- data.frame(read.csv("datasets-hpml-k.csv"))



##################################################################################################
# ARGS COMMAND LINE                                                                              #
##################################################################################################
args <- commandArgs(TRUE)



##################################################################################################
# Get dataset information                                                                        #
##################################################################################################
ds <- datasets[as.numeric(args[1]),]
cat("\nHPML-K DS \t ", as.numeric(args[1]))



##################################################################################################
# Get the number of cores                                                                        #
##################################################################################################
number_cores <- as.numeric(args[2])
cat("\nHPML-K: cores \t ", number_cores)



##################################################################################################
# Get the number of folds                                                                        #
##################################################################################################
number_folds <- as.numeric(args[3])
cat("\nHPML-K: folds \t ", number_folds)



##################################################################################################
# Get the number of folds                                                                        #
##################################################################################################
folderResults <- toString(args[4])
cat("\nHPML-K: folder \t ", folderResults)



##################################################################################################
# Get dataset name                                                                               #
##################################################################################################
dataset_name <- toString(ds$Name) 
cat("\nHPML-K: nome \t ", dataset_name)


##################################################################################################
# DON'T RUN -- it's only for test the code
# ds = datasets[2,]
# dataset_name = ds$Name
# number_dataset = ds$Id
# number_cores = 10
# number_folds = 10
 folderResults = "/dev/shm/res"
##################################################################################################


##################################################################################################
# CONFIG THE FOLDER RESULTS                                                                      #
##################################################################################################
if(dir.exists(folderResults)==FALSE){
  dir.create(folderResults)
}

##################################################################################################
# LOAD RUN.R                                                                                     #
##################################################################################################
setwd(FolderScripts)
source("run.R")



##################################################################################################
diretorios = directories(dataset_name, folderResults)



########################################################################################################################
cat("\n Copy partitions from google drive")
destino = paste(diretorios$folderPartitions, "/", dataset_name, sep="")
origem = paste("cloud:[2021]ResultadosExperimentos/Generate-Partitions-Kohonen/", dataset_name, sep="")
comando1 = paste("rclone -v copy ", origem, " ", destino, sep="")
print(system(comando1))



##################################################################################################
# execute the code and get the total execution time                                              #
# n_dataset, number_cores, number_folds, folderResults                                           #
##################################################################################################
timeFinal <- system.time(results <- sps(args[1], number_cores, number_folds, folderResults))
print(timeFinal)

# DONT RUN ONLY FOR TEST
#timeFinal <- system.time(results <- sps(2, number_cores, number_folds, folderResults))

Folder = paste(diretorios$folderDatasetResults, "/", dataset_name, sep="")
if(dir.exists(Folder)==FALSE){
  dir.create(Folder)
}



##################################################################################################
cat("\nSave Rds")
str0 <- paste(diretorios$folderResultsDataset, "/", dataset_name, "-results-silhouete.rds", sep="")
save(results, file = str0)



##################################################################################################
cat("\nSave Rdata")
str1 <- paste(diretorios$folderResultsDataset, "/", dataset_name, "-results-silhouete.RData", sep="")
save(results, file = str1)



##################################################################################################
cat("\nCompress results")
setwd(diretorios$folderResultsDataset)
str3 = paste("tar -zcvf ", dataset_name, "-results-silhouete.tar.gz ", diretorios$folderResultsDataset, sep="")
print(system(str3))



##################################################################################################
cat("\nCopy file tar")
str4 = paste("cp ", diretorios$folderResultsDataset, "/", dataset_name, "-results-silhouete.tar.gz ", Folder, sep="")
print(system(str4))



########################################################################################################################
cat("\n Copy Results to google drive")
destino = paste("cloud:[2021]ResultadosExperimentos/Best-Partition-Silhouete/", dataset_name, sep="")
comando1 = paste("rclone -v copy ", Folder, " ", destino, sep="")
print(system(comando1))



########################################################################################################################
cat("\n Copy Outupt to google drive")
origem = diretorios$folderOutputDataset
destino = paste("cloud:[2021]ResultadosExperimentos/Best-Partition-Silhouete/", dataset_name, sep="")
comando2 = paste("rclone -v copy ", origem, " ", destino, sep="")
print(system(comando2))



##################################################################################################
#cat("\nDelete folder results temporary")
#str5 = paste("rm -r ", diretorios$folderResults, sep="")
#print(system(str5))



##################################################################################################
#cat("\nDelete folder output dataset")
#str7 = paste("rm -r ", diretorios$folderOutputDataset, sep="")
#print(system(str7))



##################################################################################################
#cat("\nDelete folder partitions")
#str6 = paste("rm -r ", diretorios$folderPartitions, "/", dataset_name, sep="")
#print(system(str6))



##################################################################################################
cat("\nDelete folder specific dataset")
str8 = paste("rm -r ", diretorios$folderSpecificDataset, sep="")
print(system(str8))


##################################################################################################
#cat("\nDelete folder results")
#str9 = paste("rm -r ", Folder, sep="")
#print(system(str9))



##################################################################################################
cat("\nClear R objects")
rm(list = ls())



##################################################################################################
cat("\nGarbage collector")
gc()



cat("\n##################################################################################################")
cat("\n# END OF SELECT BEST PARTITION WITH SILHOUETE. THANKS GOD !!                                     #")
cat("\n##################################################################################################")
cat("\n\n\n\n") 

if(interactive()==TRUE){ flush.console() }

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################