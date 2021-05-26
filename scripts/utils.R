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
# Script 2 - Utils                                                                               #
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
# FUNCTION DIRECTORIES                                                                           #
#   Objective:                                                                                   #
#      Creates all the necessary folders for the project. These are the main folders that must   # 
#      be created and used before the script starts to run                                       #  
#   Parameters:                                                                                  #
#      None                                                                                      #
#   Return:                                                                                      #
#      All path directories                                                                      #
##################################################################################################
directories <- function(dataset_name, folderResults){
  
  retorno = list()
  
  if(dir.exists(folderResults) == TRUE){
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  } else {
    dir.create(folderResults)
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  }
  
  folderUtils = paste(FolderRoot, "/utils", sep="")
  if(dir.exists(folderUtils) == TRUE){
    setwd(folderUtils)
    dir_folderUtils = dir(folderUtils)
    n_folderUtils = length(dir_folderUtils)
  } else {
    dir.create(folderUtils)
    setwd(folderUtils)
    dir_folderUtils = dir(folderUtils)
    n_folderUtils = length(dir_folderUtils)
  }
  
  folderFinalPartitions = paste(FolderRoot, "/FinalPartitions", sep="")
  if(dir.exists(folderFinalPartitions) == TRUE){
    setwd(folderFinalPartitions)
    dir_folderFinalPartitions = dir(folderFinalPartitions)
    n_folderFinalPartitions = length(dir_folderFinalPartitions)
  } else {
    dir.create(folderFinalPartitions)
    setwd(folderFinalPartitions)
    dir_folderFinalPartitions = dir(folderFinalPartitions)
    n_folderFinalPartitions = length(dir_folderFinalPartitions)
  }
  
  folderDatasets = paste(FolderRoot, "/datasets", sep="")
  if(dir.exists(folderDatasets) == TRUE){
    setwd(folderDatasets)
    dir_folderDatasets = dir(folderDatasets)
    n_folderDatasets = length(dir_folderDatasets)
  } else {
    dir.create(folderDatasets)
    setwd(folderDatasets)
    dir_folderDatasets = dir(folderDatasets)
    n_folderDatasets = length(dir_folderDatasets)
  }
  
  folderDatasetX = paste(folderDatasets, "/", dataset_name, sep="")
  if(dir.exists(folderDatasetX) == TRUE){
    setwd(folderDatasetX)
    dir_folderDatasetX = dir(folderDatasetX)
    n_folderDatasetX = length(dir_folderDatasetX)
  } else {
    dir.create(folderDatasetX)
    setwd(folderDatasetX)
    dir_folderDatasetX = dir(folderDatasetX)
    n_folderDatasetX = length(dir_folderDatasetX)
  }
  
  folderLabelSpace = paste(folderDatasetX, "/LabelSpace", sep="")
  if(dir.exists(folderLabelSpace) == TRUE){
    setwd(folderLabelSpace)
    dir_folderLabelSpace = dir(folderLabelSpace)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  } else {
    dir.create(folderLabelSpace)
    setwd(folderLabelSpace)
    dir_folderLabelSpace = dir(folderLabelSpace)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  }
  
  folderNamesLabels = paste(folderDatasetX, "/NamesLabels", sep="")
  if(dir.exists(folderNamesLabels) == TRUE){
    setwd(folderNamesLabels)
    dir_folderNamesLabels = dir(folderNamesLabels)
    n_folderNamesLabels = length(dir_folderNamesLabels)
  } else {
    dir.create(folderNamesLabels)
    setwd(folderNamesLabels)
    dir_folderLabelSpace = dir(folderNamesLabels)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  }
  
  folderCV = paste(folderDatasetX, "/CrossValidation", sep="")
  if(dir.exists(folderCV) == TRUE){
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  } else {
    dir.create(folderCV)
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  }
  
  folderCVTR = paste(folderCV, "/Tr", sep="")
  if(dir.exists(folderCVTR) == TRUE){
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  } else {
    dir.create(folderCVTR)
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  }
  
  folderCVTS = paste(folderCV, "/Ts", sep="")
  if(dir.exists(folderCVTS) == TRUE){
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  } else {
    dir.create(folderCVTS)
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  }
  
  folderCVVL = paste(folderCV, "/Vl", sep="")
  if(dir.exists(folderCVVL) == TRUE){
    setwd(folderCVVL)
    dir_folderCVVL = dir(folderCVVL)
    n_folderCVVL = length(dir_folderCVVL)
  } else {
    dir.create(folderCVVL)
    setwd(folderCVVL)
    dir_folderCVVL = dir(folderCVVL)
    n_folderCVVL = length(dir_folderCVVL)
  }
  
  folderResultsDataset = paste(folderResults, "/", dataset_name, sep="")
  if(dir.exists(folderResultsDataset) == TRUE){
    setwd(folderResultsDataset)
    dir_folderResultsDataset = dir(folderResultsDataset)
    n_folderResultsDataset = length(dir_folderResultsDataset)
  } else {
    dir.create(folderResultsDataset)
    setwd(folderResultsDataset)
    dir_folderResultsDataset = dir(folderResultsDataset)
    n_folderResultsDataset = length(dir_folderResultsDataset)
  }
  
  folderResultSilhouete = paste(folderResultsDataset, "/Silhouete", sep="")
  if(dir.exists(folderResultSilhouete) == TRUE){
    setwd(folderResultSilhouete)
    dir_folderResultSilhouete = dir(folderResultSilhouete)
    n_folderResultSilhouete = length(dir_folderResultSilhouete)
  } else {
    dir.create(folderResultSilhouete)
    setwd(folderResultSilhouete)
    dir_folderResultSilhouete = dir(folderResultSilhouete)
    n_folderResultSilhouete = length(dir_folderResultSilhouete)
  }
  
  folderReports = paste(FolderRoot, "/Reports", sep="")
  if(dir.exists(folderReports) == TRUE){
    setwd(folderReports)
    dir_folderReports = dir(folderReports)
    n_folderReports = length(dir_folderReports)
  } else {
    dir.create(folderReports)
    setwd(folderReports)
    dir_folderReports = dir(folderReports)
    n_folderReports = length(dir_folderReports)
  }
  
  folderReportsDataset = paste(folderReports, "/", dataset_name, sep="")
  if(dir.exists(folderReportsDataset) == TRUE){
    setwd(folderReportsDataset)
    dir_folderReportsDataset = dir(folderReportsDataset)
    n_folderReportsDataset = length(dir_folderReportsDataset)
  } else {
    dir.create(folderReportsDataset)
    setwd(folderReportsDataset)
    dir_folderReportsDataset = dir(folderReportsDataset)
    n_folderReportsDataset = length(dir_folderReportsDataset)
  }
  
  folderReportSilhouete = paste(folderReportsDataset, "/Silhouete", sep="")
  if(dir.exists(folderReportSilhouete) == TRUE){
    setwd(folderReportSilhouete)
    dir_folderReportSilhouete = dir(folderReportSilhouete)
    n_folderReportSilhouete = length(dir_folderReportSilhouete)
  } else {
    dir.create(folderReportSilhouete)
    setwd(folderReportSilhouete)
    dir_folderReportSilhouete = dir(folderReportSilhouete)
    n_folderReportSilhouete = length(dir_folderReportSilhouete)
  }
  
  # return folders
  retorno$folderResults = folderResults
  retorno$folderFinalPartitions = folderFinalPartitions
  retorno$folderResultsDataset = folderResultsDataset
  retorno$folderResultSilhouete = folderResultSilhouete
  retorno$folderUtils = folderUtils
  retorno$folderDatasets = folderDatasets
  retorno$folderDatasetX = folderDatasetX
  retorno$folderReports = folderReports
  retorno$folderReportsDataset = folderReportsDataset
  retorno$folderReportSilhouete = folderReportSilhouete
  retorno$folderNamesLabels = folderNamesLabels
  retorno$folderLabelSpace = folderLabelSpace
  retorno$folderCV = folderCV
  retorno$folderCVTR = folderCVTR
  retorno$folderCVTS = folderCVTS
  retorno$folderCVVL = folderCVVL
  
  retorno$dir_folderResults = dir_folderResults
  retorno$dir_folderFinalPartitions = dir_folderFinalPartitions
  retorno$dir_folderResultsDataset = dir_folderResultsDataset
  retorno$dir_folderResultSilhouete = dir_folderResultSilhouete
  retorno$dir_folderUtils = dir_folderUtils
  retorno$dir_folderDatasets = dir_folderDatasets
  retorno$dir_folderDatasetX = dir_folderDatasetX
  retorno$dir_folderReports = dir_folderReports
  retorno$dir_folderReportSilhouete = dir_folderReportSilhouete
  retorno$dir_folderNamesLabels = dir_folderNamesLabels
  retorno$dir_folderLabelSpace = dir_folderLabelSpace
  retorno$dir_folderCV = dir_folderCV
  retorno$dir_folderCVTR = dir_folderCVTR
  retorno$dir_folderCVTS = dir_folderCVTS
  retorno$dir_folderCVVL = dir_folderCVVL
  
  retorno$n_folderResults = n_folderResults
  retorno$n_folderResultsDataset = n_folderResultsDataset
  retorno$n_folderResultSilhouete = n_folderResultSilhouete
  retorno$n_folderFinalPartitions = n_folderFinalPartitions
  retorno$n_folderUtils = n_folderUtils
  retorno$n_folderDatasets = n_folderDatasets
  retorno$n_folderDatasetX = n_folderDatasetX
  retorno$n_folderReports = n_folderReports
  retorno$n_folderReportsDataset = n_folderReportsDataset
  retorno$n_folderReportSilhouete = n_folderReportSilhouete
  retorno$n_folderNamesLabels = n_folderNamesLabels
  retorno$n_folderLabelSpace = n_folderLabelSpace
  retorno$n_folderCV = n_folderCV
  retorno$n_folderCVTR = n_folderCVTR
  retorno$n_folderCVTS = n_folderCVTS
  retorno$n_folderCVVL = n_folderCVVL
  
  return(retorno)
  gc()
}



##################################################################################################
# FUNCTION LABEL SPACE                                                                           #
#   Objective                                                                                    #
#       Separates the label space from the rest of the data to be used as input for              # 
#       calculating correlations                                                                 #                                                                                        
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       Folder: folder where the folds are                                                       #
#   Return:                                                                                      #
#       Training set labels space                                                                #
##################################################################################################
labelSpace <- function(ds, dataset_name, number_folds, folderResults){
  
  retorno = list()
  classes = list()
  diretorios = directories(dataset_name, folderResults)
  
  # from the first FOLD to the last
  k = 1
  while(k<=number_folds){
    #cat("\n\tFold: ", k)
    # enter folder train
    setwd(diretorios$folderCVTR)
    # get the correct split
    nome_arquivo = paste(dataset_name, "-Split-Tr-", k, ".csv", sep="")
    arquivo = data.frame(read.csv(nome_arquivo))
    # split label space from input space
    classes[[k]] = arquivo[,ds$LabelStart:ds$LabelEnd]
    namesLabels = c(colnames(classes[[k]]))
    k = k + 1 # increment FOLD
    gc() # garbage collection
  } # End While of the 10-folds
  
  retorno$NamesLabels = namesLabels
  retorno$Classes = classes
  return(retorno)
  gc()
  cat("\n##################################################################################################")
  cat("\n# FUNCTION LABEL SPACE: END                                                                      #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION INFO DATA SET                                                                         #
#  Objective                                                                                     #
#     Gets the information that is in the "datasets.csv" file.                                    #  
#  Parameters                                                                                    #
#     dataset: the specific dataset                                                              #
#  Return                                                                                        #
#     Everything in the spreadsheet                                                              #
##################################################################################################
infoDataSet <- function(dataset){
  retorno = list()
  retorno$id = dataset$ID
  retorno$name = dataset$Name
  retorno$instances = dataset$Instances
  retorno$inputs = dataset$Inputs
  retorno$labels = dataset$Labels
  retorno$LabelsSets = dataset$LabelsSets
  retorno$single = dataset$Single
  retorno$maxfreq = dataset$MaxFreq
  retorno$card = dataset$Card
  retorno$dens = dataset$Dens
  retorno$mean = dataset$Mean
  retorno$scumble = dataset$Scumble
  retorno$tcs = dataset$TCS
  retorno$attStart = dataset$AttStart
  retorno$attEnd = dataset$AttEnd
  retorno$labStart = dataset$LabelStart
  retorno$labEnd = dataset$LabelEnd
  retorno$distinct = dataset$Distinct
  retorno$xn = dataset$xn
  retorno$yn = dataset$yn
  retorno$gridn = dataset$gridn
  retorno$xt = dataset$xt
  retorno$yt = dataset$yt
  retorno$gridt = dataset$gridt
  return(retorno)
  gc()
}


##################################################################################################
# Please, any errors, contact us!                                                                #
# Thank you very much!                                                                           #
##################################################################################################