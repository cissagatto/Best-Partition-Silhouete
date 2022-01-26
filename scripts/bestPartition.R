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
# FUNCTION COMPUTE SILHOUETE                                                                     #
#   Objective:                                                                                   #
#      Compute sillhouete for each partition/fold                                                #
#   Parameters:                                                                                  #
#      ds: information about the specific dataset                                                #
#      resLS: label space from the specific dataset                                              #
#      number_dataset: number of the specific dataset                                            #
#      number_cores: number of cores to process in paralel                                       #
#      number_folds: number of folds for the cross-validation                                    #
#      folderResults: folder to process                                                          #
#   Return:                                                                                      #          
#      Silhouete Graphics                                                                        #
#      Silhouete Values                                                                          #
##################################################################################################
comuputeSilhouete <- function (ds, resLS, dataset_name, number_folds, folderResults){
  
  if(interactive()==TRUE){ flush.console() }
  
  diretorios = directories(dataset_name, folderResults)
  
  cat("\nFrom 1 to 10 folds!")
  f = 1
  silhoueteParalel <- foreach(f = 1:number_folds) %dopar% {
  #while(f<=number_folds){
    
    cat("\nFold: ", f)   
    
    ############################################################################################################
    cat("\nLoad sources and packages")
    sistema = c(Sys.info())
    shm = 0
    FolderRoot = ""
    if (sistema[1] == "Linux"){
      shm = 1
      FolderRoot = paste("/home/", sistema[7], "/Best-Partition-Silhouete", sep="")
    } else {
      shm = 0
      FolderRoot = paste("C:/Users/", sistema[7], "/Best-Partition-Silhouete", sep="")
    }
    FolderScripts = paste(FolderRoot, "/scripts", sep="")
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    ########################################################################
    FolderSplit = paste(diretorios$folderResults, "/", dataset_name, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==FALSE){
      dir.create(FolderSplit)
    }
    
    ########################################################################
    fold = c(0)
    part = c(0)
    maximo = c(0)
    minimo = c(0)
    mediana = c(0)
    media = c(0)
    primeiroQuadrante = c(0)
    terceiroQuadrante = c(0)
    valueSilhouete = c(0)
    Silhouete = data.frame(fold, part, maximo, minimo, mediana, media, 
                           primeiroQuadrante, terceiroQuadrante, valueSilhouete)
    
    ########################################################################
    # get the space label
    espacoDeRotulos = data.frame(resLS$Classes[f])
    espacoDeRotulos2 = data.frame(t(espacoDeRotulos))
    labels = rownames(espacoDeRotulos2)
    espacoDeRotulos2 = cbind(labels, espacoDeRotulos2)
    espacoDeRotulos2 = data.frame(espacoDeRotulos2[order(espacoDeRotulos2$labels, decreasing = FALSE),])
    
    ########################################################################
    cat("\n\nGrupos por particão")
    
    Folder = paste(diretorios$folderPartitions, "/", dataset_name, "/Split-",f, sep="")
    setwd(Folder)
    cat("\n\n", Folder)

    # fold-1-groups-per-partitions.csv
    num.groups = data.frame(read.csv(paste("fold-",f,"-groups-per-partition.csv", sep="")))
    num.part = as.numeric(nrow(num.groups))
    
    ########################################################################    
    np = 1
    cont = 0
    new.num.part = 0
  
      ########################################################################
      p = 2
      while(p<=num.part){
        cat("\nPartition ", p)
        
        ########################################################################
        FolderPartition = paste(FolderSplit, "/Partition-", p, sep="")
        if(dir.exists(FolderPartition)==FALSE){
          dir.create(FolderPartition)
        }
        
        ########################################################################  
        # get the number of groups for this partition
        num.groups2 = num.groups[np,]
        num.group3 = as.numeric(num.groups2$num.groups)
        
        ########################################################################  
        # get the labels with the respective groups
        cat("\n\nAbrindo partição")
        FolderP = paste(Folder, "/Partition-", p, sep="")      
        setwd(FolderP)
        cat("\n\n", Folder)
        
        particao = data.frame(read.csv(paste("partition-", p, ".csv", sep="")))
        particao = data.frame(particao[order(particao$label, decreasing = FALSE),])
        groups_label_space = cbind(particao, espacoDeRotulos2)
        groups_label_space2 = groups_label_space[,-2]
        
        ########################################################################        
        if(num.group3==1){
          cat("\nOnly one group of labels (global partition)")
          
          fold = f
          part = p
          maximo = NA
          minimo = NA
          mediana = NA
          media = NA
          primeiroQuadrante = NA
          terceiroQuadrante = NA
          valueSilhouete = NA
          Silhouete = rbind(Silhouete, data.frame(fold, part, maximo, minimo, mediana, media, 
                                                  primeiroQuadrante, terceiroQuadrante, valueSilhouete))
          
        } else {
          cat("\ntwo or more labels in the group")
          
          groups_label_space3 = groups_label_space2[,-2]
          a = dist(groups_label_space3)
          b = as.dist(a)
          sil = silhouette(groups_label_space3[,1], b)
          
          if(all(is.na(sil))){
            cat("\nOne label per group (local partition)")
            
            fold = f
            part = p
            maximo = NA
            minimo = NA
            mediana = NA
            media = NA
            primeiroQuadrante = NA
            terceiroQuadrante = NA
            valueSilhouete = NA
            Silhouete = rbind(Silhouete, data.frame(fold, part, maximo, minimo, mediana, media, 
                                                    primeiroQuadrante, terceiroQuadrante, valueSilhouete))
          } else {
            
            sil = sortSilhouette(sil) 
            
            setwd(FolderPartition)
            write.csv(sil, paste("res-silho-p-", p, ".csv", sep=""))
            
            setwd(FolderPartition)
            pdf(paste("sil-p-", p, ".pdf", sep=""), width = 10, height = 8)
            print(plot(sil))
            dev.off()
            cat("\n")     
            
            setwd(FolderPartition)
            pdf(paste("fviz-sil-p-", p, ".pdf", sep=""), width = 10, height = 8)
            print(fviz_silhouette(sil))
            dev.off()
            cat("\n")     
            
            # Summary of silhouette analysis
            si.sum = summary(sil)
            res.si.sum = unlist(si.sum)
            
            fold = f
            part = p
            maximo = res.si.sum$si.summary.Max.
            minimo = res.si.sum$si.summary.Min.
            mediana = res.si.sum$si.summary.Median
            media = res.si.sum$si.summary.Mean
            primeiroQuadrante = res.si.sum$`si.summary.1st Qu.`
            terceiroQuadrante = res.si.sum$`si.summary.3rd Qu.`
            valueSilhouete = res.si.sum$avg.width
            Silhouete = rbind(Silhouete, data.frame(fold, part, maximo, minimo, mediana, media, 
                                                    primeiroQuadrante, terceiroQuadrante, valueSilhouete)) 
            
          }
          
        }
        
        # incrementa o número da partição do arquivo csv
        np = np + 1
        
        # incrementa o número da partição do while
        p = p + 1
        
        if(interactive()==TRUE){ flush.console() }
        
        gc()
      } # fim da partição
      
    
    setwd(FolderSplit)
    write.csv(Silhouete[-1,], paste("fold-", f, "-silhouete.csv", sep=""), row.names = FALSE)
    
    if(interactive()==TRUE){ flush.console() }
    gc()
    
  } # fim do fold
  
  if(interactive()==TRUE){ flush.console() }
  gc()
  cat("\n##################################################################################################")
  cat("\n# END COMPUTE SILHOUETE                                                                          #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION COMPUTE BEST SILHOUETE                                                                #
#   Objective:                                                                                   #
#      Selcet the best partition                                                                 #
#   Parameters:                                                                                  #
#      ds: information about the specific dataset                                                #
#      resLS: label space from the specific dataset                                              #
#      number_dataset: number of the specific dataset                                            #
#      number_cores: number of cores to process in paralel                                       #
#      number_folds: number of folds for the cross-validation                                    #
#      folderResults: folder to process                                                          #
#   Return:                                                                                      #          
#      Graphics from Silhouete                                                                   #
#      Silhouete Values                                                                          #
##################################################################################################
selectBestPartition <- function(number_folds, dataset_name, folderResults){
  
  if(interactive()==TRUE){ flush.console() }
  
  diretorios = directories(dataset_name, folderResults)
  
  fold = c(0)
  part = c(0)
  maximo = c(0)
  minimo = c(0)
  mediana = c(0)
  media = c(0)
  primeiroQuadrante = c(0)
  terceiroQuadrante = c(0)
  valueSilhouete = c(0)
  bestPartitions = data.frame(fold, part, maximo, minimo, mediana, media, primeiroQuadrante, terceiroQuadrante, valueSilhouete)
  
  
  ########################################################################
  fold = c(0)
  partition = c(0)
  num.groups = c(0)
  allNumGroups = data.frame(fold, partition, num.groups)
  
  
  cat("\nFrom 1 to 10 folds!")
  f = 1
  while(f<=number_folds){
    
    FolderSplit = paste(diretorios$folderResultsDataset, "/Split-", f, sep="")
    setwd(FolderSplit)
    silhouete = data.frame(read.csv(paste("fold-",f,"-silhouete.csv", sep="")))
    num.part = nrow(silhouete)
    indice = as.numeric(which.max(silhouete$valueSilhouete))
    
    silhouete2 = silhouete[indice,]
    
    fold = as.numeric(silhouete2$fold)
    part = as.numeric(silhouete2$part)
    maximo = as.numeric(silhouete2$maximo)
    minimo = as.numeric(silhouete2$minimo)
    mediana = as.numeric(silhouete2$mediana)
    media = as.numeric(silhouete2$media)
    primeiroQuadrante = as.numeric(silhouete2$primeiroQuadrante)
    terceiroQuadrante = as.numeric(silhouete2$terceiroQuadrante)
    valueSilhouete = as.numeric(silhouete2$valueSilhouete)
    bestPartitions = rbind(bestPartitions, data.frame(fold, part, maximo, minimo, 
                                            mediana, media, primeiroQuadrante, terceiroQuadrante, valueSilhouete))
    
    ########################################################################  
    Folder = paste(diretorios$folderPartitions, "/", dataset_name, "/Split-",f, sep="")
    setwd(Folder)
    num.groups = data.frame(read.csv(paste("fold-",f,"-groups-per-partition.csv", sep="")))
    num.groups2 = num.groups[,-1]
    num.groups3 = filter(num.groups2, num.groups2$partition == part)
    
    fold = f
    partition = num.groups3$partition
    num.groups = num.groups3$num.groups
    allNumGroups = rbind(allNumGroups, data.frame(fold, partition, num.groups))
    
    ########################################################################  
    # get the labels with the respective groups
    FolderP = paste(diretorios$folderPartitions, "/", dataset_name, "/Split-", f, "/Partition-", part, sep="")
    
    destino = paste(diretorios$folderOutputDataset, "/Split-", f, sep="")
    if(dir.exists(destino)==FALSE){
      dir.create(destino)
    }
    str = paste("cp -r ", FolderP, " ", destino, sep="")
    print(system(str))
    
    str2 = paste("cp ", diretorios$folderPartitions, "/", dataset_name, "/Split-", f, "/fold-", f, "-groups-per-partition.csv ", destino, sep="")
    print(system(str2))
    
    f = f + 1
    if(interactive()==TRUE){ flush.console() }
    gc()
  } # fim do fold

  ##################################################################################################
  Folder = paste(diretorios$folderDatasetResults, "/", dataset_name, sep="")
  if(dir.exists(Folder)==FALSE){
    dir.create(Folder)
  }
  
  setwd(diretorios$folderResultsDataset)
  write.csv(bestPartitions[-1,], paste(dataset_name, "-best-partitions.csv", sep=""), row.names = FALSE)
  
  setwd(Folder)
  write.csv(bestPartitions[-1,], paste(dataset_name, "-best-partitions.csv", sep=""), row.names = FALSE)
  
  setwd(diretorios$folderOutputDataset)
  bestPartitions2 = bestPartitions[, c(1,2)]
  write.csv(bestPartitions2[-1,], paste(dataset_name, "-best-partitions.csv", sep=""), row.names = FALSE)
  
  ##################################################################################################
  allNumGroups2 = allNumGroups[-1,]
  
  setwd(diretorios$folderResultsDataset)
  write.csv(allNumGroups2, paste(dataset_name, "-all-num-groups.csv", sep=""), row.names = FALSE)
  
  setwd(Folder)
  write.csv(allNumGroups2, paste(dataset_name, "-all-num-groups.csv", sep=""), row.names = FALSE)
  
  setwd(diretorios$folderOutputDataset)
  write.csv(allNumGroups2, paste(dataset_name, "-all-num-groups.csv", sep=""), row.names = FALSE)
  
  ##################################################################################################
  if(interactive()==TRUE){ flush.console() }
  gc()
  
  cat("\n##################################################################################################")
  cat("\n# END FUNCTION SELECT ALL BEST PARTITION                                                         #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION ASD                                                                                   #
#   Objective                                                                                    #
#       Compute statistics about the partitions                                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#   Return                                                                                       #
#       Sum, mean, median, standart deviation, max and min partitions                            #
##################################################################################################
asd <- function(ds, dataset_name, diretorios, namesLabels){
    
  # function return 
  retorno = list()
  
  # get the best partitions of the dataset
  setwd(diretorios$folderOutputDataset)
  nome = paste(dataset_name, "-best-partitions.csv", sep="")
  bP = data.frame(read.csv(nome))
  
  nome2 = paste(dataset_name, "-all-num-groups.csv", sep="")
  particoes = data.frame(read.csv(nome2))
  
  frequencia = count(particoes, particoes$num.groups)
  names(frequencia) = c("groups","frequency")
  
  setwd(diretorios$folderResultsDataset)
  write.csv(frequencia, paste(dataset_name, "-frequency-chosed-groups.csv", sep=""), row.names = FALSE)
  
  Folder = paste(diretorios$folderDatasetResults, "/", dataset_name, sep="")
  if(dir.exists(Folder)==FALSE){
    dir.create(Folder)
  }
  setwd(Folder)
  write.csv(frequencia, paste(dataset_name, "-frequency-chosed-groups.csv", sep=""), row.names = FALSE)
  
  # computes statistics
  soma = apply(particoes, 2, sum)
  media = apply(particoes, 2, mean)
  mediana = apply(particoes, 2, median)
  desvioPadrao = apply(particoes, 2, sd)
  minimo = apply(particoes, 2, min)
  maximo = apply(particoes, 2, max)
  sumario = rbind(soma, media, mediana, desvioPadrao, minimo, maximo)
  
  # saves results in the RESULTS folder
  setwd(diretorios$folderResultsDataset)
  write.csv(sumario, paste(dataset_name, "-statistic-sumary-best-part.csv", sep=""))
  
  setwd(Folder)
  write.csv(sumario, paste(dataset_name, "-statistic-sumary-best-part.csv", sep=""))
  
  # function return
  retorno$sumario = sumario
  return(retorno)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# Statistics: END                                                                                #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}





##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
