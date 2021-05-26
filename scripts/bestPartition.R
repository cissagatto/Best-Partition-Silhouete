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
# 
##################################################################################################
comuputeSilhouete <- function (ds, resLS, dataset_name, number_folds, folderResults){
  
  if(interactive()==TRUE){ flush.console() }
  
  diretorios = directories(dataset_name, folderResults)
  
  cat("\nFrom 1 to 10 folds!")
  f = 2
  silhoueteParalel <- foreach(f = 1:number_folds) %dopar%{
    
    cat("\nFold: ", f)   
    
    ############################################################################################################
    cat("\nLoad sources and packages")
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
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    ########################################################################
    FolderSplit = paste(diretorios$folderResultSilhouete, "/Split-", f, sep="")
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
    Folder = paste(diretorios$folderFinalPartitions, "/", dataset_name, "/InfoPartitions/Split-",f, sep="")
    setwd(Folder)
    num.group = data.frame(read.csv(paste("fold-",f,"-new-summary-partitions.csv", sep="")))
    num.part = as.numeric(nrow(num.group))
    
    np = 1
    cont = 0
    new.num.part = 0
    
    p = 2
    while(p<=num.part){
      cat("\nPartition ", p)
      
      ########################################################################
      FolderPartition = paste(FolderSplit, "/Partition-", p, sep="")
      if(dir.exists(FolderPartition)==FALSE){
        dir.create(FolderPartition)
      }
      
      # get the number of groups for this partition
      num.group2 = num.group[np,]
      num.group3 = as.numeric(num.group2$new.num.groups)
      
      # get the labels with the respective groups
      FolderP = paste(Folder, "/Partition-", p, sep="")
      setwd(FolderP)
      particao = data.frame(read.csv(paste("new-final-partition-", p, ".csv", sep="")))
      particao = particao[,-1]
      particao = data.frame(particao[order(particao$labels2, decreasing = FALSE),])
      groups_label_space = cbind(particao, espacoDeRotulos2)
      groups_label_space = groups_label_space[,-2]
      
      if(num.group3==1){
        cat("\nonly one group of labels")
        
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
        
        groups_label_space2 = groups_label_space[,-2]
        a = dist(groups_label_space2)
        b = as.dist(a)
        sil = silhouette(groups_label_space2$group2, b)
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
      
      # incrementa o número da partição do arquivo csv
      np = np + 1
      
      # incrementa o número da partição do while
      p = p + 1
      
      if(interactive()==TRUE){ flush.console() }
      
      gc()
    } # fim da partição
    
    setwd(FolderSplit)
    write.csv(Silhouete[-1,], paste("final-silhouete-fold-", f, ".csv", sep=""), row.names = FALSE)
    
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
  
  cat("\nFrom 1 to 10 folds!")
  f = 1
  while(f<=number_folds){
    
    FolderSplit = paste(diretorios$folderResultSilhouete, "/Split-", f, sep="")
    setwd(FolderSplit)
    silhouete = data.frame(read.csv(paste("final-silhouete-fold-",f,".csv", sep="")))
    num.part = nrow(silhouete)
    indice = as.numeric(which.max(silhouete$valueSilhouete))
    
    silhouete2 = silhouete[indice,]
    
    fold = silhouete2$fold
    part = silhouete2$part
    maximo = silhouete2$maximo
    minimo = silhouete2$minimo
    mediana = silhouete2$mediana
    media = silhouete2$media
    primeiroQuadrante = silhouete2$primeiroQuadrante
    terceiroQuadrante = silhouete2$terceiroQuadrante
    valueSilhouete = silhouete2$valueSilhouete
    bestPartitions = rbind(bestPartitions, data.frame(fold, part, maximo, minimo, 
                                            mediana, media, primeiroQuadrante, terceiroQuadrante, valueSilhouete))
    
    f = f + 1
    if(interactive()==TRUE){ flush.console() }
    gc()
  } # fim do fold
  
  setwd(diretorios$folderResultsDataset)
  write.csv(bestPartitions[-1,], paste(dataset_name, "-best-partitions.csv", sep=""), row.names = FALSE)
  
  setwd(diretorios$folderReportsDataset)
  write.csv(bestPartitions[-1,], paste(dataset_name, "-best-partitions.csv", sep=""), row.names = FALSE)
  
  if(interactive()==TRUE){ flush.console() }
  gc()
  cat("\n##################################################################################################")
  cat("\n# END FUNCTION SELECT ALL BEST PARTITION                                                         #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


bestPartition <- function(ds, resLS, dataset_name, number_folds, folderResults){
  
  diretorios = directories(dataset_name, folderResults)
  
  cat("\n\n################################################################################################")
  cat("\n# Run: Compute silhouete                                                                         #")
  timeCS = system.time(resCS <- comuputeSilhouete(ds, resLS, dataset_name, number_folds, folderResults))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# Run: Select the best partition                                                                 #")
  timeBP = system.time(resBP <- selectBestPartition(number_folds, dataset_name, folderResults))
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\nRuntime")
  timesSilho = rbind(timeCS, timeBP)
  setwd(diretorios$folderReportsDataset)
  write.csv(timesSilho, "Silho-RunTime.csv")
  cat("\n##################################################################################################")
  
  if(interactive()==TRUE){ flush.console() }
  gc()
  cat("\n##################################################################################################")
  cat("\n# END BEST PARTITION                                                                             #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################