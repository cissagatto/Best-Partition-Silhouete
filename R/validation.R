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





########################################################################
# FUNCTION ASD                                                          
#   Objective                                                           
#       Compute statistics about the partitions                         
#   Parameters                                                          
#       ds: specific dataset information                                
#       parameters$Dataset.Name: dataset name. It is used to save files.           
#   Return                                                              
#       Sum, mean, median, standart deviation, max and min partitions   
########################################################################
asd <- function(parameters){
  
  # diretorios <- directories(parameters$Dataset.Name, 
  #                           parameters$Folder.Results,
  #                           parameters$Similarity)
  
  # function return 
  retorno = list()
  library("dplyr")

  # get the best partitions of the dataset
  nome = paste(parameters$Folders$folderOutputDataset, "/", 
        parameters$Dataset.Name, "-Best-Silhouete.csv", sep="")
  bP = data.frame(read.csv(nome))
  
  frequencia = data.frame(count(bP, vars = part))
  names(frequencia) = c("partition","frequency")
  
  setwd(parameters$Folders$folderOutputDataset)
  write.csv(frequencia, paste(parameters$Dataset.Name,
                              "-frequency-chosed-groups.csv", 
                              sep=""), row.names = FALSE)
  
  # computes statistics
  soma = apply(bP, 2, sum)
  media = apply(bP, 2, mean)
  mediana = apply(bP, 2, median)
  desvioPadrao = apply(bP, 2, sd)
  minimo = apply(bP, 2, min)
  maximo = apply(bP, 2, max)
  sumario = rbind(soma, media, mediana, desvioPadrao, minimo, maximo)
  
  # saves results in the RESULTS folder
  setwd(parameters$Folders$folderOutputDataset)
  write.csv(sumario, paste(parameters$Dataset.Name, "-statistic-sumary-best-part.csv", 
                           sep=""))
  
  # function return
  retorno$sumario = sumario
  return(retorno)
  
  gc()
  cat("\n#############################################################")
  cat("\n# Statistics: END                                           #")
  cat("\n#############################################################")
  cat("\n\n\n\n")
}



###########################################################################
#
###########################################################################
bestPartitions <- function(parameters){
  
  #######################################################################
  #cat("\nworkspace")
  FolderRoot = "~/Best-Partition-Silhouette"
  FolderScripts = "~/Best-Partition-Silhouette/R"
  
  setwd(FolderScripts)
  source("libraries.R")
  
  setwd(FolderScripts)
  source("utils.R")
  
  # diretorios <- directories(parameters$Dataset.Name, 
  #                           parameters$Folder.Results,
  #                           parameters$Similarity)
  
  todos = data.frame()
  best.part = data.frame()
  
  f = 1
  while(f<=number_folds){
    
    cat("\n=========================================================")
    cat("\nFold: ", f)
    cat("\n=========================================================")
    
    ##################################################################
    #cat("\nfolders split")
    FolderSplitOrigem = paste(parameters$Folders$folderValidate, "/Split-", f, sep="")
    FolderSplitDestino = paste(parameters$Folders$folderOutputDataset, 
                               "/Split-", f, sep="")
    FolderPSO = paste(parameters$Folders$olderPartitions, "/", parameters$Dataset.Name, 
                      "/Split-", f, sep="")
    
    ##################################################################
    setwd(FolderSplitOrigem)
    nome = paste("fold-", f, "-silho.csv", sep="")
    silho = data.frame(read.csv(nome))
    
    ##################################################################
    todos = rbind(todos, silho)
    
    ##################################################################
    indice = as.numeric(which.max(silho$valueSilhouete))
    silhouette = silho[indice,]
    best.part = rbind(best.part, silhouette)
    
    best.part2 = best.part[f,]
    
    ##################################################################
    FolderP = paste(diretorios$folderPartitions, "/", parameters$Dataset.Name, 
          "/Split-", f, sep="")
    
    FolderPP = paste(FolderP, "/Partition-", as.numeric(best.part2$part), sep="")
    
    FolderD = paste(parameters$Folders$folderOutputDataset,
                    "/Split-", f, sep="")
    if(dir.exists(FolderD)==FALSE){dir.create(FolderD)}
    
    print(system(paste("cp -r ", FolderPP , " ",  FolderD, sep="")))
    
    nome = paste(FolderP, "/fold-", f, "-groups-per-partition.csv", sep="")
    nome2 = paste("cp -r ", nome, " ", FolderD, sep="")
    print(system(nome2))
    
    f = f + 1
    gc()
    
  }
  
  setwd(parameters$Folders$folderOutputDataset)
  write.csv(todos, paste(parameters$Dataset.Name, "-All-Silhouete.csv", sep=""),
            row.names = FALSE)
  
  write.csv(best.part, paste(parameters$Dataset.Name, "-Best-Silhouete.csv", sep=""),
            row.names = FALSE)
  
}



###########################################################################
#
###########################################################################
validate <- function (parameters){
  
  
  f = 1
  silhoueteParalel <- foreach(f = 1:number_folds) %dopar% {
  #while(f<=number_folds){
    
    cat("\n=========================================================")
    cat("\nFold: ", f)
    cat("\n=========================================================")
    
    #######################################################################
    #cat("\nworkspace")
    FolderRoot = "~/Best-Partition-Silhouette"
    FolderScripts = "~/Best-Partition-Silhouette/R"
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    # diretorios <- directories(parameters$Dataset.Name, 
    #                           parameters$Folder.Results,
    #                           parameters$Similarity)
    
    
    ########################################################################
    #cat("\nget the space label")
    espacoDeRotulos = data.frame(parameters$resLS$Classes[f])
    espacoDeRotulos = data.frame(t(espacoDeRotulos))
    labels = rownames(espacoDeRotulos)
    espacoDeRotulos = cbind(labels, espacoDeRotulos)
    espacoDeRotulos = data.frame(espacoDeRotulos[order(espacoDeRotulos$labels, 
                                                       decreasing = FALSE),])
    
    
    ##################################################################
    #cat("\nfolders split")
    FolderSplitOrigem = paste(parameters$Folders$folderPartitions, "/", 
                              parameters$Dataset.Name, "/Split-", f, sep="")
    FolderSplitDestino = paste(parameters$Folders$folderValidate, 
                               "/Split-", f, sep="")
    if(dir.exists(FolderSplitDestino)==FALSE){dir.create(FolderSplitDestino)}
    
    
    ##################################################################
    #cat("\nOPEN FILE WITH PARTITION INFORMATION")
    nome = paste(FolderSplitOrigem, "/fold-", 
                 f, "-groups-per-partition.csv", sep="")
    particoes = data.frame(read.csv(nome))
    
    
    ##################################################################
    # obtendo informações da partição deste split
    particoes.f = particoes[f,]
    
    
    ##################################################################
    # PEGANDO O NÚMERO TOTAL DE PARTIÇÕES
    num.part = nrow(particoes)+1
    num.groups = particoes.f$num.groups
    
    #######################################################################
    fold = c(0)
    part = c(0)
    maximo = c(0)
    minimo = c(0)
    mediana = c(0)
    media = c(0)
    primeiroQuadrante = c(0)
    terceiroQuadrante = c(0)
    valueSilhouete = c(0)
    Silhouete = data.frame(fold, part, maximo, minimo, mediana, 
                           media, primeiroQuadrante, terceiroQuadrante, 
                           valueSilhouete)
    
    p = 2
    while(p<=num.part){
      
      cat("\n%%%%%%%%%%%%%%%%%%%%")
      cat("\nPartition: ", p)
      cat("\n%%%%%%%%%%%%%%%%%%%%")
      
      ##################################################################
      #cat("\nfolders part")
      FolderPartOrigem = paste(FolderSplitOrigem, "/Partition-", p, sep="")
      FolderPartDestino = paste(FolderSplitDestino, "/Partition-", p, sep="")
      if(dir.exists(FolderPartDestino)==FALSE){dir.create(FolderPartDestino)}
      
      ##################################################################
      #cat("\nabrindo partição")
      namae = paste(FolderPartOrigem, "/partition-", p, ".csv", sep="")
      conf_part = data.frame(read.csv(namae))
      
      
      #######################################################################
      #cat("\njuntando grupos com espaco de rotulos")
      particao_final = cbind(conf_part, espacoDeRotulos)
      particao_final = particao_final[,-2]
      
      
      #######################################################################
      a = dist(espacoDeRotulos[, -1])
      b = as.dist(a)
      sil = silhouette(particao_final$group, b)
      sil = sortSilhouette(sil)
      
      
      #######################################################################
      setwd(FolderPartDestino)
      write.csv(sil,
                paste("silho-fold-", f, "-part-",
                      p, ".csv", sep = ""),
                row.names = FALSE)
      
      if (all(is.na(sil)) == TRUE) {
        #cat("\nOne label per group (local partition)\n")
        fold = f
        part = p
        maximo = NA
        minimo = NA
        mediana = NA
        media = NA
        primeiroQuadrante = NA
        terceiroQuadrante = NA
        valueSilhouete = NA
        Silhouete = rbind(Silhouete, data.frame(fold, part, 
                                                maximo, minimo, 
                                                mediana, media,
                                                primeiroQuadrante,
                                                terceiroQuadrante,
                                                valueSilhouete))
        
        setwd(FolderPartDestino)
        write.csv(sil, paste("silho-fold-", f, "-part-", p, 
                             ".csv", sep = ""), row.names = FALSE)
        
      } else {
        #cat("\nMore than one label per group\n")
        
        setwd(FolderPartDestino)
        pdf(paste("silho-fold-", f, "-part-", p, ".pdf", sep = ""), 
            width = 10, height = 8)
        print(plot(sil))
        dev.off()
        cat("\n")
        
        setwd(FolderPartDestino)
        pdf(paste("fviz-silh-fold-", f, "-part-", p, ".pdf", sep = ""), 
            width = 10,height = 8)
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
        Silhouete = rbind(Silhouete, data.frame(fold, part, 
                                                maximo, minimo, 
                                                mediana, media,
                                                primeiroQuadrante,
                                                terceiroQuadrante,
                                                valueSilhouete))
        
        write.csv(Silhouete[-1, ], paste(FolderSplitDestino, "/fold-", 
                                         f, "-silho.csv", sep = ""), 
                  row.names = FALSE)
        
      } # fim do if
 
      p = p + 1
      gc() 
    } # fim da particao
    
    #f = f + 1
    gc()
    
  } # fim do fold
 
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# END COMPUTE SILHOUETE                                                                          #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



#######################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com        #
# Thank you very much!                                                #
#######################################################################
