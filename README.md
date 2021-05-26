# Select the best hybrid partition with silhouete

This code is part of my PhD research at PPG-CC/DC/UFSCar. The aim is select the best hybrid partition for multilabel classification.

The figure bellow shows the fluxogram for this code:

## Scripts
This source code consists of an R project for R Studio and the following R scripts:

1. libraries.R
2. utils.R
3. bestPartition.R
4. run.R
5. silhouete.R

## Jars
Confirms if the folder UTILS contains the following files: Clus.jar, R_csv_2_arff.jar, and weka.jar. Without these jars, the code not runs. Also, confirms if the folder _libs_ is present with the jars: Clus.jar, commons-math-1.0.jar, jgap.jar and weka.jar.

## Datasets Folder


## Folder Path
Place a copy of this code in _"C:/Users/[username]/Best-Partition-Silhouete"_ or _"/[home/username]/Best-Partition-Silhouete_. Our files are configured to obtain the paths of the folders from the root. You can change this in the code if you want.

## File "datasets.csv"
A file called "datasets-hpmlk.csv" must be in the *datasets* folder. This file is used to read information about the datasets and they are used in the code. All 74 datasets available in cometa are in this file. If you want to use another dataset, please, add the following information about the dataset in the file:

_Id, Name, Domain, Labels, Instances, Attributes, Inputs, Labelsets, Single, Max freq, Card, Dens, MeanIR, Scumble, TCS, AttStart, AttEnd, LabelStart, LabelEnd, xn, yn, gridn"

The _"Id"_ of the dataset is a mandatory parameter (_n_dataset_) in the command line to run all code. The fields are used in a lot of internal functions. Please, make sure that this information is available before running the code. 

NOTE: Please, pay attention to the datasets file names and the names in the CSV file. They must be the same, on the contrary, an error may occur.

## Folder Strucutre


## Software Requirements
This code was develop in []. The R language version was []. Please make sure all the dependencies are installed (verify libraries.R). This code does not provide an installation of the packages.

## Hardware Requirements
This code may or may not be executed in parallel, however, it is highly recommended that you run it in parallel. The number of cores can be configured via the command line (_number_cores_). If *number_cores = 1* the code will run sequentially. In our experiments, we used ten cores. For reproducibility, we recommend that you also use ten cores.


## Preparing your experiment


The results stored in the folder _OUTPUT_ it will be used in the next phase: Test-Best-Partition-Clus, Test-Best-Partition-ECC, etc. The result for a dataset must be put in the folder _BESTPARTITIONS_.




## RUN
To run the code, open the terminal, enter */home/[username]/Best-Partition-Silhouete/scripts/* folder, and type:

```
Rscript silhouete.R [number_dataset] [number_cores] [number_folds] [name_folder_results]
```

Where:

_number_dataset_ is the dataset number in the datasets.csv file

_number_cores_ is the total cores you want to use in parallel execution.

_number_folds_ is the number of folds you want for cross-validation

_name_folders_results is the name of the folder to save the results

All parameters are mandatory. Example:

```
Rscript silhouete.R 17 5 10 /dev/shm/results/flags
```

This will execute the code for the dataset number 17 in the _dataset-hpmlk.csv_, with 5 cores, 10 folds and the process will be store in the _/dev/shm/results/flags_. This code automatically saves the results process in this folder to other folder in the root folder of the project. This is necessary to run faster.


## Acknowledgment
This study is financed in part by the Coordenação de Aperfeiçoamento de Pessoal de Nível Superior - Brasil (CAPES) - Finance Code 001

## Links

[Post-Graduate Program in Computer Science](http://ppgcc.dc.ufscar.br/pt-br)

[Biomal](http://www.biomal.ufscar.br/)

[Computer Department](https://site.dc.ufscar.br/)

[CAPES](https://www.gov.br/capes/pt-br)

[Embarcados](https://www.embarcados.com.br/author/cissa/)

[Linkedin](https://www.linkedin.com/in/elainececiliagatto/)

[Linkedin](https://www.linkedin.com/company/27241216)

[Instagram](https://www.instagram.com/professoracissa/)

[Facebook](https://www.facebook.com/ProfessoraCissa/)

[Twitter](https://twitter.com/professoracissa)

# Thanks


