#### criando todos os objetos para as análises
source(here("data", "correcao.sampaio.data.R"), encoding = "UTF-8")
#head(sampaio.data)

########## Only work with the spp pf mark 
#sort(unique(sampaio.data$bin))
library(dplyr)
sampaio.data.f<- filter (sampaio.data, bin == " "
                                      | bin == "Atelocynus microtis"
                                      | bin == "Cabassous unicinctus"
                                      | bin == "Crypturellus unknown" 
                                      | bin == "Cuniculus paca"
                                      | bin == "Dasyprocta fuliginosa"
                                      | bin == "Dasypus kappleri"
                                      | bin == "Dasypus novemcinctus"
                                      | bin == "Dasypus unknown"
                                      | bin == "Didelphis marsupialis"
                                      | bin == "Eira barbara"
                                      | bin == "Leopardus pardalis"
                                      | bin == "Leopardus wiedii"
                                      | bin == "Mazama americana"
                                      | bin == "Mazama nemorivaga"
                                      | bin == "Mitu tuberosum"
                                      | bin == "Myoprocta pratti"
                                      | bin == "Myrmecophaga tridactyla"
                                      | bin == "Nasua nasua"
                                      | bin == "Panthera onca"
                                      | bin == "Pecari tajacu"
                                      | bin == "Penelope jacquacu"
                                      | bin == "Priodontes maximus"
                                      | bin == "Procyon cancrivorous"
                                      | bin == "Psophia leucoptera"
                                      | bin == "Puma concolor"
                                      | bin == "Puma yagouaroundi"
                                      | bin == "Sciurus spadiceus"
                                      | bin == "Speothos venaticus"
                                      | bin == "Tamandua tetradactyla"
                                      | bin == "Tapirus terrestris"
                                      | bin == "Tayassu pecari"
                                      | bin == "Tinamus guttatus"
                                      | bin == "Tinamus major"
                                      | bin == "Tinamus tao"
                                      | bin == "Tinamus unknown")
#sort(unique(sampaio.data.f$bin))
##### Organizando os nomes das sp.
sampaio.data.f[sampaio.data.f$bin == "Crypturellus unknown",]$bin<-"Crypturellus spp"
#sampaio.data.f[sampaio.data.f$bin == "Sciurus spadiceus",]$bin<-"Hadrosciurus spadiceus" 
#sampaio.data.f[sampaio.data.f$bin == "Mazama unknown",]$bin<- "Mazama Sp"
sampaio.data.f[sampaio.data.f$bin == "Cabassous unicinctus",]$bin <- "Nonspecific small cingulata"
sampaio.data.f[sampaio.data.f$bin == "Dasypus kappleri",]$bin <- "Nonspecific small cingulata"
sampaio.data.f[sampaio.data.f$bin == "Dasypus novemcinctus",]$bin <- "Nonspecific small cingulata"
sampaio.data.f[sampaio.data.f$bin == "Dasypus unknown",]$bin <- "Nonspecific small cingulata"
#sampaio.data.f[sampaio.data.f$bin == "Psophia leucoptera",]$bin<-"Psophia spp."
sampaio.data.f[sampaio.data.f$bin == "Tinamus guttatus",]$bin<- "Tinamus spp"    
sampaio.data.f[sampaio.data.f$bin == "Tinamus major",]$bin<- "Tinamus spp"    
sampaio.data.f[sampaio.data.f$bin == "Tinamus tao",]$bin<- "Tinamus spp"    
sampaio.data.f[sampaio.data.f$bin == "Tinamus unknown",]$bin<- "Tinamus spp"  


######## Excluindo dados com mais de 40 dias de amostragem
### Criando a um objeto mostrando a primeira foto para cada câmera
#colnames(sampaio.data.f)

#camera.primeira.foto<-aggregate(sampaio.data.f$data.hora.correta, list(sampaio.data.f$Camera.Trap.Name), min)
#colnames(camera.primeira.foto)
#head(camera.primeira.foto)
#colnames(camera.primeira.foto)<-c("Camera.Trap.Name","primeira.foto")

#### Unindo a coluna da primeira foto da camera no objeto Acre.cov

#teste<-merge(sampaio.data.f,camera.primeira.foto, by="Camera.Trap.Name")
#dim(sampaio.data.f)
#dim(teste)
### Criando uma coluna entre a diferençaa da foto com a primeira
#teste$diff.foto.primeirafoto<-difftime(as.POSIXct(teste$data.hora.correta),
 #                                      as.POSIXct(teste$primeira.foto), units = "days")
#dim(teste)

#### Filtrando dados para não mais que 40 dias
#sampaio.data.f.2<-teste[teste$diff.foto.primeirafoto<39, ]
#dim(sampaio.data.f.2)
#length(unique(sampaio.data.f.2$Camera.Trap.Name))
#### 410 CT
#range(sampaio.data.f.2$diff.foto.primeirafoto)


### Mark data
abr<-read.csv(here("data", "abr.data.2.csv"), sep = ",",  header = T, check.names=FALSE)
head(abr)
tail(abr)
colnames(abr)[1]<-"Camera.Trap.Name"
length(unique(abr$Camera.Trap.Name))
abr$Camera.Trap.Name<-as.character(abr$Camera.Trap.Name)
abr[abr$Camera.Trap.Name=="2043a",]$Camera.Trap.Name<-"2043"
abr[abr$Camera.Trap.Name=="2043b",]$Camera.Trap.Name<-"2043"
abr[abr$Camera.Trap.Name=="30.13",]$Camera.Trap.Name<-"3013"
dim(abr)
#abr[36841,1]<-3013
#abr[36840,]
### Removendo cameras que eu não tenho os metadados
abr<-abr[!abr$Camera.Trap.Name == "6024" &
      !abr$Camera.Trap.Name == "6025"&
        !abr$Camera.Trap.Name == "6042"&
        !abr$Camera.Trap.Name == "6044"&
        !abr$Camera.Trap.Name == "6045", ]
sort(unique(abr$Camera.Trap.Name))

#length(unique(abr$Camera.Trap.Name))
library(lubridate)
abr$data.hora.correta<-mdy_hm(abr$data.hora.correta)
# Avaliando as spp do mark
### Corrigindo Dasyspus spp
abr[abr$bin == "Dasyspus spp",]$bin<-"Nonspecific small cingulata"
abr[abr$bin == "Nonspecific Cingulata small",]$bin<-"Nonspecific small cingulata"

abr[abr$bin == "Crypyurelus spp",]$bin<-"Crypturellus spp"

sort(unique(abr$bin))
#head(abr)

#### Excluindo dados após 40 dias 
### Criando a um objeto mostrando a primeira foto para cada câmera
#colnames(abr)

#ct.abr.primeira.foto<-aggregate(abr$data.hora.correta, list(abr$Camera.Trap.Name), min)
#colnames(ct.abr.primeira.foto)
#head(ct.abr.primeira.foto)
#colnames(ct.abr.primeira.foto)<-c("Camera.Trap.Name","primeira.foto")
#### Unindo a coluna da primeira foto da camera no objeto Acre.cov

#teste.2<-merge(abr[c(1:4)],ct.abr.primeira.foto, by="Camera.Trap.Name")
#head(teste.2)
#dim(teste.2)
### Criando uma coluna entre a diferençaa da foto com a primeira
#teste.2$diff.foto.primeirafoto<-difftime(as.POSIXct(teste.2$data.hora.correta),
  #                                     as.POSIXct(teste.2$primeira.foto), units = "days")
#range(teste.2$diff.foto.primeirafoto)
#### Filtrando dados para não mais que 40 dias
#abr<-teste.2[teste.2$diff.foto.primeirafoto<39, ]
#dim(abr)
#length(unique(abr$Camera.Trap.Name))
#### 288 CT
#range(abr$diff.foto.primeirafoto)
#head(abr)

#### Unindo os dois conjunto de dados
colnames(sampaio.data.f)
colnames(abr)
data<-dplyr::bind_rows(sampaio.data.f[c(3,33,36)] , abr[c(1,2,4)])
head(data)
tail(data)
length(unique(data$Camera.Trap.Name))

###### Inserir dados de covariáveis das cameras Sampaio data
Covariaveis <- read.csv(here("data","Dados_todas_cameras_com_dados.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
colnames(Covariaveis)[1]<-"Camera.Trap.Name"
Covariaveis$Instalacao <- as.Date(Covariaveis$Instalacao, format= "%d/%m/%Y")
Covariaveis$Retirada <- as.Date(Covariaveis$Retirada, format= "%d/%m/%Y")
Covariaveis$Problem1_to <- as.Date(Covariaveis$Problem1_to, format= "%d/%m/%Y")
Covariaveis$Problem1_from <- as.Date(Covariaveis$Problem1_from, format= "%d/%m/%Y")
Covariaveis<-Covariaveis[order(Covariaveis$Camera.Trap.Name),]

### Organizando a data de retirada em função dos 40 dias de amostragem
#Covariaveis$Retirada.2<-if_else(Covariaveis$Retirada-Covariaveis$Instalacao > 39, Covariaveis$Instalacao + 39, Covariaveis$Retirada)

### Organizando as cameras com problemas
library(dplyr)
#Covariaveis$Problem1_from <-if_else(Covariaveis$Problem1_from < Covariaveis$Retirada, Covariaveis$Problem1_from, Covariaveis$Problem1_from[1])
#Covariaveis$Problem1_to <-if_else(Covariaveis$Problem1_to > Covariaveis$Retirada, Covariaveis$Retirada, Covariaveis$Problem1_to[1])

###### Inserir dados de covariáveis das cameras Sampaio data
abr.CT<-read.csv(here("data","abr.CT.data.csv"), sep = ";", encoding = "UTF-8", header = T, check.names=FALSE)
colnames(abr.CT)[1]<-"Camera.Trap.Name"
head(abr.CT)
tail(abr.CT)
abr.CT<-abr.CT[order(abr.CT$Camera.Trap.Name),] ### Ordenando os sítios
abr.CT<-abr.CT[1:3]
abr.CT$Camera.Trap.Name<-as.character(abr.CT$Camera.Trap.Name)
sort(abr.CT$Camera.Trap.Name)

library(lubridate)
abr.CT$Instalacao.1<-dmy_hm(abr.CT$Instalacao.1)
abr.CT$Instalacao<- strftime(abr.CT$Instalacao.1, format = "%Y-%m-%d")
abr.CT$Instalacao<-as.Date(abr.CT$Instalacao)
abr.CT$Retirada.1<-dmy_hm(abr.CT$Retirada.1)
abr.CT$Retirada<-strftime(abr.CT$Retirada.1, format = "%Y-%m-%d")
abr.CT$Retirada<-as.Date(abr.CT$Retirada)
head(abr.CT)

#### Unindo os dados de covariáveis
colnames(Covariaveis)
colnames(abr.CT)
data.CT<-dplyr::bind_rows(Covariaveis[c(1,11,12,23,24)], abr.CT[c(1,4,5)])
head(data.CT)
tail(data.CT)
length(unique(data$Camera.Trap.Name))
range(data.CT$Retirada-data.CT$Instalacao)
tail(data.CT)
names(data.CT)
data.CT$Problem1_from
data.CT$Problem1_to

#install.packages("camtrapR")
library(camtrapR) 
ct <- cameraOperation(CTtable = data.CT, stationCol = "Camera.Trap.Name",setupCol = "Instalacao",
                      retrievalCol = "Retirada",
                      writecsv = FALSE, hasProblems = T, dateFormat = "%Y-%m-%d")
# Survey report
reportTest <- surveyReport (recordTable=data, 
                                    CTtable=data.CT,
                                    speciesCol="bin",
                                    stationCol="Camera.Trap.Name",
                                    cameraCol ="Camera.Trap.Name",
                                    setupCol = "Instalacao",
                                    retrievalCol = "Retirada",
                                    CTDateFormat = "%Y-%m-%d",
                                    recordDateTimeCol="data.hora.correta",
                                    recordDateTimeFormat="%Y-%m-%d %H:%M:%S",
                                    CTHasProblems = T)
reportTest [1]
reportTest [2]

# Criando historico de deteçõeso (com esforço)
sort(unique(data$bin))
length(unique(data$bin))-1

############################## 1
h.det.paca <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                               speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                               species ="Cuniculus paca",
                               occasionLength=5, day1 = "station", includeEffort = TRUE,
                               scaleEffort = FALSE)
############################## 2
h.det.catitu <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                 speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                 species ="Pecari tajacu",
                                 occasionLength=5, day1 = "station", includeEffort = TRUE,
                                 scaleEffort = FALSE)
############################## 3
h.det.vermelho <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Mazama americana",
                                   occasionLength=5, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
############################## 4
h.det.cutia <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                species ="Dasyprocta fuliginosa",
                                occasionLength=5, day1 = "station", includeEffort = TRUE,
                                scaleEffort = FALSE)
############################## 5
h.det.queixada <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Tayassu pecari",
                                   occasionLength=5, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
############################## 6
h.det.nambu<- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                               speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                               species ="Tinamus spp",
                               occasionLength=5, day1 = "station", includeEffort = TRUE,
                               scaleEffort = FALSE)
############################## 7
h.det.tatu <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                               speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                               species ="Nonspecific small cingulata",
                               occasionLength=5, day1 = "station", includeEffort = TRUE,
                               scaleEffort = FALSE)
############################## 8
###### INSERI NA RCI
#ct.cinza <- cameraOperation(CTtable = data.CT[!data.CT$RESEX=="RCI",], stationCol = "Camera.Trap.Name",setupCol = "Instalacao",
 #                           retrievalCol = "Retirada.2",
  #                          writecsv = FALSE, hasProblems = T, dateFormat = "%Y-%m-%d")
h.det.cinza <- detectionHistory(recordTable= data,camOp = ct, stationCol="Camera.Trap.Name",
                                speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                species ="Mazama nemorivaga",
                                occasionLength=5, day1 = "station", includeEffort = TRUE,
                                scaleEffort = FALSE)
############################## 9
h.det.mutum <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                species ="Mitu tuberosum",
                                occasionLength=5, day1 = "station", includeEffort = TRUE,
                                scaleEffort = FALSE)
############################# 10
h.det.anta <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                               speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                               species ="Tapirus terrestris",
                               occasionLength=5, day1 = "station", includeEffort = TRUE,
                               scaleEffort = FALSE)
############################## 11
##### NA RDU
#ct.jacu <- cameraOperation(CTtable = data.CT[!data.CT$RESEX=="RDU",], stationCol = "Camera.Trap.Name",setupCol = "Instalacao",
 #                          retrievalCol = "Retirada.2",
  #                         writecsv = FALSE, hasProblems = T, dateFormat = "%Y-%m-%d")

h.det.jacu <- detectionHistory(recordTable=data, camOp = ct, stationCol="Camera.Trap.Name",
                               speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                               species ="Penelope jacquacu",
                               occasionLength=5, day1 = "station", includeEffort = TRUE,
                               scaleEffort = FALSE)
############################## 12
##### NA RDU
h.det.catipuru <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Sciurus spadiceus",
                                   occasionLength=5, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
############################## 13 
h.det.jacamim<- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                 speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                 species ="Psophia leucoptera",
                                 occasionLength=5, day1 = "station", includeEffort = TRUE,
                                 scaleEffort = FALSE)
############################## 14
h.det.coati <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                species ="Nasua nasua",
                                occasionLength=5, day1 = "station", includeEffort = TRUE,
                                scaleEffort = FALSE)
############################## 15
h.det.onca.p <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                 speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                 species ="Panthera onca",
                                 occasionLength=5, day1 = "station", includeEffort = TRUE,
                                 scaleEffort = FALSE)
############################## 16
h.det.onca.v <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                 speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                 species ="Puma concolor",
                                 occasionLength=5, day1 = "station", includeEffort = TRUE,
                                 scaleEffort = FALSE)
############################## 17
h.det.canastra <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Priodontes maximus",
                                   occasionLength=5, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
############################## 18
h.det.bandeira <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Myrmecophaga tridactyla",
                                   occasionLength=5, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
############################## 19
h.det.atelocynus <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Atelocynus microtis",
                                   occasionLength=5, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
############################## 20
h.det.crypturellus <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                     speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                     species ="Crypturellus spp",
                                     occasionLength=5, day1 = "station", includeEffort = TRUE,
                                     scaleEffort = FALSE)
############################## 21
h.det.didelphis <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                     speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                     species ="Didelphis marsupialis",
                                     occasionLength=5, day1 = "station", includeEffort = TRUE,
                                     scaleEffort = FALSE)
############################## 22
h.det.irara <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Eira barbara",
                                    occasionLength=5, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)
############################## 23
h.det.pardalis <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Leopardus pardalis",
                                    occasionLength=5, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)
############################## 24
h.det.wiedii <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Leopardus wiedii",
                                   occasionLength=5, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
############################## 25
h.det.cutiara <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                 speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                 species ="Myoprocta pratti",
                                 occasionLength=5, day1 = "station", includeEffort = TRUE,
                                 scaleEffort = FALSE)
############################## 26
h.det.procyon <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Procyon cancrivorus",
                                  occasionLength=5, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
############################## 27
h.det.jaguarundi <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Puma yagouaroundi",
                                  occasionLength=5, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
############################## 28
h.det.vinagre <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                     speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                     species ="Speothos venaticus",
                                     occasionLength=5, day1 = "station", includeEffort = TRUE,
                                     scaleEffort = FALSE)
############################## 29
h.det.mambira <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Tamandua tetradactyla",
                                  occasionLength=5, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)



