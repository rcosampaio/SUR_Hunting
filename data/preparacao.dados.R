#### criando todos os objetos para as análises
source(here("data", "correcao.sampaio.data.R"), encoding = "UTF-8")
#head(sampaio.data)
#install.packages("https://www.mbr-pwrc.usgs.gov/software/bin/RPresence.tar.gz",repo=NULL)

########## Only work with the spp pf mark 
#sort(unique(sampaio.data$bin))
library(dplyr)
sampaio.data.f<- filter (sampaio.data, bin == " "
                                      | bin == "Crypturellus unknown" 
                                      | bin == "Cuniculus paca"
                                      | bin == "Dasyprocta fuliginosa"
                                      | bin == "Sciurus spadiceus"
                                      | bin == "Mazama americana"
                                      | bin == "Mazama nemorivaga"
                                      | bin == "Mazama unknown"
                                      | bin == "Mitu tuberosum"
                                      | bin == "Myrmecophaga tridactyla"
                                      | bin == "Nasua nasua"
                                      | bin == "Cabassous unicinctus"
                                      | bin == "Dasypus kappleri"
                                      | bin == "Dasypus novemcinctus"
                                      | bin == "Dasypus unknown"
                                      | bin == "Panthera onca"
                                      | bin == "Pecari tajacu"
                                      | bin == "Penelope jacquacu"
                                      | bin == "Priodontes maximus"
                                      | bin == "Psophia leucoptera"
                                      | bin == "Penelope jacquacu"
                                      | bin == "Puma concolor"
                                      | bin == "Tapirus terrestris"
                                      | bin == "Tayassu pecari"
                                      | bin == "Puma concolor"
                                      | bin == "Tinamus guttatus"
                                      | bin == "Tinamus major"
                                      | bin == "Tinamus tao"
                                      | bin == "Tinamus unknown")
#sort(unique(sampaio.data.f$bin))
##### Organizando os nomes das sp.
sampaio.data.f[sampaio.data.f$bin == "Crypturellus unknown",]$bin<-"Crypturellus spp"
sampaio.data.f[sampaio.data.f$bin == "Sciurus spadiceus",]$bin<-"Hadrosciurus spadiceus" 
sampaio.data.f[sampaio.data.f$bin == "Mazama unknown",]$bin<- "Mazama Sp"
sampaio.data.f[sampaio.data.f$bin == "Cabassous unicinctus",]$bin <- "Nonspecific small cingulata"
sampaio.data.f[sampaio.data.f$bin == "Dasypus kappleri",]$bin <- "Nonspecific small cingulata"
sampaio.data.f[sampaio.data.f$bin == "Dasypus novemcinctus",]$bin <- "Nonspecific small cingulata"
sampaio.data.f[sampaio.data.f$bin == "Dasypus unknown",]$bin <- "Nonspecific small cingulata"
sampaio.data.f[sampaio.data.f$bin == "Psophia leucoptera",]$bin<-"Psophia spp."
sampaio.data.f[sampaio.data.f$bin == "Tinamus guttatus",]$bin<- "Tinamus spp."    
sampaio.data.f[sampaio.data.f$bin == "Tinamus major",]$bin<- "Tinamus spp."    
sampaio.data.f[sampaio.data.f$bin == "Tinamus tao",]$bin<- "Tinamus spp."    
sampaio.data.f[sampaio.data.f$bin == "Tinamus unknown",]$bin<- "Tinamus spp."  


######## Excluindo dados com mais de 40 dias de amostragem
### Criando a um objeto mostrando a primeira foto para cada câmera
colnames(sampaio.data.f)

camera.primeira.foto<-aggregate(sampaio.data.f$data.hora.correta, list(sampaio.data.f$Camera.Trap.Name), min)
colnames(camera.primeira.foto)
head(camera.primeira.foto)
colnames(camera.primeira.foto)<-c("Camera.Trap.Name","primeira.foto")

#### Unindo a coluna da primeira foto da camera no objeto Acre.cov

teste<-merge(sampaio.data.f,camera.primeira.foto, by="Camera.Trap.Name")
#dim(sampaio.data.f)
#dim(teste)
### Criando uma coluna entre a diferençaa da foto com a primeira
teste$diff.foto.primeirafoto<-difftime(as.POSIXct(teste$data.hora.correta),
                                       as.POSIXct(teste$primeira.foto), units = "days")
dim(teste)

#### Filtrando dados para não mais que 40 dias
sampaio.data.f.2<-teste[teste$diff.foto.primeirafoto<39, ]
dim(sampaio.data.f.2)
length(unique(sampaio.data.f.2$Camera.Trap.Name))
#### 410 CT
range(sampaio.data.f.2$diff.foto.primeirafoto)


### Mark data
abr<-read.csv(here("data", "abr.data.csv"), sep = ";",  header = T, check.names=FALSE)
head(abr)
colnames(abr)[1]<-"Camera.Trap.Name"
abr$Camera.Trap.Name<-as.character(abr$Camera.Trap.Name)
#length(unique(abr$Camera.Trap.Name))
library(lubridate)
abr$data.hora.correta<-dmy_hm(abr$data.hora.correta)
# Avaliando as spp do mark
### Corrigindo Dasyspus spp
abr[abr$bin == "Dasyspus spp",]$bin<-"Nonspecific small cingulata"
#sort(unique(abr$bin))
#head(abr)

#### Excluindo dados após 40 dias 
### Criando a um objeto mostrando a primeira foto para cada câmera
colnames(abr)

ct.abr.primeira.foto<-aggregate(abr$data.hora.correta, list(abr$Camera.Trap.Name), min)
colnames(ct.abr.primeira.foto)
head(ct.abr.primeira.foto)
colnames(ct.abr.primeira.foto)<-c("Camera.Trap.Name","primeira.foto")
#### Unindo a coluna da primeira foto da camera no objeto Acre.cov

teste.2<-merge(abr[c(1:4)],ct.abr.primeira.foto, by="Camera.Trap.Name")
head(teste.2)
#dim(teste.2)
### Criando uma coluna entre a diferençaa da foto com a primeira
teste.2$diff.foto.primeirafoto<-difftime(as.POSIXct(teste.2$data.hora.correta),
                                       as.POSIXct(teste.2$primeira.foto), units = "days")
range(teste.2$diff.foto.primeirafoto)
#### Filtrando dados para não mais que 40 dias
abr<-teste.2[teste.2$diff.foto.primeirafoto<39, ]
dim(abr)
length(unique(abr$Camera.Trap.Name))
#### 288 CT
range(abr$diff.foto.primeirafoto)
head(abr)

#### Unindo os dois conjunto de dados
colnames(sampaio.data.f.2)
colnames(abr)
data<-dplyr::bind_rows(sampaio.data.f.2 , abr[c(1,2,4,6)])
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
Covariaveis$Retirada.2<-if_else(Covariaveis$Retirada-Covariaveis$Instalacao > 39, Covariaveis$Instalacao + 39, Covariaveis$Retirada)

### Organizando as cameras com problemas
library(dplyr)
Covariaveis$Problem1_from <-if_else(Covariaveis$Problem1_from < Covariaveis$Retirada.2, Covariaveis$Problem1_from, Covariaveis$Problem1_from[1])
Covariaveis$Problem1_to <-if_else(Covariaveis$Problem1_to > Covariaveis$Retirada.2, Covariaveis$Retirada.2, Covariaveis$Problem1_to[1])

###### Inserir dados de covariáveis das cameras Sampaio data
abr.CT<-read.csv(here("data","abr.CT.data.csv"), sep = ";", encoding = "UTF-8", header = T, check.names=FALSE)
colnames(abr.CT)[1]<-"Camera.Trap.Name"
head(abr.CT)
tail(abr.CT)
abr.CT<-abr.CT[order(abr.CT$Camera.Trap.Name),] ### Ordenando os sítios
abr.CT<-abr.CT[1:3]
abr.CT$Camera.Trap.Name<-as.character(abr.CT$Camera.Trap.Name)
library(lubridate)
abr.CT$Instalacao.1<-dmy_hm(abr.CT$Instalacao.1)
abr.CT$Instalacao<- strftime(abr.CT$Instalacao.1, format = "%Y-%m-%d")
abr.CT$Instalacao<-as.Date(abr.CT$Instalacao)
abr.CT$Retirada.1<-dmy_hm(abr.CT$Retirada.1)
abr.CT$Retirada<-strftime(abr.CT$Retirada.1, format = "%Y-%m-%d")
abr.CT$Retirada<-as.Date(abr.CT$Retirada)
head(abr.CT)

### Calculando o esforço por CT
ct.effort.abr<-aggregate(abr$diff.foto.primeirafoto, list(abr$Camera.Trap.Name), max)
colnames(ct.effort.abr)<-c("Camera.Trap.Name","Effort")
head(ct.effort.abr)
range(ct.effort.abr$Effort)
class(ct.effort.abr$Effort)
#plot(ct.effort.abr$Effort)
abr.CT$Camera.Trap.Name==ct.effort.abr$Camera.Trap.Name

### Organizando a data de retirada em função dos 40 dias de amostragem
abr.CT$Retirada.2<-if_else(abr.CT$Retirada-abr.CT$Instalacao > 39, abr.CT$Instalacao + 39, abr.CT$Retirada)


#### Unindo os dados de covariáveis
colnames(Covariaveis)
colnames(abr.CT)
data.CT<-dplyr::bind_rows(Covariaveis[c(1,11,12,34,23,24)], abr.CT[c(1,4,5,6)])
head(data.CT)
tail(data.CT)
length(unique(data$Camera.Trap.Name))
range(data.CT$Retirada.2-data.CT$Instalacao)

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


# Criando historico de deteçõeso (com esforço), ARTIODACTILOS
h.det.paca <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                               speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                               species ="Cuniculus paca",
                               occasionLength=5, day1 = "station", includeEffort = TRUE,
                               scaleEffort = FALSE)
h.det.paca.7 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                 speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                 species ="Cuniculus paca",
                                 occasionLength=7, day1 = "station", includeEffort = TRUE,
                                 scaleEffort = FALSE)
h.det.paca.10 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Cuniculus paca",
                                  occasionLength=10, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
h.det.paca.12 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Cuniculus paca",
                                  occasionLength=12, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
h.det.paca.15 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Cuniculus paca",
                                  occasionLength=15, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
##############################
h.det.catitu <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                 speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                 species ="Pecari tajacu",
                                 occasionLength=5, day1 = "station", includeEffort = TRUE,
                                 scaleEffort = FALSE)
h.det.catitu.7 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Pecari tajacu",
                                   occasionLength=7, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.catitu.10 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Pecari tajacu",
                                    occasionLength=10, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)
h.det.catitu.12 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Pecari tajacu",
                                    occasionLength=12, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)
h.det.catitu.15 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Pecari tajacu",
                                    occasionLength=15, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)
##############################
h.det.vermelho <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Mazama americana",
                                   occasionLength=5, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.vermelho.7 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                     speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                     species ="Mazama americana",
                                     occasionLength=7, day1 = "station", includeEffort = TRUE,
                                     scaleEffort = FALSE)
h.det.vermelho.10 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                      speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                      species ="Mazama americana",
                                      occasionLength=10, day1 = "station", includeEffort = TRUE,
                                      scaleEffort = FALSE)
h.det.vermelho.12 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                      speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                      species ="Mazama americana",
                                      occasionLength=12, day1 = "station", includeEffort = TRUE,
                                      scaleEffort = FALSE)
h.det.vermelho.15 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                      speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                      species ="Mazama americana",
                                      occasionLength=15, day1 = "station", includeEffort = TRUE,
                                      scaleEffort = FALSE)
##############################
h.det.cutia <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                species ="Dasyprocta fuliginosa",
                                occasionLength=5, day1 = "station", includeEffort = TRUE,
                                scaleEffort = FALSE)
h.det.cutia.7 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Dasyprocta fuliginosa",
                                  occasionLength=7, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
h.det.cutia.10 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Dasyprocta fuliginosa",
                                   occasionLength=10, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.cutia.12 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Dasyprocta fuliginosa",
                                   occasionLength=12, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.cutia.15 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Dasyprocta fuliginosa",
                                   occasionLength=15, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
##############################
h.det.queixada <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Tayassu pecari",
                                   occasionLength=5, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.queixada.7 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                     speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                     species ="Tayassu pecari",
                                     occasionLength=7, day1 = "station", includeEffort = TRUE,
                                     scaleEffort = FALSE)
h.det.queixada.10 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                      speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                      species ="Tayassu pecari",
                                      occasionLength=10, day1 = "station", includeEffort = TRUE,
                                      scaleEffort = FALSE)
h.det.queixada.12 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                      speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                      species ="Tayassu pecari",
                                      occasionLength=12, day1 = "station", includeEffort = TRUE,
                                      scaleEffort = FALSE)
h.det.queixada.15 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                      speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                      species ="Tayassu pecari",
                                      occasionLength=15, day1 = "station", includeEffort = TRUE,
                                      scaleEffort = FALSE)
##############################
h.det.nambu<- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                               speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                               species ="Tinamus spp.",
                               occasionLength=5, day1 = "station", includeEffort = TRUE,
                               scaleEffort = FALSE)
h.det.nambu.7<- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                 speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                 species ="Tinamus spp.",
                                 occasionLength=7, day1 = "station", includeEffort = TRUE,
                                 scaleEffort = FALSE)
h.det.nambu.10<- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Tinamus spp.",
                                  occasionLength=10, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
h.det.nambu.12<- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Tinamus spp.",
                                  occasionLength=12, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
h.det.nambu.15<- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Tinamus spp.",
                                  occasionLength=15, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
##############################
h.det.tatu <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                               speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                               species ="Nonspecific small cingulata",
                               occasionLength=5, day1 = "station", includeEffort = TRUE,
                               scaleEffort = FALSE)
h.det.tatu.7 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                 speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                 species ="Nonspecific small cingulata",
                                 occasionLength=5, day1 = "station", includeEffort = TRUE,
                                 scaleEffort = FALSE)
h.det.tatu.10 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Nonspecific small cingulata",
                                  occasionLength=10, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
h.det.tatu.12 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Nonspecific small cingulata",
                                  occasionLength=12, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)

h.det.tatu.15 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Nonspecific small cingulata",
                                  occasionLength=15, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)

##############################
###### INSERI NA RCI
#ct.cinza <- cameraOperation(CTtable = data.CT[!data.CT$RESEX=="RCI",], stationCol = "Camera.Trap.Name",setupCol = "Instalacao",
 #                           retrievalCol = "Retirada.2",
  #                          writecsv = FALSE, hasProblems = T, dateFormat = "%Y-%m-%d")
h.det.cinza <- detectionHistory(recordTable= data,camOp = ct, stationCol="Camera.Trap.Name",
                                speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                species ="Mazama nemorivaga",
                                occasionLength=5, day1 = "station", includeEffort = TRUE,
                                scaleEffort = FALSE)
h.det.cinza.7 <- detectionHistory(recordTable= data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Mazama nemorivaga",
                                  occasionLength=7, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
h.det.cinza.10 <- detectionHistory(recordTable= data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Mazama nemorivaga",
                                   occasionLength=10, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.cinza.12 <- detectionHistory(recordTable= data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Mazama nemorivaga",
                                   occasionLength=12, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.cinza.15 <- detectionHistory(recordTable= data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Mazama nemorivaga",
                                   occasionLength=15, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
##############################
h.det.mutum <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                species ="Mitu tuberosum",
                                occasionLength=5, day1 = "station", includeEffort = TRUE,
                                scaleEffort = FALSE)
h.det.mutum.7 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Mitu tuberosum",
                                  occasionLength=7, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
h.det.mutum.10 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Mitu tuberosum",
                                   occasionLength=10, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.mutum.12 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Mitu tuberosum",
                                   occasionLength=10, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.mutum.15 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Mitu tuberosum",
                                   occasionLength=15, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
########################
h.det.anta <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                               speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                               species ="Tapirus terrestris",
                               occasionLength=5, day1 = "station", includeEffort = TRUE,
                               scaleEffort = FALSE)
h.det.anta.7 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                 speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                 species ="Tapirus terrestris",
                                 occasionLength=7, day1 = "station", includeEffort = TRUE,
                                 scaleEffort = FALSE)
h.det.anta.10 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Tapirus terrestris",
                                  occasionLength=10, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
h.det.anta.12 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Tapirus terrestris",
                                  occasionLength=12, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
h.det.anta.15 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Tapirus terrestris",
                                  occasionLength=15, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
##############################
##### NA RDU
#ct.jacu <- cameraOperation(CTtable = data.CT[!data.CT$RESEX=="RDU",], stationCol = "Camera.Trap.Name",setupCol = "Instalacao",
 #                          retrievalCol = "Retirada.2",
  #                         writecsv = FALSE, hasProblems = T, dateFormat = "%Y-%m-%d")

h.det.jacu <- detectionHistory(recordTable=data, camOp = ct, stationCol="Camera.Trap.Name",
                               speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                               species ="Penelope jacquacu",
                               occasionLength=5, day1 = "station", includeEffort = TRUE,
                               scaleEffort = FALSE)
##############################
##### NA RDU
h.det.catipuru <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Hadrosciurus spadiceus",
                                   occasionLength=5, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
##############################
h.det.jacamim<- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                 speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                 species ="Psophia spp.",
                                 occasionLength=5, day1 = "station", includeEffort = TRUE,
                                 scaleEffort = FALSE)
h.det.jacamim.7<- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Psophia spp.",
                                   occasionLength=7, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.jacamim.10<- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Psophia spp.",
                                    occasionLength=10, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)
h.det.jacamim.12<- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Psophia spp.",
                                    occasionLength=12, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)

h.det.jacamim.15<- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Psophia spp.",
                                    occasionLength=15, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)
##############################
h.det.coati <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                species ="Nasua nasua",
                                occasionLength=5, day1 = "station", includeEffort = TRUE,
                                scaleEffort = FALSE)
h.det.coati.7 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                  speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                  species ="Nasua nasua",
                                  occasionLength=7, day1 = "station", includeEffort = TRUE,
                                  scaleEffort = FALSE)
h.det.coati.10 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Nasua nasua",
                                   occasionLength=10, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.coati.12 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Nasua nasua",
                                   occasionLength=12, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.coati.15 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Nasua nasua",
                                   occasionLength=15, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
##############################
h.det.onca.p <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                 speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                 species ="Panthera onca",
                                 occasionLength=5, day1 = "station", includeEffort = TRUE,
                                 scaleEffort = FALSE)
h.det.onca.p.7 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Panthera onca",
                                   occasionLength=7, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.onca.p.10 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Panthera onca",
                                    occasionLength=10, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)
h.det.onca.p.12 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Panthera onca",
                                    occasionLength=12, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)
h.det.onca.p.15 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Panthera onca",
                                    occasionLength=15, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)
##############################
h.det.onca.v <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                 speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                 species ="Puma concolor",
                                 occasionLength=5, day1 = "station", includeEffort = TRUE,
                                 scaleEffort = FALSE)
h.det.onca.v.7 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Puma concolor",
                                   occasionLength=7, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.onca.v.10 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Puma concolor",
                                    occasionLength=10, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)
h.det.onca.v.12 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Puma concolor",
                                    occasionLength=12, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)
h.det.onca.v.15 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                    speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                    species ="Puma concolor",
                                    occasionLength=15, day1 = "station", includeEffort = TRUE,
                                    scaleEffort = FALSE)

##############################
h.det.canastra <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Priodontes maximus",
                                   occasionLength=5, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.canastra.7 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                     speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                     species ="Priodontes maximus",
                                     occasionLength=7, day1 = "station", includeEffort = TRUE,
                                     scaleEffort = FALSE)
h.det.canastra.10 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                      speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                      species ="Priodontes maximus",
                                      occasionLength=10, day1 = "station", includeEffort = TRUE,
                                      scaleEffort = FALSE)
h.det.canastra.12 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                      speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                      species ="Priodontes maximus",
                                      occasionLength=12, day1 = "station", includeEffort = TRUE,
                                      scaleEffort = FALSE)
h.det.canastra.15 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                      speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                      species ="Priodontes maximus",
                                      occasionLength=15, day1 = "station", includeEffort = TRUE,
                                      scaleEffort = FALSE)
##############################
h.det.bandeira <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                   speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                   species ="Myrmecophaga tridactyla",
                                   occasionLength=5, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE)
h.det.bandeira.7 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                     speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                     species ="Myrmecophaga tridactyla",
                                     occasionLength=7, day1 = "station", includeEffort = TRUE,
                                     scaleEffort = FALSE)
h.det.bandeira.10 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                      speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                      species ="Myrmecophaga tridactyla",
                                      occasionLength=10, day1 = "station", includeEffort = TRUE,
                                      scaleEffort = FALSE)
h.det.bandeira.12 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                      speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                      species ="Myrmecophaga tridactyla",
                                      occasionLength=12, day1 = "station", includeEffort = TRUE,
                                      scaleEffort = FALSE)
h.det.bandeira.15 <- detectionHistory(recordTable=data,camOp = ct, stationCol="Camera.Trap.Name",
                                      speciesCol ="bin", recordDateTimeCol="data.hora.correta",
                                      species ="Myrmecophaga tridactyla",
                                      occasionLength=15, day1 = "station", includeEffort = TRUE,
                                      scaleEffort = FALSE)
