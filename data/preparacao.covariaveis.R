#### CAMERAS
##abrindo dados flooded ao redor das cameras (m2)
# Area do buffer 500 m = pi*(500ˆ2) = 785398.2
flood.cam <- read.csv(here("data","flood.cam.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
flood.cam<-flood.cam[c(1,4,5)]
colnames(flood.cam)[1]<-"camera_id"
flood.cam$Flo_camera<-flood.cam$Flo_camera/(pi*(500^2))
dim(flood.cam)
### Abrindo area alagada comunidade
flo.com <- read.csv(here("data","flooded_comindade.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
colnames(flo.com)[1]<-"Comunidade"
flo.com<-aggregate(flo.com$area_km2, list(flo.com$Comunidade), sum)
colnames(flo.com)<-c("Comunidade","flo.com")
head(flo.com)
dim(flo.com)
### transformando dados para porpor??o
flo.com$flo.com<-flo.com$flo.com/(pi*(5^2))

##abrindo dados flooded ao redor das cameras (m2)
hidro.cam <- read.csv(here("data","hidro.cam.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
dim(hidro.cam)
colnames(hidro.cam)[1]<-"camera_id"
colnames(hidro.cam)[5]<- "hidro.cam.comp"
hidro.cam<-hidro.cam[c(1,5)]

##abrindo dados destamento ao redor das cameras (m2)
desmat.cam <- read.csv(here("data","desmat.cam.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
colnames(desmat.cam)[1]<-"camera_id"
desmat.cam<-desmat.cam[c(1,5)]
desmat.cam$desmat<-desmat.cam$desmat/(pi*(500^2))
dim(desmat.cam)

##abrindo dados dist. cameras para as comunidades (m)
cam.dist.cameras<- read.csv(here("data","com.dist.cameras.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
colnames(cam.dist.cameras)[1]<-"camera_id"
colnames(cam.dist.cameras)[5]<- "com.distance"
dim(cam.dist.cameras)

##Distancia cameras para UCs
UC.cam<- read.csv(here("data","UC.cam.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
colnames(UC.cam)[1]<-"camera_id"
#UC.cam$camera_id
colnames(UC.cam)[5]<-"UC.cam.distance"
UC.cam<-UC.cam[-4]
UC.cam<-UC.cam[c(1,4)]
dim(UC.cam)

##abrindo dados vertical distance to the nearest drainage (VDND)
hand<- read.csv(here("data","Hand.cameras.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
colnames(hand)[1]<-"camera_id"
colnames(hand)[5]<-"VDND"
hand<-hand[c(1,5)]
dim(hand)
##### Criando o HP
### HP Comunidades
hp.com<- read.csv(here("data","HP.com.2.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
colnames(hp.com)[1]<-"camera_id"
hp.com$HP.com<-hp.com$Casas_1km/sqrt(hp.com$Distancia/1000) #### LOG?????
range(hp.com$Distancia)
dim(hp.com)
head(hp.com)
#hist(hp.com$Distancia/1000)

#cor(hp.com[-1], use="complete.obs", method = "pearson") ### Correlação entre HP.com e tamanho da comunidade
hp.com.final<-aggregate(hp.com$HP, list(hp.com$camera_id), sum)
dim(hp.com.final)
colnames(hp.com.final)<-c("camera_id","HP.com")

##### filtrando os dados para apenas as comunidades mais proximas
head(hp.com)
library(tidyverse)
df.g <- group_by(hp.com, camera_id)
head(df.g)
hp.com.2<-as.data.frame(filter(df.g, row_number(Distancia) == 1))
dim(hp.com.2)
head(hp.com.2)
###### Criando o HP.com final
hp.com.3<-merge(hp.com.final,hp.com.2[c(1:3)], by ="camera_id" )
head(hp.com.3)
dim(hp.com.3)

#cor(hp.com.3[-1], use="complete.obs", method = "pearson") ### Correlação entre HP.com e tamanho da comunidade
#length(hp.com.2$camera_id)
#hp.com.final$HP.com<-hp.com.final$HP.com/2
#head(hp.com.final)

### HP cidades
hp.city<- read.csv(here("data","HP.city.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
colnames(hp.city)[1]<-"Estacao"
head(hp.city)
#hp.city$Estacao
hp.city<-hp.city[hp.city$Estacao=="Seca",] ### Filtrando os dados somente da Seca
hp.city<-merge(hp.city, cam.dist.cameras, by= "Comunidade") ### Unindo os dados da distÃ¢ncia das cÃ¢meras para a comunidade
hp.city$Dist.city.2<-hp.city$Dist.city+hp.city$com.distance
hp.city$HP.city<-hp.city$Pop.urbana/sqrt(hp.city$Dist.city.2/1000)
head(hp.city)
names(hp.city)
hp.city<-hp.city[c(6,5,10,11)]
dim(hp.city)
head(hp.city)
cor(hp.city[-c(1)], use="complete.obs", method = "pearson") ### Correlação entre HP.city e tamanho da cidade

### unido todos os HP
HP<-merge(hp.com.2,hp.city, by = "camera_id")
head(HP)
HP$HP<-(HP$HP.com+HP$HP.city)/3
HP$HP.local<-HP$HP.com/2
names(HP)
names(HP)[2]<-"Com.dist"
names(HP)[3]<-"Com.size"
names(HP)[5]<-"City.size"
names(HP)[6]<-"City.dist"
HP<-HP[c(1:3,9,4,5:8)]
names(HP)
cor(HP[c(2:9)], use="complete.obs", method = "pearson") ### Correlação entre HP.city e tamanho da cidade

head(HP)
dim(HP)

### Dados do esforço amostral
### HP cidades
effort<- read.csv(here("data","CT.data.Effort.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
colnames(effort)[1]<-"camera_id"
colnames(effort)[2]<-"Effort"
dim(effort)
### add coordenadas geográficas
coord<- read.csv(here("data","cam.coord.metros.csv"), sep = ";", encoding = "UTF-8", header = T, check.names=FALSE)
colnames(coord)[1]<-"camera_id"
dim(coord)
head(coord)
### add ecorregiões
ecoreg<- read.csv(here("data","cameras_ecoreg.csv"), sep = ";", encoding = "UTF-8", header = T, check.names=FALSE)
colnames(ecoreg)[1]<-"camera_id"
ecoreg<-ecoreg[c(1,2,4,8)]
dim(ecoreg)

### ad modelo das cameras
model <- read.csv(here("data","camera.model.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
colnames(model)[1]<-"camera_id"
dim(model)

### ad lat, long, dist (NN)
latXlong.NN <- read.csv(here("data","lat.long.NN.cameras.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
#head(latXlong.NN)
colnames(latXlong.NN)[c(1,2,3)]<-c("camera_id", "X1", "Y1")
latXlong.NN$latXlong<-latXlong.NN$X*latXlong.NN$Y
#latXlong.NN<-latXlong.NN[c(1,4,5)]
dim(latXlong.NN)
head(latXlong.NN)

#criando o data.frame para as covs das cameras
cameras<-Reduce(function(...) merge(..., all = TRUE), list(ecoreg,flood.cam,flo.com,hidro.cam,desmat.cam,
                                                           UC.cam, HP, effort,coord,model,latXlong.NN,hand))
### Inserindo 0 no lugar de NA
colnames(cameras)
cameras$RESEX
cameras$desmat<-replace(cameras$desmat, is.na(cameras$desmat), 0) 
cameras$Flo_camera<-replace(cameras$Flo_camera, is.na(cameras$Flo_camera), 0) 
cameras$flo.com<-replace(cameras$flo.com, is.na(cameras$flo.com), 0) 
cameras$hidro.cam.comp<-replace(cameras$hidro.cam.comp, is.na(cameras$hidro.cam.comp), 0) 
cor.test(cameras$Flo_camera,cameras$flo.com)
cameras[cameras$Effort==0,]$Effort<-0.5

##### unidos dados para comunidades com os dados de cameras
covariaveis<-cameras

# Criando um objeto com o esforço
effort.2<-reportTest[[1]][c(1,7)]
colnames(effort.2)[1]<-"camera_id"
names(effort.2)
### criando um novo objeto
covariaveis.2<-merge(covariaveis, effort.2, by = "camera_id")
head(covariaveis.2)

##### Filtrando cameras com mais de 20 dias de amostragem
#covariaveis.2$Eff.2<-ifelse(covariaveis.2$n_nights_active<20,NA,covariaveis.2$n_nights_active)
#head(covariaveis.2)

### Escalonando efforÃ§o
#covariaveis.2$Eff.2<- scale (covariaveis.2$Eff, center = TRUE, scale = TRUE)

covariaveis.2<-covariaveis.2[c(289:720,1:288),] ### ordenando sítios para ficar igual aos históricos
#head(covariaveis.2)
#tail(covariaveis.2)

cov.3<-covariaveis.2
head(cov.3)
dim(na.omit(cov.3))

###Comunidades
length(unique(na.omit(cov.3)$Comunidade))
cov.3$Comunidade
camera.comunidade<-aggregate(na.omit(cov.3)$camera_id, by = list(na.omit(cov.3)$Comunidade), length)
range(camera.comunidade$x)
sum(camera.comunidade$x)
mean(camera.comunidade$x)
sd(camera.comunidade$x)


range(na.omit(cov.3)$hidro.cam.comp)
mean (na.omit(cov.3)$hidro.cam.comp)
sd   (na.omit(cov.3)$hidro.cam.comp)

range(na.omit(cov.3)$Flo_camera)
mean (na.omit(cov.3)$Flo_camera)
sd   (na.omit(cov.3)$Flo_camera)

range(na.omit(cov.3)$desmat)
mean (na.omit(cov.3)$desmat)
sd   (na.omit(cov.3)$desmat)

range(cov.3$X1)
mean (cov.3$X1)
sd   (cov.3$X1)

range(na.omit(cov.3)$UC.cam.distance)
mean (na.omit(cov.3)$UC.cam.distance)
sd   (na.omit(cov.3)$UC.cam.distance)

range(na.omit(cov.3)$HP.local)
mean (na.omit(cov.3)$HP.local)
sd   (na.omit(cov.3)$HP.local)

range(cov.3$Com.size)
mean (cov.3$Com.size)
sd   (cov.3$Com.size)

range(cov.3$Com.dist)
mean (cov.3$Com.dist)
sd   (cov.3$Com.dist)

range(na.omit(cov.3)$HP.city)
mean (na.omit(cov.3)$HP.city)
sd   (na.omit(cov.3)$HP.city)

range(cov.3$City.size)
mean (cov.3$City.size)
sd   (cov.3$City.size)

range(cov.3$City.dist)
mean (cov.3$City.dist)
sd   (cov.3$City.dist)

#### ANALISANDO CORRELAÇÕES E COLINEARIDADE
### Correlaçãoo das covariáveis independetes
#colnames(covariaveis)
#head(covariaveis)
#teste <- covariaveis[c(5,7,8,9,12,16)]
#head(teste)
#colnames(teste)
#colnames(teste)<-c("FLOOD","STREAM", "HABITAT", "PROTE", "COM.HP", "CITY.HP")
#str(teste)
#correlacaco<-cor(teste, use="complete.obs", method = "pearson") 
##correlacaco<-cor(teste[-1], use="complete.obs", method = "spearman") 
#write.csv(correlacaco, file = "correalcao.csv")
#cor(teste, use="complete.obs", method = "pearson")
#psych::pairs.panels(teste, lm=T, cex.cor = 3)
#library(usdm)
#vif(teste)
#write.csv(usdm::vif(teste), file = "VIF.csv")
#GGally::ggpairs(teste)
##### Retirando correlacionadas
#colnames(teste)
#teste.2<-teste[c(1,3:6,10,12,14)]
#colnames(teste.2)
#psych::pairs.panels(teste.2, lm=T, cex.cor = 4)
#library(usdm)
#vif(teste.2)
#write.csv(usdm::vif(teste.2), file = "VIF.csv")
#GGally::ggpairs(teste.2)
#

#### ANALISANDO DISTRIBUIÃO DAS VARIVEIS EXPLICATÃRIAS
# CAMERAS
## Desmatamento ao redor das cÃ¢meras
#covariaveis$desmat
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$desmat,ylab = "Desmat")
#dotchart(covariaveis$desmat,xlab = "Desmat",ylab = "Order of the data")
#hist    (covariaveis$desmat,xlab = "Desmat")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$desmat)
#shapiro.test(covariaveis$desmat)
#
#log(covariaveis$desmat+1)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(covariaveis$desmat+1),ylab = "Desmat")
#dotchart(log(covariaveis$desmat+1),xlab = "Desmat",ylab = "Order of the data")
#hist    (log(covariaveis$desmat+1),xlab = "Desmat")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(covariaveis$desmat+1))
#shapiro.test(log(covariaveis$desmat+1))

## Floooded ao redor das cÃ¢meras
#covariaveis$Flo_camera
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$Flo_camera,ylab = "flooded")
#dotchart(covariaveis$Flo_camera,xlab = "flooded",ylab = "Order of the data")
#hist    (covariaveis$Flo_camera,xlab = "flooded")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$Flo_camera)
#shapiro.test(covariaveis$Flo_camera)
#
#log(covariaveis$Flo_camera+1)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(covariaveis$Flo_camera+1),ylab = "Desmat")
#dotchart(log(covariaveis$Flo_camera+1),xlab = "Desmat",ylab = "Order of the data")
#hist    (log(covariaveis$Flo_camera+1),xlab = "Desmat")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(covariaveis$Flo_camera+1))
#shapiro.test(log(covariaveis$Flo_camera+1))



# Floooded ao redor das cÃ¢meras
#covariaveis$flo.com
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$flo.com,ylab = "flooded")
#dotchart(covariaveis$flo.com,xlab = "flooded",ylab = "Order of the data")
#hist    (covariaveis$flo.com,xlab = "flooded")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$flo.com)
#shapiro.test(covariaveis$flo.com)
#
#log(covariaveis$flo.com+.1)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(covariaveis$flo.com+.1),ylab = "Desmat")
#dotchart(log(covariaveis$flo.com+.1),xlab = "Desmat",ylab = "Order of the data")
#hist    (log(covariaveis$flo.com+.1),xlab = "Desmat")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(covariaveis$flo.com+.1))
#shapiro.test(log(covariaveis$flo.com+.1))
#

### hidrografia ao redor das cÃ¢meras (log)
#covariaveis$hidro.cam.comp
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$hidro.cam.comp,ylab = "hidro")
#dotchart(covariaveis$hidro.cam.comp,xlab = "hidro",ylab = "Order of the data")
#hist    (covariaveis$hidro.cam.comp,xlab = "hidro")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$hidro.cam.comp)
#shapiro.test(covariaveis$hidro.cam.comp)

#log(covariaveis$hidro.cam.comp+1)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(covariaveis$hidro.cam.comp+1),ylab = "hidro")
#dotchart(log(covariaveis$hidro.cam.comp+1),xlab = "hidro",ylab = "Order of the data")
#hist    (log(covariaveis$hidro.cam.comp+1),xlab = "hidro")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(covariaveis$hidro.cam.comp+1))
#shapiro.test(log(covariaveis$hidro.cam.comp+1))
#covariaveis$hidro.cam.comp.log<-log(covariaveis$hidro.cam.comp+1)

### dist. camera UC -  cÃ¢meras
#covariaveis$UC.cam.distance
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$UC.cam.distance,ylab = "Dist")
#dotchart(covariaveis$UC.cam.distance,xlab = "Dist",ylab = "Order of the data")
#hist    (covariaveis$UC.cam.distance,xlab = "Dist")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$UC.cam.distance)
#shapiro.test(covariaveis$UC.cam.distance)

### HAND The vertical distance to the nearest drainage (VDND)
#covariaveis$VDND
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$VDND,ylab = "VDND")
#dotchart(covariaveis$VDND,xlab = "VDND",ylab = "Order of the data")
#hist    (covariaveis$VDND,xlab = "VDND")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$VDND)
#shapiro.test(covariaveis$VDND)
#
#log(covariaveis$VDND+.1)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(covariaveis$VDND+.1),ylab = "VDND")
#dotchart(log(covariaveis$VDND+.1),xlab = "VDND",ylab = "Order of the data")
#hist    (log(covariaveis$VDND+.1),xlab = "VDND")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(covariaveis$VDND+.1))
#shapiro.test(log(covariaveis$VDND+.1))
#
### HP - hunting pressure

#covariaveis$HP
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$HP,ylab = "HP")
#dotchart(covariaveis$HP,xlab = "HP",ylab = "Order of the data")
#hist    (covariaveis$HP,xlab = "HP")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$HP)
#shapiro.test(covariaveis$HP)
##LOG
#log(covariaveis$HP)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(covariaveis$HP),ylab = "HP")
#dotchart(log(covariaveis$HP),xlab = "HP",ylab = "Order of the data")
#hist    (log(covariaveis$HP),xlab = "HP")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(covariaveis$HP))
#shapiro.test(log(covariaveis$HP))

#covariaveis$HP.city
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$HP.city,ylab = "HP")
#dotchart(covariaveis$HP.city,xlab = "HP",ylab = "Order of the data")
#hist    (covariaveis$HP.city,xlab = "HP")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$HP.city)
#shapiro.test(covariaveis$HP.city)
# #LOG
#log(covariaveis$HP.city)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(covariaveis$HP.city),ylab = "HP")
#dotchart(log(covariaveis$HP.city),xlab = "HP",ylab = "Order of the data")
#hist    (log(covariaveis$HP.city),xlab = "HP")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(covariaveis$HP.city))
#shapiro.test(log(covariaveis$HP.city))

# HP - hunting pressure Comunidades
#covariaveis$HP.local
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$HP.local,ylab = "HP")
#dotchart(covariaveis$HP.local,xlab = "HP",ylab = "Order of the data")
#hist    (covariaveis$HP.local,xlab = "HP")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$HP.local)
#shapiro.test(covariaveis$HP.local)
##LOG
#log(covariaveis$HP.com)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(covariaveis$HP.local),ylab = "HP")
#dotchart(log(covariaveis$HP.local),xlab = "HP",ylab = "Order of the data")
#hist    (log(covariaveis$HP.local),xlab = "HP")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(covariaveis$HP.local))
#shapiro.test(log(covariaveis$HP.local))
#

# Latitude
#covariaveis$X
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$X,ylab = "HP")
#dotchart(covariaveis$X,xlab = "HP",ylab = "Order of the data")
#hist    (covariaveis$X,xlab = "HP")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$X)
#shapiro.test(covariaveis$X)
##LOG
#log(-1*covariaveis$X)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(-1*covariaveis$X),ylab = "HP")
#dotchart(log(-1*covariaveis$X),xlab = "HP",ylab = "Order of the data")
#hist    (log(-1*covariaveis$X),xlab = "HP")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(-1*covariaveis$X))
#shapiro.test(log(-1*covariaveis$X))

# Longitude
#covariaveis$Y
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$Y,ylab = "HP")
#dotchart(covariaveis$Y,xlab = "HP",ylab = "Order of the data")
#hist    (covariaveis$Y,xlab = "HP")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$Y)
#shapiro.test(covariaveis$Y)
##LOG
#log(-1*covariaveis$Y)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(-1*covariaveis$Y),ylab = "HP")
#dotchart(log(-1*covariaveis$Y),xlab = "HP",ylab = "Order of the data")
#hist    (log(-1*covariaveis$Y),xlab = "HP")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(-1*covariaveis$Y))
#shapiro.test(log(-1*covariaveis$Y))

#head(covariaveis)
## NN
#covariaveis$NN
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$NN,ylab = "NN")
#dotchart(covariaveis$NN,xlab = "NN",ylab = "Order of the data")
#hist    (covariaveis$NN,xlab = "NN")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$NN)
#shapiro.test(covariaveis$NN)
##LOG
#log(covariaveis$NN)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(covariaveis$NN),ylab = "NN")
#dotchart(log(covariaveis$NN),xlab = "NN",ylab = "Order of the data")
#hist    (log(covariaveis$NN),xlab = "NN")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(covariaveis$NN))
#shapiro.test(log(covariaveis$NN))
#
#head(covariaveis)
## NN
#covariaveis$latXlong
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$latXlong,ylab = "latXlong")
#dotchart(covariaveis$latXlong,xlab = "latXlong",ylab = "Order of the data")
#hist    (covariaveis$latXlong,xlab = "latXlong")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$latXlong)
#shapiro.test(covariaveis$latXlong)
##LOG
#log(covariaveis$latXlong)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(covariaveis$latXlong),ylab = "latXlong")
#dotchart(log(covariaveis$latXlong),xlab = "latXlong",ylab = "Order of the data")
#hist    (log(covariaveis$latXlong),xlab = "latXlong")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(covariaveis$latXlong))
#shapiro.test(log(covariaveis$latXlong))

#head(covariaveis)
## DISTÂNCIA DA COMUNIDADE
#covariaveis$Com.dist/1000
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$Com.dist/1000,ylab = "NN")
#dotchart(covariaveis$Com.dist/1000,xlab = "NN",ylab = "Order of the data")
#hist    (covariaveis$Com.dist/1000,xlab = "NN")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$Com.dist/1000)
#shapiro.test(covariaveis$Com.dist/1000)
##SQRT
#sqrt(covariaveis$Com.dist/1000)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (sqrt(covariaveis$Com.dist/1000),ylab = "NN")
#dotchart(sqrt(covariaveis$Com.dist/1000),xlab = "NN",ylab = "Order of the data")
#hist    (sqrt(covariaveis$Com.dist/1000),xlab = "NN")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(sqrt(covariaveis$Com.dist/1000))
#shapiro.test(sqrt(covariaveis$Com.dist/1000))
#
#head(covariaveis)
## Casas_1km
#covariaveis$Casas_1km
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$Casas_1km,ylab = "Communidade")
#dotchart(covariaveis$Casas_1km,xlab = "Communidade",ylab = "Order of the data")
#hist    (covariaveis$Casas_1km,xlab = "Communidade")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$Casas_1km)
#shapiro.test(covariaveis$Casas_1km)
##LOG.10
#log(covariaveis$Casas_1km,10)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(covariaveis$Casas_1km,10),ylab = "latXlong")
#dotchart(log(covariaveis$Casas_1km,10),xlab = "latXlong",ylab = "Order of the data")
#hist    (log(covariaveis$Casas_1km,10),xlab = "latXlong")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(covariaveis$Casas_1km,10))
#shapiro.test(log(covariaveis$Casas_1km,10))
#
#head(covariaveis)
## POPULAÇÃO URBANA
#covariaveis$Pop.urbana
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$Pop.urbana,ylab = "CITY")
#dotchart(covariaveis$Pop.urbana,xlab = "CITY",ylab = "Order of the data")
#hist    (covariaveis$Pop.urbana,xlab = "CITY")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$Pop.urbana)
#shapiro.test(covariaveis$Pop.urbana)
##LOG
#log(covariaveis$Pop.urbana)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(covariaveis$Pop.urbana),ylab = "CITY")
#dotchart(log(covariaveis$Pop.urbana),xlab = "CITY",ylab = "Order of the data")
#hist    (log(covariaveis$Pop.urbana),xlab = "CITY")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(covariaveis$Pop.urbana))
#shapiro.test(log(covariaveis$Pop.urbana))
#
#head(covariaveis)
## DIST.CITY
#covariaveis$City.dist/1000
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$City.dist/1000,ylab = "City.dist")
#dotchart(covariaveis$City.dist/1000,xlab = "City.dist",ylab = "Order of the data")
#hist    (covariaveis$City.dist/1000,xlab = "City.dist")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$City.dist/1000)
#shapiro.test(covariaveis$City.dist/1000)
##SQRT
#sqrt(covariaveis$City.dist/1000)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (sqrt(covariaveis$City.dist/1000),ylab = "CITY.DIST")
#dotchart(sqrt(covariaveis$City.dist/1000),xlab = "CITY.DIST",ylab = "Order of the data")
#hist    (sqrt(covariaveis$City.dist/1000),xlab = "CITY.DIST")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(sqrt(covariaveis$City.dist/1000))
#shapiro.test(sqrt(covariaveis$City.dist/1000))

#head(covariaveis)
# ESFORÇO
#covariaveis$Effort
#covariaveis[covariaveis$Effort==0,]$Effort<-0.5
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (covariaveis$Effort,ylab = "Effort")
#dotchart(covariaveis$Effort,xlab = "Effort",ylab = "Order of the data")
#hist    (covariaveis$Effort,xlab = "Effort")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(covariaveis$Effort)
#shapiro.test(covariaveis$Effort)
##LOG
#log(covariaveis$Effort)
#par(mfrow= c (1,3), mar = c(5,4,2,1))
#boxplot (log(covariaveis$Effort),ylab = "Effort")
#dotchart(log(covariaveis$Effort),xlab = "Effort",ylab = "Order of the data")
#hist    (log(covariaveis$Effort),xlab = "Effort")
#par(mfrow= c (1,1), mar = c(5,4,2,1))
#qqnorm(log(covariaveis$Effort))
#shapiro.test(sqrt(covariaveis$Effort))

## log nas covs que precisam de transformação
cov.3$hidro.cam.comp<-log(cov.3$hidro.cam.comp+1)
cov.3$HP  <-log(cov.3$HP)
cov.3$HP.local  <-log(cov.3$HP.local)
cov.3$HP.city  <-log(cov.3$HP.city)
cov.3$NN  <-log(cov.3$NN)
cov.3$VDND  <-log(cov.3$VDND+0.1)
cov.3$Com.size  <-log(cov.3$Com.size,10)
cov.3$Com.dist  <-sqrt(cov.3$Com.dist)
cov.3$City.size  <-log(cov.3$City.size)
cov.3$City.dist  <-sqrt(cov.3$City.dist)
cov.3$lat<- cov.3$X1
cov.3$lon<- cov.3$Y1


### Escalonando
colnames(cov.3)
names(cov.3[c(5:17,22:29)])
head(cov.3)
cov.3[c(5:17,22:27)]<-lapply(cov.3[c(5:17,22:27)], function(x) scale (x, center = TRUE, scale = TRUE)) 
length(covariaveis$camera_id)
### Mundando o nome das RESEX
cov.3[cov.3$RESEX == 'Arapixi',]$RESEX<- "REA"
cov.3[cov.3$RESEX == 'Cazumbá-iracema',]$RESEX<- "RCI"
cov.3[cov.3$RESEX == 'Médio Purus',]$RESEX<- "RMP"
cov.3[cov.3$RESEX == 'Riozinho_Liberdade',]$RESEX<- "RRL"

#### Ajustes nos dados
colnames(cov.3)
colnames(cov.3)[5]<- "Flo"
colnames(cov.3)[6]<- "Flo.Com"
colnames(cov.3)[7]<- "Hyd"
colnames(cov.3)[8]<- "Def"
colnames(cov.3)[9]<- "UC"


colnames(cov.3)[12]<- "HP.L"
colnames(cov.3)[16]<- "HP.C"
colnames(cov.3)[18]<- "Eff"
colnames(cov.3)[21]<- "Mod"
colnames(cov.3)[25]<- "LxL"
colnames(cov.3)[27]<- "Eff.2"


write.csv(cov.3, file = "covariaveis.csv")

# Criando um objeto com o esforço
#effort.2<-reportTest[[1]][c(1,7)]
#colnames(effort.2)[1]<-"camera_id"
#
#### criando um novo objeto
#covariaveis.2<-merge(covariaveis, effort.2, by = "camera_id")
##head(covariaveis.2)
#
##### Avaliando o esfor?o amostral
##range(covariaveis.2$n_nights_active)
##plot(covariaveis.2$n_nights_active)
##dim(covariaveis.2[covariaveis.2$n_nights_active<20,])
###### 23 CT que tiveram esfor?o menor que 20 dias!
#
#covariaveis.2$Eff.2<-ifelse(covariaveis.2$n_nights_active<20,NA,covariaveis.2$n_nights_active)
##head(covariaveis.2)
#
#### Escalonando efforço
#covariaveis.2$Eff.3<- scale (covariaveis.2$Eff.2, center = TRUE, scale = TRUE)
#
#covariaveis.2<-covariaveis.2[c(289:720,1:288),] ### ordenando sítios para ficar igual aos históricos
#head(covariaveis.2)
#tail(covariaveis.2)

#cov.3<-covariaveis
#cov.3$RESEX[cov.3$RESEX == "RDA"] <-"RMJ"
#cov.3$RESEX <-factor(cov.3$RESEX, levels = c ("RMJ","RMP","REA","RDU","RCI","RRL"), ordered=F)
#
#
#cov.3$Eco_reg[cov.3$Eco_reg == "PV"] <-"JPF"
#cov.3$Eco_reg[cov.3$Eco_reg == "MAV"] <-"UTF"
#cov.3$Eco_reg <-factor(cov.3$Eco_reg, levels = c ("SWA", "JPF", "UTF"), ordered=F)
#head(cov.3)
#covariaveis<-cov.3
#plot(cov.3$Eco_reg, cov.3$Def)
#cov.3$Eco_reg
