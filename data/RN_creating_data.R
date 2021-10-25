library(here)
here()

source(here("data","preparacao.dados.R"), encoding = "UTF-8")
source(here("data","preparacao.covariaveis.R"), encoding = "UTF-8")


###  Trials
trials.k<-rowSums(!is.na(h.det.paca$detection_history))

### Species sucess
paca        <-rowSums((h.det.paca$detection_history), na.rm=T)
catitu      <-rowSums((h.det.catitu$detection_history), na.rm=T)
vermelho    <-rowSums((h.det.vermelho$detection_history), na.rm=T)
cutia       <-rowSums((h.det.cutia$detection_history), na.rm=T)
queixada    <-rowSums((h.det.queixada$detection_history), na.rm=T)
nambu       <-rowSums((h.det.nambu$detection_history), na.rm=T)
tatu        <-rowSums((h.det.tatu$detection_history), na.rm=T)
cinza       <-rowSums((h.det.cinza$detection_history), na.rm=T)
mutum       <-rowSums((h.det.mutum$detection_history), na.rm=T)
anta        <-rowSums((h.det.anta$detection_history), na.rm=T)
jacu        <-rowSums((h.det.jacu$detection_history), na.rm=T)
catipuru    <-rowSums((h.det.catipuru$detection_history), na.rm=T)
jacamim     <-rowSums((h.det.jacamim$detection_history), na.rm=T)
coati       <-rowSums((h.det.coati$detection_history), na.rm=T)
onca.p      <-rowSums((h.det.onca.p$detection_history), na.rm=T)
onca.v      <-rowSums((h.det.onca.v$detection_history), na.rm=T)
canastra    <-rowSums((h.det.canastra$detection_history), na.rm=T)
bandeira    <-rowSums((h.det.bandeira$detection_history), na.rm=T)
atelocynus  <-rowSums((h.det.atelocynus$detection_history), na.rm=T)
crypturellus<-rowSums((h.det.crypturellus$detection_history), na.rm=T)
mucura      <-rowSums((h.det.didelphis$detection_history), na.rm=T)
irara       <-rowSums((h.det.irara$detection_history), na.rm=T)
m.acu       <-rowSums((h.det.pardalis$detection_history), na.rm=T)
m.peludo    <-rowSums((h.det.wiedii$detection_history), na.rm=T)
cutiara     <-rowSums((h.det.cutiara$detection_history), na.rm=T)
procyon     <-rowSums((h.det.procyon$detection_history), na.rm=T)
m.preto     <-rowSums((h.det.jaguarundi$detection_history), na.rm=T)
vinagre     <-rowSums((h.det.vinagre$detection_history), na.rm=T)
mambira     <-rowSums((h.det.mambira$detection_history), na.rm=T)

RN.data<-cbind(paca,catitu,vermelho,cutia,queixada,nambu,tatu,cinza,mutum,anta,jacu,catipuru,jacamim,coati,onca.p,onca.v,canastra,bandeira,
               atelocynus, crypturellus,mucura,irara, m.acu, m.peludo, cutiara, procyon, m.preto, vinagre, mambira,
               trials.k,cov.3)
head(RN.data)


#### Excluindo dados de espécies que não ocorrem em determinada região

RN.data[RN.data$RESEX == "RCI",]$cinza<-NA
RN.data[RN.data$RESEX == "RDU",]$jacu<-NA
RN.data[RN.data$RESEX == "RDU",]$catipuru<-NA
RN.data[RN.data$RESEX == "RDU",]$atelocynus<-NA
### Saving data
write.table(RN.data, file = here("data","RN.data.csv"), sep = ";", fileEncoding  = "UTF-8")

