library(here)
here()

source(here("data","preparacao.dados.R"))
source(here("data","preparacao.covariaveis.R"))


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


RN.data<-cbind(paca,catitu,vermelho,cutia,queixada,nambu,tatu,cinza,mutum,anta,jacu,catipuru,jacamim,coati,onca.p,onca.v,canastra,bandeira,trials.k,cov.3)
head(RN.data)

### Saving data
write.table(RN.data, file = here("data","RN.data.csv"), sep = ";", fileEncoding  = "UTF-8")