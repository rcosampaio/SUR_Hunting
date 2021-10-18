## ----Source this file--------
source(here("data", "camera trap analysis code-WILDID-09-20-17.R"), encoding = "UTF-8")

## ----Load data from Acre-------
Acre <- f.readin.fix.data(here("data","Wild_ID_HP_RESEX_Padrao_Evento.csv"))
#str(Acre)
#write.csv(sort(unique(Acre$Camera.Trap.Name)), file = "cameras_wild.id.csv") 

Acre.cov<-Acre
### Criando coluna indicando data e hora correta da retirada
Acre.cov$Retirada.hora<-NA
### Criando uma coluna com as datas corretas
Acre.cov$data.hora.correta <- Acre.cov$td.photo

####### 1
##### Camera C2-T4-C1-1050 - Cazumbá
### Extraindo fotos com problemas desta camera
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01130306.JPG" & Acre.cov$Camera.Trap.Name == "C2-T4-C1-1050"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01130307.JPG" & Acre.cov$Camera.Trap.Name == "C2-T4-C1-1050"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01130308.JPG"& Acre.cov$Camera.Trap.Name == "C2-T4-C1-1050"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01140309.JPG"& Acre.cov$Camera.Trap.Name == "C2-T4-C1-1050"),]
Acre.cov[Acre.cov$Camera.Trap.Name == "C2-T4-C1-1050",]$Retirada.hora <- '2018/08/17 08:30:00'
### Obtenção das difenças de tempo (dias) entre a data correta e data errada
difftime(as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "C2-T4-C1-1050",] $Retirada.hora),
         as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "C2-T4-C1-1050",] $td.photo), units = "days")
### add o valor na data errada para o valor corresponder à data correta, nas linhas onde há datas errada
Acre.cov[Acre.cov$Camera.Trap.Name == "C2-T4-C1-1050",]$data.hora.correta[1:174] <- 
  ddays(1312.28444) + Acre.cov[Acre.cov$Camera.Trap.Name == "C2-T4-C1-1050",]$td.photo[1:174]
# Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "C2-T4-C1-1050",]$data.hora.correta

#### 2
##### Camera CUI-T1-C1-500 - Cazumbá
### As datas foram configuradas no mês anterior 
### Corrigindo data das fotos de spp

Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C1-500",]$td.photo

###### Add o valor exato para correção
Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C1-500",]$data.hora.correta <- 
  ddays(31) + Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C1-500",]$td.photo
# Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C1-500",]$data.hora.correta

######### 3
##### Camera CUI-T1-C2-950 - Cazumbá
### Extraindo fotos com problemas desta camera
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010118.JPG"&
                        Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010119.JPG"&
                        Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010120.JPG"&
                        Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010121.JPG"&
                        Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010122.JPG"&
                        Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010123.JPG"&
                        Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010126.JPG"&
                        Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010127.JPG"&
                        Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010129.JPG"&
                        Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010130.JPG" &
                      Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),] 
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010131.JPG"&
                      Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010132.JPG"&
                      Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010133.JPG"&
                      Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010134.JPG"&
                      Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010135.JPG"&
                      Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]
### Corrigindo data das fotos da onça e mucura
#Leopardus pardalis
Acre.cov[(Acre.cov$Raw.Name == "01010124.JPG"
          &Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]$data.hora.correta<-'2018/06/21 20:00:01'
Acre.cov[(Acre.cov$Raw.Name == "01010125.JPG"
         &Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]$data.hora.correta<-'2018/06/21 20:00:03'
#Mucura
Acre.cov[(Acre.cov$Raw.Name == "01010128.JPG"
          &Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950"),]$data.hora.correta<-'2018/06/22 20:00:01'
### Observando os dados
Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950",]$td.photo
### Corrigindo os dados para o mês seguinte
Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950",]$data.hora.correta[1:123] <- Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950",]$td.photo[1:123]+ddays(31)
#### Checando os dados
Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C2-950",]$data.hora.correta

##### 4
##### Camera CUI-T1-C5-4100 - Cazumbá
### Add data e hora de referência
Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C5-4100",]$Retirada.hora <- '2018/06/03 07:06:00'
### Obtendo valor a ser add
difftime(as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C5-4100",] $Retirada.hora),
         as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C5-4100",] $td.photo), units = "days")
###### Add o valor exato para correção
Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C5-4100",]$data.hora.correta[1:258] <- 
  ddays(518.4208218) + Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C5-4100",]$td.photo[1:258]
# Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "CUI-T1-C5-4100",]$data.hora.correta

##### 5
##### Camera GRA-T3-C5-4330 - Cazumbá
### Extraindo fotos com problemas desta camera
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010278.JPG" & 
                        Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010279.JPG"& 
                        Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010280.JPG"& 
                        Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010284.JPG"& 
                        Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010285.JPG"& 
                        Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010286.JPG"& 
                        Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]
### Corrigindo data das fotos da onça e mucura
#Veado vermelho
Acre.cov[(Acre.cov$Raw.Name == "01010272.JPG"
          &Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]$data.hora.correta<-'2018/07/08 20:01:00'
Acre.cov[(Acre.cov$Raw.Name == "01010273.JPG"
          &Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]$data.hora.correta<-'2018/07/08 20:01:02'
Acre.cov[(Acre.cov$Raw.Name == "01010274.JPG"
          &Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]$data.hora.correta<-'2018/07/08 20:01:04'
#Pecari tajacu
Acre.cov[(Acre.cov$Raw.Name == "01010275.JPG"
          &Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]$data.hora.correta<-'2018/07/09 05:01:00'
Acre.cov[(Acre.cov$Raw.Name == "01010276.JPG"
          &Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]$data.hora.correta<-'2018/07/09 05:01:02'
Acre.cov[(Acre.cov$Raw.Name == "01010277.JPG"
          &Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]$data.hora.correta<-'2018/07/09 05:01:04'
#Dasyprocta fuliginosa
Acre.cov[(Acre.cov$Raw.Name == "01010281.JPG"
          &Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]$data.hora.correta<-'2018/07/10 05:01:01'
Acre.cov[(Acre.cov$Raw.Name == "01010282.JPG"
          &Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]$data.hora.correta<-'2018/07/10 05:01:03'
Acre.cov[(Acre.cov$Raw.Name == "01010283.JPG"
          &Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330"),]$data.hora.correta<-'2018/07/10 05:01:05'

###### Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "GRA-T3-C5-4330",]$data.hora.correta

##### 6
##### Camera RED-T1-C4-3850 - Cazumbá
### Extraindo fotos com problemas desta camera
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010235.JPG" &
                        Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010236.JPG"&
                        Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010237.JPG"&
                        Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010238.JPG"&
                        Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010239.JPG"&
                        Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010240.JPG"&
                        Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010241.JPG"&
                        Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010242.JPG"&
                        Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010243.JPG"&
                        Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010244.JPG"&
                        Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010245.JPG"&
                        Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010246.JPG"&
                        Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850"),]
### Add data de referência
Acre.cov[Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850",]$Retirada.hora <- '2018/09/10 05:28:00'
Acre.cov[Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850",]$td.photo
### Obtendo o valor a ser add
difftime(as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850",] $Retirada.hora),
         as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850",] $td.photo), units = "days")
###### Add o valor para correção das datas
Acre.cov[Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850",]$data.hora.correta[1:45] <- 
  ddays(1348.353) + Acre.cov[Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850",]$td.photo[1:45]
# Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "RED-T1-C4-3850",]$Retirada.hora 

##### 7
##### Camera ALE-T1-C1-288 - Riozinho
### Add o valor de referência
Acre.cov[Acre.cov$Camera.Trap.Name == "ALE-T1-C1-288",]$Retirada.hora <- '2018/10/10 07:00:01'
### Obtendo o valor a ser add
difftime(as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "ALE-T1-C1-288",] $Retirada.hora),
         as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "ALE-T1-C1-288",] $td.photo), units = "days")
###### Add o valor para coreção das fotos
Acre.cov[Acre.cov$Camera.Trap.Name == "ALE-T1-C1-288",]$data.hora.correta[1:228] <- 
  ddays(1378.4166435) + Acre.cov[Acre.cov$Camera.Trap.Name == "ALE-T1-C1-288",]$td.photo[1:228]
#Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "ALE-T1-C1-288",]$data.hora.correta

###### 8
##### Camera C1-T2-C1-1600 - Riozinho
### Add o valores de refência
Acre.cov[Acre.cov$Camera.Trap.Name == "C1-T2-C1-1600",]$Retirada.hora <- '2018/11/02 09:27:59'
### Obtendo o valor a ser add
difftime(as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "C1-T2-C1-1600",] $Retirada.hora),
         as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "C1-T2-C1-1600",] $td.photo), units = "days")
### Add o valor para correção
Acre.cov[Acre.cov$Camera.Trap.Name == "C1-T2-C1-1600",]$data.hora.correta[1:225] <- 
  ddays(670.5194) + Acre.cov[Acre.cov$Camera.Trap.Name == "C1-T2-C1-1600",]$td.photo[1:225]
# Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "C1-T2-C1-1600",]$data.hora.correta

###### 9
##### Camera C4-T1-C3-3400 - Riozinho
### Extraindo fotos com problemas desta camera
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010162.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010164.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010166.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010167.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010169.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010170.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010171.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010172.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010173.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010174.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010175.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010176.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010177.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010178.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010179.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010180.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010182.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010183.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010184.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010185.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010187.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010188.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010189.JPG"&
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010190.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010191.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010193.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010194.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010195.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010196.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010197.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010199.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010200.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010201.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010203.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010205.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010206.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010208.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010209.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010210.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010211.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010213.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010214.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010215.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010217.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010218.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010220.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010221.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010222.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010223.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010224.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010225.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um
Acre.cov<-Acre.cov [!(Acre.cov$Raw.Name == "01010227.JPG" &
                        Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),] # mais de um

### Corrigindo data das fotos de spp
Acre.cov[(Acre.cov$Raw.Name == "01010163.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/10/26 21:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010165.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/10/26 23:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010168.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/10/27 00:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010181.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/10/27 21:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010186.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/10/28 05:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010192.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/10/29 05:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010198.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/10/29 11:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010202.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/10/29 23:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010204.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/10/30 03:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010207.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/10/31 00:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010212.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/10/31 03:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010216.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/11/01 03:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010219.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/11/02 03:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010226.JPG"
          &Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400"),]$data.hora.correta<-'2018/11/03 07:10:10'

### Criando as datas de referência
Acre.cov[Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400",]$Retirada.hora <- '2018/10/04 15:27:59'
### Obtendo os valores a serem adicionados
difftime(as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400",] $Retirada.hora),
         as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400",] $td.photo), units = "days")
###### Add os valores para correção
Acre.cov[Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400",]$data.hora.correta[3:124] <- 
  ddays(1372.759) + Acre.cov[Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400",]$td.photo[3:124]
# Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "C4-T1-C3-3400",]$data.hora.correta

#### 10
##### Camera C4-T3-C4-3740 - Riozinho
### Add as datas de referência
Acre.cov[Acre.cov$Camera.Trap.Name == "C4-T3-C4-3740",]$Retirada.hora <- '2018/11/21 06:33:01'
### Obtendo os valores a serem add
difftime(as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "C4-T3-C4-3740",] $Retirada.hora),
         as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "C4-T3-C4-3740",] $td.photo), units = "days")
###### Add os valores de correção
Acre.cov[Acre.cov$Camera.Trap.Name == "C4-T3-C4-3740",]$data.hora.correta[1:197] <- 
  ddays(654.25815) + Acre.cov[Acre.cov$Camera.Trap.Name == "C4-T3-C4-3740",]$td.photo[1:197]
# Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "C4-T3-C4-3740",]$data.hora.correta

#### 11
##### Camera C1-T1-C1-1400 - Riozinho
### Add duas últimas fotos tem datas erradas
Acre.cov[Acre.cov$Camera.Trap.Name == "C1-T1-C1-1400",]$td.photo
### Corrigindo data das fotos de spp
Acre.cov[(Acre.cov$Raw.Name == "01010065.JPG"
         &Acre.cov$Camera.Trap.Name == "C1-T1-C1-1400"),]$data.hora.correta<-'2018/11/28 7:10:10'
Acre.cov[(Acre.cov$Raw.Name == "01010066.JPG"
         &Acre.cov$Camera.Trap.Name == "C1-T1-C1-1400"),]$data.hora.correta<-'2018/11/28 7:10:15'
### Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "C1-T1-C1-1400",]$data.hora.correta

#### 12
##### Camera MAP-T2-C1-2500 - Riozinho
### As datas foram configuradas para 2017!!!!
### Corrigindo data das fotos de spp

###### Add o valor exato para correção
Acre.cov[Acre.cov$Camera.Trap.Name == "MAP-T2-C1-2500",]$data.hora.correta <- 
  ddays(365) + Acre.cov[Acre.cov$Camera.Trap.Name == "MAP-T2-C1-2500",]$td.photo
# Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "MAP-T2-C1-2500",]$data.hora.correta
  

#### 13
##### Camera SJE-T1-C1-1200 - Arapixi
## As datas foram configuradas para 2017 e 1970!!!!
### Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200",]$td.photo
### Corrigindo data das fotos de spp
Acre.cov[(Acre.cov$Raw.Name == "01010028.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/16 17:00:00'
Acre.cov[(Acre.cov$Raw.Name == "01010029.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/16 17:00:01'
Acre.cov[(Acre.cov$Raw.Name == "01010030.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/16 17:00:02'
Acre.cov[(Acre.cov$Raw.Name == "01010031.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/16 17:00:03'
Acre.cov[(Acre.cov$Raw.Name == "01010032.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/16 17:00:04'
Acre.cov[(Acre.cov$Raw.Name == "01010033.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/16 17:00:06'
Acre.cov[(Acre.cov$Raw.Name == "01020034.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/17 23:00:00'
Acre.cov[(Acre.cov$Raw.Name == "01020035.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/17 23:00:01'
Acre.cov[(Acre.cov$Raw.Name == "01020036.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/17 23:00:02'
Acre.cov[(Acre.cov$Raw.Name == "01020037.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/17 23:00:03'
Acre.cov[(Acre.cov$Raw.Name == "01020038.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/17 23:00:04'
Acre.cov[(Acre.cov$Raw.Name == "01020039.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/17 23:00:05'
Acre.cov[(Acre.cov$Raw.Name == "01030040.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/19 12:00:05'
Acre.cov[(Acre.cov$Raw.Name == "01030041.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/19 12:00:06'
Acre.cov[(Acre.cov$Raw.Name == "01030042.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/19 12:00:07'
Acre.cov[(Acre.cov$Raw.Name == "01040043.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/20 05:00:07'
Acre.cov[(Acre.cov$Raw.Name == "01040044.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/20 05:00:08'
Acre.cov[(Acre.cov$Raw.Name == "01040045.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/20 05:00:09'
Acre.cov[(Acre.cov$Raw.Name == "01010150.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/07 08:00:03'
Acre.cov[(Acre.cov$Raw.Name == "01010149.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/07 08:00:02'
Acre.cov[(Acre.cov$Raw.Name == "01010148.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/07 08:00:00'
Acre.cov[(Acre.cov$Raw.Name == "01020147.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/07 02:00:00'
Acre.cov[(Acre.cov$Raw.Name == "01020146.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/07 01:59:55'
Acre.cov[(Acre.cov$Raw.Name == "01020145.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/07 01:59:53'
Acre.cov[(Acre.cov$Raw.Name == "01010144.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/06 12:01:00'
Acre.cov[(Acre.cov$Raw.Name == "01010143.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/06 12:00:50'
Acre.cov[(Acre.cov$Raw.Name == "01010142.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/06 12:00:40'
Acre.cov[(Acre.cov$Raw.Name == "01010141.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/06 12:00:30'
Acre.cov[(Acre.cov$Raw.Name == "01010140.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/06 12:00:20'
Acre.cov[(Acre.cov$Raw.Name == "01010139.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/06 12:00:10'
Acre.cov[(Acre.cov$Raw.Name == "01010138.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 23:00:09'
Acre.cov[(Acre.cov$Raw.Name == "01010137.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 23:00:08'
Acre.cov[(Acre.cov$Raw.Name == "01010136.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 23:00:07'
Acre.cov[(Acre.cov$Raw.Name == "01010135.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 23:00:06'
Acre.cov[(Acre.cov$Raw.Name == "01010134.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 23:00:05'
Acre.cov[(Acre.cov$Raw.Name == "01010133.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 23:00:04'
Acre.cov[(Acre.cov$Raw.Name == "01010132.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 23:00:03'
Acre.cov[(Acre.cov$Raw.Name == "01010131.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 23:00:02'
Acre.cov[(Acre.cov$Raw.Name == "01010130.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 23:00:01'
Acre.cov[(Acre.cov$Raw.Name == "01010129.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 17:00:05'
Acre.cov[(Acre.cov$Raw.Name == "01010128.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 17:00:04'
Acre.cov[(Acre.cov$Raw.Name == "01010127.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 17:00:03'
Acre.cov[(Acre.cov$Raw.Name == "01010126.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 08:00:05'
Acre.cov[(Acre.cov$Raw.Name == "01010125.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 08:00:03'
Acre.cov[(Acre.cov$Raw.Name == "01010124.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 08:00:01'
Acre.cov[(Acre.cov$Raw.Name == "01160123.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 04:00:05'
Acre.cov[(Acre.cov$Raw.Name == "01160122.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 04:00:03'
Acre.cov[(Acre.cov$Raw.Name == "01160121.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/05 04:00:01'
Acre.cov[(Acre.cov$Raw.Name == "01150120.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/04 20:00:05'
Acre.cov[(Acre.cov$Raw.Name == "01150119.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/04 20:00:03'
Acre.cov[(Acre.cov$Raw.Name == "01150118.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/06/04 20:00:01'
Acre.cov[(Acre.cov$Raw.Name == "01010117.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/20 08:00:05'
Acre.cov[(Acre.cov$Raw.Name == "01010116.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/20 08:00:03'
Acre.cov[(Acre.cov$Raw.Name == "01010115.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/20 08:00:01'
Acre.cov[(Acre.cov$Raw.Name == "01130113.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/19 02:00:05'
Acre.cov[(Acre.cov$Raw.Name == "01130112.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/19 02:00:03'
Acre.cov[(Acre.cov$Raw.Name == "01010111.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:59'
Acre.cov[(Acre.cov$Raw.Name == "01010110.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:57'
Acre.cov[(Acre.cov$Raw.Name == "01010109.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:55'
Acre.cov[(Acre.cov$Raw.Name == "01010108.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:54'
Acre.cov[(Acre.cov$Raw.Name == "01010107.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:53'
Acre.cov[(Acre.cov$Raw.Name == "01010106.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:52'
Acre.cov[(Acre.cov$Raw.Name == "01010105.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:51'
Acre.cov[(Acre.cov$Raw.Name == "01010104.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:50'
Acre.cov[(Acre.cov$Raw.Name == "01010103.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:49'
Acre.cov[(Acre.cov$Raw.Name == "01010102.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:48'
Acre.cov[(Acre.cov$Raw.Name == "01010101.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:47'
Acre.cov[(Acre.cov$Raw.Name == "01010100.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:46'
Acre.cov[(Acre.cov$Raw.Name == "01010099.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:45'
Acre.cov[(Acre.cov$Raw.Name == "01010098.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:44'
Acre.cov[(Acre.cov$Raw.Name == "01010097.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:43'
Acre.cov[(Acre.cov$Raw.Name == "01010096.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:42'
Acre.cov[(Acre.cov$Raw.Name == "01010095.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:41'
Acre.cov[(Acre.cov$Raw.Name == "01010094.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:40'
Acre.cov[(Acre.cov$Raw.Name == "01010093.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:39'
Acre.cov[(Acre.cov$Raw.Name == "01010092.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:38'
Acre.cov[(Acre.cov$Raw.Name == "01010091.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:37'
Acre.cov[(Acre.cov$Raw.Name == "01010090.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:36'
Acre.cov[(Acre.cov$Raw.Name == "01010089.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:34'
Acre.cov[(Acre.cov$Raw.Name == "01010088.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/06 12:00:32'
Acre.cov[(Acre.cov$Raw.Name == "01010087.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/05 10:00:32'
Acre.cov[(Acre.cov$Raw.Name == "01010086.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/05 10:00:31'
Acre.cov[(Acre.cov$Raw.Name == "01010085.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/05 10:00:30'
Acre.cov[(Acre.cov$Raw.Name == "01010084.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/05 10:00:29'
Acre.cov[(Acre.cov$Raw.Name == "01010083.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/05 10:00:28'
Acre.cov[(Acre.cov$Raw.Name == "01010082.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/05 10:00:27'
Acre.cov[(Acre.cov$Raw.Name == "01010081.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/05 10:00:26'
Acre.cov[(Acre.cov$Raw.Name == "01010080.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/05 10:00:25'
Acre.cov[(Acre.cov$Raw.Name == "01010079.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/05 10:00:24'
Acre.cov[(Acre.cov$Raw.Name == "01010078.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/05 10:00:23'
Acre.cov[(Acre.cov$Raw.Name == "01010077.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/05 10:00:22'
Acre.cov[(Acre.cov$Raw.Name == "01010076.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/05 10:00:21'
Acre.cov[(Acre.cov$Raw.Name == "01100075.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/04 17:00:21'
Acre.cov[(Acre.cov$Raw.Name == "01100074.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/04 17:00:20'
Acre.cov[(Acre.cov$Raw.Name == "01100073.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/05/04 17:00:19'
Acre.cov[(Acre.cov$Raw.Name == "01010072.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/24 08:00:29'
Acre.cov[(Acre.cov$Raw.Name == "01010071.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/24 08:00:28'
Acre.cov[(Acre.cov$Raw.Name == "01010070.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/24 08:00:27'
Acre.cov[(Acre.cov$Raw.Name == "01010069.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/24 08:00:26'
Acre.cov[(Acre.cov$Raw.Name == "01010068.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/24 08:00:25'
Acre.cov[(Acre.cov$Raw.Name == "01010067.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/24 08:00:24'
Acre.cov[(Acre.cov$Raw.Name == "01010066.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/24 08:00:23'
Acre.cov[(Acre.cov$Raw.Name == "01010065.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/24 08:00:22'
Acre.cov[(Acre.cov$Raw.Name == "01010064.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/24 08:00:21'
Acre.cov[(Acre.cov$Raw.Name == "01010063.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/24 08:00:20'
Acre.cov[(Acre.cov$Raw.Name == "01010062.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/24 08:00:19'
Acre.cov[(Acre.cov$Raw.Name == "01010061.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/24 08:00:18'
Acre.cov[(Acre.cov$Raw.Name == "01010060.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/23 08:00:18'
Acre.cov[(Acre.cov$Raw.Name == "01010059.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/23 08:00:16'
Acre.cov[(Acre.cov$Raw.Name == "01010058.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/23 08:00:14'
Acre.cov[(Acre.cov$Raw.Name == "01010057.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/23 16:00:14'
Acre.cov[(Acre.cov$Raw.Name == "01010056.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/23 16:00:12'
Acre.cov[(Acre.cov$Raw.Name == "01010055.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/23 16:00:10'
Acre.cov[(Acre.cov$Raw.Name == "01010054.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/22 08:00:10'
Acre.cov[(Acre.cov$Raw.Name == "01010053.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/22 08:00:09'
Acre.cov[(Acre.cov$Raw.Name == "01010052.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/22 08:00:05'
Acre.cov[(Acre.cov$Raw.Name == "01010051.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/21 16:00:05'
Acre.cov[(Acre.cov$Raw.Name == "01010050.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/21 16:00:07'
Acre.cov[(Acre.cov$Raw.Name == "01010049.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/21 16:00:05'
Acre.cov[(Acre.cov$Raw.Name == "01010048.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/20 15:00:05'
Acre.cov[(Acre.cov$Raw.Name == "01010047.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/20 15:00:03'
Acre.cov[(Acre.cov$Raw.Name == "01010046.JPG"
          &Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200"),]$data.hora.correta<-'2019/04/20 15:00:01'

### Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200",]$td.photo
Acre.cov[Acre.cov$Camera.Trap.Name == "SJE-T1-C1-1200",]$data.hora.correta

#### 14
##### Camera SJE-T1-C4-3900 - Arapixi

### Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "SJE-T1-C4-3900",]$td.photo
#### Esta camera desconfigurou-se em 2019/05/28 08:54:00
### Add as datas de referência
Acre.cov[Acre.cov$Camera.Trap.Name == "SJE-T1-C4-3900",]$Retirada.hora <- '2019/05/28 08:54:00'
### Obtendo os valores a serem add
difftime(as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "SJE-T1-C4-3900",]$Retirada.hora),
         as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "SJE-T1-C4-3900",]$td.photo), units = "days")
###### Add os valores de correção
Acre.cov[Acre.cov$Camera.Trap.Name == "SJE-T1-C4-3900",]$data.hora.correta[1:129] <- 
  ddays(877.4125) + Acre.cov[Acre.cov$Camera.Trap.Name == "SJE-T1-C4-3900",]$td.photo[1:129]
# Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "SJE-T1-C4-3900",]$data.hora.correta

#### 15 - RMP
##### Camera IUT-T1-C5-3300 - Médio Purus
### Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "IUT-T1-C5-3300",]$td.photo
### Add as datas de referência 23/08/2019
Acre.cov[Acre.cov$Camera.Trap.Name == "IUT-T1-C5-3300",]$Retirada.hora <- '2019/08/23 10:00:00'
### Obtendo os valores a serem add
difftime(as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "IUT-T1-C5-3300",]$Retirada.hora),
         as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "IUT-T1-C5-3300",]$td.photo), units = "days")
###### Add os valores de correção
Acre.cov[Acre.cov$Camera.Trap.Name == "IUT-T1-C5-3300",]$data.hora.correta[1:305] <- 
  ddays(1644.74) + sort(Acre.cov[Acre.cov$Camera.Trap.Name == "IUT-T1-C5-3300",]$td.photo)[1:305]

# Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "IUT-T1-C5-3300",]$data.hora.correta

#### 16 - RMP
##### Camera MAL-T2-C5-5000 - Médio Purus
### Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "MAL-T2-C5-5000",]$td.photo
### Add as datas de referência 23/08/2019
Acre.cov[Acre.cov$Camera.Trap.Name == "MAL-T2-C5-5000",]$Retirada.hora <- '2019/08/28 11:00:00'
### Obtendo os valores a serem add
difftime(as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "MAL-T2-C5-5000",]$Retirada.hora),
         as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "MAL-T2-C5-5000",]$td.photo), units = "days")
###### Add os valores de correção
Acre.cov[Acre.cov$Camera.Trap.Name == "MAL-T2-C5-5000",]$data.hora.correta[1:74] <- 
  ddays(946.43) + Acre.cov[Acre.cov$Camera.Trap.Name == "MAL-T2-C5-5000",]$td.photo[1:74]
# Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "MAL-T2-C5-5000",]$data.hora.correta

#### 17 - RMP
##### Camera REM-T2-C4-4000 - Médio Purus
### Verificando
Acre.cov[Acre.cov$Camera.Trap.Name == "REM-T2-C4-4000",]$td.photo
### Add as datas de referência 23/08/2019
Acre.cov[Acre.cov$Camera.Trap.Name == "REM-T2-C4-4000",]$Retirada.hora <- '2019/08/26 11:00:00'
### Obtendo os valores a serem add
difftime(as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "REM-T2-C4-4000",]$Retirada.hora),
         as.POSIXct(Acre.cov[Acre.cov$Camera.Trap.Name == "REM-T2-C4-4000",]$td.photo), units = "days")
###### Add os valores de correção
Acre.cov[Acre.cov$Camera.Trap.Name == "REM-T2-C4-4000",]$data.hora.correta[1:155] <- 
  ddays(1655.50) + Acre.cov[Acre.cov$Camera.Trap.Name == "REM-T2-C4-4000",]$td.photo[1:155]
Acre.cov[Acre.cov$Camera.Trap.Name == "REM-T2-C4-4000",]$data.hora.correta[222:224] <- 
  ddays(1655.50) + Acre.cov[Acre.cov$Camera.Trap.Name == "REM-T2-C4-4000",]$td.photo[222:224]

# Verificando
#Acre.cov[Acre.cov$Camera.Trap.Name == "REM-T2-C4-4000",]$data.hora.correta
##### Fim

sampaio.data<-Acre.cov