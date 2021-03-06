### Load data
library(here)
#library(rjags) ### for mac - https://sourceforge.net/projects/mcmc-jags/
library(R2jags)
library(dplyr)
library(Rmisc)
library(conflicted)
conflict_prefer("here", "here")

# -------- Load data -----------
RN.data<-read.csv(file = here::here("data","RN.data.csv"), sep = ";", fileEncoding  = "UTF-8")
head(RN.data)
tail(RN.data)
dim(RN.data)
# -------- Load sp and guild data -------
sp.guilds<-read.csv(file =  here::here("data","sp.guilds.csv"), sep = ",", fileEncoding  = "UTF-8")
head(sp.guilds)

##----- Specify model in JAGS language ----- adaptaded from Ferreira et al., 2020.
sink(here("data","RN.model.formula.guilds.txt"))
cat("model {

      # prior distributions on community level estimates - hyperparameters
			psi ~ dunif(0,1)	# inclusion rate that generates wi
			
			for(z in 1:nguilds){ ### Z loops over the 4 guilds categories
			# mean value (mu)
			# parameter related to abundance
			mu.a0[z] ~ dnorm(0,0.5)  # intercept on lambda
      mu.a1[z] ~ dnorm(0,0.5)  # slope on lambda for hydrography
      mu.a2[z] ~ dnorm(0,0.5)  # slope on lambda for floodded
      mu.a3[z] ~ dnorm(0,0.5)  # slope on lambda for deforestation
      mu.a4[z] ~ dnorm(0,0.5)  # slope on lambda for PA
      mu.a5[z] ~ dnorm(0,0.5)  # slope on lambda for City HP
      mu.a6[z] ~ dnorm(0,0.5)  # slope on lambda for Local HP
      mu.com[z]~ dnorm(0,0.5)  # intecept on lambda for community
			
			# parameter related to detectability
			mu.r0[z] ~ dnorm(0,0.5)  # intercept on lambda
      mu.r1[z] ~ dnorm(0,0.5)  # slope on lambda for hydrography
      mu.r2[z] ~ dnorm(0,0.5)  # slope on lambda for floodded
      mu.r3[z] ~ dnorm(0,0.5)  # slope on lambda for deforestation
      mu.r4[z] ~ dnorm(0,0.5)  # slope on lambda for PA
      mu.r5[z] ~ dnorm(0,0.5)  # slope on lambda for City HP
      mu.r6[z] ~ dnorm(0,0.5)  # slope on lambda for Local HP
      mu.r7[z] ~ dnorm(0,0.5)  # slope on lambda for Effort
      
      # standard deviation
			# parameter related to abundance
      sigma.a0[z] ~ dunif(0,10)	# intercept
      sigma.a1[z] ~ dunif(0,10)	# hydrography
			sigma.a2[z] ~ dunif(0,10)	# floodded
			sigma.a3[z] ~ dunif(0,10)	# deforestation
			sigma.a4[z] ~ dunif(0,10)	# PA
			sigma.a5[z] ~ dunif(0,10)	# City HP
			sigma.a6[z] ~ dunif(0,10)	# Local HP
			sigma.com[z]~ dunif(0,10)	# Community
		
			# parameter related to detectability
			sigma.r0[z] ~ dunif(0,10) # intercept
			sigma.r1[z] ~ dunif(0,10) # hydrography
			sigma.r2[z] ~ dunif(0,10) # floodded
			sigma.r3[z] ~ dunif(0,10) # deforestation
			sigma.r4[z] ~ dunif(0,10) # PA
			sigma.r5[z] ~ dunif(0,10) # City HP
			sigma.r6[z] ~ dunif(0,10) # Local HP
			sigma.r7[z] ~ dunif(0,10) # Effort
			
			# create precision
			# parameter related to abundance
			tau.a0[z] <- pow(sigma.a0[z],-2)
			tau.a1[z] <- pow(sigma.a1[z],-2)
			tau.a2[z] <- pow(sigma.a2[z],-2)
			tau.a3[z] <- pow(sigma.a3[z],-2)
			tau.a4[z] <- pow(sigma.a4[z],-2)
			tau.a5[z] <- pow(sigma.a5[z],-2)
			tau.a6[z] <- pow(sigma.a6[z],-2)
			tau.com[z]<- pow(sigma.com[z],-2)
			
			# parameter related to detectability
			tau.r0[z] <- pow(sigma.r0[z],-2)
			tau.r1[z] <- pow(sigma.r1[z],-2)
			tau.r2[z] <- pow(sigma.r2[z],-2)
			tau.r3[z] <- pow(sigma.r3[z],-2)
			tau.r4[z] <- pow(sigma.r4[z],-2)
			tau.r5[z] <- pow(sigma.r5[z],-2)
			tau.r6[z] <- pow(sigma.r6[z],-2)
			tau.r7[z] <- pow(sigma.r7[z],-2)
      }
			
      ## generating priors for Community random variable for each species; governed by community-level hyperparameters
			for(i in 1:nspecies){
			 for (j in 1:n.comunidade){
			    acom[j,i] ~ dnorm (mu.com[G[i]], tau.com[G[i]]) # random effect communities
			 }}
			
			for(i in 1:nspecies) {
				# generating parameters for each species related to abundance; governed by community-level hyperparameters
				a0[i] ~ dnorm(mu.a0[G[i]],tau.a0[G[i]])#I(-10,10)
				a1[i] ~ dnorm(mu.a1[G[i]],tau.a1[G[i]])#I(-10,10)
				a2[i] ~ dnorm(mu.a2[G[i]],tau.a2[G[i]])#I(-10,10)
				a3[i] ~ dnorm(mu.a3[G[i]],tau.a3[G[i]])#I(-10,10)
				a4[i] ~ dnorm(mu.a4[G[i]],tau.a4[G[i]])#I(-10,10)
				a5[i] ~ dnorm(mu.a5[G[i]],tau.a5[G[i]])#I(-10,10)
				a6[i] ~ dnorm(mu.a6[G[i]],tau.a6[G[i]])#I(-10,10)
				
				# generating parameters for each species related to individual detection; governed by community-level hyperparameters
				r0[i] ~ dnorm(mu.r0[G[i]],tau.r0[G[i]])#I(-10,10)
				r1[i] ~ dnorm(mu.r1[G[i]],tau.r1[G[i]])#I(-10,10)
				r2[i] ~ dnorm(mu.r2[G[i]],tau.r2[G[i]])#I(-10,10)
				r3[i] ~ dnorm(mu.r3[G[i]],tau.r3[G[i]])#I(-10,10)
				r4[i] ~ dnorm(mu.r4[G[i]],tau.r4[G[i]])#I(-10,10)
				r5[i] ~ dnorm(mu.r5[G[i]],tau.r5[G[i]])#I(-10,10)
				r6[i] ~ dnorm(mu.r6[G[i]],tau.r6[G[i]])#I(-10,10)
				r7[i] ~ dnorm(mu.r7[G[i]],tau.r7[G[i]])#I(-10,10)

        # indicator variable whether each species is exposed to sampling or not
				w[i] ~ dbern(psi)
				
				#likelihood - Ecological model for latent abundance of species i in sites j
				for(j in 1:nSites){
				# population abundances.
        log(lambda[j,i]) <- a0[i] + a1[i]*Hyd[j] + a2[i]*Flo[j] + a3[i]*Def[j] + a4[i]*UC[j] + a5[i]*HP.C[j] + a6[i]*HP.L[j] + acom[Comunidade[j],i] # equation (4)
        
        N[j,i] ~ dpois(lambda[j,i]) # latent abundance of each species in each site
        #A[j,i] <- N[j,i]*w[i]		# latent abundance only for extant species
				#o[j,i] <- step(N[j,i]-1)	# occupancy of each species in each site
				
				# detection process model
				r[j,i] <- 1/(1+exp(-(r0[i] + r1[i]*Hyd[j]  + r2[i]*Flo[j] + r3[i]*Def[j] + r4[i]*UC[j] + r5[i]*HP.C[j] + r6[i]*HP.L[j] + r7[i]*Eff.2[j])))
				p[j,i] <- 1-pow(1-r[j,i],N[j,i])	
				y[j,i] ~ dbin(p[j,i], k[j])  # model observation data as binomial outcome with prob p and k trials
      
				}}}" ,fill=TRUE)
sink()

# Input:
nSites       <- dim(RN.data)[1]
k            <- RN.data$trials.k
Hyd          <- as.vector(RN.data$Hyd)
Flo          <- as.vector(RN.data$Flo)
Def          <- as.vector(RN.data$Def)
UC           <- as.vector(RN.data$UC)
HP.C         <- as.vector(RN.data$HP.C)
HP.L         <- as.vector(RN.data$HP.L)
Comunidade   <- dense_rank(RN.data$Comunidade)
n.comunidade <- length(unique(RN.data$Comunidade))
effort       <- as.vector(RN.data$Eff.2)
nspecies     <- 29 #29 species
y            <- RN.data[1:29]	
#new.sp       <- cbind(rep(0,720),rep(0,720),rep(0,720),rep(0,720),rep(0,720),rep(0,720),rep(0,720),rep(0,720),rep(0,720),rep(0,720))
#y            <- as.vector(cbind(y,new.sp)) # number of augmented species 10
nguilds      <- 4 # Browser, Carnivore,Frugivore, Granivore, Insectivore, Omnivorous
G            <- as.vector(sp.guilds$Guild)
g            <- rep(NA,length=10)
G            <- c(G,g)

jags_data <- list(y=y, nspecies=nspecies, k=k, Hyd=Hyd, Flo=Flo, Def=Def, UC=UC, HP.C=HP.C, HP.L=HP.L,Eff.2=effort,nguilds=nguilds, 
                  G=G,Comunidade=Comunidade, n.comunidade=n.comunidade, nSites=nSites)

parameters <- c("lambda","r", "N", "mu.a1", "mu.a2", "mu.a3", "mu.a4", "mu.a5", "mu.a6",
                "a0", "a1", "a2", "a3", "a4", "a5", "a6", "acom", 
                "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7","G")

# rodar o modelo
out <- jags(jags_data, inits=NULL, parameters, here("data","RN.model.formula.guilds.txt"),
            n.chain=3, n.burnin=1000, n.iter=10000, n.thin=100)

# Summarize posteriors
print(out, dig = 2)

# Save results  
saveRDS(out, here("data", "RN_multitaxa_guilds.rds"))

### load model
RN_multitaxa<-readRDS(here("data", "RN_multitaxa.rds"))
out<-RN_multitaxa

##----- 2 - Model fit -----
hist(out$BUGSoutput$summary[,"Rhat"], nclass=8, main="Rhat", xlab="", las=1)
summary(out$BUGSoutput$summary[,"Rhat"])

# Estimated population size per site
dim(out$BUGSoutput$sims.list$N)
mean <- round(apply(out$BUGSoutput$sims.list$N, 2, mean), 2)
hist(mean)



##----- 3 - guild responses to abundance covariates -----
out$BUGSoutput$sims.list$G # to see the guild number by colum


out$BUGSoutput$sims.list$mu.a1 #hyperparameter for a1

#------- STREAM DENSITY ------
## ---- Frugivores - 1 
guild<-out$BUGSoutput$sims.list$mu.a1[,1]
lam.fru<- as.data.frame(t(CI(guild, ci=0.95)))
lam.fru$Guild<-"Frugivore"

## ---- Carnivores - 2
guild<-out$BUGSoutput$sims.list$mu.a1[,2]
lam.carn<- as.data.frame(t(CI(guild, ci=0.95)))
lam.carn$Guild<-"Carnivore"

## ---- Insetivores - 3
guild<-out$BUGSoutput$sims.list$mu.a1[,3]
lam.ins<- as.data.frame(t(CI(guild, ci=0.95)))
lam.ins$Guild<-"Insectivore"

## ---- Granivore - 4
guild<-out$BUGSoutput$sims.list$mu.a1[,4]
lam.gra<- as.data.frame(t(CI(guild, ci=0.95)))
lam.gra$Guild<-"Granivore"

### merging
Stream<-rbind(lam.fru,lam.carn,lam.ins,lam.gra)

### Plot for visual checking 
Stream$cor<-ifelse(Stream$lower < 0 &          Stream$upper < 0, "vermelho",
                 ifelse(Stream$lower > 0 & Stream$upper > 0, "azul", "preto"))
Stream

library(ggplot2)
####HYD
(hyd.beta<-ggplot(Stream, aes(y=mean, x=reorder(Guild, mean), ymin=lower, ymax=upper, colour = cor)) +  
    geom_pointrange(size=0.7) + 
    coord_flip() +
    xlab("") + 
    ylab("") + 
    geom_hline(yintercept=0, linetype = "dashed", col="darkgray")+
    scale_color_manual(values = c("blue","black","red"))+
    theme (panel.background = element_rect(fill = "transparent", colour = NA),
           axis.line = element_line(colour = "black"),
           axis.text = element_text(size=14),
           legend.key.size = unit(.2, 'cm'),
           plot.margin=unit(c(.1,.1,0,0),"cm"),
           plot.title = element_text(face = "bold", hjust = 0.5, size = 16))+
    guides(color = FALSE)+
    ggtitle("Stream length"))



#------- FLOODPLAIN EXTENSION ------
## ---- Frugivores - 1 
guild<-out$BUGSoutput$sims.list$mu.a2[,1]
lam.fru<- as.data.frame(t(CI(guild, ci=0.95)))
lam.fru$Guild<-"Frugivore"
## ---- Carnivores - 2
guild<-out$BUGSoutput$sims.list$mu.a2[,2]
lam.carn<- as.data.frame(t(CI(guild, ci=0.95)))
lam.carn$Guild<-"Carnivore"

## ---- Insetivores - 3
guild<-out$BUGSoutput$sims.list$mu.a2[,3]
lam.ins<- as.data.frame(t(CI(guild, ci=0.95)))
lam.ins$Guild<-"Insectivore"

## ---- Granivore - 4
guild<-out$BUGSoutput$sims.list$mu.a2[,4]
lam.gra<- as.data.frame(t(CI(guild, ci=0.95)))
lam.gra$Guild<-"Granivore"

### merging
Flood<-rbind(lam.fru,lam.carn,lam.ins,lam.gra)

### Plot for visual checking 
Flood$cor<-ifelse(Flood$lower < 0 &          Flood$upper < 0, "vermelho",
                   ifelse(Flood$lower > 0 & Flood$upper > 0, "azul", "preto"))
Flood

library(ggplot2)
####Flood
(Flood.beta<-ggplot(Flood, aes(y=mean, x=reorder(Guild, mean), ymin=lower, ymax=upper, colour = cor)) +  
    geom_pointrange(size=0.7) + 
    coord_flip() +
    xlab("") + 
    ylab("") + 
    geom_hline(yintercept=0, linetype = "dashed", col="darkgray")+
    scale_color_manual(values = c("black","red"))+
    theme (panel.background = element_rect(fill = "transparent", colour = NA),
           axis.line = element_line(colour = "black"),
           axis.text = element_text(size=14),
           legend.key.size = unit(.2, 'cm'),
           plot.margin=unit(c(.1,.1,0,0),"cm"),
           plot.title = element_text(face = "bold", hjust = 0.5, size = 16))+
    guides(color = FALSE)+
    ggtitle("Floodplain"))


#------- DEFORESTATION ------
## ---- Frugivores - 1 
guild<-out$BUGSoutput$sims.list$mu.a3[,1]
lam.fru<- as.data.frame(t(CI(guild, ci=0.95)))
lam.fru$Guild<-"Frugivore"
## ---- Carnivores - 2
guild<-out$BUGSoutput$sims.list$mu.a3[,2]
lam.carn<- as.data.frame(t(CI(guild, ci=0.95)))
lam.carn$Guild<-"Carnivore"

## ---- Insetivores - 3
guild<-out$BUGSoutput$sims.list$mu.a3[,3]
lam.ins<- as.data.frame(t(CI(guild, ci=0.95)))
lam.ins$Guild<-"Insectivore"

## ---- Granivore - 4
guild<-out$BUGSoutput$sims.list$mu.a3[,4]
lam.gra<- as.data.frame(t(CI(guild, ci=0.95)))
lam.gra$Guild<-"Granivore"

### merging
Def<-rbind(lam.fru,lam.carn,lam.ins,lam.gra)

### Plot for visual checking 
Def$cor<-ifelse(Def$lower < 0 &          Def$upper < 0, "vermelho",
                  ifelse(Def$lower > 0 & Def$upper > 0, "azul", "preto"))
Def

library(ggplot2)
####Def
(Def.beta<-ggplot(Def, aes(y=mean, x=reorder(Guild, mean), ymin=lower, ymax=upper, colour = cor)) +  
    geom_pointrange(size=0.7) + 
    coord_flip() +
    xlab("") + 
    ylab("") + 
    geom_hline(yintercept=0, linetype = "dashed", col="darkgray")+
    scale_color_manual(values = c("blue","black","red"))+
    theme (panel.background = element_rect(fill = "transparent", colour = NA),
           axis.line = element_line(colour = "black"),
           axis.text = element_text(size=14),
           legend.key.size = unit(.2, 'cm'),
           plot.margin=unit(c(.1,.1,0,0),"cm"),
           plot.title = element_text(face = "bold", hjust = 0.5, size = 16))+
    guides(color = FALSE)+
    ggtitle("Deforestation"))


#------- PROTECTION LEVEL ------
## ---- Frugivores - 1 
guild<-out$BUGSoutput$sims.list$mu.a4[,1]
lam.fru<- as.data.frame(t(CI(guild, ci=0.95)))
lam.fru$Guild<-"Frugivore"
## ---- Carnivores - 2
guild<-out$BUGSoutput$sims.list$mu.a4[,2]
lam.carn<- as.data.frame(t(CI(guild, ci=0.95)))
lam.carn$Guild<-"Carnivore"

## ---- Insetivores - 3
guild<-out$BUGSoutput$sims.list$mu.a4[,3]
lam.ins<- as.data.frame(t(CI(guild, ci=0.95)))
lam.ins$Guild<-"Insectivore"

## ---- Granivore - 4
guild<-out$BUGSoutput$sims.list$mu.a4[,4]
lam.gra<- as.data.frame(t(CI(guild, ci=0.95)))
lam.gra$Guild<-"Granivore"

### merging
Prot<-rbind(lam.fru,lam.carn,lam.ins,lam.gra)

### Plot for visual checking 
Prot$cor<-ifelse(Prot$lower < 0 &       Prot$upper < 0, "vermelho",
                ifelse(Prot$lower > 0 & Prot$upper > 0, "azul", "preto"))
Prot

library(ggplot2)
####Protection level
(Prot.beta<-ggplot(Prot, aes(y=mean, x=reorder(Guild, mean), ymin=lower, ymax=upper, colour = cor)) +  
    geom_pointrange(size=0.7) + 
    coord_flip() +
    xlab("") + 
    ylab("") + 
    geom_hline(yintercept=0, linetype = "dashed", col="darkgray")+
    scale_color_manual(values = c("blue","black","red"))+
    theme (panel.background = element_rect(fill = "transparent", colour = NA),
           axis.line = element_line(colour = "black"),
           axis.text = element_text(size=14),
           legend.key.size = unit(.2, 'cm'),
           plot.margin=unit(c(.1,.1,0,0),"cm"),
           plot.title = element_text(face = "bold", hjust = 0.5, size = 16))+
    guides(color = FALSE)+
    ggtitle("Protection level"))


#------- CITY HUNTING PRESSURE ------
## ---- Frugivores - 1 
guild<-out$BUGSoutput$sims.list$mu.a5[,1]
lam.fru<- as.data.frame(t(CI(guild, ci=0.95)))
lam.fru$Guild<-"Frugivore"
## ---- Carnivores - 2
guild<-out$BUGSoutput$sims.list$mu.a5[,2]
lam.carn<- as.data.frame(t(CI(guild, ci=0.95)))
lam.carn$Guild<-"Carnivore"

## ---- Insetivores - 3
guild<-out$BUGSoutput$sims.list$mu.a5[,3]
lam.ins<- as.data.frame(t(CI(guild, ci=0.95)))
lam.ins$Guild<-"Insectivore"

## ---- Granivore - 4
guild<-out$BUGSoutput$sims.list$mu.a5[,4]
lam.gra<- as.data.frame(t(CI(guild, ci=0.95)))
lam.gra$Guild<-"Granivore"

### merging
HP.C<-rbind(lam.fru,lam.carn,lam.ins,lam.gra)

### Plot for visual checking 
HP.C$cor<-ifelse(HP.C$lower < 0 &        HP.C$upper < 0, "vermelho",
                 ifelse(HP.C$lower > 0 & HP.C$upper > 0, "azul", "preto"))
HP.C

library(ggplot2)
####City Hunting Pressure
(HP.C.beta<-ggplot(HP.C, aes(y=mean, x=reorder(Guild, mean), ymin=lower, ymax=upper, colour = cor)) +  
    geom_pointrange(size=0.7) + 
    coord_flip() +
    xlab("") + 
    ylab("") + 
    geom_hline(yintercept=0, linetype = "dashed", col="darkgray")+
    scale_color_manual(values = c("blue","red"))+
    theme (panel.background = element_rect(fill = "transparent", colour = NA),
           axis.line = element_line(colour = "black"),
           axis.text = element_text(size=14),
           legend.key.size = unit(.2, 'cm'),
           plot.margin=unit(c(.1,.1,0,0),"cm"),
           plot.title = element_text(face = "bold", hjust = 0.5, size = 16))+
    guides(color = FALSE)+
    ggtitle("City Hunting Pressure"))

#------- LOCAL HUNTING PRESSURE ------
## ---- Frugivores - 1 
guild<-out$BUGSoutput$sims.list$mu.a6[,1]
lam.fru<- as.data.frame(t(CI(guild, ci=0.95)))
lam.fru$Guild<-"Frugivore"
## ---- Carnivores - 2
guild<-out$BUGSoutput$sims.list$mu.a6[,2]
lam.carn<- as.data.frame(t(CI(guild, ci=0.95)))
lam.carn$Guild<-"Carnivore"

## ---- Insetivores - 3
guild<-out$BUGSoutput$sims.list$mu.a6[,3]
lam.ins<- as.data.frame(t(CI(guild, ci=0.95)))
lam.ins$Guild<-"Insectivore"

## ---- Granivore - 4
guild<-out$BUGSoutput$sims.list$mu.a6[,4]
lam.gra<- as.data.frame(t(CI(guild, ci=0.95)))
lam.gra$Guild<-"Granivore"

### merging
HP.L<-rbind(lam.fru,lam.carn,lam.ins,lam.gra)

### Plot for visual checking 
HP.L$cor<-ifelse(HP.L$lower < 0 &        HP.L$upper < 0, "vermelho",
                 ifelse(HP.L$lower > 0 & HP.L$upper > 0, "azul", "preto"))
HP.L

library(ggplot2)
####Local Hunting Pressure
(HP.L.beta<-ggplot(HP.L, aes(y=mean, x=reorder(Guild, mean), ymin=lower, ymax=upper, colour = cor)) +  
    geom_pointrange(size=0.7) + 
    coord_flip() +
    xlab("") + 
    ylab("") + 
    geom_hline(yintercept=0, linetype = "dashed", col="darkgray")+
    scale_color_manual(values = c("blue","red"))+
    theme (panel.background = element_rect(fill = "transparent", colour = NA),
           axis.line = element_line(colour = "black"),
           axis.text = element_text(size=14),
           legend.key.size = unit(.2, 'cm'),
           plot.margin=unit(c(.1,.1,0,0),"cm"),
           plot.title = element_text(face = "bold", hjust = 0.5, size = 16))+
    guides(color = "none")+
    ggtitle("Local Hunting Pressure"))





# save jpeg N estimate
jpeg(here("results", "N_estimate_t1.jpg"), res=100, width = 800, height = 800)
par(mfrow=c(2,1))
par(mar=c(3,3,1,1))
hist(cunpac_N[['2017']], nclass=10, main="", xlab="", las=1)
mtext("a", side = 3, line = -1.3, adj = 0.05, cex = 1.2, font = 2, col = "black")
hist(dasvar_N[['2017']], nclass=10, main="", xlab="", las=1)
mtext("b", side = 3, line = -1.3, adj = 0.05, cex = 1.2, font = 2, col = "black")
dev.off()



# table summarizing model parameters
parameters.table <- function(data) {
  out <- data
  parameters_table <- tibble(parameter=c("initial_abundance", "lambda", "r"),
                             mean=c(round(apply(out$BUGSoutput$sims.list$N, 2, mean), 2),
                                    round(apply(out$BUGSoutput$sims.list$lambda, 2, mean), 2),
                                    #round(mean(out$BUGSoutput$sims.list$S), 2),
                                    #round(mean(out$BUGSoutput$sims.list$omega), 2),
                                    #round(mean(out$BUGSoutput$sims.list$G), 2),
                                    #round(mean(out$BUGSoutput$sims.list$gamma), 2),
                                    round(mean(out$BUGSoutput$sims.list$r), 2)),
                             lower=c(round(apply(out$BUGSoutput$sims.list$N,2, quantile, probs=0.025), 2),
                                     round(apply(out$BUGSoutput$sims.list$lambda, 2, quantile, probs=0.025), 2),
                                     #round(quantile(out$BUGSoutput$sims.list$S, probs=0.025), 2),
                                     #round(quantile(out$BUGSoutput$sims.list$omega, probs=0.025), 2),
                                     #round(quantile(out$BUGSoutput$sims.list$G, probs=0.025), 2),
                                     #round(quantile(out$BUGSoutput$sims.list$gamma, probs=0.025), 2),
                                     round(quantile(out$BUGSoutput$sims.list$r, probs=0.025), 2) ),
                             upper= c(round(apply(out$BUGSoutput$sims.list$N, 2, quantile, probs=0.975), 2),
                                      round(apply(out$BUGSoutput$sims.list$lambda, 2, quantile, probs=0.975), 2),
                                      #round(quantile(out$BUGSoutput$sims.list$S, probs=0.975), 2),
                                      #round(quantile(out$BUGSoutput$sims.list$omega, probs=0.975), 2),
                                      #round(quantile(out$BUGSoutput$sims.list$G, probs=0.975), 2),
                                      #round(quantile(out$BUGSoutput$sims.list$gamma, probs=0.975), 2),
                                      round(quantile(out$BUGSoutput$sims.list$p, probs=0.975), 2) ) )
  parameters_table <- data.frame(parameters_table)
  parameters_table
}

parameters.table(cunpac1)


#----- covariate effects

# mean and 95% CI of covariate effects
#a1*Hyd[i] + a2*Flo[i] + a3*Def[i] + a4*UC[i] + a5*HP.C[i] + a6*HP.L[i]
(effects_cunpac <- tibble(variable=c("Hyd", "Flo","Def", "UC", "HP.C", "HP.L"),
                          mean=c(round(mean(cunpac1$BUGSoutput$sims.list$a1), 2),
                                 round(mean(cunpac1$BUGSoutput$sims.list$a2), 2),
                                 round(mean(cunpac1$BUGSoutput$sims.list$a3), 2),
                                 round(mean(cunpac1$BUGSoutput$sims.list$a4), 2),
                                 round(mean(cunpac1$BUGSoutput$sims.list$a5), 2),
                                 round(mean(cunpac1$BUGSoutput$sims.list$a6), 2)),
                          
                          lowerCI=c(round(quantile(cunpac1$BUGSoutput$sims.list$a1, probs=c(0.025)), 2),
                                    round(quantile(cunpac1$BUGSoutput$sims.list$a2, probs=c(0.025)), 2),
                                    round(quantile(cunpac1$BUGSoutput$sims.list$a3, probs=c(0.025)), 2),
                                    round(quantile(cunpac1$BUGSoutput$sims.list$a4, probs=c(0.025)), 2),
                                    round(quantile(cunpac1$BUGSoutput$sims.list$a5, probs=c(0.025)), 2),
                                    round(quantile(cunpac1$BUGSoutput$sims.list$a6, probs=c(0.025)), 2)),
                          
                          upperCI=c(round(quantile(cunpac1$BUGSoutput$sims.list$a1, probs=c(0.975)), 2),
                                    round(quantile(cunpac1$BUGSoutput$sims.list$a2, probs=c(0.975)), 2),
                                    round(quantile(cunpac1$BUGSoutput$sims.list$a3, probs=c(0.975)), 2),
                                    round(quantile(cunpac1$BUGSoutput$sims.list$a4, probs=c(0.975)), 2),
                                    round(quantile(cunpac1$BUGSoutput$sims.list$a5, probs=c(0.975)), 2),
                                    round(quantile(cunpac1$BUGSoutput$sims.list$a6, probs=c(0.975)), 2))))


# plot posterior distribution of predictor effects
# use function "Fig_S2and3"
Fig_S2and3(cunpac1)
Fig_S2and3(dasvar1)

# save jpeg
jpeg(here("results", "Fig_S2.jpg"), res=120, width = 800, height = 600)
Fig_S2and3(cunpac1)
dev.off()

jpeg(here("results", "Fig_S3.jpg"), res=120, width = 800, height = 600)
Fig_S2and3(dasvar1)
dev.off()


# plot significant effects with uncertainty
# intensity on initial abundance was the only significant effect both for Cuniculus and Dasyprocta:

dev.off()
effect.on.N(cunpac1)
#effect.on.S(cunpac1)
#effect.on.G(cunpac1)

effect.on.N(dasvar1)
#effect.on.S(dasvar1)
#effect.on.G(dasvar1)

# save jpeg
jpeg(here("results", "Fig_2.jpg"), res=100, width = 600, height = 600)
par(mfrow=c(2,1))
par(mar=c(2,3,2,1))
effect.on.N(cunpac1)
mtext("a", side = 3, line = -1.3, adj = 0.05, cex = 1.2, font = 2, col = "black")
par(mar=c(4,3,1,1))
effect.on.N(dasvar1)
mtext("b", side = 3, line = -1.3, adj = 0.05, cex = 1.2, font = 2, col = "black")
dev.off()



