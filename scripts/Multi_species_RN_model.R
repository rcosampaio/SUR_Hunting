### Load data
library(here)
library(conflicted)
conflict_prefer("here", "here")
library(rjags) ### for mac - https://sourceforge.net/projects/mcmc-jags/
library(R2jags)
library(dplyr)
library(Rmisc)


# -------- Load data -----------
RN.data<-read.csv(file =  here("data","RN.data.csv"), sep = ";", fileEncoding  = "UTF-8")
head(RN.data)
tail(RN.data)
dim(RN.data)
# -------- Load sp and guild data -------
sp.guilds<-read.csv(file =  here("data","sp.guilds.csv"), sep = ",", fileEncoding  = "UTF-8")
head(sp.guilds)

##----- Specify model in JAGS language ----- adaptaded from Yamaura et al., 2011 and Carvalho Jr et al., 2020
sink(file = here("data","RN.model.formula.txt"))
cat("model { 

      # prior distributions on community level estimates - hyperparameters
			psi ~ dunif(0,1)	# inclusion rate that generates wi
			
      # mean value (mu)
			# parameter related to abundance
			mu.a0 ~ dnorm(0,0.5)  # intercept on lambda
      mu.a1 ~ dnorm(0,0.5)  # slope on lambda for hydrography
      mu.a2 ~ dnorm(0,0.5)  # slope on lambda for floodded
      mu.a3 ~ dnorm(0,0.5)  # slope on lambda for deforestation
      mu.a4 ~ dnorm(0,0.5)  # slope on lambda for PA
      mu.a5 ~ dnorm(0,0.5)  # slope on lambda for City HP
      mu.a6 ~ dnorm(0,0.5)  # slope on lambda for Local HP
      mu.com~ dnorm(0,0.5)  # intecept on lambda for community
			
			# parameter related to detectability
			mu.r0 ~ dnorm(0,0.5)  # intercept on lambda
      mu.r1 ~ dnorm(0,0.5)  # slope on lambda for hydrography
      mu.r2 ~ dnorm(0,0.5)  # slope on lambda for floodded
      mu.r3 ~ dnorm(0,0.5)  # slope on lambda for deforestation
      mu.r4 ~ dnorm(0,0.5)  # slope on lambda for PA
      mu.r5 ~ dnorm(0,0.5)  # slope on lambda for City HP
      mu.r6 ~ dnorm(0,0.5)  # slope on lambda for Local HP
      mu.r7 ~ dnorm(0,0.5)  # slope on lambda for Effort
      
      # standard deviation
			# parameter related to abundance
      sigma.a0 ~ dunif(0,10)	# intercept
      sigma.a1 ~ dunif(0,10)	# hydrography
			sigma.a2 ~ dunif(0,10)	# floodded
			sigma.a3 ~ dunif(0,10)	# deforestation
			sigma.a4 ~ dunif(0,10)	# PA
			sigma.a5 ~ dunif(0,10)	# City HP
			sigma.a6 ~ dunif(0,10)	# Local HP
			sigma.com~ dunif(0,10)	# Community
		
			# parameter related to detectability
			sigma.r0 ~ dunif(0,10) # intercept
			sigma.r1 ~ dunif(0,10) # hydrography
			sigma.r2 ~ dunif(0,10) # floodded
			sigma.r3 ~ dunif(0,10) # deforestation
			sigma.r4 ~ dunif(0,10) # PA
			sigma.r5 ~ dunif(0,10) # City HP
			sigma.r6 ~ dunif(0,10) # Local HP
			sigma.r7 ~ dunif(0,10) # Effort
			
			# create precision
			# parameter related to abundance
			tau.a0 <- pow(sigma.a0,-2)
			tau.a1 <- pow(sigma.a1,-2)
			tau.a2 <- pow(sigma.a2,-2)
			tau.a3 <- pow(sigma.a3,-2)
			tau.a4 <- pow(sigma.a4,-2)
			tau.a5 <- pow(sigma.a5,-2)
			tau.a6 <- pow(sigma.a6,-2)
			tau.com<- pow(sigma.com,-2)
			
			# parameter related to detectability
			tau.r0 <- pow(sigma.r0,-2)
			tau.r1 <- pow(sigma.r1,-2)
			tau.r2 <- pow(sigma.r2,-2)
			tau.r3 <- pow(sigma.r3,-2)
			tau.r4 <- pow(sigma.r4,-2)
			tau.r5 <- pow(sigma.r5,-2)
			tau.r6 <- pow(sigma.r6,-2)
			tau.r7 <- pow(sigma.r7,-2)
			
      ## generating priors for Community random variable for each species; governed by community-level hyperparameters
			for(i in 1:nspecies){
			 for (j in 1:n.comunidade){
			    acom[j,i] ~ dnorm (mu.com, tau.com) # random effect communities
			 } #j
			} #i
			
			for(i in 1:nspecies) {
				# generating parameters for each species related to abundance; governed by community-level hyperparameters
				a0[i] ~ dnorm(mu.a0,tau.a0)#I(-10,10)
				a1[i] ~ dnorm(mu.a1,tau.a1)#I(-10,10)
				a2[i] ~ dnorm(mu.a2,tau.a2)#I(-10,10)
				a3[i] ~ dnorm(mu.a3,tau.a3)#I(-10,10)
				a4[i] ~ dnorm(mu.a4,tau.a4)#I(-10,10)
				a5[i] ~ dnorm(mu.a5,tau.a5)#I(-10,10)
				a6[i] ~ dnorm(mu.a6,tau.a6)#I(-10,10)
				
				# generating parameters for each species related to individual detection; governed by community-level hyperparameters
				r0[i] ~ dnorm(mu.r0,tau.r0)#I(-10,10)
				r1[i] ~ dnorm(mu.r1,tau.r1)#I(-10,10)
				r2[i] ~ dnorm(mu.r2,tau.r2)#I(-10,10)
				r3[i] ~ dnorm(mu.r3,tau.r3)#I(-10,10)
				r4[i] ~ dnorm(mu.r4,tau.r4)#I(-10,10)
				r5[i] ~ dnorm(mu.r5,tau.r5)#I(-10,10)
				r6[i] ~ dnorm(mu.r6,tau.r6)#I(-10,10)
				r7[i] ~ dnorm(mu.r7,tau.r7)#I(-10,10)

        # indicator variable whether each species is exposed to sampling or not
				w[i] ~ dbern(psi)
				
				#likelihood - Ecological model for latent abundance of species i in sites j
				for(j in 1:nSites){
				# population abundances.
        log(lambda[j,i]) <- a0[i] + a1[i]*Hyd[j] + a2[i]*Flo[j] + a3[i]*Def[j] + a4[i]*UC[j] + a5[i]*HP.C[j] + a6[i]*HP.L[j] + acom[Comunidade[j],i]
        
        N[j,i] ~ dpois(lambda[j,i]) # latent abundance of each species in each site
        #A[j,i] <- N[j,i] * w[i]		  # latent abundance only for extant species
				#o[j,i] <- step(N[j,i]-1)  	# occupancy of each species in each site
				
				# detection process model
				r[j,i] <- 1/(1+exp(-(r0[i] + r1[i]*Hyd[j]  + r2[i]*Flo[j] + r3[i]*Def[j] + r4[i]*UC[j] + r5[i]*HP.C[j] + r6[i]*HP.L[j] + r7[i]*Eff.2[j])))
				p[j,i] <- 1-pow(1-r[j,i],N[j,i])	
				y[j,i] ~ dbin(p[j,i], k[j])  # model observation data as binomial outcome with prob p and k trials
        
        #o1[j,i] <- o[j,i]*G1[i]		# occupancy of guild 1
				#o2[j,i] <- o[j,i]*G2[i]		# occupancy of guild 2
				#o3[j,i] <- o[j,i]*G3[i]		# occupancy of guild 3
				#o4[j,i] <- o[j,i]*G4[i]		# occupancy of guild 4
				
				#A1[j,i] <- A[j,i]*G1[i]		# abundance of guild 1
				#A2[j,i] <- A[j,i]*G2[i]		# abundance of guild 2
				#A3[j,i] <- A[j,i]*G3[i]		# abundance of guild 3
				#A4[j,i] <- A[j,i]*G4[i]		# abundance of guild 4
				
				}#j
			  }#i
		     ## counting species richness at each site
		     #for(j in 1:nSites){
		     #SR0[j]	<- sum(o[j,])	# whole species
		     #SR1[j]	<- sum(o1[j,])	# group 1
		     #SR2[j]	<- sum(o2[j,])	# group 2
		     #SR3[j]	<- sum(o3[j,])	# group 3
		     #SR4[j]	<- sum(o4[j,])	# group 4
	     
		     ## counting abundance at each site
		     #AB0[j]	<- sum(A[j,])	# whole species
		     #AB1[j]	<- sum(A1[j,])	# group 1
		     #AB2[j]	<- sum(A2[j,])	# group 2
		     #AB3[j]	<- sum(A3[j,])	# group 3
		     #AB4[j]	<- sum(A4[j,])	# group 4
	       #}#j
         }
         ",fill=TRUE)
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
#y            <- cbind(y,new.sp) # number of augmented species 10
nguilds      <- 4 # Browser, Carnivore,Frugivore, Granivore, Insectivore, Omnivorous
G            <- as.vector(sp.guilds$Guild)
g            <- rep(NA,length=10)
G            <- c(G,g)

jags_data <- list(y=y, nspecies=nspecies, k=k, Hyd=Hyd, Flo=Flo, Def=Def, UC=UC, HP.C=HP.C, HP.L=HP.L,Eff.2=effort,
                  #nguilds=nguilds, G=G,
                  Comunidade=Comunidade, n.comunidade=n.comunidade, nSites=nSites)

parameters <- c("lambda","r", "N", "A","o", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "acom", 
                "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7")
                #"G", "SR0", "SR1", "SR2", "SR3", "SR4", "AB0", "AB1", "AB2", "AB3", "AB4")
                
# rodar o modelo
out <- jags(jags_data, inits=NULL, parameters,  here("data","RN.model.formula.txt"),
            n.chain=3, n.burnin=1000, n.iter=10000, n.thin=100)

# Summarize posteriors
print(out, dig = 2)
tail(out)

# Save results  
saveRDS(out, here("data", "RN_multitaxa.rds"))

### load model
RN_multitaxa<-readRDS(here("data", "RN_multitaxa.rds"))
out<-RN_multitaxa

# check convergence
hist(out$BUGSoutput$summary[,"Rhat"], nclass=8, main="Rhat", xlab="", las=1)
summary(out$BUGSoutput$summary[,"Rhat"])

# Estimated population size per site
dim(out$BUGSoutput$sims.list$N)
mean <- round(apply(out$BUGSoutput$sims.list$N, 2, mean), 2)
hist(mean)


##----- 2 - Model fit -----

# check convergence
#hist(RN_multitaxa$BUGSoutput$summary[,"Rhat"], nclass=8, main="Rhat", xlab="", las=1)
#summary(RN_multitaxa$BUGSoutput$summary[,"Rhat"])

out$BUGSoutput$sims.list$AB0
out$BUGSoutput$sims.list$SR0

library(Rmisc)


##----- 3 - Individual species responses to abundance covariates -----
out$BUGSoutput$sims.list[["a1"]][,1:29]

#------- STREAM DENSITY ------
species <- colnames(y)
lam.coef          <- out$BUGSoutput$sims.list[["a1"]][,1:29]; colnames(lam.coef)<- species
lam.coef.Mean     <- apply(lam.coef, 2, mean)
lam.coef.Low      <- apply(lam.coef, 2, quantile, probs=0.05)
lam.coef.Upper    <- apply(lam.coef, 2, quantile, probs=0.95)
lam.coef.DF       <- rbind(lam.coef.Mean, lam.coef.Low, lam.coef.Upper); 

# transpose table in order to verifify significance (CI does not overlap zero)
lamb <- as.data.frame(t(lam.coef.DF[,1:ncol(lam.coef.DF)]))
colnames(lamb) <- c("mean", "lowerCI", "upperCI")
(lamb <- round(lamb[order(lamb$mean, decreasing=TRUE),],2)) # reorder based on species names

### Plot for visual checking 
lamb$cor<-ifelse(lamb$lowerCI < 0 &          lamb$upperCI < 0, "vermelho",
                      ifelse(lamb$lowerCI > 0 & lamb$upperCI > 0, "azul", "preto"))
lamb$sp<-rownames(lamb)

library(ggplot2)
####HYD
(hyd.beta<-ggplot(lamb, aes(y=mean, x=reorder(sp, mean), ymin=lowerCI, ymax=upperCI, colour = cor)) +  
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












7# plot mean and 95% CI for a.psi1 to check visually
par(mar=c(9,3,1.5,1.5))
plot(0,0, xlim=range(c(1:nrow(a.psi1bySpecies))), ylim=range(c(a.psi1bySpecies$lowerCI-1,a.psi1bySpecies$upperCI+1)), type = "n", xaxt="n",
     pch=19, xlab="", ylab="", las=2)
abline(h = 0,col = "grey")
arrows(x0 = 1:nrow(a.psi1bySpecies), x1 = 1:nrow(a.psi1bySpecies), y0=a.psi1bySpecies$lowerCI, y1=a.psi1bySpecies$upperCI, length=0, angle=90, code=3)
points(1:nrow(a.psi1bySpecies), a.psi1bySpecies$mean, pch = 20, cex = 1)
axis(side=1,at=1:nrow(a.psi1bySpecies),labels=row.names(a.psi1bySpecies), las=2, cex.axis=0.8)


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

logging_effects_cunpac
#logging_effects_dasvar


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



