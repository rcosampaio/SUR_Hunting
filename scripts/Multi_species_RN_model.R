### Load data
RN.data<-read.csv(file = here("data","RN.data.csv"), sep = ";", fileEncoding  = "UTF-8")
head(RN.data)

library(R2jags)
library(here)


##----- 2 - Specify model in JAGS language -----

#sink(here("Bayes", "paca_teste.txt"))
sink(here("data","paca_teste.txt"))
cat("
model {
  # Priors
  
  r ~ dunif(0,1)  # per individual detection probability (assuming it remains constant across sites and years)
  
  a0 ~ dnorm(0,0.5)  # intercept on lambda
  a1 ~ dnorm(0,0.5)  # slope on lambda for hydrography
  a2 ~ dnorm(0,0.5)  # slope on lambda for floodded
  a3 ~ dnorm(0,0.5)  # slope on lambda for deforestation
  a4 ~ dnorm(0,0.5)  # slope on lambda for Pa
  a5 ~ dnorm(0,0.5)  # slope on lambda for City HP
  a6 ~ dnorm(0,0.5)  # slope on lambda for Local HP
  
  ### random community effects 
  
  for (i in 1:n.comunidade){
  eps.comm[i] ~ dnorm (0, tau)  # random community effects
  }
  tau <- 1/(sd.tau*sd.tau)
  sd.tau ~ dunif(0,3)


  for(i in 1:nSites){
  
    # population abundanceos.
    log(lambda[i]) <- a0 + a1*Hyd[i] + a2*Flo[i] + a3*Def[i] + a4*UC[i] + a5*HP.C[i] + a6*HP.L[i] + eps.comm[Comunidade[i]]
    
  
  N[i] ~ dpois(lambda[i])
    
  # detection model
  p[i] <- 1-pow(1-r, N[i])  # probability of detecting the species per sampling replicate
  y[i] ~ dbin(p[i], k[i])  # model observation data as binomial outcome with prob p and k trials
        
      }
    } # i

",fill=TRUE)
sink()


##----- Run separately for each species -----#
#----- Cuniculus:

# Input:
nSites <- dim(RN.data)[1]
k <- RN.data$trials.k

### Criando os vetores das covariaveis

Hyd          <- as.vector(RN.data$Hyd)
Flo          <- as.vector(RN.data$Flo)
Def          <- as.vector(RN.data$Def)
UC           <- as.vector(RN.data$UC)
HP.C         <- as.vector(RN.data$HP.C)
HP.L         <- as.vector(RN.data$HP.L)
Comunidade   <- dense_rank(RN.data$Comunidade)
n.comunidade <- length(unique(RN.data$Comunidade))


jags_data <- list(y=RN.data$paca, k=k, Hyd=Hyd, Flo=Flo, Def=Def, UC=UC, HP.C=HP.C, HP.L=HP.L, Comunidade=Comunidade, n.comunidade=n.comunidade, nSites=nSites)

parameters <- c("lambda","r", "N", "a0", "a1", "a2", "a3", "a4", "a5", "a6")

# rodar o modelo
# obs: parece que o modelo e bem sensivel ao valor inicial de lambda
# as vezes precisa ajustar caso a caso dependendo da spp, verificar isso depois
out <- jags(jags_data, inits=NULL, parameters, "paca_teste.txt",
            n.chain=3, n.burnin=1000, n.iter=5000, n.thin=100)
# Summarize posteriors
print(out, dig = 2)

# Save results  
saveRDS(out, "cuniculus.rds")

# check convergence
hist(out$BUGSoutput$summary[,"Rhat"], nclass=8, main="Rhat", xlab="", las=1)
summary(out$BUGSoutput$summary[,"Rhat"])

# Estimated population size per site
# NB! code bellow is not considering separate years, fix it later
dim(out$BUGSoutput$sims.list$N)
mean <- round(apply(out$BUGSoutput$sims.list$N, 2, mean), 2)
hist(mean)
#mean_year4 <- round(apply(out$BUGSoutput$sims.list$N[,,4], 2, mean), 2)
#Nestimate <- tibble(cams=data$trials$cams, year1=mean_year1, year2=mean_year2, year3=mean_year3, year4=mean_year4)
#Nestimate <- tibble(cams=data$trials$cams, year1=mean_year1, year2=mean_year2, year3=mean_year3)
#print(Nestimate, n=Inf)
#Nestimate$change <- round(Nestimate$year2-Nestimate$year1, 2)

#par(mfrow=c(2,2))
#hist(Nestimate$year1, nclass=5, main="N year1", xlab="", las=1)
#hist(Nestimate$year2, nclass=5, main="N year2", xlab="", las=1)
#hist(Nestimate$year3, nclass=5, main="N year3", xlab="", las=1)
#hist(Nestimate$year4, nclass=5, main="N year4", xlab="", las=1)
#hist(Nestimate$change, nclass=5, main="Change in N", xlab="", las=1)
#dev.off()

# save jpeg
#jpeg(here("results", "N_cuniculus.jpg"), res=100, width = 600, height = 600) # Open jpeg file
#jpeg(here("results", "N_dasyprocta.jpg"), res=100, width = 600, height = 600)
#jpeg(here("results", "N_tapirus.jpg"), res=100, width = 600, height = 600)
#jpeg(here("results", "N_myrmecophaga.jpg"), res=100, width = 600, height = 600)
#par(mfrow=c(2,2))
#hist(Nestimate$year1, nclass=5, main="N year1", xlab="", las=1)
#hist(Nestimate$year2, nclass=5, main="N year2", xlab="", las=1)
#hist(Nestimate$year3, nclass=5, main="N year3", xlab="", las=1)
#hist(Nestimate$year4, nclass=5, main="N year4", xlab="", las=1)
#hist(Nestimate$change, nclass=5, main="Change in N", xlab="", las=1)
#dev.off()


cunpac1<-readRDS("cuniculus.rds")


##----- 2 - Model fit -----

# check convergence
hist(cunpac1$BUGSoutput$summary[,"Rhat"], nclass=8, main="Rhat", xlab="", las=1)
summary(cunpac1$BUGSoutput$summary[,"Rhat"])

# Evaluate fit
#mean(cunpac1$BUGSoutput$sims.list$fit.new > cunpac1$BUGSoutput$sims.list$fit) # bayesian p-value
# bayesian p-values is the proportion of points above the 1:1 line of equality
# bayesian p-values close to zero or one are suspicious, we've got 0.58 which is a good fit!
#mean(cunpac1$BUGSoutput$mean$fit) / mean(cunpac1$BUGSoutput$mean$fit.new)
# ratio of real and simulated (perfect) data is close to one, i.e. the fit is good

#mean(dasvar1$BUGSoutput$sims.list$fit.new > dasvar1$BUGSoutput$sims.list$fit) # bayesian p-value
#mean(dasvar1$BUGSoutput$mean$fit) / mean(dasvar1$BUGSoutput$mean$fit.new)

# save jpeg
#jpeg(here("results", "model_fit.jpg"), res=120, width = 1200, height = 600)
#par(mfrow=c(1,2))
#plot(cunpac1$BUGSoutput$sims.list$fit, cunpac1$BUGSoutput$sims.list$fit.new, main = "", xlab = "Discrepancy actual data", ylab = "Discrepancy replicate data", frame.plot = FALSE)
#abline(0, 1, lwd = 2, col = "black")
#mtext("a", side = 3, line = -1.3, adj = 0.05, cex = 1.2, font = 2, col = "black")
#mtext("Bayesian p-value = 0.37", cex = 0.8, font = 2, col = "black")
#plot(dasvar1$BUGSoutput$sims.list$fit, dasvar1$BUGSoutput$sims.list$fit.new, main = "", xlab = "Discrepancy actual data", ylab = "", frame.plot = FALSE)
#abline(0, 1, lwd = 2, col = "black")
#mtext("b", side = 3, line = -1.3, adj = 0.05, cex = 1.2, font = 2, col = "black")
#mtext("Bayesian p-value = 0.55", cex = 0.8, font = 2, col = "black")
#dev.off()


##----- 3 - Explore results -----


#----- abundance estimate

# Estimated abundance size per site

cunpac_N<-round(apply(out$BUGSoutput$sims.list$N, 2, mean), 2)
cunpac_N_97<-round(apply(out$BUGSoutput$sims.list$N, 2, quantile,prob=0.975), 2)
cunpac_N_2.5<-round(apply(out$BUGSoutput$sims.list$N, 2, quantile,prob=0.025), 2)
cunpac<-cbind(cunpac_N,cunpac_N_2.5,cunpac_N_97)


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



