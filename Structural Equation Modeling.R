#install.packages("lavaan")

library(lavaan)
setwd(dir='H:/matlab/squid_impro/SEM')
datap <- read.csv("H:/matlab/Pacific Saury/SEM/path.csv")
stdatap <- scale(datap,center = TRUE,scale = TRUE)


# Initial path model
model1 <- '# regressions 
          WSST ~ KCdis
          WSSH ~ KCdis
          SSST1 ~ KEFlat
          SSST2 ~ KEFlat
          SSSH1 ~ KEFlat
          SSSH2 ~ KEFlat
          CPUEps ~  WSSH + WSST + KCdis + SSST1 + SSST2 + KEFlat
          LNRjs ~  WSSH + WSST + KCdis + SSST1 + SSST2 + KEFlat
          LNRPSjs ~  WSSH + WSST + KCdis + SSST1 + SSST2 + KEFlat
          
          # corr
          KCdis ~~ KEF
          WSST ~~  WSSH
          SSST1 ~~ SSSH1
          SSST2 ~~ SSSH2
          SSST1 ~~ SSST2
          SSSH1 ~~ SSSH2
          '

#Run the model
fit1 <- sem(model1,data = stdatap,
            se = "bootstrap", bootstrap = 1000)

#Examine the model results and eliminate the insignificant paths based on the p-values
summary(fit1,standardized = TRUE)

#Check whether the model meets the relevant standards. If not, proceed to the next step. 
#If this condition is met, the final model will be obtained.
fitMeasures(fit1,c('chisq','df','pvalue','cfi','rmsea'))

#Examine the modification indices (MI) of the structural equation model (SEM), 
#and add the paths with higher MI values to the model.
#Repeat the above process until the standard is reached.
mf <- modificationindices(fit1)
mf <- mf[order(mf$mi,decreasing = TRUE),]
head(mf,15)

####################################################################################
#The final model
model2 <- '# regressions 
          
          WSSH ~ KCdis
          SSST1 ~ KEFlat
          SSST2 ~ KEFlat
          SSSH1 ~ KEFlat
          SSSH2 ~ KEFlat
          CPUEps ~  WSSH  +  SSST1  
          LNRjs ~  WSST + SSSH2  
          LNRPSjs ~  WSST +SSSH2
          WSSH  ~  SSSH1
         
          # corr
          KCdis ~~ KEFlat
          WSSH ~~  SSST1
          WSST ~~ WSSH
          SSST1 ~~ SSST2
          SSST2 ~~ SSSH2
          SSSH1 ~~ SSSH2
          LNRjs ~~ LNRPSjs
          KCdis ~~ WSST
          '
#Run the model
fit2 <- sem(model2,data = stdatap,
            se = "bootstrap", bootstrap = 1000)

#Examine the model results
summary(fit1,standardized = TRUE)

fitMeasures(fit1,c('chisq','df','pvalue','cfi','rmsea'))


