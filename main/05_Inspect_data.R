## Author: Bastiaen Boekelo
## Date: June 2018

## Goal:
## We hebben nu per perceel verschillende statistieken. 
## Per perceel zijn er voor verschillende variabelen het aantal mutaties geteld.  
## Deze mutaties zijn voortgekomen uit afwijkingen t.o.v. het jaargemiddelde. Wijkt het teveel af, dan telde de observatie als een mutatie.
## We hebben nu 4 verschillende thresholds (1.5, 2, 2.5, 3, 3.5) en voor elke combinatie is er per observatie (per band) bepaald of de waarde 'verdacht' is
## Uitgangspunt van dit script is om te onderzoeken welke combinatie (band, threshold en variabelenset) het beste resultaat geeft.
## Daarvoor is de functie GET_STATS geschreven die elke combinatie langsgaat.

rm(list=ls())
library(raster)
library(maptools)
library(rgdal)
library(plyr)

ODIR_stats <- "C:/Data/SBIR/data/Statistics/all_sats/05_variables/01_variable_sets/"
ODIR_stats_all <- "C:/Data/SBIR/data/Statistics/all_sats/05_variables/02_all/"

##############################
#### Lees polygondata in #####
##############################

p_raw <- readOGR(dsn = 'C:/Data/SBIR/data/Statistics/all_sats/04_yearstats_shape', layer = 'Statistieken_percelen')
pdf <- as.data.frame(p_raw)
p <- na.omit(pdf)
rm(pdf,p_raw)


nr_thresholds <- 4 ## Hoeveel thresholds zijn er? (AGG)


###############################################
###### Reading and transforming cov data ######
###############################################


cov <- read.csv("C:/Data/SBIR/data/Statistics/all_sats/03_yearstats/03_mean_cov_difmax/COV_and_difmax.csv")


####################################################################################################
####################################################################################################
################################ Per Variabele per band ############################################
####################################################################################################
####################################################################################################

#jaren <- c("2015", "2015_2016", "2016",  "2016_2017", "2017", "2017_2018")
jaren <- c("2015", "2015_2016", "2016",  "2016_2017")
cov_thresholds <- c(0.01, 0.015, 0.02, 0.025, 0.03, 0.035, 0.04, 0.05)

COV_THRESHOLD <- 0.03
run <- 0
nr_sel_var <- 7
MOMENT <- 2
BAND <- 3
AGG <- 2
i <- 1

GET_STATS <- function(p, nr_sel_var, jaren){
  run <- 0
  ## variable combinations
  nr_var <- nr_sel_var
  combi <- combn(1:7, nr_var)
  
  for(COV_THRESHOLD in cov_thresholds){
    for(MOMENT in 1:(length(jaren)/2)){
      for(AGG in 1:nr_thresholds){
        for(BAND in 1:3){
          
          ## First subset covariance
          cov_mean <- cov$mean_cov[cov$band == BAND & cov$jaar == jaren[MOMENT*2-1]]
          perceel <- cov$perceel[cov$band == BAND & cov$jaar == jaren[MOMENT*2-1]]
          cov_sel <- as.data.frame(cbind(cov_mean, perceel))
          
          ## Then make the right subsets of the already calculated data
          p1m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable == "mut_high",][,AGG]
          p1o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable == "mut_high",][,AGG]
          p2m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable  == "mut_low",][,AGG]
          p2o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable  == "mut_low",][,AGG]
          p3m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmax",][,AGG]
          p3o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmax",][,AGG]
          p4m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmin",][,AGG]
          p4o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmin",][,AGG]
          p5m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "max90",][,AGG]
          p5o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "max90",][,AGG]
          p6m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",][,AGG]
          p6o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",][,AGG]
          
          ppo <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$OBJECTID
          ppm <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",]$OBJECTID
          pco <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$categorie
          pcm <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",]$categorie
          
          varnames <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10", "perceel",  "categorie")
          
          dm <- as.data.frame(cbind(p1m,p2m,p3m,p4m,p5m,p6m))
          dm <- cbind(dm, ppm, pcm)
          names(dm) <- varnames
          dm <- merge(dm, cov_sel, by = "perceel", by.y = "perceel")
          
          do <- as.data.frame(cbind(p1o,p2o,p3o,p4o,p5o,p6o))
          do <- cbind(do, ppo, pco)
          names(do) <- varnames
          do <- merge(do, cov_sel, by = "perceel", by.y = "perceel")
          
          ## Make a threshold for the cov
          do$cov_mean[do$cov_mean <= COV_THRESHOLD] <- 0
          dm$cov_mean[dm$cov_mean <= COV_THRESHOLD] <- 0
          
          for(i in 1:ncol(combi)){
            run <- run + 1
            
            ################## for unmutated part ####################################
            
            i1 <- do$mut_high == 0
            i2 <- do$mut_low == 0
            i3 <- do$difmax == 0
            i4 <- do$difmin == 0
            i5 <- do$max90 == 0
            i6 <- do$min10 == 0
            i7 <- do$cov_mean == 0
              
            ## Zet de variabelen achter elkaar i.p.v. onder elkaar
            muts <- cbind(i1,i2,i3,i4,i5,i6,i7) 
            
            ## Bepaal welke set van variabelen de index bepalen en wat deze index is.
            if(nr_var == 1){
              index <-  !(muts[,combi[1,i]])
            } else if(nr_var == 2) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]])
            } else if(nr_var == 3) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]])
            } else if(nr_var == 4) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]])
            } else if(nr_var == 5) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]])
            } else if(nr_var == 6) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]] * muts[,combi[6,i]])
            } else if(nr_var == 7) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]] * muts[,combi[6,i]] * muts[,combi[7,i]])
            } else { nr_var = NULL}
            
            ## Als er ergens een perceel eenmaal een outlier heeft gehad, markeer het als gemuteerd (=1)
            do$mutatie <- 0
            do$mutatie[index] <- 1 
            
            ################## for mutated part ####################################
            
            i1 <- do$mut_high == 0
            i2 <- do$mut_low == 0
            i3 <- do$difmax == 0
            i4 <- do$difmin == 0
            i5 <- do$max90 == 0
            i6 <- do$min10 == 0
            i7 <- do$cov_mean == 0
            
            muts <- cbind(i1,i2,i3,i4,i5,i6,i7)
            
            if(nr_var == 1){
              index <-  !(muts[,combi[1,i]])
            } else if(nr_var == 2) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]])
            } else if(nr_var == 3) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]])
            } else if(nr_var == 4) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]])
            } else if(nr_var == 5) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]])
            } else if(nr_var == 6) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]] * muts[,combi[6,i]])
            } else if(nr_var == 7) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]] * muts[,combi[6,i]] * muts[,combi[7,i]])
            } else { nr_var = NULL}
            
            
            dm$mutatie <- 0
            dm$mutatie[index] <- 1 
            
            
            ## Make the stats
            p_not_mutated <- do
            p_mutated <- dm
            
            percelen_TP <- as.numeric(as.character(p_mutated[p_mutated$mutatie == 1,]$perceel))
            percelen_FP <- as.numeric(as.character(p_not_mutated[p_not_mutated$mutatie == 1,]$perceel))
            percelen_FN <- as.numeric(as.character(p_mutated[p_mutated$mutatie == 0,]$perceel))
            percelen_TN <- as.numeric(as.character(p_not_mutated[p_not_mutated$mutatie == 0,]$perceel))
            
            TP <- length(percelen_TP)         ## TP
            FP <- length(percelen_FP)         ## FP
            FN <- length(percelen_FN)         ## FN (0 !!!!)
            TN <- length(percelen_TN)         ## TN (Mits FN=0 --> zo hoog mogelijk!)
            
            ## Onderstaand maakt een lijst van alle percelen en hun classificatie. Waarschijnlijk is allen FN relevant voor nadere inspectie
            #test <- paste("TP: ", paste(percelen_TP, collapse = ';'),
            #              ", FP: ", paste(percelen_TP, collapse = ';'),
            #              ", FN: ", paste(percelen_TP, collapse = ';'),
            #              ", TN: ", paste(percelen_TP, collapse = ';'), sep= "")
            
            FN_var <- paste("FN: ", paste(percelen_FN, collapse = ';'))
            
            
            #newline <- paste("AGG", AGG/2+0.5, "jaar",  jaren[MOMENT*2-1], "var", paste((combi[,i]), collapse = '_'), "band", BAND, "stats", TP, FP, FN, TN, sep=", ")
            newline <- c(AGG/2+0.5,jaren[MOMENT*2-1], paste((combi[,i]), collapse= '_'), paste(varnames, collapse= ' '), BAND, TP, FP, FN, TN, FN_var)
            print(newline)
            if(run == 1){
              allstats <- data.frame( threshold=numeric(), jaar=integer(), variabelen=character(), allvar=character(), band=character(), TP=integer(),FP=integer(),FN=integer(),TN=integer(), FN_percelen=character(), stringsAsFactors=FALSE)
              
              allstats[run,] <- newline
            } else {
              allstats <- rbind(allstats, newline)
              assign('allstats',allstats,envir=.GlobalEnv)
            }
          }
        }
      }
    }
  }
}



GET_STATS(p, 1, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_", 1, ".csv", sep=""), row.names=F)
GET_STATS(p, 2, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_", 2, ".csv", sep=""), row.names=F)
GET_STATS(p, 3, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_", 3, ".csv", sep=""), row.names=F)
GET_STATS(p, 4, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_", 4, ".csv", sep=""), row.names=F)
GET_STATS(p, 5, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_", 5, ".csv", sep=""), row.names=F)
GET_STATS(p, 6, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_", 6, ".csv", sep=""), row.names=F)
GET_STATS(p, 7, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_", 7, ".csv", sep=""), row.names=F)


####################################################################################################
####################################################################################################
################################      Per Variabele     ############################################
####################################################################################################
####################################################################################################

MOMENT <- 1
BAND <- 3
AGG <- 2
run <- 0
nr_var <- 3

## selectie criteria
#jaren <- c("2015", "2015_2016", "2016",  "2016_2017", "2017", "2017_2018")
jaren <- c("2015", "2015_2016", "2016",  "2016_2017")

GET_STATS_NOBAND <- function(p, nr_sel_var, jaren){
  run <- 0
  
  ## variable combinations
  nr_var <- nr_sel_var
  combi <- combn(1:7, nr_var)
  
  for(COV_THRESHOLD in cov_thresholds){
    for(MOMENT in 1:(length(jaren)/2)){
      for(AGG in 1:nr_thresholds){
        
        ## First subset covariance
        cov_mean1 <- cov$mean_cov[cov$band == 1 & cov$jaar == jaren[MOMENT*2-1]]
        cov_mean2 <- cov$mean_cov[cov$band == 2 & cov$jaar == jaren[MOMENT*2-1]]
        cov_mean3 <- cov$mean_cov[cov$band == 3 & cov$jaar == jaren[MOMENT*2-1]]
        perceel <- cov$perceel[cov$band == 1 & cov$jaar == jaren[MOMENT*2-1]]
        cov_sel <- as.data.frame(cbind(cov_mean1,cov_mean2, cov_mean3, perceel))
        cov_sel$cov_mean <- pmax(cov_sel$cov_mean1, cov_sel$cov_mean2, cov_sel$cov_mean3)
        cov_sel <- cov_sel[c("cov_mean", "perceel")]
        
        p1m1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable == "mut_high",][,AGG]
        p1o1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable == "mut_high",][,AGG]
        p2m1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable  == "mut_low",][,AGG]
        p2o1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable  == "mut_low",][,AGG]
        p3m1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmax",][,AGG]
        p3o1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmax",][,AGG]
        p4m1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmin",][,AGG]
        p4o1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmin",][,AGG]
        p5m1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "max90",][,AGG]
        p5o1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "max90",][,AGG]
        p6m1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",][,AGG]
        p6o1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",][,AGG]
        
        p1m2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable == "mut_high",][,AGG]
        p1o2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable == "mut_high",][,AGG]
        p2m2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable  == "mut_low",][,AGG]
        p2o2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable  == "mut_low",][,AGG]
        p3m2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmax",][,AGG]
        p3o2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmax",][,AGG]
        p4m2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmin",][,AGG]
        p4o2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmin",][,AGG]
        p5m2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "max90",][,AGG]
        p5o2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "max90",][,AGG]
        p6m2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",][,AGG]
        p6o2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",][,AGG]
        
        p1m3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable == "mut_high",][,AGG]
        p1o3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable == "mut_high",][,AGG]
        p2m3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable  == "mut_low",][,AGG]
        p2o3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable  == "mut_low",][,AGG]
        p3m3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmax",][,AGG]
        p3o3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmax",][,AGG]
        p4m3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmin",][,AGG]
        p4o3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmin",][,AGG]
        p5m3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "max90",][,AGG]
        p5o3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "max90",][,AGG]
        p6m3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",][,AGG]
        p6o3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",][,AGG]
        
        
        if(length(p1m1) == length(p1m2) & length(p1m1) == length(p1m3)){
          p1m <- p1m1 + p1m2 + p1m3
          p1o <- p1o1 + p1o2 + p1o3
          p2m <- p2m1 + p2m2 + p2m3
          p2o <- p2o1 + p2o2 + p2o3
          p3m <- p3m1 + p3m2 + p3m3
          p3o <- p3o1 + p3o2 + p3o3
          p4m <- p4m1 + p4m2 + p4m3
          p4o <- p4o1 + p4o2 + p4o3
          p5m <- p5m1 + p5m2 + p5m3
          p5o <- p5o1 + p5o2 + p5o3
          p6m <- p6m1 + p6m2 + p6m3
          p6o <- p6o1 + p6o2 + p6o3
          
          ppo <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$OBJECTID
          ppm <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",]$OBJECTID
          pco <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$categorie
          pcm <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",]$categorie
          
          
          varnames <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10", "perceel",  "categorie")
          
          dm <- as.data.frame(cbind(p1m,p2m,p3m,p4m,p5m,p6m))
          dm <- cbind(dm, ppm, pcm)
          names(dm) <- varnames
          dm <- merge(dm, cov_sel, by = "perceel", by.y = "perceel")
          
          do <- as.data.frame(cbind(p1o,p2o,p3o,p4o,p5o,p6o))
          do <- cbind(do, ppo, pco)
          names(do) <- varnames
          do <- merge(do, cov_sel, by = "perceel", by.y = "perceel")
          
          ## Make a threshold for the cov
          do$cov_mean[do$cov_mean <= COV_THRESHOLD] <- 0
          dm$cov_mean[dm$cov_mean <= COV_THRESHOLD] <- 0
          
          for(i in 1:ncol(combi)){
            run <- run + 1
            
            ################## for unmutated part ####################################
            
            i1 <- do$mut_high == 0
            i2 <- do$mut_low == 0
            i3 <- do$difmax == 0
            i4 <- do$difmin == 0
            i5 <- do$max90 == 0
            i6 <- do$min10 == 0
            i7 <- do$cov_mean == 0
            
            ## Zet de variabelen achter elkaar i.p.v. onder elkaar
            muts <- cbind(i1,i2,i3,i4,i5,i6,i7) 
            
            ## Bepaal welke set van variabelen de index bepalen en wat deze index is.
            if(nr_var == 1){
              index <-  !(muts[,combi[1,i]])
            } else if(nr_var == 2) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]])
            } else if(nr_var == 3) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]])
            } else if(nr_var == 4) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]])
            } else if(nr_var == 5) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]])
            } else if(nr_var == 6) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]] * muts[,combi[6,i]])
            } else if(nr_var == 7) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]] * muts[,combi[6,i]] * muts[,combi[7,i]])
            } else { nr_var = NULL}
            
            ## Als er ergens een perceel eenmaal een outlier heeft gehad, markeer het als gemuteerd (=1)
            do$mutatie <- 0
            do$mutatie[index] <- 1 
            
            ################## for mutated part ####################################
            
            i1 <- do$mut_high == 0
            i2 <- do$mut_low == 0
            i3 <- do$difmax == 0
            i4 <- do$difmin == 0
            i5 <- do$max90 == 0
            i6 <- do$min10 == 0
            i7 <- do$cov_mean == 0
            
            muts <- cbind(i1,i2,i3,i4,i5,i6,i7)
            
            if(nr_var == 1){
              index <-  !(muts[,combi[1,i]])
            } else if(nr_var == 2) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]])
            } else if(nr_var == 3) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]])
            } else if(nr_var == 4) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]])
            } else if(nr_var == 5) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]])
            } else if(nr_var == 6) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]] * muts[,combi[6,i]])
            } else if(nr_var == 7) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]] * muts[,combi[6,i]] * muts[,combi[7,i]])
            } else { nr_var = NULL}
            
            
            dm$mutatie <- 0
            dm$mutatie[index] <- 1 
            
            
            ## Make the stats
            p_not_mutated <- do
            p_mutated <- dm
            
            percelen_TP <- as.numeric(as.character(p_mutated[p_mutated$mutatie == 1,]$perceel))
            percelen_FP <- as.numeric(as.character(p_not_mutated[p_not_mutated$mutatie == 1,]$perceel))
            percelen_FN <- as.numeric(as.character(p_mutated[p_mutated$mutatie == 0,]$perceel))
            percelen_TN <- as.numeric(as.character(p_not_mutated[p_not_mutated$mutatie == 0,]$perceel))
            
            TP <- length(percelen_TP)         ## TP
            FP <- length(percelen_FP)         ## FP
            FN <- length(percelen_FN)         ## FN (0 !!!!)
            TN <- length(percelen_TN)         ## TN (Mits FN=0 --> zo hoog mogelijk!)
            
            ## Onderstaand maakt een lijst van alle percelen en hun classificatie. Waarschijnlijk is allen FN relevant voor nadere inspectie
            #test <- paste("TP: ", paste(percelen_TP, collapse = ';'),
            #              ", FP: ", paste(percelen_TP, collapse = ';'),
            #              ", FN: ", paste(percelen_TP, collapse = ';'),
            #              ", TN: ", paste(percelen_TP, collapse = ';'), sep= "")
            
            FN_var <- paste("FN: ", paste(percelen_FN, collapse = ';'))
            
            
            #newline <- paste("AGG", AGG/2+0.5, "jaar",  jaren[MOMENT*2-1], "var", paste((combi[,i]), collapse = '_'), "band", BAND, "stats", TP, FP, FN, TN, sep=", ")
            newline <- c(AGG/2+0.5,jaren[MOMENT*2-1], paste((combi[,i]), collapse= '_'), paste(varnames, collapse= ' '), BAND, TP, FP, FN, TN, FN_var)
            print(newline)
            if(run == 1){
              allstats <- data.frame( threshold=numeric(), jaar=integer(), variabelen=character(), allvar=character(), band=character(), TP=integer(),FP=integer(),FN=integer(),TN=integer(), FN_percelen=character(), stringsAsFactors=FALSE)
              
              allstats[run,] <- newline
            } else {
              allstats <- rbind(allstats, newline)
              assign('allstats',allstats,envir=.GlobalEnv)
            }
          }
        }
      }
    }
  }
}

GET_STATS_NOBAND(p, 1, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_bandsamen", 1, ".csv", sep=""), row.names=F)
GET_STATS_NOBAND(p, 2, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_bandsamen", 2, ".csv", sep=""), row.names=F)
GET_STATS_NOBAND(p, 3, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_bandsamen", 3, ".csv", sep=""), row.names=F)
GET_STATS_NOBAND(p, 4, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_bandsamen", 4, ".csv", sep=""), row.names=F)
GET_STATS_NOBAND(p, 5, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_bandsamen", 5, ".csv", sep=""), row.names=F)
GET_STATS_NOBAND(p, 6, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_bandsamen", 6, ".csv", sep=""), row.names=F)
GET_STATS_NOBAND(p, 7, jaren)
write.csv(allstats, paste(ODIR_stats, "varstats_bandsamen", 7, ".csv", sep=""), row.names=F)


# Inspect data

files <- list.files(path = "C:/Data/SBIR/data/Statistics/all_sats/05_variables/01_variable_sets",pattern = "varstats", recursive=T, full.names=T)
i <- 0
for(FILE in files){
  i <- i + 1
  newdata <- read.csv(FILE, header=T)
  if(i > 1){
    data <- rbind(data, newdata)
  } else {
    data <- newdata
  }
}
write.csv(data,"C:/Data/SBIR/data/Statistics/all_sats/05_variables/02_all/all_var_combinations.csv", row.names=F)

#####################################################
####### Selection from data that scores well ########
#####################################################

data$ratio <- data$FN/data$TN
data <- data[data$ratio < 0.03 & data$TN > 30 & data$FN < 5,]

write.csv(data,"C:/Data/SBIR/data/Statistics/all_sats/05_variables/03_best_variable_combinations/stats_var_selection.csv", row.names=F)













