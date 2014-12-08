# --------------- Librairies
library(splines)
library(survival)
library(MASS)
library(PermPSDRF)
library(doBy)
library(stringr)
library(reshape2)
library(tcltk)
library(openxlsx)
library(rmarkdown)
# --------------- Initialisation
# rm(list = ls()) # Efface tous les objets
rep <- tclvalue(tkchooseDirectory(title="Choix du répertoire de travail"))
setwd(rep)
repdata <- tclvalue(tkchooseDirectory(title="Choix du répertoire contenant les données"))
########################## Import ##########################
psdrf_Xls2Rdata(repdata)        # Reconstruction des Donnees brutes
psdrf_Codes()            # Reconstruction des tables de codification
########################## Premières sorties ###############
load("Tables/psdrfDonneesBrutes.Rdata")
load("Tables/psdrfCodes.Rdata")
psdrf_Verif(repdata, 2)           # Vérification classeur(s) avec psdrf_Verif.Rmd
psdrf_ClasseurRem()      # Edition d'un classeur Excel facilitant la remesure
# fiches de remesure : PsdrfFicheRem.Rnw
# plan de localisation des arbres : psdrf_PlanArbres.Rnw()
########################## Traitement ######################
load("Tables/psdrfDonneesBrutes.Rdata")
load("Tables/psdrfCodes.Rdata")
psdrf_Calculs()
# --------------- Aggregation par placettes
load("Tables/psdrfTablesBrutes.Rdata")
psdrf_AgregArbres()
# --------------- creation des tables foret, massif, ...
load("Tables/psdrfTablesElaboreesPlac.RData")
psdrf_AgregPlacettes()
########################## Sorties plus élaborées ######################
# --------------- Shapes par placettes
load("Tables/psdrfTablesElaboreesPlac.RData")
psdrf_ShapesPlac()
load("Tables/PsdrfTablesElaborees.RData")
psdrf_Tables2Xls() # pas forcément nécessaire

