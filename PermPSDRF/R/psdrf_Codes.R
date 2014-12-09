#' Création des tables de code.
#' @description Creation d'une archive RData a partir du classeur Excel PsdrfListes.xlsx.
#' @return La fonction construit les tables suivantes.
#'
#' CodeDurete, CodeEcologie, CodeEcorce, CodeEssence, CodeTypoArbres, Communes, EssReg,
#' Reserves, Tiers, Tarifs,
#'
#' Elles sont enregistrées dans le dossier Tables sous le nom : psdrfCodes.Rdata.
#' @author Bruciamacchie Max
#' @import tcltk
#' @import openxlsx
#' @export
psdrf_Codes <- function() {
  rep <- getwd()

  file <- tk_choose.files(caption = "Choix fichier codes", filters=matrix(c(".xls",".xlsx"),1,2, byrow = T))
  setwd(dirname(file))
  CodeDurete     <- read.xlsx(file, sheet="CodeDurete")
  CodeEcologie   <- read.xlsx(file, sheet="CodeEcologie")
  CodeEcorce     <- read.xlsx(file, sheet="CodeEcorce")
  CodeEssence    <- read.xlsx(file, sheet="CodeEssence")
  CodeTypoArbres <- read.xlsx(file, sheet="CodeTypoArbres")
  Reserves       <- read.xlsx(file, sheet="Reserves")
  Tarifs         <- read.xlsx(file, sheet="Tarifs")
  Tiers          <- read.xlsx(file, sheet="Tiers")
  Communes       <- read.xlsx(file, sheet="Communes")
  EssReg         <- read.xlsx(file, sheet="EssReg")
  Cat            <- read.xlsx(file, sheet="Cat")
  # --------------- Sauvegarde
  setwd(rep)
  dir.create("Tables", showWarnings = F)
  save(Tiers,CodeEcorce,CodeEcologie,CodeEssence,CodeDurete,Reserves,Tarifs,CodeTypoArbres,Communes,EssReg,Cat,
                file = "Tables/psdrfCodes.Rdata")
}
