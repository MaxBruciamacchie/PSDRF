#' Conversion des données brutes de la base PSDRF en 2 archives RData.
#' @description Import des classeurs Excel de chaque réserve et sauvegarde dans l'archive
#' psdrfDonneesBrutes.Rdata.
#'
#' Import des différentes tables de codification et sauvegarde dans l'archive psdrfCodes.Rdata.
#' @return La fonction exporte dans le dossier Tables l'archive suivante :
#'
#' psdrfDonneesBrutes.Rdata
#'
#' @param repdata = répertoire contenant les données
#' @author Bruciamacchie Max
#' @import tcltk
#' @import openxlsx
#' @export
psdrf_Xls2Rdata <- function(repdata) {
  rep <- getwd()
  setwd(repdata)
  ListeFich <- list.files()
  temp <- grep("~$", ListeFich)
  if (length(temp) > 0) ListeFich <- ListeFich[-grep("~$", ListeFich)]
  ListeFich <- ListeFich[grep(".xls", ListeFich)]
  # ---------------- Initialisation
  Placettes <- data.frame()
  arbres    <- data.frame()
  Rege      <- data.frame()
  Transect  <- data.frame()
  BMSsup30  <- data.frame()
  Reperes   <- data.frame()
  Cycles    <- data.frame()
  # ---------------- Import
  for (i in 1:length(ListeFich)) {
    file <- ListeFich[[i]]
    TempPlacettes         <- read.xlsx(file, sheet="Placettes")
    TempArbres            <- read.xlsx(file, sheet="Arbres")
    TempRege              <- read.xlsx(file, sheet="Rege")
    TempTransect          <- read.xlsx(file, sheet="Transect")
    TempBMSsup30          <- read.xlsx(file, sheet="BMSsup30")
    TempReperes           <- read.xlsx(file, sheet="Reperes")
    TempCycles            <- read.xlsx(file, sheet="Cycles")
    # -------
    Placettes <- rbind(Placettes,TempPlacettes)
    arbres    <- rbind(arbres,TempArbres)
    Rege      <- rbind(Rege,TempRege)
    Transect  <- rbind(Transect,TempTransect)
    BMSsup30  <- rbind(BMSsup30,TempBMSsup30)
    Reperes   <- rbind(Reperes,TempReperes)
    Cycles    <- rbind(Cycles,TempCycles)
    print(ListeFich[[i]])
  }
  # --------------- Sauvegarde
  setwd(rep)
  dir.create("Tables", showWarnings = F)
  save(Placettes,arbres,Rege,Transect,BMSsup30,Reperes,Cycles,
                file = "Tables/psdrfDonneesBrutes.Rdata")
}
