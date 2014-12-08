#' Edition du carnet de chaque dispositif.
#' @description La fonction permet de sélectionner un ou plusieurs dispositifs et d'éditer un rapport aux formats html, pdf ou doc.
#'
#' @return Edition d'un rapport par dispositif
#'
#' @param format paramètre permettant de fixer le format du fichier en sortie.
#' @details Le format de sortie sera  \cr
#' du .html si le paramètre vaut 0 (valeur par défaut),  \cr
#' du .pdf si le format est égal à 1,  \cr
#' du .doc si format est égal à 2
#' @author Bruciamacchie Max
#' @import rmarkdown
#' @import tcltk
#' @import openxlsx
#' @export

psdrf_EditCarnet <- function(format=0){
  # -------------- Gestion des dossiers
  rep <- getwd()
  dir.create("Out", showWarnings = F)
  dir.create("Out/Carnets", showWarnings = F)
  outputdir="../Out/Carnets/"
  template = "Template/psdrf_Carnet.Rmd"
  setwd(tclvalue(tkchooseDirectory(title="Choisir le répertoire de stockage des données")))
  Liste <- list.files()
  ListDisp <- tk_select.list(as.character(Liste), multiple = T, title = "Choisir une ou plusieurs réserves")
  setwd(rep)
  # -------------- Chargement des données
  load("Tables/psdrfCodes.Rdata")
  load("Tables/psdrfDonneesBrutes.Rdata")
  load("Tables/psdrfTablesBrutes.RData")
  load("Tables/psdrfTablesElaborees.RData")
  load("Tables/psdrfTablesElaboreesPlac.RData")
  data(ser)
  placettes <- readOGR(dsn="SIG/Vecteurs/", layer="PsdrfPlacettes")
  limite <- readOGR(dsn="SIG/Vecteurs/", layer="Perimetre")

  # -------------- Edition des rapports
  # disp = ListDisp[1]
  if (format==0) {
    formatout = "html_document"
    extent = ".html"
  } else if (format==1) {
    formatout = "pdf_document"
    extent = ".pdf"
  }  else if (format==2) {
    formatout = "word_document"
    extent = ".doc"
  }
  for (disp in ListDisp){
    tryCatch(render(input = template,
                    output_format = formatout,
                    output_dir = outputdir,
                    output_file = paste0(gsub('([[:punct:]])|\\s+','_',disp),extent)),
             finally= print(paste("Edition du carnet du dispositif :", disp)))
  }
}



