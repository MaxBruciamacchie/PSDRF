#' Vérification des données brutes de chaque dispositif.
#' @description La fonction permet de sélectionner un ou plusieurs dispositifs et d'éditer un rapport aux formats html, pdf ou doc.
#'
#' @return Edition d'un rapport par dispositif
#'
#' @param repdata = répertoire contenant les données
#' @param format paramètre permettant de fixer le format du fichier en sortie.
#' @details Le format de sortie sera  \cr
#' du .html si le paramètre vaut 0 (valeur par défaut),  \cr
#' du .pdf si le format est égal à 1,  \cr
#' du .doc si format est égal à 2
#' @author Bruciamacchie Max
#' @import rmarkdown
#' @import tcltk
#' @import openxlsx
#' @import tools
#' @export

psdrf_Verif <- function(repdata, format=0){
  # -------------- Gestion des dossiers
  rep <- getwd()
  dir.create("Out", showWarnings = F)
  dir.create("Out/Verif", showWarnings = F)
  outputdir="../Out/Verif/"
  template = "Template/psdrf_Verif.Rmd"
#   setwd(tclvalue(tkchooseDirectory(title="Choisir le répertoire de stockage des données")))
  setwd(repdata)
  Liste <- list.files()
  ListDisp <- tk_select.list(as.character(Liste), multiple = T, title = "Choisir une ou plusieurs réserves")
  setwd(rep)
  # -------------- Edition des rapports
  # disp = ListDisp[1]  disp2 = ListDisp[2]
  if (format==0) {
    formatout = "html_document"
    extent = "html"
  } else if (format==1) {
    formatout = "pdf_document"
    extent = "pdf"
  }  else if (format==2) {
    formatout = "word_document"
    extent = "docx"
  }
  for (disp in ListDisp){
    tryCatch(render(input = template,
                    output_format = output_format(knitr = knitr_options(),
                                                  pandoc = pandoc_options(to = extent)),
                    output_dir = outputdir,
                    output_file = paste0(file_path_sans_ext(disp),".",extent),
                    encoding="UTF-8"),
#              Rqe : marche pour word et html mais pas pour pdf.
#              Bricolage : revenir à l'écriture précédente lorsque extent=pdf...
#              Ok encoding résoud le problème des accents. Reste la question de pdf...
#             +suppression des warnings
             finally= print(paste("Edition du rapport de vérification du dispositif :", file_path_sans_ext(disp))))
  }
}

