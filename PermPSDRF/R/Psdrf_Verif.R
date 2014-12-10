#' Vérification des données brutes de chaque dispositif.
#' @description La fonction permet de sélectionner un ou plusieurs dispositifs et d'éditer un rapport aux formats html, pdf ou doc.
#'
#' @return Edition d'un rapport par dispositif.
#'
#' @param repdata = répertoire contenant les données
#' @param modif = argument permettant de modifier le format du rapport. Une boîte de dialogue permet de choisir
#' le format de sortie parmi les 3 suivants : .html, .docx, .pdf.
#' @author Bruciamacchie Max
#' @import rmarkdown
#' @import tcltk
#' @import openxlsx
#' @import tools
#' @export

psdrf_Verif <- function(repdata, modif=F){
  # -------------- Gestion des dossiers
  rep <- getwd()
  dir.create("Out", showWarnings = F)
  dir.create("Out/Verif", showWarnings = F)
  outputdir="../Out/Verif/"
  template = "Template/psdrf_Verif.Rmd"
  setwd(repdata)
  Liste <- list.files()
  ListDisp <- tk_select.list(as.character(Liste), multiple = T, title = "Choisir une ou plusieurs réserves")
  setwd(rep)
  # -------------- Edition des rapports
  if (modif) {
    ListeFormat <- c("html","docx","pdf")
    format <- tk_select.list(as.character(ListeFormat), multiple=F, title = "Choix du format de fichier")
    extent = format
  } else{
    extent = "docx"
  }
  for (disp in ListDisp){
    if (extent != "pdf") {
      tryCatch(render(input = template,
                      output_format = output_format(knitr = knitr_options(),
                                                    pandoc = pandoc_options(to = extent)),
                      output_dir = outputdir,
                      output_file = paste0(file_path_sans_ext(disp),".",extent),
                      encoding="UTF-8"),
               finally= print(paste("Edition du rapport de vérification du dispositif :", file_path_sans_ext(disp))))
    } else {
      tryCatch(render(input = template,
                      output_format = "pdf_document",
                      output_dir = outputdir,
                      output_file = paste0(file_path_sans_ext(disp),".",extent),
                      encoding="UTF-8"),
               finally= print(paste("Edition du rapport de vérification du dispositif :", file_path_sans_ext(disp))))

    }
  }
}

