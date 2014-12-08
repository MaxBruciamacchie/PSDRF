#' Préparation du fichier de remesure.
#' @description Création d'un classeur destiné à faciliter la saisie de la prochaine campagne de mesure.
#' Il permettra également d'ajouter les nouvelles données dans les archives de la forêt.
#' Une boîte de dialogue permet de sélectionner la réserve souhaitée.
#'
#' @return La fonction construit un classeur Excel. Il est enregistré dans le dossier out
#' sous le nom : Remesures_NomDisp.xlsx
#'
#' Exemple : Remesures_RB Grands Monts.xlsx.
#' @author Bruciamacchie Max
#' @import tcltk
#' @import openxlsx
#' @export
psdrf_ClasseurRem <- function() {
  Liste <- as.list(Reserves[,2])
  Nom <- tk_select.list(as.character(Liste), multiple = F, title = "Choisir une réserve")
  Choix <- Reserves$NumDisp[which(Reserves$Nom==Nom)]
  # ---------------- Préparation du fichier Arbres -----------
  t <- arbres
  t <- subset(t, NumDisp==Choix, select=-1)
  dernierCycle <- max(t$Cycle)
  t <- subset(t, Cycle==dernierCycle, select=-2)
  t <- t[,-15]
  names(t) <- c("Plac","Num","Ess","Azim","Dist","Diam1","Diam2","Type","Haut","StadeE","StadeD",
              "Taillis","Observation","Codeco")
  t$Cycle <- dernierCycle +1
  t <- subset(t, select=c("Cycle","Plac","Num","Ess","Azim","Dist","Type","Diam1","Diam2","Haut","StadeE","StadeD",
                        "Codeco","Taillis","Observation"))
  a <- t[,7:15]
  names(a) <- paste0(names(a),2)
  t <- cbind(t,a)
  t[,16:24] <- NA
  t <- subset(t, select = c("Cycle","Plac","Num","Ess","Azim","Dist","Type","Type2","Diam1","Diam12",
                            "Diam2","Diam22","Haut","Haut2","StadeE","StadeE2","StadeD","StadeD2",
                            "Codeco","Codeco2","Taillis","Taillis2","Observation","Observation2"))
  ArbresTour <- t
  # ---------------- Préparation du fichier BMSsup30
  t <- BMSsup30
  t <- subset(t, NumDisp==Choix, select=-1)
  dernierCycle <- max(t$Cycle)
  t <- subset(t, Cycle==dernierCycle, select=-1)
  t$Cycle <- dernierCycle +1
  t <- subset(t, select=c("Cycle","NumPlac","code","num","azimut","distance","top_diam","mid_diam","length",
                          "contact","windfall","bark_stage","rot_stage","observation"))
  a <- t[,7:14]
  names(a) <- paste0(names(a),2)
  t <- cbind(t,a)
  t[,15:22] <- NA
  t <- subset(t, select = c("Cycle","NumPlac","code","num","azimut","distance","top_diam","top_diam2",
                            "mid_diam","mid_diam2","length","length2","contact","contact2","windfall","windfall2",
                            "bark_stage","bark_stage2","rot_stage","rot_stage2","observation","observation2"))
  BMSsup30Tour <- t
  # ---------------- Préparation du fichier Transect
  t <- Transect
  t <- subset(t, NumDisp==Choix, select=-1)
  dernierCycle <- max(t$Cycle)
  t <- subset(t, Cycle==dernierCycle, select=-1)
  t$Cycle <- dernierCycle +1
  a <- t[,4:10]
  names(a) <- paste0(names(a),2)
  t <- cbind(t,a)
  t[,12:18] <- NA
  t <- subset(t, select=c("Cycle","NumPlac","num","code","diam","diam2","angle","angle2","contact","contact2",
                          "windfall","windfall2","bark_stage","bark_stage2","rot_stage","rot_stage2",
                          "observation","observation2"))
  TransectTour <- t
    # ---------------- Préparation du fichier Rege
  t <- Rege
  t <- subset(t, NumDisp==Choix, select=-1)
  dernierCycle <- max(t$Cycle)
  t <- subset(t, Cycle==dernierCycle, select=-1)
  t$Cycle <- dernierCycle +1
  a <- t[,4:11]
  names(a) <- paste0(names(a),2)
  t <- cbind(t,a)
  t[,13:20] <- NA
  t <- subset(t, select=c("Cycle","NumPlac","subplot","code","class1","class12","class2","class22","class3","class32",
                          "Total","Total2","seed_cover","seed_cover2","coppice","coppice2","browsing","browsing2",
                          "observation","observation2"))
  RegeTour <- t
  # ------------------- Sauvegarde
  Tabs <- list(ArbresTour,BMSsup30Tour,TransectTour,RegeTour)
  Noms <- c("Arbres","BMSsup30","Transect","Rege")
  wb <- createWorkbook()
  for (i in 1:length(Tabs)) {
    addWorksheet(wb, Noms[i])
    writeData(wb, Noms[i], Tabs[[i]])
  }
  dir.create("Out", showWarnings = F)
  saveWorkbook(wb, paste0("Out/Remesures/",Nom,".xlsx"), overwrite = TRUE)
}
