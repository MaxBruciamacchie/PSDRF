#' Export des tables elaborees au format xlsx.
#' @description Sauvegarde des tables élaborées dans un classeur Excel au format xlsx, dans un dossier Out.
#' @return La fonction exporte dans un classeur au format .xlsx les tables suivantes :
#'
#' psdrfDendroCC, psdrfDendroReserve, psdrfDendroReserveCat, psdrfDendroReserveEss,
#' psdrfDendroReserveEssReg, psdrfDendroStrate
#'
#' Le classeur est enregistré dans le dossier Résultats/Out sous le nom : TabPlacettes.xlsx.
#' @author Bruciamacchie Max
#' @export
psdrf_Tables2Xls <- function() {
  dir.create("out", showWarnings = F)
  dir.create("Out/Resultats", showWarnings = F)
  Tabs <- list(psdrfNGVdisp,psdrfNGVstrate,psdrfNGVcc,
       psdrfNGVdispEss,psdrfNGVdispEssReg,psdrfNGVdispEssRegPar,
       psdrfNGVstrateEssReg,psdrfNGVstrateEssRegPar,
       psdrfNGVdispCat,psdrfNGVdispCatEssReg,psdrfNGVdispCatEssRegPar,
       psdrfHistDisp,psdrfHistStrate,psdrfHistDispEssReg,
       psdrfBMdisp,psdrfBMdispStadeD,psdrfBMdispStadeE,
       psdrfBMPdispClasse,psdrfBMSdispClasse,psdrfBMPdispTypo,
       psdrfRegeDisp,psdrfRegeDispEssReg,psdrfRegeDispEssRegPar)

  Noms <- c("psdrfNGVdisp","psdrfNGVstrate","psdrfNGVcc",
       "psdrfNGVdispEss","psdrfNGVdispEssReg","psdrfNGVdispEssRegPar",
       "psdrfNGVstrateEssReg","psdrfNGVstrateEssRegPar",
       "psdrfNGVdispCat","psdrfNGVdispCatEssReg","psdrfNGVdispCatEssRegPar",
       "psdrfHistDisp","psdrfHistStrate","psdrfHistDispEssReg",
       "psdrfBMdisp","psdrfBMdispStadeD","psdrfBMdispStadeE",
       "psdrfBMPdispClasse","psdrfBMSdispClasse","psdrfBMPdispTypo",
       "psdrfRegeDisp","psdrfRegeDispEssReg","psdrfRegeDispEssRegPar")
  wb <- createWorkbook()
  for (i in 1:length(Tabs)) {
    addWorksheet(wb, Noms[i])
    writeData(wb, Noms[i], Tabs[[i]])
  }
  saveWorkbook(wb, "Out/Resultats/TabPlacettes.xlsx", overwrite = TRUE)
}

