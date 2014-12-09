#' Aggrégation des placettes à une échelle choisie par l'opérateur.
#' @description Sous-programme permettant de calculer les moyennes, coefficient de variations et erreurs relatives par groupes de placettes
#' tels que choisis dans la feuille Placettes.
#' @return La fonction construit les tables suivantes. Elles sont enregistrées dans le dossier Tables
#'
#' psdrfNGVdisp, psdrfNGVstrate, psdrfNGVcc,
#' psdrfNGVdispEss, psdrfNGVdispEssReg, psdrfNGVdispCat,
#' psdrfHistRB, psdrfHistStrate, psdrfHistRBEssReg,
#' psdrfBMRB, psdrfBMRBstadeD, psdrfBMRBstadeE
#'
#' @author Bruciamacchie Max
#' @export
psdrf_AgregPlacettes <- function() {
  ############################ Fonction calcul de moyenne et d'ecart-type pour un tableau et un regroupement donnés
  psdrf_AgregMoySdEr <- function(df, Regroup="Strate", var.sup=NULL, debut=4, ncol=4) {
    if (Regroup=="RB") {Placettes$RB <- Placettes$NumDisp}
    t1 <- subset(Placettes, select=c(Regroup,"NumDisp","NumPlac","poids"))
    names(t1)[1] <- "Regroup"
    df <- merge(df, t1, by=c("NumDisp","NumPlac"), all.x=T, sort=F)
    #   debut <- ifelse(var.sup=="", debut,debut+1)
    debut <- debut + length(var.sup)
    xnam <- names(df)[debut:(debut+ncol-1)]
    fmla1 <- paste0(xnam,"*poids", collapse= " + ")
    fmla2 <- paste0(xnam,"^2*poids", collapse= " + ")
    fmla <- paste(paste(fmla1,fmla2, sep=" + "), " ~ NumDisp + Regroup + Cycle")
    if (length(var.sup) > 0) {
      for (i in 1:length(var.sup)) {
        fmla <- paste(fmla,var.sup [i],sep=" + ")
      }
    }
    fmla <- as.formula(fmla)

    TabSum <- summaryBy(fmla, data=df, FUN= sum, na.rm=T, keep.names=T)
    Nbre <- summaryBy(poids ~ NumDisp + Regroup, data=t1, FUN=c(length,sum))
    names(Nbre)[3:4] <- c("Nbre", "poids")
    Nbre <- merge(Nbre, Reserves[,1:2], by="NumDisp", all.x=T)
    TabSum <- merge(TabSum, Nbre, by=c("NumDisp", "Regroup"), all.x=T)
    b <- ((TabSum$poids*TabSum[,(debut+ncol):(debut-1+2*ncol)] - TabSum[,debut:(debut-1+ncol)]^2)/TabSum$poids/(TabSum$poids-1))^0.5
    Moy <- TabSum[,debut:(debut-1+ncol)]/TabSum$poids
    CV <- b/Moy* 100
    Er <- qt(0.975, TabSum$Nbre)* CV/TabSum$Nbre^0.5
    TabSum <- cbind(TabSum$Nom,TabSum[,1:(debut-1)],Moy,CV,Er,TabSum$Nbre)
    names(TabSum)[1] <- "Nom"
    names(TabSum)[(debut+1):(debut+1+3*ncol)] <- c( paste0("Moy",xnam),paste0("CV",xnam),paste0("Er",xnam),"Nbre")
    TabSum
  }

  # ---------- Agregation dendrométrie ----------
  Titre <- c("Vha","VhaIFN","Gha","Nha","Vha_cv","VhaIFN_cv","Gha_cv","Nha_cv","Vha_er","VhaIFN_er","Gha_er","Nha_er")
  psdrfNGVdisp <- psdrf_AgregMoySdEr(psdrfPlaNGV,"RB")
  names(psdrfNGVdisp)[5:16] <- Titre
  psdrfNGVstrate <- psdrf_AgregMoySdEr(psdrfPlaNGV,"Strate")
  names(psdrfNGVstrate)[5:16] <- Titre
  psdrfNGVcc <- psdrf_AgregMoySdEr(psdrfPlaNGV,"corine_code")
  names(psdrfNGVcc)[5:16] <- Titre
  psdrfNGVcc <- psdrf_AgregMoySdEr(psdrfPlaNGV,"Groupe1")
  names(psdrfNGVcc)[5:16] <- Titre
  psdrfNGVcc <- psdrf_AgregMoySdEr(psdrfPlaNGV,"Groupe2")
  names(psdrfNGVcc)[5:16] <- Titre
  # --------- Donnees par essences -------------
  psdrfNGVdispEssReg      <- psdrf_AgregMoySdEr(psdrfPlaArbresEssReg, Regroup="RB", var.sup="EssReg")
  psdrfNGVstrateEssReg    <- psdrf_AgregMoySdEr(psdrfPlaArbresEssReg, Regroup="Strate", var.sup="EssReg")
  psdrfNGVdispEssRegPar   <- psdrf_AgregMoySdEr(psdrfPlaArbresEssReg, Regroup="RB", var.sup="EssRegPar")
  psdrfNGVstrateEssRegPar <- psdrf_AgregMoySdEr(psdrfPlaArbresEssReg, Regroup="Strate", var.sup="EssRegPar")
  psdrfNGVdispEss         <- psdrf_AgregMoySdEr(psdrfPlaArbresEss, Regroup="RB", var.sup="code")
  # --------- Donnees par structure -------------
  psdrfNGVdispCat       <- psdrf_AgregMoySdEr(psdrfPlaArbresCat, Regroup="RB", var.sup="Cat")
  psdrfNGVdispCatEssReg <- psdrf_AgregMoySdEr(psdrfPlaArbresCatEssReg, Regroup="RB", var.sup=c("Cat","EssReg"))
  psdrfNGVdispCatEssRegPar <- psdrf_AgregMoySdEr(psdrfPlaArbresCatEssRegPar, Regroup="RB", var.sup=c("Cat","EssRegPar"))
  # --------- Histogrammes -------
  psdrfHistDisp       <- psdrf_AgregMoySdEr(psdrfPlaArbresClasse, Regroup="RB", var.sup="Classe")
  psdrfHistStrate   <- psdrf_AgregMoySdEr(psdrfPlaArbresClasse, Regroup="Strate", var.sup="Classe")
  psdrfHistDispEssReg <- psdrf_AgregMoySdEr(psdrfPlaArbresClasseEssReg, Regroup="RB", var.sup=c("Classe","EssReg"))
  # ---------- Agregation Bois mort ----------
  psdrfBMdisp <- psdrf_AgregMoySdEr(psdrfPlaBM , Regroup="RB", ncol=5)
  psdrfBMdispStadeD <- psdrf_AgregMoySdEr(psdrfPlaBMTStadeD , Regroup="RB", ncol=1, var.sup=c("Type","StadeD"))
  psdrfBMdispStadeE <- psdrf_AgregMoySdEr(psdrfPlaBMTStadeE , Regroup="RB", ncol=1, var.sup=c("Type","StadeE"))
  psdrfBMPdispClasse <- psdrf_AgregMoySdEr(psdrfPlaBMPclasse, Regroup="RB", var.sup="Classe")
  psdrfBMSdispClasse <- psdrf_AgregMoySdEr(psdrfPlaBMSclasse, Regroup="RB", var.sup="Classe")
  psdrfBMPdispTypo   <- psdrf_AgregMoySdEr(psdrfPlaBMPtypo, Regroup="RB", ncol=1, var.sup=c("libelle","Classe"))
  # ---------- Regeneration ----------
  psdrfRegeDisp <- psdrf_AgregMoySdEr(psdrfPlaRege, Regroup="RB", ncol=4)
  psdrfRegeDispEssReg <- psdrf_AgregMoySdEr(psdrfPlaRegeEssReg, Regroup="RB", ncol=4, var.sup="EssReg")
  psdrfRegeDispEssRegPar <- psdrf_AgregMoySdEr(psdrfPlaRegeEssRegPar, Regroup="RB", ncol=4, var.sup="EssRegPar")
  # --------- Sauvegarde -------
  save(psdrfNGVdisp,psdrfNGVstrate,psdrfNGVcc,
       psdrfNGVdispEss,psdrfNGVdispEssReg,psdrfNGVdispEssRegPar,
       psdrfNGVstrateEssReg,psdrfNGVstrateEssRegPar,
       psdrfNGVdispCat,psdrfNGVdispCatEssReg,psdrfNGVdispCatEssRegPar,
       psdrfHistDisp,psdrfHistStrate,psdrfHistDispEssReg,
       psdrfBMdisp,psdrfBMdispStadeD,psdrfBMdispStadeE,
       psdrfBMPdispClasse,psdrfBMSdispClasse,psdrfBMPdispTypo,
       psdrfRegeDisp,psdrfRegeDispEssReg,psdrfRegeDispEssRegPar,
       file="Tables/PsdrfTablesElaborees.RData")
}
