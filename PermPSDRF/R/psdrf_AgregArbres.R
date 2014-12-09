#' Aggrégation des arbres à l'échelle de la placette.
#' @description Sous-programme permettant d'aggréger toutes les variables à l'échelle de la placette.
#' Il fournit de nombreux tableaux croisés.
#' @return La fonction construit les tables suivantes. Elles sont enregistrées dans le dossier Tables
#'
#' psdrfPlaBMSlineV (volume de bois mort au sol par placettes, classe de diamètre, stadeD et stadeE),
#' psdrfPlaBMSlineVEss, psdrfPlaBMSlineVClasse, psdrfPlaBMSlineVStadeD,
#' psdrfPlaBMSlineVStadeE, psdrfPlaBMSsup30V, psdrfPlaBMSsup30VEss, psdrfPlaBMSsup30VClasse,
#' psdrfPlaBMSsup30VStadeD, psdrfPlaBMSsup30VStadeE, psdrfPlaBoisMT,
#' psdrfPlaBMT, psdrfPlaBMTStadeD, psdrfPlaBMTStadeE, psdrfPla,
#' psdrfPlaArbresV, psdrfPlaArbresVEss, psdrfPlaArbresVEssReg,
#' psdrfPlaArbresVClasse, psdrfPlaArbresVCat, psdrfPlaArbresVClasseEssReg,
#' psdrfPlaBMSinf, psdrfPlaBMSsup, psdrfPlaBMPinf, psdrfPlaBMPsup,psdrfPlaBM,
#' psdrfPlaRege,
#'
#' @author Bruciamacchie Max
#' @import doBy
#' @export
psdrf_AgregArbres <- function() {
  # -------------- Bois mort au sol de petite dimension
  psdrfPlaBMSinfClasseStades <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Classe + StadeD + StadeE,
                                data=BMSline, FUN=sum, keep.names=T)
  psdrfPlaBMSinf <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle,
                              data=psdrfPlaBMSinfClasseStades, FUN=sum, keep.names=T)
  psdrfPlaBMSinfClasse <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Classe,
                                      data=psdrfPlaBMSinfClasseStades, FUN=sum, keep.names=T, na.rm=T)
  psdrfPlaBMSinfStadeD <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + StadeD,
                                      data=psdrfPlaBMSinfClasseStades, FUN=sum, keep.names=T, na.rm=T)
  psdrfPlaBMSinfStadeE <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + StadeE,
                                      data=psdrfPlaBMSinfClasseStades, FUN=sum, keep.names=T, na.rm=T)
  psdrfPlaBMSinfEss  <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + code, data=BMSline, FUN=sum, keep.names=T)
  psdrfPlaBMSinfEss  <- merge(psdrfPlaBMSinfEss, CodeEssence[,2:3], by="code", all.x=T, sort=F)
  print("Bois mort au sol inf 30")
  # -------------- Bois mort au sol sup a 30 cm
  psdrfPlaBMSsupClasseStades  <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Classe + StadeD + StadeE,
                                     data=BMSsup30, FUN=sum, keep.names=T)
  psdrfPlaBMSsup     <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle,
                                  data=psdrfPlaBMSsupClasseStades, FUN=sum, keep.names=T)
  psdrfPlaBMSsupClasse <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Classe,
                                       data=psdrfPlaBMSsupClasseStades, FUN=sum, keep.names=T, na.rm=T)
  psdrfPlaBMSsupStadeD <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + StadeD,
                                       data=psdrfPlaBMSsupClasseStades, FUN=sum, keep.names=T, na.rm=T)
  psdrfPlaBMSsupStadeE <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + StadeE,
                                       data=psdrfPlaBMSsupClasseStades, FUN=sum, keep.names=T, na.rm=T)
  psdrfPlaBMSsupEss  <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + code,
                                     data=BMSsup30, FUN=sum, keep.names=T)
  psdrfPlaBMSsupEss  <- merge(psdrfPlaBMSsupEss, CodeEssence[,2:3], by="code", all.x=T, sort=F)
  print("Bois mort au sol sup 30")
  # -------------- Bois mort sur pied de petite dimension
  names(BMP)[12:13] <- c("StadeE","StadeD")
  BMPinf <- subset(BMP, Diam <30)
  psdrfPlaBMPinfClasseStades  <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Classe + StadeD + StadeE,
                                   data=BMPinf, FUN=sum, keep.names=T)
  psdrfPlaBMPinf  <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle,
                               data=psdrfPlaBMPinfClasseStades, FUN=sum, keep.names=T)
  psdrfPlaBMPinfClasse  <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Classe,
                                    data=psdrfPlaBMPinfClasseStades, FUN=sum, keep.names=T)
  psdrfPlaBMPinfStadeD  <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + rot_stage,
                                    data=psdrfPlaBMPinfClasseStades, FUN=sum, keep.names=T)
  psdrfPlaBMPinfStadeE  <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + bark_stage,
                                    data=psdrfPlaBMPinfClasseStades, FUN=sum, keep.names=T)
  psdrfPlaBMPinfEss     <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + code,
                                    data=BMPinf, FUN=sum, keep.names=T)
  print("Bois mort sur pied inf 30")
  # -------------- Bois mort sur pied sup a 30 cm
  BMPsup <- subset(BMP, Diam >30)
  psdrfPlaBMPsupClasseStades  <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Classe + StadeD + StadeE,
                                   data=BMPsup, FUN=sum, keep.names=T)
  psdrfPlaBMPsup        <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle,
                                     data=psdrfPlaBMPsupClasseStades, FUN=sum, keep.names=T)
  psdrfPlaBMPsupClasse  <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Classe,
                                    data=psdrfPlaBMPsupClasseStades, FUN=sum, keep.names=T)
  psdrfPlaBMPsupStadeD  <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + rot_stage,
                                    data=psdrfPlaBMPsupClasseStades, FUN=sum, keep.names=T)
  psdrfPlaBMPsupStadeE  <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + bark_stage,
                                    data=psdrfPlaBMPsupClasseStades, FUN=sum, keep.names=T)
  psdrfPlaBMPsupEss     <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + code,
                                    data=BMPsup, FUN=sum, keep.names=T)
  print("Bois mort sur pied sup 30")
  # -------------- Bois mort sur pied
  psdrfPlaBMPtypo <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Classe + typo, data=BMP, FUN=sum, keep.names=T)
  psdrfPlaBMPtypo <- merge(psdrfPlaBMPtypo, CodeTypoArbres[,1:3], by.x="typo", by.y="id", all.x=T)
  print("Bois mort sur pied par type")
  # -------------- Bois mort total
  psdrfPlaBMTClasseStades <- data.frame()
  tab <- psdrfPlaBMPinfClasseStades
  tab$Type <- "BMPinf"
  psdrfPlaBMTClasseStades <- rbind(psdrfPlaBMTClasseStades, tab)
  tab <- psdrfPlaBMPsupClasseStades
  tab$Type <- "BMPsup"
  psdrfPlaBMTClasseStades <- rbind(psdrfPlaBMTClasseStades, tab)
  tab <- psdrfPlaBMSinfClasseStades
  tab$Type <- "BMSinf"
  psdrfPlaBMTClasseStades <- rbind(psdrfPlaBMTClasseStades, tab)
  tab <- psdrfPlaBMSsupClasseStades
  tab$Type <- "BMSsup"
  psdrfPlaBMTClasseStades <- rbind(psdrfPlaBMTClasseStades, tab)

  psdrfPlaBMTtype <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Type,
                           data=psdrfPlaBMTClasseStades, FUN=sum, keep.names=T)
  psdrfPlaBMTStadeD <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Type + StadeD,
                                 data=psdrfPlaBMTClasseStades, FUN=sum, keep.names=T)
  psdrfPlaBMTStadeE <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Type + StadeE,
                                 data=psdrfPlaBMTClasseStades, FUN=sum, keep.names=T)

  psdrfPlaBM <- dcast(psdrfPlaBMTtype,  NumDisp + NumPlac + Cycle ~ Type, value.var="Vha", na.rm=T)
  names(psdrfPlaBM)[4:7] <- c("VSinf","VSsup","VPinf","VPsup")
  psdrfPlaBM[4:7][is.na(psdrfPlaBM[4:7])] <- 0
  psdrfPlaBM$VhaBMT <- psdrfPlaBM$VSinf + psdrfPlaBM$VSsup + psdrfPlaBM$VPinf + psdrfPlaBM$VPsup
  psdrfPlaBMPclasse <- rbind(psdrfPlaBMPinfClasse,psdrfPlaBMPsupClasse)
  psdrfPlaBMPclasse <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Classe,
                                 data=psdrfPlaBMPclasse, FUN=sum, keep.names=T)
  psdrfPlaBMSclasse <- rbind(psdrfPlaBMSinfClasse,psdrfPlaBMSsupClasse)
  psdrfPlaBMSclasse <- summaryBy(Vha ~ NumDisp + NumPlac + Cycle + Classe,
                                 data=psdrfPlaBMSclasse, FUN=sum, keep.names=T)
  print("Bois mort total")
  # -------------- Bois vivant (volume et surface terriere)
  tab <- Arbres
  psdrfPlaArbresClasseEss <- summaryBy(Vha + VhaIFN + Gha + Nha ~
                                         NumDisp + NumPlac + Cycle + Classe + Cat + code + EssReg + EssRegPar,
                               data=tab, FUN=sum, keep.names=T)
  print("psdrfPlaArbresClasseEss")
  psdrfPlaArbresEss       <- summaryBy(Vha + VhaIFN + Gha + Nha ~ NumDisp + NumPlac + Cycle + code,
                                     data=psdrfPlaArbresClasseEss, FUN=sum, keep.names=T)
  psdrfPlaArbresEssReg   <- summaryBy(Vha + VhaIFN + Gha + Nha ~ NumDisp + NumPlac + Cycle + EssReg,
                                       data=psdrfPlaArbresClasseEss, FUN=sum, na.rm=T, keep.names=T)
  psdrfPlaArbresEssRegPar <- summaryBy(Vha + VhaIFN + Gha + Nha ~ NumDisp + NumPlac + Cycle + EssRegPar,
                                       data=psdrfPlaArbresClasseEss, FUN=sum, na.rm=T, keep.names=T)
  print("tables arbres par essence")
  psdrfPlaArbresClasse <- summaryBy(Vha + VhaIFN + Gha + Nha ~ NumDisp + NumPlac + Cycle + Classe,
                                     data=psdrfPlaArbresClasseEss, FUN=sum, keep.names=T)
  psdrfPlaArbresClasseEssReg <- summaryBy(Vha + VhaIFN + Gha + Nha ~ NumDisp + NumPlac + Cycle + EssReg + Classe,
                                     data=psdrfPlaArbresClasseEss, FUN=sum, keep.names=T)
  psdrfPlaArbresClasseEssRegPar <- summaryBy(Vha + VhaIFN + Gha + Nha ~ NumDisp + NumPlac + Cycle + EssRegPar + Classe,
                                     data=psdrfPlaArbresClasseEss, FUN=sum, keep.names=T)
  psdrfPlaArbresCat    <- summaryBy(Vha + VhaIFN + Gha + Nha ~ NumDisp + NumPlac + Cycle + Cat,
                                     data=psdrfPlaArbresClasseEss, FUN=sum, keep.names=T)
  psdrfPlaArbresCatEssReg    <- summaryBy(Vha + VhaIFN + Gha + Nha ~ NumDisp + NumPlac + Cycle + Cat + EssReg,
                                     data=psdrfPlaArbresClasseEss, FUN=sum, keep.names=T)
  psdrfPlaArbresCatEssRegPar <- summaryBy(Vha + VhaIFN + Gha + Nha ~ NumDisp + NumPlac + Cycle + Cat + EssRegPar,
                                     data=psdrfPlaArbresClasseEss, FUN=sum, keep.names=T)
  print("tables arbres par dimension")
  psdrfPlaNGV        <- summaryBy(Vha + VhaIFN + Gha + Nha ~ NumDisp + NumPlac + Cycle,
                               data=psdrfPlaArbresCat, FUN=sum, keep.names=T)
  psdrfPlaRes <- merge(psdrfPlaNGV, psdrfPlaBM, by=c("NumDisp","NumPlac","Cycle"), all.x=T)
  print("Fin des tables arbres")
  # -------------- Regeneration
  psdrfPlaRegeEss     <- summaryBy(seed_cover + Classe1Ha + Classe2Ha + Classe3Ha ~
                                     NumDisp + NumPlac + Cycle + code + EssReg + EssRegPar,
                                   data=Rege, FUN=sum, keep.names=T)
  psdrfPlaRegeEss[,7:10]  <- psdrfPlaRegeEss[,7:10]/3
  names(psdrfPlaRegeEss)[7:10] <- c("Recouv","Classe1","Classe2","Classe3")
  psdrfPlaRege        <- summaryBy(Recouv + Classe1 + Classe2 + Classe3 ~ NumDisp + NumPlac + Cycle,
                                   data=psdrfPlaRegeEss, FUN=sum, keep.names=T)
  psdrfPlaRegeEssReg  <- summaryBy(Recouv + Classe1 + Classe2 + Classe3 ~ NumDisp + NumPlac + Cycle + EssReg,
                                   data=psdrfPlaRegeEss, FUN=sum, keep.names=T)
  psdrfPlaRegeEssRegPar <- summaryBy(Recouv + Classe1 + Classe2 + Classe3 ~ NumDisp + NumPlac + Cycle + EssRegPar,
                                   data=psdrfPlaRegeEss, FUN=sum, keep.names=T)
  print("Regeneration")
  # --------- Sauvegarde -------
  save(psdrfPlaBMSinfClasseStades,psdrfPlaBMSinf,psdrfPlaBMSinfClasse,psdrfPlaBMSinfStadeD,psdrfPlaBMSinfStadeE,
       psdrfPlaBMSinfEss,
       psdrfPlaBMSsupClasseStades,psdrfPlaBMSsup,psdrfPlaBMSsupClasse,psdrfPlaBMSsupStadeD,psdrfPlaBMSsupStadeE,
       psdrfPlaBMSsupEss,
       psdrfPlaBMPinfClasseStades,psdrfPlaBMPinf,psdrfPlaBMPinfClasse,psdrfPlaBMPinfStadeD,psdrfPlaBMPinfStadeE,
       psdrfPlaBMPinfEss,
       psdrfPlaBMPsupClasseStades,psdrfPlaBMPsup,psdrfPlaBMPsupClasse,psdrfPlaBMPsupStadeD,psdrfPlaBMPsupStadeE,
       psdrfPlaBMPsupEss,psdrfPlaBMPtypo,
       psdrfPlaBMTClasseStades,psdrfPlaBMTtype,psdrfPlaBMTStadeD,psdrfPlaBMTStadeE,psdrfPlaBM,
       psdrfPlaBMPclasse,psdrfPlaBMSclasse,
       psdrfPlaArbresClasseEss,psdrfPlaArbresEss,psdrfPlaArbresEssReg,psdrfPlaArbresEssRegPar,psdrfPlaArbresClasse,
       psdrfPlaArbresClasseEssReg,psdrfPlaArbresClasseEssRegPar,
       psdrfPlaArbresCat,psdrfPlaArbresCatEssReg,psdrfPlaArbresCatEssRegPar,
       psdrfPlaNGV,psdrfPlaRes,
       psdrfPlaRegeEss,psdrfPlaRege,psdrfPlaRegeEssReg,psdrfPlaRegeEssRegPar,
       file="Tables/psdrfTablesElaboreesPlac.RData")
}
