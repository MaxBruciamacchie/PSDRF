#' Jonction des tables attributaires avec le shape placettes.
#' @description La fonction récupère les data.frames construits à l'échelle des placettes
#' pour faire la jonction des données dendrométriques (psdrfPla), de la composition en essence
#' (psdrfPlaArbresVEssReg) et des données de bois mort (psdrfPlaBM).
#' @param shp = shape des placettes avec comme système de projection RGF93/lambert-93.
#' @return La fonction construit le shape PsdrfPlacettes.shp dans le dossier SIG/Vecteurs.
#' @import sp
#' @import rgdal
#' @import tools
#' @author Bruciamacchie Max
#' @export
psdrf_ShapesPlac <-function() {
  shp <- tk_choose.files(caption = "Choix du Shape Coordonnées des placettes",
                         filters=matrix(c(".shp",".shp"),1,2, byrow = T))
  Coords <- readOGR(dsn=dirname(shp), layer=basename(file_path_sans_ext(shp)))
  # ----------------------- Fusion
  t1 <- psdrfPlaNGV
  tab <- psdrfPlaArbresEssReg  # Composition
  tab <- dcast(tab,  NumDisp + NumPlac + Cycle ~ EssReg, value.var="Vha", na.rm=T)
  t1 <- merge(t1, tab, by=c("NumDisp","NumPlac","Cycle"), all.x=T, sort=F)
  tab <- psdrfPlaBM      # Bois mort
  names(tab)[8] <- "VhaBMT"
  t1 <- merge(t1, tab, by=c("NumDisp","NumPlac", "Cycle"), all.x=T, sort=F)
  t1[is.na(t1)] <- 0
#   res <- sp::merge(Coords, t1, by=c("NumDisp","NumPlac"), all.x=F, all.y=T)
  res <- merge(Coords, t1, by=c("NumDisp","NumPlac"), all.x=F, all.y=T)
#   ----------------------- Ecriture et sauvegarde --------------
  rep = "SIG/Vecteurs"
  writeOGR(res, rep, "PsdrfPlacettes", driver="ESRI Shapefile", overwrite_layer=T)
}
