#' Calcul des variables dendrométriques et écologiques.
#' @description Cette fonction commence par calculer le poids de chaque arbre,
#' puis pour chacun d'eux, calcule les variables dendrométriques et écologiques ramenées à l'hectare.
#' @return La fonction construit les tables suivantes.
#'
#' Arbres, BMP, BMSline, BMSsup30, Rege
#'
#' Elles sont enregistrées dans le dossier Tables sous le nom : psdrfTablesBrutes.RData.
#' @author Bruciamacchie Max
#' @export
psdrf_Calculs <- function() {
# ------- Table arbres  ------
t      <- arbres
t$X    <- t$distance*sin(t$azimut/200*pi)
t$Y    <- t$distance*cos(t$azimut/200*pi)
pos <- which(is.na(t$dbh2) | t$dbh2==0)
t$dbh2[pos] <- t$dbh1[pos]
t$Diam <- (t$dbh1 + t$dbh2)/2
t$Classe <- floor(t$Diam/5+0.5)*5

t$Cat <- cut(t$Diam, breaks = c(0, 17.5, 27.5, 47.5, 67.5, 500),
                  labels = c("PER","PB","BM","GB","TGB"), include.lowest = T, right = F)
t$CatPar <- t$Cat
if (dim(Cat)[1] > 0) {
  for (i in dim(Cat)[1]) {
    num <- Cat$NumDisp[i]
    pos <- which(t$NumDisp==num)
    limites <- c(0, as.vector(Cat[i,-1]), 500)
    t$CatPar[pos] <- cut(t$Diam[pos], breaks = limites, labels = c("PER","PB","BM","GB","TGB"),
                      include.lowest = T, right = F)
  }
}
print("Calcul des variables Diam, Classe et cat")

# Extraction des arbres vivants
Arbres <- t[which(is.na(t$typo)),]

# Calcul du poids des arbres (Attention : très important !!!!!!!!)
Arbres <- merge(Arbres, Cycles[,1:4], by = c("NumDisp","NumPlac","Cycle"), all.x = T, sort=F)
Arbres$Lim <- ifelse(Arbres$dbh1 >= Arbres$distance * Arbres$relasco, 0, 1)
Arbres$Poids <- 0
SeuilDiam <- 30
pos <- which(Arbres$Diam < SeuilDiam & Arbres$distance <=10)
Arbres$Poids[pos] <- 10000/pi/10^2
pos <- which(Arbres$Diam >= SeuilDiam)
Arbres$Poids[pos] <- 10000*Arbres$relasco[pos]^2/pi/Arbres$dbh1[pos]^2*(1-Arbres$Lim[pos])
print("Calcul du poids des arbres")

# Calcul de la surface terriere
Arbres$g <- pi*Arbres$dbh1^2/40000
Arbres$Gha <- Arbres$g*Arbres$Poids
# Calcul du volume
Arbres <- merge(Arbres, Tarifs, by = c("NumDisp","code"), all.x=T, sort=F)
######################### A supprimer des que base complète #########################
pos <- which(is.na(Arbres$TypeTarif))
if (length(pos) >0) {
  Arbres$TypeTarif[pos] <- "SchL"
  Arbres$NumTarif[which(is.na(Arbres$NumTarif))] <- 6
}

################### Calcul du volume gestionnaire ###################################
print("Calcul du volume gestionnaire")
Arbres$V <- NA
pos <- which(Arbres$TypeTarif=="SchR")
if (length(pos) > 0) {
  Arbres$V[pos] <- 5/70000*(8+Arbres$NumTarif[pos])*(Arbres$Diam[pos]-5)*(Arbres$Diam[pos]-10)}
pos <- which(Arbres$TypeTarif=="SchI")
if (length(pos) > 0) {
  Arbres$V[pos] <- 5/80000*(8+Arbres$NumTarif[pos])*(Arbres$Diam[pos]-2.5)*(Arbres$Diam[pos]-7.5)}
pos <- which(Arbres$TypeTarif=="SchL")
if (length(pos) > 0) {
  Arbres$V[pos] <- 5/90000*(8+Arbres$NumTarif[pos])*(Arbres$Diam[pos]-5)*Arbres$Diam[pos]}
pos <- which(Arbres$TypeTarif=="SchTL")
if (length(pos) > 0) {
  Arbres$V[pos] <-  5/101250*(8+Arbres$NumTarif[pos])*Arbres$Diam[pos]^2}
Arbres$V[which(Arbres$V<0)] <- 0
Arbres$Vha <- Arbres$V*Arbres$Poids
Arbres$typo <- NULL
Arbres$Nha    <- Arbres$Poids; Arbres$Poids  <- NULL

################### Calcul du volume IFN ###################################
print("Calcul du volume géométrique bois fort tige")
Arbres$VIFN <- NA
pos <- which(Arbres$TypeTarifIFN=="SchR")
if (length(pos) > 0) {
  Arbres$VIFN[pos] <- 5/70000*(8+Arbres$NumTarifIFN[pos])*(Arbres$Diam[pos]-5)*(Arbres$Diam[pos]-10)}
pos <- which(Arbres$TypeTarifIFN=="SchI")
if (length(pos) > 0) {
  Arbres$VIFN[pos] <- 5/80000*(8+Arbres$NumTarifIFN[pos])*(Arbres$Diam[pos]-2.5)*(Arbres$Diam[pos]-7.5)}
pos <- which(Arbres$TypeTarifIFN=="SchL")
if (length(pos) > 0) {
  Arbres$VIFN[pos] <- 5/90000*(8+Arbres$NumTarifIFN[pos])*(Arbres$Diam[pos]-5)*Arbres$Diam[pos]}
pos <- which(Arbres$TypeTarifIFN=="SchTL")
if (length(pos) > 0) {
  Arbres$VIFN[pos] <-  5/101250*(8+Arbres$NumTarifIFN[pos])*Arbres$Diam[pos]^2}
Arbres$VIFN[which(Arbres$VIFN<0)] <- 0
Arbres$VhaIFN <- Arbres$VIFN*Arbres$Nha # Pas gênant si il y a des NA

Arbres <- merge(Arbres, CodeEssence[,c(2,4)], by="code", all.x=T)
Arbres <- merge(Arbres, EssReg, by=c("NumDisp","code"), all.x=T)
pos <- which(is.na(Arbres$EssRegPar))
Arbres$EssRegPar[pos] <- Arbres$EssReg[pos]

# ------- Bois mort sur pied ---------------------------
print("Calcul des variables de bois mort sur pied")
t <- t[which(!is.na(t$bark_stage)),]
pos <- which(is.na(t$dbh2) | t$dbh2==0)
t$Diam[pos]    <- t$dbh1[pos]
t$Classe  <- floor(t$Diam/5+0.5)*5
t$Nha   <- ifelse(t$Diam <30, 10000/pi/10^2, 10000/pi/20^2)
t$Nha[which(t$distance > 10 & t$Diam < 30)] <- 0
t$Nha[which(t$distance > 20)] <- 0
t$g       <- pi*t$Diam^2/40000
t$Vol     <- ifelse(is.na(t$height), 8*t$g, pi/40000*(t$Diam-(t$height/2-1.3))^2*t$height)
t$Gha     <- t$g * t$Nha
t$Vha     <- t$Vol * t$Nha
BMP <- t

# ------- Bois mort au sol ---------------------------
print("Calcul des variables de bois mort au sol")
# ----- Echantillonnage lineaire
t   <- merge(Transect, CodeDurete[,c("code","libelle")], by.x="rot_stage", by.y="code", all.x=T, sort=F)
t   <- merge(t, CodeEcorce[,c("code","libelle")], by.x="bark_stage", by.y="code", all.x=T, sort=F)
names(t)[13:14] <- c("Dureté", "Ecorce")
t$angle[is.na(t$angle)] <- 0 # remplissage des NA pour les angles par des 0
t$Classe  <- floor(t$diam/5+0.5)*5
t$Vha     <- pi^2/8/60*t$diam^2/cos(t$angle/180*pi)
t$StadeE  <- t$bark_stage
t$StadeD  <- t$rot_stage
BMSline   <- subset(t, select =c("NumDisp","NumPlac","Cycle","code","StadeD","StadeE","diam","Classe","Vha"))
# BMSline   <- subset(t, select =c("plot","cycle","num","species","libelle","StadeD","StadeE","diam","Classe","Vha"))

# ----- Cercle de 20 m
t  <- BMSsup30
t$StadeE  <- t$bark_stage
t$StadeD  <- t$rot_stage

t$base_diam[is.na(t$base_diam)] <- 0
t$top_diam[is.na(t$top_diam)] <- 0
t$mid_diam[is.na(t$mid_diam)] <- 0
t$Vha <- 0
t$Classe <- 0
# ---- formule de Huber
pos <- which((t$base_diam+ t$top_diam)==0)
t$Vha[pos] <- pi/40000*t$mid_diam[pos]^2*t$length[pos] * 10000/pi/20^2
t$Classe[pos] <- floor(t$mid_diam[pos]/5+0.5)*5
# ---- formule de Smalian
pos <- which((t$base_diam+ t$top_diam)!=0 & t$mid_diam==0)
t$Vha[pos] <- pi/80000*(t$base_diam[pos]^2+t$top_diam[pos]^2)*t$length[pos] * 10000/pi/20^2
t$Classe[pos] <- floor((t$base_diam[pos]+t$top_diam[pos])/2/5+0.5)*5
# ---- formule de Newton
pos <- which((t$base_diam+ t$top_diam)!=0 & t$mid_diam!=0)
t$Vha[pos] <- pi/240000*(t$base_diam[pos]^2+t$top_diam[pos]^2 + 4*t$mid_diam[pos]^2)*t$length[pos] * 10000/pi/20^2
t$Classe[pos] <- floor((t$base_diam[pos]+t$top_diam[pos]+t$base_diam[pos])/3/5+0.5)*5

BMSsup30 <- t


# ------- Table Rege  ------
Rege$seed_cover[is.na(Rege$seed_cover)] <- 0
Rege$class1[which(is.na(Rege$class1))] <- 0
Rege$class2[which(is.na(Rege$class2))] <- 0
Rege$class3[which(is.na(Rege$class3))] <- 0
Rege      <- merge(Rege, CodeEssence[,2:3], by="code", all.x=T, sort=F)
Rege$Classe1Ha <- Rege$class1* 10000/pi/1.5^2
Rege$Classe2Ha <- Rege$class2* 10000/pi/1.5^2
Rege$Classe3Ha <- Rege$class3* 10000/pi/1.5^2

Rege <- merge(Rege, CodeEssence[,c(2,4)], by="code", all.x=T)
Rege <- merge(Rege, EssReg, by=c("NumDisp","code"), all.x=T)
pos <- which(is.na(Rege$EssRegPar))
Rege$EssRegPar[pos] <- Rege$EssReg[pos]
print("Calcul des variables de régénération")

# ----- Enregistrement -----------------
save(Arbres, BMP, BMSline, BMSsup30, Rege,
     file="Tables/psdrfTablesBrutes.RData")
}
