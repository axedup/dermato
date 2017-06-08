dermato<-read.csv2("C:/Users/adupont/Documents/thesemarie/dermato.csv",na.strings=c("","non précisé"))
noms<-read.csv2("C:/Users/adupont/Documents/thesemarie/noms.csv",na.strings=c("","non précisé"))
summary(dermato)

dermato$nip<-as.factor(dermato$nip)
dermato$immunodep<-as.factor(dermato$immunodep)
levels(dermato$immunodep)<-c("Non","Greffe","VIH","Autre")
table(dermato$immunodep,exclude=NULL)
dermato$vih<-as.factor(dermato$vih)
levels(dermato$vih)<-c("Controlée")
table(dermato$vih,exclude=NULL)

table(dermato$trans,exclude=NULL)
table(dermato$autre_cancer,exclude=NULL)

for (i in c("trans", "plaie", "ulcere", "epider", "brulure", "irradie", "prof", "uva",
            "autre_cancer", "insuff_organe", "comor_transplant", "maladie_chro", 
            "cardiovascu", "aap", "is", "ac", "gg_meta", 
            "pul_meta", "souscut_meta", "autre_meta","aeg_nc", "extlc_nc",
            "extm_nc", "refus_nc","ggsenti", "ggsentip","epn","emboles","rte", "rte_neo", 
            "rte_adj", "rte_meta", "rte_seule", "rct","renduop"
)){
  table(dermato[,i],exclude=NULL)
 
}





for (i in c("trans", "plaie", "ulcere", "epider", "brulure", "irradie", "prof", "uva",
            "autre_cancer", "insuff_organe", "comor_transplant", "maladie_chro", 
            "cardiovascu", "aap", "is", "ac", "gg_meta", 
            "pul_meta", "souscut_meta", "autre_meta","aeg_nc", "extlc_nc",
            "extm_nc", "refus_nc","ggsenti", "ggsentip","epn","emboles","rte", "rte_neo", 
            "rte_adj", "rte_meta", "rte_seule", "rct","renduop"
            )){
  dermato[,i]<-factor(dermato[,i])
  levels(dermato[,i])<-c("Non","Oui")
}

table(dermato$loca_prim,exclude=NULL)
dermato$loca_prim<-as.factor(dermato$loca_prim)
levels(dermato$loca_prim)<-c("Mbr sup","Tronc","Mbr Inf","Perine","Tete et cou")
table(dermato$loca_prim,exclude=NULL)

table(dermato$tete_cou,exclude=NULL)
dermato$tete_cou<-as.factor(dermato$tete_cou)
levels(dermato$tete_cou)<-c("Cuir chevelu","Peri orificiel","Autres")
table(dermato$tete_cou,exclude=NULL)

table(dermato$gg,exclude=NULL)
dermato$gg<-as.factor(dermato$gg)
levels(dermato$gg)<-c("N0","N2","N3")
table(dermato$gg,exclude=NULL)


table(dermato$meta,exclude=NULL)
dermato$meta<-as.factor(dermato$meta)
levels(dermato$meta)<-c("M0","M1")
table(dermato$meta,exclude=NULL)



table(dermato$ct,exclude=NULL)
dermato$ct<-as.factor(dermato$ct)
levels(dermato$ct)<-c("T1","T2","T3","T4")
table(dermato$ct,exclude=NULL)

table(dermato$ct,exclude=NULL)
dermato$ct<-as.factor(dermato$ct)
levels(dermato$ct)<-c("T1","T2","T3","T4")
table(dermato$ct,exclude=NULL)


table(dermato$chir_prim,exclude=NULL)
dermato$chir_prim<-as.factor(dermato$chir_prim)
levels(dermato$chir_prim)<-c("Non","D'emblée","Après CT","Après RCT")
table(dermato$chir_prim,exclude=NULL)



table(dermato$neoadj,exclude=NULL)
dermato$neoadj<-as.factor(dermato$neoadj)
levels(dermato$neoadj)<-c("Non","CT","CT-ERBITUX","RTE-ERBITUX","ERBITUX","RTE-CT","RTE")
table(dermato$neoadj,exclude=NULL)


table(dermato$curage,exclude=NULL)
dermato$curage<-as.factor(dermato$curage)
levels(dermato$curage)<-c("Non","Synchrone","Métachrone")
table(dermato$curage,exclude=NULL)


table(dermato$ypt,exclude=NULL)
dermato$ypt<-as.factor(dermato$ypt)
levels(dermato$ypt)<-c("pT1","pT2","pT3","pT4")
table(dermato$ypt,exclude=NULL)


table(dermato$ypn,exclude=NULL)
dermato$ypn<-as.factor(dermato$ypn)
levels(dermato$ypn)<-c("pN0","pN1","pN2","pN3")
table(dermato$ypn,exclude=NULL)


table(dermato$ngg,exclude=NULL)
dermato$ngg<-as.factor(dermato$ngg)
levels(dermato$ngg)<-c("<6",">=6")
table(dermato$ngg,exclude=NULL)


table(dermato$ggr,exclude=NULL)
dermato$ggr<-as.factor(dermato$ggr)
levels(dermato$ggr)<-c("RC-","RC+")
table(dermato$ggr,exclude=NULL)


table(dermato$histo,exclude=NULL)
dermato$histo<-as.factor(dermato$histo)
levels(dermato$histo)<-c("CEC com","Verruqueux","mixte (basosquameux)", "cellules fusiformes",
"acantholytique","à cellules claires")
table(dermato$histo,exclude=NULL)


table(dermato$epais,exclude=NULL)
dermato$epais<-as.factor(dermato$epais)
levels(dermato$epais)<-c("> 2mm")
table(dermato$epais,exclude=NULL)


table(dermato$diff,exclude=NULL)
dermato$diff<-as.factor(dermato$diff)
levels(dermato$diff)<-c("Bien","Peu ou pas")
table(dermato$diff,exclude=NULL)


table(dermato$marges_chir,exclude=NULL)
dermato$marges_chir<-as.factor(dermato$marges_chir)
levels(dermato$marges_chir)<-c("R0","R1")
table(dermato$marges_chir,exclude=NULL)


table(dermato$type_pla,exclude=NULL)
dermato$type_pla<-as.factor(dermato$type_pla)
levels(dermato$type_pla)<-c("cisplatine","carboplatine")
table(dermato$type_pla,exclude=NULL)

dermato$tox_max<-as.factor(as.character(dermato$tox_max))


table(dermato$rte_loc,exclude=NULL)
dermato$rte_loc<-as.factor(dermato$rte_loc)
levels(dermato$rte_loc)<-c("primitif","GG","GG+ prim","Antalgique")
table(dermato$rte_loc,exclude=NULL)


table(dermato$rte_tox,exclude=NULL)
dermato$rte_tox<-as.factor(dermato$rte_tox)
levels(dermato$rte_tox)<-c("Radiodermite","Lymphoedme","PF grade 4 ","Dysphagie")
table(dermato$rte_tox,exclude=NULL)


table(dermato$local,exclude=NULL)
dermato$local<-as.factor(dermato$local)
levels(dermato$local)<-c("Locoregionale","à distance","locor+ distance")
table(dermato$local,exclude=NULL)

dermato$nb_lignestt<-as.factor(as.character(dermato$nb_lignestt))
dermato$ligne_br<-as.factor(as.character(dermato$ligne_br))
levels(dermato$ligne_br)<-c("progression constante", "LIGNE 1", "LIGNE 2", "LIGNE 3", 
                    "progression constante")


table(dermato$br,exclude=NULL)
dermato$br<-as.factor(dermato$br)
levels(dermato$br)<-c("Complete","Partielle","Stable","Progression")
table(dermato$br,exclude=NULL)


table(dermato$ecog_pre,exclude=NULL)
dermato$ecog_pre<-as.factor(dermato$ecog_pre)
table(dermato$ecog_post,exclude=NULL)
dermato$ecog_post<-as.factor(dermato$ecog_post)


table(dermato$strategie1,exclude=NULL)
dermato$strategie1<-as.factor(dermato$strategie1)
levels(dermato$strategie1)<-c("Neoadj","Méta/palliative")
table(dermato$strategie1,exclude=NULL)


table(dermato$strategie2,exclude=NULL)
dermato$strategie2<-as.factor(dermato$strategie2)
levels(dermato$strategie2)<-c("Neoadj","Adj","Méta/palliative")
table(dermato$strategie2,exclude=NULL)

table(dermato$strategie3,exclude=NULL)
dermato$strategie3<-as.factor(dermato$strategie3)
levels(dermato$strategie3)<-c("Méta/palliative")
table(dermato$strategie3,exclude=NULL)

table(dermato$strategie4,exclude=NULL)
dermato$strategie4<-as.factor(dermato$strategie4)
levels(dermato$strategie4)<-c("Méta/palliative")
table(dermato$strategie4,exclude=NULL)

table(dermato$grp_tt1,exclude=NULL)
dermato$grp_tt1<-as.factor(dermato$grp_tt1)
levels(dermato$grp_tt1)<-c("CT+Erbi","Erbitux","CT","RTE+Erbi","RTE+CT","RTE+CT+Erbi","RTE Seule","Anti PD1 ATu")
table(dermato$grp_tt1,exclude=NULL)

table(dermato$grp_tt2,exclude=NULL)
dermato$grp_tt2<-as.factor(dermato$grp_tt2)
levels(dermato$grp_tt2)<-c("CT+Erbi","Erbitux","CT","RTE+Erbi","RTE+CT","RTE+CT+Erbi","RTE Seule","Anti PD1 ATu")
table(dermato$grp_tt2,exclude=NULL)

table(dermato$grp_tt3,exclude=NULL)
dermato$grp_tt3<-as.factor(dermato$grp_tt3)
levels(dermato$grp_tt3)<-c("CT+Erbi","CT","RTE+Erbi","Anti PD1 ATu","RTE+CT","RTE+CT+Erbi","RTE Seule")
table(dermato$grp_tt3,exclude=NULL)

table(dermato$grp_tt4,exclude=NULL)
dermato$grp_tt4<-as.factor(dermato$grp_tt4)
levels(dermato$grp_tt4)<-c("CT+Erbi","Erbitux","CT","RTE+Erbi","RTE+CT","RTE+CT+Erbi","RTE Seule","Anti PD1 ATu")
table(dermato$grp_tt4,exclude=NULL)


dermato$chir1<-ifelse(is.na(dermato$date_chir1),"Pas de chir","Chir")
table(dermato$chir1,exclude=NULL)
dermato$chir2<-ifelse(is.na(dermato$date_chir2),"Pas de chir","Chir")



dermato$ddn<-as.Date(as.character(dermato$ddn),format="%d/%m/%Y")
dermato$date_dia_iv<-as.Date(as.character(dermato$date_dia_iv),format="%d/%m/%Y")
dermato$date_fu<-as.Date(as.character(dermato$date_fu),format="%d/%m/%Y")

table(dermato$deces,exclude = NULL)
dermato$decesf<-ifelse(dermato$deces==1 | dermato$deces==2,1,0)
table(dermato$decesf,exclude = NULL)

table(dermato$deces,exclude=NULL)
dermato$deces<-as.factor(dermato$deces)
levels(dermato$deces)<-c("Pas de decès","Onco","Autres causes")
table(dermato$deces,exclude=NULL)



dermato$delai_dc<-difftime(dermato$date_fu,dermato$date_dia_iv)/30.25
