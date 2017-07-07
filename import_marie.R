library(TraMineR)
library(ggplot2)
library(grid)


dermato<-read.csv2("C:/Users/adupont/Documents/thesemarie/dermato.csv",na.strings=c("","non précisé"))
noms<-read.csv2("C:/Users/adupont/Documents/thesemarie/noms.csv",na.strings=c("","non précisé"))
summary(dermato)

dermato$nip<-as.factor(dermato$nip)
reforder<-dermato$nip
dermato$immunodep<-as.factor(dermato$immunodep)
levels(dermato$immunodep)<-c("Non","Greffe","VIH","Autre")
table(dermato$immunodep,exclude=NULL)
dermato$vih<-as.factor(dermato$vih)
levels(dermato$vih)<-c("Controlée")
table(dermato$vih,exclude=NULL)



levels(dermato$phototype)<-c("1", "2"  , "3"  , "6"   , "Albinos (1)","4","5")
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
dermato$date_chir<-as.Date(as.character(dermato$date_chir),format="%d/%m/%Y")
dermato$date_l1<-as.Date(as.character(dermato$date_l1),format="%d/%m/%Y")
dermato$date_l2<-as.Date(as.character(dermato$date_l2),format="%d/%m/%Y")
dermato$date_l3<-as.Date(as.character(dermato$date_l3),format="%d/%m/%Y")
dermato$date_l4<-as.Date(as.character(dermato$date_l4),format="%d/%m/%Y")

dermato$date_p1<-as.Date(as.character(dermato$date_p1),format="%d/%m/%Y")
dermato$date_ppc1<-as.Date(as.character(dermato$date_ppc1),format="%d/%m/%Y")

dermato$date_p2<-as.Date(as.character(dermato$date_p2),format="%d/%m/%Y")
#dermato$date_ppc2<-as.Date(as.character(dermato$date_ppc2),format="%d/%m/%Y")

dermato$date_p3<-as.Date(as.character(dermato$date_p3),format="%d/%m/%Y")
dermato$date_p4<-as.Date(as.character(dermato$date_p4),format="%d/%m/%Y")
table(dermato$deces,exclude = NULL)
dermato$decesf<-ifelse(dermato$deces==1 | dermato$deces==2,1,0)
table(dermato$decesf,exclude = NULL)

table(dermato$deces,exclude=NULL)
dermato$deces<-as.factor(dermato$deces)
levels(dermato$deces)<-c("Pas de decès","Onco","Autres causes")
table(dermato$deces,exclude=NULL)



dermato$delai_dc<-difftime(dermato$date_fu,dermato$date_dia_iv)/30.25


### PFC###

dermato$pfst<-ifelse(dermato$reponse_l1=="PROGRESSION" | dermato$response_chir1 %in% c("progression")|
  dermato$reponse_l2=="PROGRESSION" & !is.na(dermato$reponse_l2)
  |   dermato$reponse_l3=="PROGRESSION" & !is.na(dermato$reponse_l3)|
    dermato$reponse_l4=="PROGRESSION" & !is.na(dermato$reponse_l4)|dermato$dn %in% c(3,5)
  ,1,0)
dermato$pfst<-ifelse(dermato$deces %in% c("Onco","Autres causes")  & 
                       dermato$pfst==0,1,dermato$pfst)
table(dermato$pfst,exclude=NULL)

dermato$date_pfst<-ifelse(dermato$reponse_l1=="PROGRESSION",dermato$date_p1,NA)
dermato$date_pfst<-ifelse(dermato$reponse_l2=="PROGRESSION" & is.na(dermato$date_pfst),
                          dermato$date_p2,dermato$date_pfst)
dermato$date_pfst<-ifelse(dermato$reponse_l3=="PROGRESSION"  & is.na(dermato$date_pfst)
                          ,dermato$date_p3,dermato$date_pfst)
dermato$date_pfst<-ifelse(dermato$reponse_l4=="PROGRESSION" & is.na(dermato$date_pfst)
                          ,dermato$date_p4,dermato$date_pfst)
dermato$date_pfst<-ifelse(dermato$dn %in% c(3,5)& is.na(dermato$date_pfst)
                          ,dermato$date_fu,dermato$date_pfst)
dermato$date_pfst<-ifelse(dermato$deces %in% c("Onco","Autres causes")  & is.na(dermato$date_pfst)
                          ,dermato$date_fu,dermato$date_pfst)
dermato$date_pfst<-ifelse(dermato$pfst==0, dermato$date_fu,dermato$date_pfst)
table(dermato$date_pfst,exclude = NULL)

dermato$date_pfst<-as.Date(dermato$date_pfst, origin="1970-01-01")


dermato$delai_pfs<-as.numeric(difftime(dermato$date_pfst,dermato$date_dia_iv)/30.25)


### PFS 1ère ligne
dermato$pfs<-ifelse(dermato$reponse_l1=="PROGRESSION"
                    | dermato$response_chir1 %in% c("progression"),1,0)
table(dermato$pfs,exclude=NULL)
dermato$pfs<-ifelse(dermato$dn %in% c(3,5)  & is.na(dermato$date_l2)
                    & is.na(dermato$date_chir1),1,dermato$pfs)



dermato$pfs<-ifelse(dermato$deces %in% c("Onco","Autres causes")  & 
                      is.na(dermato$date_l2),1,dermato$pfs)

table(dermato$pfs,is.na(dermato$date_l2),exclude=NULL)



dermato$date_pfs<-ifelse(!is.na(dermato$date_p1),dermato$date_p1,NA)
dermato$date_pfs<-ifelse(is.na(dermato$date_pfs),dermato$date_ppc1,dermato$date_pfs)
dermato$date_pfs<-ifelse(is.na(dermato$date_pfs) & !is.na(dermato$date_l2),dermato$date_l2,dermato$date_pfs)
dermato$date_pfs<-ifelse(is.na(dermato$date_pfs),dermato$date_fu,dermato$date_pfs)
dermato$date_pfs<-as.Date(dermato$date_pfs,origin = "1970-01-01")

dermato$delai_pfs1<-as.numeric(difftime(dermato$date_pfs,dermato$date_l1)/30.25)


dermato$d1<-dermato$decesf
dermato$d1<-ifelse(!is.na(dermato$date_l2),0,dermato$d1)

dermato$date_deces1<-dermato$date_fu
dermato$date_deces1<-ifelse(dermato$d1==1,dermato$date_fu,dermato$date_l2)
dermato$date_deces1<-ifelse(dermato$d1==0 & is.na(dermato$date_l2),dermato$date_fu,dermato$date_deces1)
dermato$date_deces1<-as.Date(dermato$date_deces1,origin = "1970-01-01")


dermato$delai_deces1<-as.numeric(difftime(dermato$date_deces1,dermato$date_l1)/30.25)


### PFS 2ème ligne
dermato$pfs2<-ifelse(dermato$reponse_l2=="PROGRESSION"
                    | dermato$response_chir2 %in% c("progression"),1,0)
dermato$pfs2<-ifelse(dermato$dn %in% c(3,5)  & is.na(dermato$date_l3),1,dermato$pfs2)

dermato$pfs2<-ifelse(dermato$deces %in% c("Onco","Autres causes")  & 
                      is.na(dermato$date_l3),1,dermato$pfs2)



table(dermato$pfs2,exclude=NULL)

dermato$date_pfs2<-ifelse(!is.na(dermato$date_p2),dermato$date_p2,dermato$date_l3)
#dermato$date_pfs2<-ifelse(is.na(dermato$date_pfs2),dermato$date_ppc2,dermato$date_pfs2)
dermato$date_pfs2<-ifelse(is.na(dermato$date_pfs2),dermato$date_fu,dermato$date_pfs2)
dermato$date_pfs2<-as.Date(dermato$date_pfs2,origin = "1970-01-01")

dermato$delai_pfs2<-as.numeric(difftime(dermato$date_pfs2,dermato$date_l2)/30.25)

dermato$d2<-dermato$decesf
dermato$d2<-ifelse(!is.na(dermato$date_l3),0,dermato$d2)

dermato$date_deces2<-dermato$date_fu
dermato$date_deces2<-ifelse(dermato$d2==1,dermato$date_fu,dermato$date_l3)
dermato$date_deces2<-ifelse(dermato$d2==0 & is.na(dermato$date_l3),dermato$date_fu,dermato$date_deces2)
dermato$date_deces2<-as.Date(dermato$date_deces2,origin = "1970-01-01")


dermato$delai_deces2<-as.numeric(difftime(dermato$date_deces2,dermato$date_l2)/30.25)






### test ### ######  


dermato<-dermato[match(reforder, dermato$nip),]

dermato$nip2<-1:42
test<-dermato[,c("nip2","date_dia_iv","date_l1","date_l2","date_l3","date_l4","date_fu")]
 
test[,2]<-as.character(test[,2])
 l<-reshape(dermato[,c("nip2","date_dia_iv","date_l1","date_l2","date_l3","date_l4","date_fu")], varying =
             c("date_dia_iv","date_l1","date_l2","date_l3","date_l4","date_fu"), v.names="dates",
            timevar="ref", times=c("date_dia_iv","date_l1","date_l2","date_l3","date_l4","date_fu"),
           direction="long")
# 
# 
 l<-l[order(l$nip2),]
# a<-dermato$date_l1
# b<-dermato$date_l1 +6
# idd<-1:42
# tu<-rep(c("vr","f","aa"),14)
# 
# fra<-data.frame(idd,a,b,tu)
# 
# 
# seqdef(fra,var=c("idd","a","b","tu"), informat = "SPELL", process = FALSE)
# seqformat(fra,var=c("idd","a","b","tu"), from = "SPELL", to = "STS")

 
 
testu<-dermato[,c("nip2","date_dia_iv","grp_tt1","grp_tt2","grp_tt3","grp_tt4","date_fu")]
testu$grp_tt1<-as.character(testu$grp_tt1) 
testu$grp_tt2<-as.character(testu$grp_tt2) 
testu$grp_tt3<-as.character(testu$grp_tt3) 
testu$grp_tt4<-as.character(testu$grp_tt4) 
testu$nip<-as.character(testu$nip) 
testu$date_dia_iv<-as.character(testu$date_dia_iv) 
testu$date_fu<-as.character(testu$date_fu) 


 testu[,2]<-as.character(test[,2])
 lu<-reshape(testu[,c("nip2","date_dia_iv","grp_tt1","grp_tt2","grp_tt3","grp_tt4","date_fu")], 
             varying =
              c("date_dia_iv","grp_tt1","grp_tt2","grp_tt3","grp_tt4","date_fu"), v.names="TT",
            timevar="ref", times=c("date_dia_iv","grp_tt1","grp_tt2","grp_tt3","grp_tt4","date_fu"),
            direction="long")
 # 
 # 
 lu<-lu[order(lu$nip2),]

 
lw<-cbind(l,lu) 
lw<-lw[!is.na(lw$dates),]
 
 
 DATA<-NULL 
 for( i in unique(lw$nip2)){
   
   data<-lw[lw$nip2==i,]
   data$end<-NA
   for (j in 1: nrow(data)-1){
     data$end[j]<-data$dates[j+1]
     
   }
   
   data$end[nrow(data)]<-data$dates[nrow(data)]
   data$end<-as.Date(data$end,origin="1970-01-01")
   data$TT[1]<-"Chmiothérapie non débutée"
   data$TT[nrow(data)]<-NA
   DATA<-rbind(DATA,data)
 }
 
 #frat<-DATA[DATA$ref=="date_dia_iv", c("nip2","dates")]
 
 DATA$dates<-as.numeric( DATA$dates)
 DATA$end<-as.numeric( DATA$end)
 
DATA$end[DATA$Chimiothrapie=="Anti PD1 ATu" & DATA$nip2=="39" & DATA$ref=="date_l3"]<-17234
 #frat$dates<-as.numeric(frat$dates)
 
 # u<-seqformat(DATA,var=c("nip","dates","end","TT"), from = "SPELL", to = "STS",id="nip",begin="dates",
 #              end="end",status = "TT",compressed = TRUE,process=TRUE,pdata=frat,pvar=c("nip","dates"))
 # 
 
 DATA$diff<-as.numeric(DATA$end-DATA$dates)/30.25
 
 
 D<-DATA[DATA$nip2==1,]

 le<-data.frame(sym=c("+ : progression","* : deces"), col=c("black","black"))
 
 
 for (i in unique(DATA$nip2)){
   DATA$h[DATA$nip2==i]<-c(nrow(DATA[DATA$nip2==i,]):1)
   
 }
 
 DATA$Chimiothrapie<-DATA$TT
 
 
 
 
 temp.plot<-ggplot(data=DATA, aes(y=diff, x=as.factor(nip2), fill=Chimiothrapie,group = h))+
   geom_bar(stat = "identity")+
   labs(title = "Séquence de chimiothérapie")+
   ylab("Délai depuis date de diagnostic stade IV (mois)")+
   xlab("Patients")+
   coord_flip()+
   scale_color_manual(breaks=le$sym, values=le$col)

  
 
 
 test<-data.frame(n=c("", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "", "*", "*", "", "*", "*", "*", "*", "*", 
                      "*", "*", "*", "*", "*", "", "*", "*", "*", "*", "*", "", "*", "*", "*", "*", "*", "*", "", "*", "*", 
                      "*"), xmin=1:42,ypos=dermato$delai_dc)
 
 
 for (ii in 1:nrow(test))
   {
     #display numbers at each visit
   temp.plot= temp.plot+ annotation_custom(grob = textGrob(test$n[ii]),
                                    xmin = test$x[ii],
                                    xmax = test$x[ii],
                                    ymin = test$ypos[ii],
                                    ymax = test$ypos[ii])

   }

 
 dermato$pfstrash<-ifelse(dermato$reponse_l1=="PROGRESSION" | dermato$response_chir1 %in% c("progression")|
                        dermato$reponse_l2=="PROGRESSION" & !is.na(dermato$reponse_l2)
                      |   dermato$reponse_l3=="PROGRESSION" & !is.na(dermato$reponse_l3)|
                        dermato$reponse_l4=="PROGRESSION" & !is.na(dermato$reponse_l4)|dermato$dn %in% c(3,5)
                      ,1,0)
 
 
 test<-data.frame(n=c("", "+", "+", "", "+", "+", "+", "+", "+", "+", "+", "", "+", "+", "", "", "+", "+", "+", "", 
                      "", "", "", "+", "", "", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "", "+", "+", "", 
                      "+"), xmin=1:42,ypos=dermato$delai_pfs)
 
 
 for (ii in 1:nrow(test))
 {
   #display numbers at each visit
   temp.plot= temp.plot+ annotation_custom(grob = textGrob(test$n[ii]),
                                           xmin = test$x[ii],
                                           xmax = test$x[ii],
                                           ymin = test$ypos[ii],
                                           ymax = test$ypos[ii])
   
 }
 
 
 
 
 dermato$chirt<-ifelse(!is.na(dermato$date_chir),1,0)
 
 
 # test<-data.frame(n=c("*", "*", "", "*", "", "", "", "*", "", "*", "*", "*", "*", "*", "", "*", "*", "*", "*", "*", 
 #                       "", "", "*", "", "*", "", "*", "*", "*", "*", "*", "*", "*", "*", "", "", "*", "", "*", "*", "", 
 #                       ""), xmin=1:42,ypos=difftime(dermato$date_chir,dermato$date_dia_iv))
 # 
 # 
 # for (ii in 1:nrow(test))
 # {
 #   #display numbers at each visit
 #   temp.plot= temp.plot+ annotation_custom(grob = textGrob(test$n[ii]),
 #                                           xmin = test$x[ii],
 #                                           xmax = test$x[ii],
 #                                           ymin = test$ypos[ii],
 #                                           ymax = test$ypos[ii])
 #   
 # }
 # 
 # 
 # 
 # 
 
 gt <- ggplot_gtable(ggplot_build(temp.plot))
 gt$layout$clip[gt$layout$name=="panel"] <- "off"
 
 pdf(file="C:/Users/adupont/Documents/thesemarie/rapport_dermato-fig9822.pdf")
plot(gt)
dev.off()


 ### En prenant en compte la chirurgie ### 

dermato$chir_emblee<-ifelse(dermato$chir_prim=="D'emblée" & difftime(dermato$date_chir,dermato$date_dia_iv)>=0,1,0)
table(dermato$chir_prim,dermato$chir_emblee,exclude=NULL)
