library(TraMineR)

patientd<-TABKRIS(baz=dermato[,],vect.var = c( "sexe", "age_dia","immunodep", "vih", "trans", "plaie", 
                                           "ulcere", "epider", "brulure", "irradie", "prof", "uva", "phototype", 
                                           "autre_cancer", "insuff_organe", "comor_transplant", "maladie_chro", 
                                           "cardiovascu", "aap", "is", "ac","ecog_pre"),
                  vect.quali = c(1,0,rep(1,21)),
                  varint=NULL,valvarint = NULL,
                  nomvarint = NULL,
                  test=NULL,
                  vecnoms=c("Sexe H/F", "Age dia stade IV","immunoD", 
                            "CV VIH stade IV", "Transplanté", "Plaie chronique", "ulcère chronique", 
                            "épidermolyse bulleuse", "brulure", "zone irradiée", "profession photoexposée", 
                            "PUVA/UVB", "phototype", "comorbidités : autre cancer", "comorbidités: insuff d'organe", 
                            "comorbidité: transplanté", "comorbidité: maladie chronique", 
                            "comorbidité: cardiovascu", "TT associé:  AAP", "TT associé:  IS", 
                            "TT associé:  AC","ECOG pré-chimio"),valeurs=NULL,
                  vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=1)
                  

tumeursd<-TABKRIS(baz=dermato,vect.var = c( "loca_prim", "tete_cou", "gg", "meta", "gg_meta", 
                                            "pul_meta", "souscut_meta", "autre_meta", "ct"),
                  vect.quali = rep(1,20),
                  varint=NULL,valvarint = NULL,
                  nomvarint = NULL,
                  test=NULL,
                  vecnoms=c("Localisation primitif", "Tête et cou ", "Gg régionaux", 
                            "Meta synchrone ", "Méta: gg à distance", 
                            "Méta: pulmonaire", "Méta: sous cutané", 
                            "Méta: autre", "cT"),valeurs=NULL,
                  vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=1)



histod<-TABKRIS(baz=dermato,vect.var = c( "ypt", "ypn", 
                                          "np", "ngg", "ggr", "histo", "epais", "diff", "epn", "emboles", 
                                          "marges_chir"),
                  vect.quali = c(1,1,0,rep(1,20)),
                  varint=NULL,valvarint = NULL,
                  nomvarint = NULL,
                  test=NULL,
                  vecnoms=c("ypT","ypN", " Nb N+", 
                            " Nb gg analysés", "Rupture cap", 
                            "Histologie", "Epaissseur", "Différenciation", 
                            "EPN", "Emboles vasc", "Marges chir"),valeurs=NULL,
                  vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=1)



chird<-TABKRIS(baz=dermato,vect.var = c( "chir_prim", 
                                         "aeg_nc", "extlc_nc", "extm_nc", "refus_nc", "neoadj", 
                                         "ggsenti", "ggsentip", "curage"),
                vect.quali = c(rep(1,20)),
                varint=NULL,valvarint = NULL,
                nomvarint = NULL,
                test=NULL,
                vecnoms=c("Chirurgie primitif", "Motif non chir: AEG", "Motif non chir: extension lr", 
                          "Motif non chir: extension métastatique", "Motif non chir: refus patient", 
                          "TT néo adjuvant", "GG sentinelle", 
                          "GG sentinelle +", "Curage"),valeurs=NULL,
                vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=1)


chimiod<-TABKRIS(baz=dermato,vect.var = c("type_pla", "tox_max", "rte", "rte_neo", 
                                           "rte_adj", "rte_meta", "rte_seule", "rct", "rte_loc", "dt_pri", 
                                           "dt_gg", "nb_lignestt","rte_tox", "renduop", "local"),
               vect.quali = c(1,rep(1,8),0,0,rep(1,9)),
               varint=NULL,valvarint = NULL,
               nomvarint = NULL,
               test=NULL,
               vecnoms=c("Type platine", "grade max toxicité", 
                         "RTE", "RTE néoadjuvante", "RTE adjuvante", "RTE métastatique", 
                         "RTE seule", "RCT concomittante", "RTE localisation", 
                         "Dose totale sur primitif (Gray)", "Dose totale sur gg (Gray)","Nbr lignes tt sys", 
                         "Toxicité post RTE", "Rendu opérable", 
                         "localisation progression/récidive"),valeurs=NULL,
               vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=1)

chimiod2<-TABKRIS(baz=dermato,vect.var = c("strategie1", "grp_tt1","reponse_l1", 
                                           "strategie2", "grp_tt2","reponse_l2" ,
                                           "strategie3", "grp_tt3","reponse_l3" ,
                                           "strategie4", "grp_tt4","reponse_l4" ),
                 vect.quali = c(1,rep(1,15)),
                 varint=NULL,valvarint = NULL,
                 nomvarint = NULL,
                 test=NULL,
                 vecnoms=c("Stratégie l1","Type l1","Reponse l1","Stratégie l2","Type l2",
                           "Réponse l2",
                           "Stratégie l3","Type l3","Reponse l3"
                           ,"Stratégie l4","Type l4","Reponse l4"),valeurs=NULL,
                 vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=1)



chimiod3<-TABKRIS(baz=dermato,vect.var = c( "grp_tt1","reponse_l1", "chir1",
                                           "strategie2", "grp_tt2","reponse_l2" , "chir2",
                                           "strategie3", "grp_tt3","reponse_l3" ,
                                           "strategie4", "grp_tt4","reponse_l4" ),
                  vect.quali = c(1,rep(1,15)),
                  varint=c("strategie1"),valvarint = c("NéoAdj","Méta/Palliatif"),
                  nomvarint = c("Stratégie 1ère ligne"),
                  test=NULL,
                  vecnoms=c("Type l1","Reponse l1","Chir","Stratégie l2","Type l2",
                            "Réponse l2","Chir2",
                            "Stratégie l3","Type l3","Reponse l3"
                            ,"Stratégie l4","Type l4","Reponse l4"),valeurs=NULL,
                  vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=1)


outd<-TABKRIS(baz=dermato,vect.var = c( "br", "ligne_br"),
                 vect.quali = c(1,1),
                 varint=NULL,valvarint = NULL,
                 nomvarint = NULL,
                 test=NULL,
                 vecnoms=c("Meilleure réponse", "Lignes meilleure réponse"),valeurs=NULL,
                 vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=2)

mvad.labels=c("CT","CT+Erbi","Erbitux","RTE+CT","RTE+CT+Erbi","RT+Erbi","Rte seule","AntiPD1")


dermato<-dermato[order(dermato$grp_tt1,dermato$grp_tt2),]


mvad.seq<-seqdef(dermato, c(86,96))
mvad.seq2<-seqdef(dermato, c(86,96,104,111))

par('mar')->parametre

par(mar=par('mar')+c(0,0,0,3))
seqtab(mvad.seq, idxs = 0)
png("C:/Users/adupont/Documents/thesemarie/seqf2.png")
seqfplot(mvad.seq,weighted=FALSE,with.legend="right",idxs=0,border=NA,xtlab=c("1 ère ligne","2 nde ligne"))
dev.off()
png("C:/Users/adupont/Documents/thesemarie/seqi2.png")
seqiplot(mvad.seq,weighted=FALSE,with.legend="right",idxs=0,border=NA,xtlab=c("1 ère ligne","2 nde ligne"))
dev.off()

seqtab(mvad.seq, idxs = 0)

png("C:/Users/adupont/Documents/thesemarie/seqf.png")
seqfplot(mvad.seq2,weighted=FALSE,with.legend="right",idxs=0,border=NA,xtlab=c("1 ère ligne","2 nde ligne","3ème ligne","4ème ligne"))
dev.off()
png("C:/Users/adupont/Documents/thesemarie/seqi.png")
seqiplot(mvad.seq2,weighted=FALSE,with.legend="right",idxs=0,border=NA,xtlab=c("1 ère ligne","2 nde ligne","3ème ligne","4ème ligne"))
dev.off()

b<-attributes(seqtab(mvad.seq, idxs = 0))$freq
b$Percent<-round(b$Percent,1)
rownames(b)<-gsub(pattern = "/1", replacement = "",  x = rownames(b))

mvad.seqna<-seqdef(dermato, c(86,96),right=NA)

seqtrate(mvad.seqna,with.missing = TRUE)
nuo<-seqtrate(mvad.seqna,with.missing = TRUE)



r2<-function(x){round(x,2)}

nuo2<-apply(nuo[,],2,function(x){as.numeric(as.character(x))})
nuo2<-apply(nuo2,2,function(x){round(x,1)})

L1<-c("CT","CT+Erbi","Erbitux","RTE seule","RTE+CT","RTE+CT+Erbi","RTE+Erbi","Pas de chimio")
nuo2<-cbind(L1,nuo2)


### Survie et PFS###

os<-Surv(event = dermato$decesf,time=dermato$delai_dc)
oss <- survfit( os ~ 1)

median_os<- capture.output(oss)
median_os<-strsplit(median_os[4], "   ")[[1]]


plot(oss,xlab="Délai depuis la date de diagnostic stade IV (mois)")

re<-summary(oss,censored = TRUE)

censure<-as.data.frame(cbind(re$time[re$n.event==0],re$surv[re$n.event==0] ))
colnames(censure)<-c("time","ce")
evenement<-as.data.frame(cbind(re$time,re$surv ))
colnames(evenement)<-c("time","ev")
debut<-data.frame(time=0,ev=1)
evenement<-rbind(debut,evenement)



intervalle<-as.data.frame(cbind(re$time,re$upper
                                ,re$lower ))
colnames(intervalle)<-c("time","haut","bas")

pfs<-Surv(event = dermato$pfst,time=dermato$delai_pfs)
pfss <- survfit( pfs ~ 1)
plot(pfss,xlab="Délai depuis la date de diagnostic stade IV (mois)")

median_pfs<- capture.output(pfss)
median_pfs<-strsplit(median_pfs[4], "   ")[[1]]

ref<-summary(pfss,censored = TRUE)

censurpf<-as.data.frame(cbind(ref$time[ref$n.event==0],ref$surv[ref$n.event==0] ))
colnames(censurpf)<-c("time","ce")
evenementpf<-as.data.frame(cbind(ref$time,ref$surv ))
colnames(evenementpf)<-c("time","ev")
debut<-data.frame(time=0,ev=1)
evenementpf<-rbind(debut,evenementpf)


intervallepf<-as.data.frame(cbind(ref$time,ref$upper
                                ,ref$lower ))
colnames(intervallepf)<-c("time","haut","bas")


km_os<-ggplot()+ geom_step(data=evenement,aes(x=time, y=ev),color="black", direction="hv")  +
  #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  geom_step(data=intervalle,aes(x=time, y=haut),color="black" ,direction="hv",linetype="dashed")+
  geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv",linetype="dashed")+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70),expand = c(0, 0),limits=c(0,max(re$time)))+
 
  
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("DurÃ©e de vie des implants") +
  xlab("Délai depuis diagnostic stade IV (Mois) ")+
  ylab("Probabilite")+
  #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
  #
  #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("")









test= data.frame(

  x = c(3.5,10,20,30,40,50,60),
  y = c(33,25,27,36,23,25,67),
  n=c(42,re$n.risk[which.min((re$time-10)<0)-1],re$n.risk[which.min((re$time-20)<0)-1],
      re$n.risk[which.min((re$time-30)<0)-1],
      re$n.risk[which.min((re$time-40)<0)-1],
      re$n.risk[which.min((re$time-50)<0)-1],
      re$n.risk[which.min((re$time-60)<0)-1]),
  ypos=c(0.04,0.04,0.04,0.04,0.04,0.04,0.04)

)


for (ii in 1:nrow(test))
{
  #display numbers at each visit
  km_os=km_os+ annotation_custom(grob = textGrob(test$n[ii]),
                                 xmin = test$x[ii],
                                 xmax = test$x[ii],
                                 ymin = test$ypos[ii],
                                 ymax = test$ypos[ii])

}


km_os=km_os+annotation_custom(grob = textGrob("Nbr à risque : "),
                              xmin = 5,
                              xmax = 5,
                              ymin = 0.1,
                              ymax = 0.1)



gtt <- ggplot_gtable(ggplot_build(km_os))
gtt$layout$clip[gtt$layout$name=="panel"] <- "off"


km_pfs<-ggplot()+ geom_step(data=evenementpf,aes(x=time, y=ev),color="black", direction="hv")  +
  #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  #geom_step(data=intervalle,aes(x=time, y=haut),color="black" ,direction="hv",linetype="dashed")+
  #geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv",linetype="dashed")+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70),expand = c(0, 0),limits=c(0,max(ref$time)))+
  
  
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("DurÃ©e de vie des implants") +
  xlab("Délai depuis diagnostic stade IV (Mois) ")+
  ylab("Probabilite")+
  #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
  #
  #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("")


### cHIR d'emblée avant ###


os<-Surv(event = dermato$decesf,time=dermato$delai_dc)
chire <- survfit( os ~  dermato$chir_emblee)
plot(chire,xlab="Délai depuis la date de diagnostic stade IV (mois)")

chires<-summary(chire,censored = TRUE)

# censure<-as.data.frame(cbind(re$time[re$n.event==0],re$surv[re$n.event==0] ))
# colnames(censure)<-c("time","ce")
evenementnonchir<-as.data.frame(cbind(chires$time[chires$strata=="dermato$chir_emblee=0"],
                                      chires$surv[chires$strata=="dermato$chir_emblee=0"] ))
colnames(evenementnonchir)<-c("time","ev")
debut<-data.frame(time=0,ev=1)
evenementnonchir<-rbind(debut,evenementnonchir)

evenementchir<-as.data.frame(cbind(chires$time[chires$strata=="dermato$chir_emblee=1"],
                                      chires$surv[chires$strata=="dermato$chir_emblee=1"] ))
colnames(evenementchir)<-c("time","ev")
debut<-data.frame(time=0,ev=1)
evenementchir<-rbind(debut,evenementchir)




# intervalle<-as.data.frame(cbind(re$time,re$upper
#                                 ,re$lower ))
# colnames(intervalle)<-c("time","haut","bas")
# 


km_os_ch<-ggplot()+ geom_step(data=evenementnonchir,aes(x=time, y=ev),color="black", direction="hv")  +
  #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  geom_step(data=evenementchir,aes(x=time, y=ev),color="blue" ,direction="hv",linetype="dashed")+
  #geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv",linetype="dashed")+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70),expand = c(0, 0),limits=c(0,max(evenementnonchir$time)+1))+
  
  
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("DurÃ©e de vie des implants") +
  xlab("Délai depuis diagnostic stade IV (Mois) ")+
  ylab("Probabilite")+
  #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
  #
  #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  annotate("text",label="Pas de chirurgie d'emblée au stade IV", y=0.3,x=50)+
  annotate("text",label="Chirurgie d'emblée au stade IV", y=0.05,x=40,color="blue")+
  ggtitle("")






#### 1ère ligne####


os1<-Surv(event = dermato$d1,time=dermato$delai_deces1)
oss1 <- survfit( os1 ~ 1)

median_osl1<- capture.output(oss1)
median_osl1<-strsplit(median_osl1[4], "   ")[[1]]

plot(oss1,xlab="Délai depuis J0 ligne1 (mois)")

re1<-summary(oss1,censored = TRUE)

censure1<-as.data.frame(cbind(re1$time[re1$n.event==0],re1$surv[re1$n.event==0] ))
colnames(censure1)<-c("time","ce")
evenement1<-as.data.frame(cbind(re1$time,re1$surv ))
colnames(evenement1)<-c("time","ev")
intervalle1<-as.data.frame(cbind(re1$time,re1$upper
                                ,re1$lower ))
colnames(intervalle1)<-c("time","haut","bas")


spfs1<-Surv(event = dermato$pfs,time=dermato$delai_pfs1)
spfss1 <- survfit( spfs1 ~ 1)

median_pfsl1<- capture.output(spfss1)
median_pfsl1<-strsplit(median_pfsl1[4], "  ")[[1]]

plot(spfss1,xlab="Délai depuis J0 ligne1 (mois)")

pf1<-summary(spfss1,censored = TRUE)

censurepf1<-as.data.frame(cbind(pf1$time[pf1$n.event==0],pf1$surv[pf1$n.event==0] ))
colnames(censurepf1)<-c("time","ce")
evenementpf1<-as.data.frame(cbind(pf1$time,pf1$surv ))
colnames(evenementpf1)<-c("time","ev")
intervallepf1<-as.data.frame(cbind(pf1$time,pf1$upper
                                 ,pf1$lower ))
colnames(intervallepf1)<-c("time","haut","bas")




km_pfs1_os1<-ggplot()+ geom_step(data=evenement1,aes(x=time, y=ev),color="black", direction="hv")  +
  #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  geom_step(data=evenementpf1,aes(x=time, y=ev),color="black", direction="hv",linetype="dashed")+

  
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70),expand = c(0, 0),limits=c(0,80))+
  
  
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("DurÃ©e de vie des implants") +
  xlab("Délai depuis J0 ligne 1 (Mois) ")+
  ylab("Probabilite")+
  #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
  #
  #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  annotate("text",label="OS", y=0.25,x=50)+
  annotate("text",label="PFS", y=0.13,x=50)+
  ggtitle("")



test= data.frame(
  
  x = c(2.5,10,20,30,40,50,60),
  y = c(33,25,27,36,23,25,67),
  n=c(42,re1$n.risk[which.min((re1$time-10)<0)-1],re1$n.risk[which.min((re1$time-20)<0)-1],
      re1$n.risk[which.min((re1$time-30)<0)-1],
      re1$n.risk[which.min((re1$time-40)<0)-1],
      re1$n.risk[which.min((re1$time-50)<0)-1],
      re1$n.risk[which.min((re1$time-60)<0)-1]),
  ypos=c(0.04,0.04,0.04,0.04,0.04,0.04,0.04)
  
)


for (ii in 1:nrow(test))
{
  #display numbers at each visit
  km_pfs1_os1=km_pfs1_os1+ annotation_custom(grob = textGrob(test$n[ii]),
                                 xmin = test$x[ii],
                                 xmax = test$x[ii],
                                 ymin = test$ypos[ii],
                                 ymax = test$ypos[ii])
  
}


km_pfs1_os1=km_pfs1_os1+annotation_custom(grob = textGrob(""),
                              xmin = 5,
                              xmax = 5,
                              ymin = 0.1,
                              ymax = 0.1)



gttl1 <- ggplot_gtable(ggplot_build(km_pfs1_os1))
gttl1$layout$clip[gttl1$layout$name=="panel"] <- "off"





#### 2EME ligne####


os2<-Surv(event = dermato$d2[!is.na(dermato$date_l2)],time=dermato$delai_deces2[!is.na(dermato$date_l2)])
oss2 <- survfit( os2 ~ 1)
plot(oss2,xlab="Délai depuis J0 ligne1 (mois)")

re2<-summary(oss2,censored = TRUE)

censure2<-as.data.frame(cbind(re2$time[re2$n.event==0],re2$surv[re2$n.event==0] ))
colnames(censure2)<-c("time","ce")
evenement2<-as.data.frame(cbind(re2$time,re2$surv ))
colnames(evenement2)<-c("time","ev")
debut<-data.frame(time=0,ev=1)
evenement2<-rbind(debut,evenement2)


intervalle2<-as.data.frame(cbind(re2$time,re2$upper
                                 ,re2$lower ))
colnames(intervalle2)<-c("time","haut","bas")


spfs2<-Surv(event = dermato$pfs2[!is.na(dermato$date_l2)],time=dermato$delai_pfs2[!is.na(dermato$date_l2)])
spfss2 <- survfit( spfs2 ~ 1)
plot(spfss2,xlab="Délai depuis J0 ligne2 (mois)")

pf2<-summary(spfss2,censored = TRUE)

censurepf2<-as.data.frame(cbind(pf2$time[pf2$n.event==0],pf2$surv[pf2$n.event==0] ))
colnames(censurepf2)<-c("time","ce")
evenementpf2<-as.data.frame(cbind(pf2$time,pf2$surv ))
colnames(evenementpf2)<-c("time","ev")
debut<-data.frame(time=0,ev=1)
evenementpf2<-rbind(debut,evenementpf2)


intervallepf2<-as.data.frame(cbind(pf2$time,pf2$upper
                                   ,pf2$lower ))
colnames(intervallepf2)<-c("time","haut","bas")




km_pfs2_os2<-ggplot()+ geom_step(data=evenement2,aes(x=time, y=ev),color="black", direction="hv")  +
  #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  geom_step(data=evenementpf2,aes(x=time, y=ev),color="black", direction="hv",linetype="dashed")+
  
  
  scale_x_continuous(breaks=c(0,4,8,12,15),expand = c(0, 0),limits=c(0,max(re2$time)))+
  
  
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("DurÃ©e de vie des implants") +
  xlab("Délai depuis J0 ligne 2 (Mois) ")+
  ylab("Probabilite")+
  #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
  #
  #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  annotate("text",label="OS", y=0.8,x=10)+
  annotate("text",label="PFS", y=0.2,x=10)+
  ggtitle("")

