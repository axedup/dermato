library(TraMineR)

patientd<-TABKRIS(baz=dermato[-14,],vect.var = c( "sexe", "age_dia","immunodep", "vih", "trans", "plaie", 
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
#h<-rownames(nuo)
#nuo<-cbind(h,nuo)


### Survie ###

os<-Surv(event = dermato$decesf,time=dermato$delai_dc)
oss <- survfit( os ~ 1)
plot(oss,xlab="Délai depuis la date de diagnostic stade IV (mois)")


