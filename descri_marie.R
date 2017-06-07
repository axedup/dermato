library(TraMineR)

patientd<-TABKRIS(baz=dermato[-14,],vect.var = c( "sexe", "immunodep", "vih", "trans", "plaie", 
                                           "ulcere", "epider", "brulure", "irradie", "prof", "uva", "phototype", 
                                           "autre_cancer", "insuff_organe", "comor_transplant", "maladie_chro", 
                                           "cardiovascu", "aap", "is", "ac","ecog_pre"),
                  vect.quali = rep(1,21),
                  varint=NULL,valvarint = NULL,
                  nomvarint = NULL,
                  test=NULL,
                  vecnoms=c("Sexe H/F", "immunoD", 
                            "CV VIH stade IV", "Transplant�", "Plaie chronique", "ulc�re chronique", 
                            "�pidermolyse bulleuse", "brulure", "zone irradi�e", "profession photoexpos�e", 
                            "PUVA/UVB", "phototype", "comorbidit�s : autre cancer", "comorbidit�s: insuff d'organe", 
                            "comorbidit�: transplant�", "comorbidit�: maladie chronique", 
                            "comorbidit�: cardiovascu", "TT associ�:  AAP", "TT associ�:  IS", 
                            "TT associ�:  AC","ECOG pr�-chimio"),valeurs=NULL,
                  vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=2)
                  

tumeursd<-TABKRIS(baz=dermato,vect.var = c( "loca_prim", "tete_cou", "gg", "meta", "gg_meta", 
                                            "pul_meta", "souscut_meta", "autre_meta", "ct"),
                  vect.quali = rep(1,20),
                  varint=NULL,valvarint = NULL,
                  nomvarint = NULL,
                  test=NULL,
                  vecnoms=c("Localisation primitif", "T�te et cou ", "Gg r�gionaux", 
                            "Meta synchrone ", "M�ta: gg � distance", 
                            "M�ta: pulmonaire", "M�ta: sous cutan�", 
                            "M�ta: autre", "cT"),valeurs=NULL,
                  vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=2)



histod<-TABKRIS(baz=dermato,vect.var = c( "ypt", "ypn", 
                                          "np", "ngg", "ggr", "histo", "epais", "diff", "epn", "emboles", 
                                          "marges_chir"),
                  vect.quali = c(1,1,0,rep(1,20)),
                  varint=NULL,valvarint = NULL,
                  nomvarint = NULL,
                  test=NULL,
                  vecnoms=c("ypT","ypN", " Nb N+", 
                            " Nb gg analys�s", "Rupture cap", 
                            "Histologie", "Epaissseur", "Diff�renciation", 
                            "EPN", "Emboles vasc", "Marges chir"),valeurs=NULL,
                  vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=2)



chird<-TABKRIS(baz=dermato,vect.var = c( "chir_prim", 
                                         "aeg_nc", "extlc_nc", "extm_nc", "refus_nc", "neoadj", 
                                         "ggsenti", "ggsentip", "curage"),
                vect.quali = c(rep(1,20)),
                varint=NULL,valvarint = NULL,
                nomvarint = NULL,
                test=NULL,
                vecnoms=c("Chirurgie primitif", "Motif non chir: AEG", "Motif non chir: extension lr", 
                          "Motif non chir: extension m�tastatique", "Motif non chir: refus patient", 
                          "TT n�o adjuvant", "GG sentinelle", 
                          "GG sentinelle +", "Curage"),valeurs=NULL,
                vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=2)


chimiod<-TABKRIS(baz=dermato,vect.var = c("type_pla", "tox_max", "rte", "rte_neo", 
                                           "rte_adj", "rte_meta", "rte_seule", "rct", "rte_loc", "dt_pri", 
                                           "dt_gg", "nb_lignestt","rte_tox", "renduop", "local"),
               vect.quali = c(1,rep(1,8),0,0,rep(1,9)),
               varint=NULL,valvarint = NULL,
               nomvarint = NULL,
               test=NULL,
               vecnoms=c("Type platine", "grade max toxicit�", 
                         "RTE", "RTE n�oadjuvante", "RTE adjuvante", "RTE m�tastatique", 
                         "RTE seule", "RCT concomittante", "RTE localisation", 
                         "Dose totale sur primitif (Gray)", "Dose totale sur gg (Gray)","Nbr lignes tt sys", 
                         "Toxicit� post RTE", "Rendu op�rable", 
                         "localisation progression/r�cidive"),valeurs=NULL,
               vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=2)

chimiod2<-TABKRIS(baz=dermato,vect.var = c("strategie1", "grp_tt1", "strategie2", "grp_tt2", 
                                           "strategie3", "grp_tt3", "strategie4", "grp_tt4"),
                 vect.quali = c(1,rep(1,9)),
                 varint=NULL,valvarint = NULL,
                 nomvarint = NULL,
                 test=NULL,
                 vecnoms=c("Strat�gie l1","Type l1","Strat�gie l2","Type l2","Strat�gie l3","Type l3"
                           ,"Strat�gie l4","Type l4"),valeurs=NULL,
                 vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=2)






outd<-TABKRIS(baz=dermato,vect.var = c( "br", "ligne_br"),
                 vect.quali = c(1,1),
                 varint=NULL,valvarint = NULL,
                 nomvarint = NULL,
                 test=NULL,
                 vecnoms=c("Meilleure r�ponse", "Lignes meilleure r�ponse"),valeurs=NULL,
                 vecrefs=NULL,varassoc=NULL, codassoc=NULL,langue="fr",digits=2)

mvad.labels=c("CT","CT+Erbi","Erbitux","RTE+CT","RTE+CT+Erbi","RT+Erbi","Rte seule","AntiPD1")


dermato<-dermato[order(dermato$grp_tt1,dermato$grp_tt2),]


mvad.seq<-seqdef(dermato, c(86,96))
mvad.seq2<-seqdef(dermato, c(86,96,104,111))

par('mar')->parametre

par(mar=par('mar')+c(0,0,0,3))
seqtab(mvad.seq, idxs = 0)
png("C:/Users/adupont/Documents/thesemarie/seqf2.png")
seqfplot(mvad.seq,weighted=FALSE,with.legend="right",idxs=0,border=NA,xtlab=c("1 �re ligne","2 nde ligne"))
dev.off()
png("C:/Users/adupont/Documents/thesemarie/seqi2.png")
seqiplot(mvad.seq,weighted=FALSE,with.legend="right",idxs=0,border=NA,xtlab=c("1 �re ligne","2 nde ligne"))
dev.off()

seqtab(mvad.seq, idxs = 0)

png("C:/Users/adupont/Documents/thesemarie/seqf.png")
seqfplot(mvad.seq2,weighted=FALSE,with.legend="right",idxs=0,border=NA,xtlab=c("1 �re ligne","2 nde ligne","3�me ligne","4�me ligne"))
dev.off()
png("C:/Users/adupont/Documents/thesemarie/seqi.png")
seqiplot(mvad.seq2,weighted=FALSE,with.legend="right",idxs=0,border=NA,xtlab=c("1 �re ligne","2 nde ligne","3�me ligne","4�me ligne"))
dev.off()