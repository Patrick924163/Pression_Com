df_etude <- read_delim("Scoring_Equip/df_etude.csv", 
                            delim = ";", escape_double = FALSE, col_types = cols(TOP_ASSUR_VIE = col_character(), 
TOP_DECES = col_character(), TOP_CAM = col_character(), TOP_TOUT_HABITAT = col_character(), TOP_HABITAT_MGEN = col_character(), 
TOP_OBS = col_character(), TOP_PIM = col_character(), TOP_PROTECT_JURIQ = col_character(), TOP_RETRAITE = col_character(), TOP_SANTE_ANIM = col_character(), 
COUVRE_BE = col_character(), COUVRE_BC = col_character()), trim_ws = TRUE)

df_etude$COUVRE_BE =   substr(df_etude$COUVRE_BE,1,1)

df_etude$COUVRE_BC =   substr(df_etude$COUVRE_BC,1,1)

facteur = append(names(df_etude %>% select(starts_with("TOP"))),c("COUVRE_BE","COUVRE_BC"))

df_etude[facteur] = lapply(df_etude[facteur], as.factor)


model0 = glm(TOP_RETRAITE~., data=df_etude[,-1],family=binomial(link=logit))

df_etude$proba_retraite = predict(model0,df_etude[,-1], type = "response")





df_etude = df_etude%>%filter((MTT_COT_AN>=273)&(Nbre_canaux_dispo!=0)&(Age>=18))

df_etude = df_etude%>%filter(! NAT_ACTIVITE %in% c("Sans activité","TNS et Agricole","Service Volontaire") )
dim(df_etude)
df_ca = CA(table(df_etude$NAT_ACTIVITE,df_etude$TYPE_COTISATION),ncp = 3, graph = FALSE)
summary(df_ca)
plot.CA(df_ca)


######################################## Nouvelle base et la bonne ####################


df_etude <- read_delim("Scoring_Equip/df_Rstudio_10_08.csv", col_types = cols(TOP_ASSUR_VIE = col_character(), 
                                                                                      TOP_DECES = col_character(), TOP_CAM = col_character(), TOP_TOUT_HABITAT = col_character(), TOP_HABITAT_MGEN = col_character(), 
                                                                                      TOP_OBS = col_character(), TOP_PIM = col_character(), TOP_PROTECT_JURIQ = col_character(), TOP_RETRAITE = col_character(), TOP_SANTE_ANIM = col_character(), 
                                                                                      COUVRE_BE = col_character(), COUVRE_BC = col_character())
           , delim = "|", escape_double = FALSE, trim_ws = TRUE)

df_etude = df_etude %>% select(!"NOIDE")


var_type <- data.frame(type=sapply(df_etude, class))
var_type$nom <- rownames(var_type)
rownames(var_type) <- NULL
var_type



df_acm = MCA(df_etude, ncp=5, graph = TRUE, quanti.sup = c(2,13,14,15,16,17,18,24,26,27,28,29,30), quali.sup = 10)


# On extrait les ?lements qui nous int?ressent, puis exportation en csv pour pouvoir mieux les manipuler dans Excel
output.var= cbind.data.frame(round(df_acm$var$coord[,1:5],2), round(df_acm$var$contrib[,1:5],2), round(df_acm$var$cos2[,1:5],2),round(df_acm$var$v.test[,1:5],2) )
write.csv2(output.var, file ="C:/Users/pkamga/Documents/Scoring_Equip/mca_score.csv")
output.var.sup= cbind.data.frame(round(df_acm$quali.sup$coord[,1:5],2), round(df_acm$quali.sup$cos2[,1:5],2),round(df_acm$quali.sup$v.test[,1:5],2))
write.csv2(output.var.sup, file ="D:/Home/ncostet/Documents/ENSEIGNEMENTS/_ENSAI mastere/ACM/TD Loisirs/mca_loisirs_resvarsup2.csv")
output.var.quanti.sup= cbind.data.frame(round(res.mca$quanti.sup$coord[,1:5],2))
write.csv2(output.var.quanti.sup, file ="D:/Home/ncostet/Documents/ENSEIGNEMENTS/_ENSAI mastere/ACM/TD Loisirs/mca_loisirs_resquantisup2.csv")

















