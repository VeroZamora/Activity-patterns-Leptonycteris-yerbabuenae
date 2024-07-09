library(tidyverse)
library(readxl)
library(glmmTMB)
cacachila<-read_csv("cacachilaNew.csv") #new and correct data
roosting<-read_csv("roosting.csv")
foraging<-read_csv("foraging.csv")

cacachila %>% 
  filter(time_spent_action == "foraging")

#########Metric: hours of activity############
#1 STEP: DO THE MODEL FORMULAS FOR EACH METRIC: hours of activity
m_formulas<- data.frame(model_number = c("m0", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8","m9", "m10", "m11", "m12", "m13", "m14"),
                        model_formula = c("activity ~ (1|id)+ (1|year)", #m0
                                          "activity ~ S + (1|id) + (1|year)", #m1
                                          "activity ~ S + Disp +(1|id) + (1|year)", #m2
                                          "activity ~ S + Temp + (1|id) + (1|year)", #m3
                                          "activity ~ S + mm + temp + Disp + (1|id)+ (1|year)", #m4
                                          "activity ~ S + Disp + Temp + (1|id)+ (1|year)", #m5
                                          "activity ~ S + Disp + Temp + mm + (1|id)+ (1|year)", #m6
                                          "activity ~ S + Disp + mm + (1|id)+ (1|year)", #m7
                                          "activity ~ Disp + (1|id) + (1|year)", #m8
                                          "activity ~ Disp + Temp + (1|id) + (1|year)", #m9
                                          "activity ~ Disp + Temp + mm + (1|id) + (1|year)", #m10
                                          "activity ~ Disp + mm + (1|id)+ (1|year)", #m11
                                          "activity ~ Temp +(1|id) + (1|year)", #m12
                                          "activity ~ Temp + mm + (1|id) + (1|year)", #m13
                                          "activity ~ mm + (1|id) + (1|year)" #m14
                        )) 
m0 <- glmmTMB(data = cacachila, TF_hora ~  (1|id) + (1|year), family = Gamma(link = "log"))
m1<-glmmTMB(data=cacachila, TF_hora~sex+ (1|id)+ (1|year), family = Gamma(link = "log"))
m2<-glmmTMB(data=cacachila, TF_hora~sex + Disp+ (1|id)+ (1|year), family = Gamma(link = "log"))
m3<-glmmTMB(data=cacachila, TF_hora~sex+ Disp+ (1|id) + (1|year), family = Gamma(link = "log"))
m4<-glmmTMB(data=cacachila, TF_hora~sex+ prep + temp + Disp+ (1|id)+ (1|year), family = Gamma(link = "log"))
m5<-glmmTMB(data=cacachila, TF_hora~sex+ Disp + temp + (1|id)+ (1|year), family = Gamma(link = "log"))
m6<-glmmTMB(data=cacachila, TF_hora~sex+ Disp + temp + prep + (1|id)+ (1|year), family = Gamma(link = "log"))
m7<-glmmTMB(data=cacachila, TF_hora~sex+ Disp + prep + (1|id)+ (1|year), family = Gamma(link = "log"))
m8<-glmmTMB(data=cacachila, TF_hora~Disp + (1|id)+ (1|year), family = Gamma(link = "log"))
m9<-glmmTMB(data=cacachila, TF_hora~Disp + temp + (1|id)+ (1|year), family = Gamma(link = "log"))
m10<-glmmTMB(data=cacachila,TF_hora~Disp + temp + prep+ (1|id)+ (1|year), family = Gamma(link = "log"))
m11<-glmmTMB(data=cacachila,TF_hora~Disp + prep+ (1|id)+ (1|year), family = Gamma(link = "log"))
m12<-glmmTMB(data=cacachila,TF_hora~temp + (1|id)+ (1|year), family = Gamma(link = "log"))
m13<-glmmTMB(data=cacachila,TF_hora~temp + prep+ (1|id)+ (1|year), family = Gamma(link = "log"))
m14<-glmmTMB(data=cacachila,TF_hora~Disp + prep + (1|id)+ (1|year), family = Gamma(link = "log"))

#########2 STEP: GET THE AIC TABLE AND AICDELTA OF MODELS#######
#Metric: hours of activity
aic<- AICtab(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14) %>% 
  as.data.frame() %>% 
  mutate(dAIC = round(dAIC, 2), # AIC delta
         response = "Hours of Activity") %>% 
  rownames_to_column("model_number") %>% 
  left_join(m_formulas)

NTF<-m4 #the best model of Hours of Activity: only 

table_TF <- summary(NTF)$coefficients$cond %>% 
  as.data.frame() %>% 
  mutate_if(is.numeric, ~signif(., 2)) %>% 
  rownames_to_column("Variable") %>% 
  mutate(Response = "Hours of Activity")
#######3 STEP: GET THE MODEL COMPARISONS USING AICDELTA FOR EACH METRIC##############
#HOURS OF ACTIVITY METRIC
require(MuMIn)
options(na.action = "na.fail") 

globalmodelTF <- glmmTMB(data=cacachila, TF_hora~sex+ Disp + temp + prep + (1|id)+ (1|year), family = Gamma(link = "log"))
combinations <- dredge(globalmodelTF) 
TF<-print(combinations)
coefTable(combinations) #Is missing the p-value
summary(combinations) 
# EXPORT AIC TABLE
writexl::write_xlsx(TF, paste0(here::here(), "/aicTF.xlsx"))


#1 STEP: DO THE MODEL FORMULAS FOR EACH METRIC: Hour of emergence
cacachila <- read.csv("cacachilaActivity.csv")

e_formulas<- data.frame(model_number = c("m0", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8","m9", "m10", "m11", "m12", "m13", "m14"),
                        model_formula = c("emergence ~ (1|id)+ (1|year)", #m0
                                          "emergence ~ S + (1|id) + (1|year)", #m1
                                          "emergence ~ S + Disp +(1|id) + (1|year)", #m2
                                          "emergence ~ S + Temp + (1|id) + (1|year)", #m3
                                          "emergence ~ S + mm + (1|id)+ (1|year)", #m4
                                          "emergence ~ S + Disp + Temp + (1|id)+ (1|year)", #m5
                                          "emergence ~ S + Disp + Temp + mm + (1|id)+ (1|year)", #m6
                                          "emergence ~ S + Disp + mm + (1|id)+ (1|year)", #m7
                                          "emergence ~ Disp + (1|id) + (1|year)", #m8
                                          "emergence ~ Disp + Temp + (1|id) + (1|year)", #m9
                                          "emergence ~ Disp + Temp + mm + (1|id) + (1|year)", #m10
                                          "emergence ~ Disp + mm + (1|id)+ (1|year)", #m11
                                          "emergence ~ Temp +(1|id) + (1|year)", #m12
                                          "emergence ~ Temp + mm + (1|id) + (1|year)", #m13
                                          "emergence ~ mm + (1|id) + (1|year)" #m14
                        )) 
m0 <- glmmTMB(data = cacachila, min_sun_emer ~  (1|id) + (1|year), family = Gamma(link = "log"))
m1<-glmmTMB(data=cacachila, min_sun_emer~sex+ (1|id)+ (1|year), family = Gamma(link = "log"))
m2<-glmmTMB(data=cacachila, min_sun_emer~sex + Disp+ (1|id)+ (1|year), family = Gamma(link = "log"))
m3<-glmmTMB(data=cacachila, min_sun_emer~sex+ Disp+ (1|id) + (1|year), family = Gamma(link = "log"))
m4<-glmmTMB(data=cacachila, min_sun_emer~sex+ prep + temp + Disp+ (1|id)+ (1|year), family = Gamma(link = "log"))
m5<-glmmTMB(data=cacachila, min_sun_emer~sex+ Disp + temp + (1|id)+ (1|year), family = Gamma(link = "log"))
m6<-glmmTMB(data=cacachila, min_sun_emer~sex+ Disp + temp + prep + (1|id)+ (1|year), family = Gamma(link = "log"))
m7<-glmmTMB(data=cacachila, min_sun_emer~sex+ Disp + prep + (1|id)+ (1|year), family = Gamma(link = "log"))
m8<-glmmTMB(data=cacachila, min_sun_emer~Disp + (1|id)+ (1|year), family = Gamma(link = "log"))
m9<-glmmTMB(data=cacachila, min_sun_emer~Disp + temp + (1|id)+ (1|year), family = Gamma(link = "log"))
m10<-glmmTMB(data=cacachila,min_sun_emer~Disp + temp + prep+ (1|id)+ (1|year), family = Gamma(link = "log"))
m11<-glmmTMB(data=cacachila,min_sun_emer~Disp + prep+ (1|id)+ (1|year), family = Gamma(link = "log"))
m12<-glmmTMB(data=cacachila,min_sun_emer~temp + (1|id)+ (1|year), family = Gamma(link = "log"))
m13<-glmmTMB(data=cacachila,min_sun_emer~temp + prep+ (1|id)+ (1|year), family = Gamma(link = "log"))
m14<-glmmTMB(data=cacachila,min_sun_emer~Disp + prep + (1|id)+ (1|year), family = Gamma(link = "log"))

#########2 STEP: GET THE AIC TABLE AND AICDELTA OF MODELS#######
#Metric: Hour of emergence
aicHE<- AICtab(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14) %>% 
  as.data.frame() %>% 
  mutate(dAIC = round(dAIC, 2), # AIC delta
         response = "Hour of Emergence") %>% 
  rownames_to_column("model_number") %>% 
  left_join(e_formulas)

mhe<-m7 #change name to the best model

table_HE<- summary(mhe)$coefficients$cond %>% 
  as.data.frame() %>% 
  mutate_if(is.numeric, ~signif(., 2)) %>% 
  rownames_to_column("Variable") %>% 
  mutate(Response = "Hour of Emergence")

globalmodelHE <- glmmTMB(data=cacachila, emer_dec~sex+ Disp + temp + prep + (1|id)+ (1|year), family = Gamma(link = "log"))
combinations <- dredge(globalmodelHE) 
HE<-print(combinations)
coefTable(combinations) #Is missing the p-value
summary(combinations) 
# EXPORT AIC TABLE
writexl::write_xlsx(HA, paste0(here::here(), "/aicHE.xlsx"))

#1 STEP: DO THE MODEL FORMULAS FOR EACH METRIC: RETURNS TO THE ROOSTS
return_formulas<- data.frame(model_number = c("m0", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8","m9", "m10", "m11", "m12", "m13", "m14"),
                          model_formula = c("returns ~ (1|id)+ (1|year)", #m0
                                            "returns ~ S + (1|id) + (1|year)", #m1
                                            "returns ~ S + Disp +(1|id) + (1|year)", #m2
                                            "returns ~ S + Temp + (1|id) + (1|year)", #m3
                                            "returns ~ S + mm + (1|id)+ (1|year)", #m4
                                            "returns ~ S + Disp + Temp + (1|id)+ (1|year)", #m5
                                            "returns ~ S + Disp + Temp + mm + (1|id)+ (1|year)", #m6
                                            "returns ~ S + Disp + mm + (1|id)+ (1|year)", #m7
                                            "returns ~ Disp + (1|id) + (1|year)", #m8
                                            "returns ~ Disp + Temp + (1|id) + (1|year)", #m9
                                            "returns ~ Disp + Temp + mm + (1|id) + (1|year)", #m10
                                            "returns ~ Disp + mm + (1|id)+ (1|year)", #m11
                                            "returns ~ Temp +(1|id) + (1|year)", #m12
                                            "returns ~ Temp + mm + (1|id) + (1|year)", #m13
                                            "returns ~ mm + (1|id) + (1|year)" #m14
                          )) 
#VRR is the column of the times bats return to the roost on the excel file
m0 <- glmmTMB(data = cacachila, no_detects_by_night ~  (1|id) + (1|year), family = nbinom2)
m1<-glmmTMB(data=cacachila, no_detects_by_night~sex+ (1|id)+ (1|year), family = nbinom2)
m2<-glmmTMB(data=cacachila, no_detects_by_night~sex + Disp+ (1|id)+ (1|year), family = nbinom2)
m3<-glmmTMB(data=cacachila, no_detects_by_night~sex+ Disp+ (1|id) + (1|year), family = nbinom2)
m4<-glmmTMB(data=cacachila, no_detects_by_night~sex+ prep + temp + Disp+ (1|id)+ (1|year), family = nbinom2)
m5<-glmmTMB(data=cacachila, no_detects_by_night~sex+ Disp + temp + (1|id)+ (1|year), family = nbinom2)
m6<-glmmTMB(data=cacachila, no_detects_by_night~sex+ Disp + temp + prep + (1|id)+ (1|year), family = nbinom2)
m7<-glmmTMB(data=cacachila, no_detects_by_night~sex+ Disp + prep + (1|id)+ (1|year), family = nbinom2)
m8<-glmmTMB(data=cacachila, no_detects_by_night~Disp + (1|id)+ (1|year), family = nbinom2)
m9<-glmmTMB(data=cacachila, no_detects_by_night~Disp + temp + (1|id)+ (1|year), family = nbinom2)
m10<-glmmTMB(data=cacachila,no_detects_by_night~Disp + temp + prep+ (1|id)+ (1|year), family = nbinom2)
m11<-glmmTMB(data=cacachila,no_detects_by_night~Disp + prep+ (1|id)+ (1|year), family = nbinom2)
m12<-glmmTMB(data=cacachila,no_detects_by_night~temp + (1|id)+ (1|year), family = nbinom2)
m13<-glmmTMB(data=cacachila,no_detects_by_night~temp + prep+ (1|id)+ (1|year), family = nbinom2)
m14<-glmmTMB(data=cacachila,no_detects_by_night~Disp + prep + (1|id)+ (1|year), family = nbinom2)

aicreturns<- AICtab(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14) %>% 
  as.data.frame() %>% 
  mutate(dAIC = round(dAIC, 2), #AIC delta
         response = "Frequency of returns") %>% 
  rownames_to_column("model_number") %>% 
  left_join(return_formulas)

mvrr<-m5

table_vrr <- summary(mvrr)$coefficients$cond %>% 
  as.data.frame() %>% 
  mutate_if(is.numeric, ~signif(., 2)) %>% 
  rownames_to_column("Variable") %>% 
  mutate(Response = "Returns")


#############GLMM OF HOURS OF ROOSTING#################
roost_formulas<- data.frame(model_number = c("m0", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8","m9", "m10", "m11", "m12", "m13", "m14"),
                        model_formula = c("hours roosting ~ (1|id)+ (1|year)", #m0
                                          "hours roosting ~ S + (1|id) + (1|year)", #m1
                                          "hours roosting ~ S + Disp +(1|id) + (1|year)", #m2
                                          "hours roosting ~ S + Temp + (1|id) + (1|year)", #m3
                                          "hours roosting ~ S + mm + (1|id)+ (1|year)", #m4
                                          "hours roosting ~ S + Disp + Temp + (1|id)+ (1|year)", #m5
                                          "hours roosting ~ S + Disp + Temp + mm + (1|id)+ (1|year)", #m6
                                          "hours roosting ~ S + Disp + mm + (1|id)+ (1|year)", #m7
                                          "hours roosting ~ Disp + (1|id) + (1|year)", #m8
                                          "hours roosting ~ Disp + Temp + (1|id) + (1|year)", #m9
                                          "hours roosting ~ Disp + Temp + mm + (1|id) + (1|year)", #m10
                                          "hours roosting ~ Disp + mm + (1|id)+ (1|year)", #m11
                                          "hours roosting ~ Temp +(1|id) + (1|year)", #m12
                                          "hours roosting ~ Temp + mm + (1|id) + (1|year)", #m13
                                          "hours roosting ~ mm + (1|id) + (1|year)" #m14
                        )) 

m0 <- glmmTMB(data= roosting, TF_hora ~  (1|id) + (1|year), family = Gamma(link = "log"))
m1<-glmmTMB(data=roosting, TF_hora ~sex+ (1|id)+ (1|year), family = Gamma(link = "log"))
m2<-glmmTMB(data=roosting, TF_hora~sex + Disp+ (1|id)+ (1|year), family = Gamma(link = "log"))
m3<-glmmTMB(data=roosting, TF_hora~sex+ Disp+ (1|id) + (1|year), family = Gamma(link = "log"))
m4<-glmmTMB(data=roosting, TF_hora~sex+ prep + temp + Disp+ (1|id)+ (1|year), family = Gamma(link = "log"))
m5<-glmmTMB(data=roosting, TF_hora~sex+ Disp + temp + (1|id)+ (1|year), family = Gamma(link = "log"))
m6<-glmmTMB(data=roosting, TF_hora~sex+ Disp + temp + prep + (1|id)+ (1|year), family = Gamma(link = "log"))
m7<-glmmTMB(data=roosting, TF_hora~sex+ Disp + prep + (1|id)+ (1|year), family = Gamma(link = "log"))
m8<-glmmTMB(data=roosting, TF_hora~Disp + (1|id)+ (1|year), family = Gamma(link = "log"))
m9<-glmmTMB(data=roosting, TF_hora~Disp + temp + (1|id)+ (1|year), family = Gamma(link = "log"))
m10<-glmmTMB(data=roosting, TF_hora~Disp + temp + prep+ (1|id)+ (1|year), family = Gamma(link = "log"))
m11<-glmmTMB(data=roosting, TF_hora~Disp + prep+ (1|id)+ (1|year), family = Gamma(link = "log"))
m12<-glmmTMB(data=roosting, TF_hora~temp + (1|id)+ (1|year), family = Gamma(link = "log"))
m13<-glmmTMB(data=roosting, TF_hora~temp + prep+ (1|id)+ (1|year), family = Gamma(link = "log"))
m14<-glmmTMB(data=roosting, TF_hora~Disp + prep + (1|id)+ (1|year), family = Gamma(link = "log"))

#########3ER PASO ES SACR LA TABLA DE AIC y AICDELTA DE TODOS LOS MODELOS#######
aicROOST<- AICtab(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14) %>% #da tablas
  as.data.frame() %>% 
  mutate(dAIC = round(dAIC, 2), #es para obtener el AIC delta
         response = "Hours roosting") %>% 
  rownames_to_column("model_number") %>% 
  left_join(roost_formulas)

mtr<-m3 #best model

table_tfr <- summary(mtr)$coefficients$cond %>% 
  as.data.frame() %>% 
  mutate_if(is.numeric, ~signif(., 2)) %>% 
  rownames_to_column("Variable") %>% 
  mutate(Response = "Hours roosting")