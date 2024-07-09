library(tidyverse)
library(lme4)
library(readxl)
library(lubridate)
library(magrittr)
library(emmeans)
carmen<-read_excel("ElCarmen.xlsx")
roost<-read_excel("roost_c.xlsx")
contrastC<-read_excel("contrast_table.xlsx")
contrastHA<-read_excel("contrast_tableHA.xlsx")
contrastHR<-read_excel("contrast_tableHR.xlsx")

model1 <- glm(emergence ~ relevel(pregnant, ref = "desired_level") + repro, family = Gamma, data = carmen)


theme_set(theme_bw() +
            theme(panel.grid = element_blank(),
                  text = element_text(size = 26)))
#####COMPARISON GLM######n
  model<-glm(n ~ repro, family=poisson, data=carmen) #revisar resultados GLM en tabla. 
  summary(model) #VRR

    repro <- c("pregnant", "non", "lactating")
  r <- unique(carmen$repro)
  #step 1
  carmen_n <- expand.grid(repro = r) %>% 
    mutate(id = NA, year = NA) #do all the possible combinations
  
carmen_pred <- predict(model, newdata = carmen_n,  re.form = NA, se.fit = TRUE, type = "response")
  #E1 is the best model, PASO 2, do predictions
  carmen_n$pred <- carmen_pred$fit
  carmen_n$se <- carmen_pred$se.fit #PASO 3
  
  carmen_n_Plt <- carmen_n %>% 
    mutate(lwrCI = pred - se * 1.96,
           uprCI = pred + se * 1.96) %>% 
    mutate(repro = factor(repro, levels = reproo)) #PASO 4
  #PASO 5, EXACTLY WHAT DOES THE PLOT MEAN? TIME OF EMERGENCE
  #THE BEHAVIOUR OF THE DATA PLUS THE PREDICTION
  
n_carmen<- emmeans(model, specs = pairwise ~ repro, type = "response")
  #NTF is the best model of emergence time
  n_carmen_DF <- n_carmen$contrasts %>% 
    data.frame() %>% 
    tibble() %>% 
    mutate(lwrCI = ratio - SE * 1.96,
           uprCI = ratio + SE * 1.96)
  #step 8, ratio contrasts
  
  ggsave("contrast_VRR_Carmen.tiff", units="mm", width=250, 
         height=180, 
         dpi=800, compression = 'lzw' )
  
  ggsave("contrast_VRR_Carmen.svg", units="mm", width=250, 
         height=180, 
         dpi=800)
  
  #USE AS TEMPLATE FOR FIGURES
  carmen$repro<-factor(carmen$repro, levels = c ("lactating", "nonrepro", "pregnant"))
  carmen$repro<-factor(carmen$repro, levels = c ("non", "pregnant", "lactating"))
    
    ggplot() +
      geom_violin(data = carmen, aes(x = repro, y = n, fill = repro), alpha = 0.3, size = 1) +
      geom_point(data = new_C_Plt, aes(x = repro, y = pred, color = repro), size = 6, position = position_dodge(width = 0.9)) +
      geom_errorbar(data = new_C_Plt, aes(x = repro, ymin = lwrCI, ymax = uprCI, color = repro), width = 0.3, size = 2, position = position_dodge(width = 0.9)) +
      theme(legend.position = "none",
            axis.line = element_line(size = 2)) +
      ylab("Frequency of returns to the roost") +
      xlab("Reproductive condition") +
      scale_fill_manual(values = c("white", "white", "white")) +
      scale_color_manual(values = c("#000000", "#000000", "#000000"))
#SEGUIR
    
    ggsave("again_carmen_n.tiff", units="mm", width=250, 
           height=180, 
           dpi=800, compression = 'lzw')
  
  ggsave("again_carmen_n.svg", units="mm", width=250, 
         height=180, 
         dpi=800)
  ####TIME OF EMERGENCE##########
  model1<-glm(min ~ repro, family=Gamma, data=carmen)
  summary(model1)
  
  C_HE <- expand.grid(repro = r)  #do all the possible combinations
  C_HE_pred <- predict(model1, newdata = C_HE,  re.form = NA, se.fit = TRUE, type = "response")
  
 C_HE$pred <- C_HE_pred$fit #step 3
  C_HE$se <- C_HE_pred$se.fit
  
  C_HE_Plt <- C_HE %>% 
    mutate(lwrCI = pred - se * 1.96,
           uprCI = pred + se * 1.96) #step4
  
#Ejemplo usado para GLM con Gamma, Done, funciona
  model1<-glm(min ~ repro, family=Gamma, data=carmen)

  # Obtain the estimated marginal means on the link scale
  emmeans_link <- emmeans(model1, ~ repro)
  
  # Back-transform the estimated marginal means to the response scale
  emmeans_response <- regrid(emmeans_link, type = "response")
  
  # Obtain delta method contrasts of the estimated marginal means
  contrasts <- pairs(emmeans_response, type = "response")
  CI <- confint(contrasts, level = 0.95)
  contrast_table <- cbind(contrasts, CI)
  
  write_xlsx(contrast_table,"D:\\contrast_table.xlsx") #step 7
  
    
   #parece que es lo mismo este: 
    ggplot() +
      geom_violin(data = carmen, aes(x = repro, y = min, fill = repro), alpha = 0.3, size = 1) +
      geom_point(data = C_HE_Plt, aes(x = repro, y = pred, color = repro), size = 6, position = position_dodge(width = 0.9)) +
      geom_errorbar(data = C_HE_Plt, aes(x = repro, ymin = lwrCI, ymax = uprCI, color = repro), width = 0.3, size = 1.9, position = position_dodge(width = 0.9)) +
      theme_classic(base_size = 14) +
      theme(legend.position = "none",
            axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),  # Corrected axis.line assignment
            axis.title = element_text(size = 14),  # Adjust axis title size
            axis.text = element_text(size = 12)) +  # Adjust axis text size
      ylab("Time of emergence (min after sunset)") +
      scale_fill_manual(values = c("gray", "gray", "gray")) +
      scale_color_manual(values = c("#000000", "#000000", "#000000"))
        #DONE
  
    ggsave("again_carmen_HE.tiff", units="mm", width=250, 
           height=180, 
           dpi=800, compression = 'lzw')
  
  ggsave("again_carmen_HE.svg", units="mm", width=250, 
         height=180, 
         dpi=800)  
  #######horas#######
  model2<-glm(horas ~ repro, family=Gamma, data=carmen)
summary(model2)
ref_grid(model2)

new_HA <- expand.grid(repro = r) %>% 
  mutate(id = NA, year = NA) #do all the possible combinations

newD_HA_pred <- predict(model2, newdata = new_HA,  re.form = NA, se.fit = TRUE, type = "response")
#E1 is the best model, PASO 2, do predictions

new_HA$pred <- newD_HA_pred$fit
new_HA$se <- newD_HA_pred$se.fit #PASO 3

repro_order <- c("non", "pregnant", "lactating")

# Mutate the repro variable to set the desired order
new_HE_Plt <- new_HE %>% 
  mutate(lwrCI = pred - se * 1.96,
         uprCI = pred + se * 1.96,
         repro = factor(repro, levels = repro_order))

ggplot() +
  geom_violin(data = carmen, aes(x = repro, y = horas, fill = repro), alpha = 0.3, size = 1) +
  geom_point(data = new_HA_Plt, aes(x = repro, y = pred, color = repro), size = 6, position = position_dodge(width = 0.9)) +
  geom_errorbar(data = new_HA_Plt, aes(x = repro, ymin = lwrCI, ymax = uprCI, color = repro), width = 0.3, size = 2, position = position_dodge(width = 0.9)) +
  theme(legend.position = "none",
        axis.line = element_line(size = 2)) +
  ylab("Hours of activity") +
  xlab("Reproductive condition") +
  scale_fill_manual(values = c("white", "white", "white")) +
  scale_color_manual(values = c("#000000", "#000000", "#000000"))
#SEGUIR

ggsave("again_carmen_HA.tiff", units="mm", width=250, 
       height=180, 
       dpi=800, compression = 'lzw')

ggsave("again_carmen_HA.svg", units="mm", width=250, 
       height=180, 
       dpi=800) 

model2<-glm(horas ~ repro, family=Gamma, data=carmen)
summary(model2) #HA
ref_grid(model2)

    emm2<- emmeans(model2, specs = pairwise ~ repro, type = "response")
    
  C_HA <- expand.grid(repro = r)  #do all the possible combinations
  C_HA_pred <- predict(model2, newdata = C_HA,  re.form = NA, se.fit = TRUE, type = "response")
  
  C_HE$pred <- C_HE_pred$fit #step 3
  C_HE$se <- C_HE_pred$se.fit
  
  C_HE_Plt <- C_HE %>% 
    mutate(lwrCI = pred - se * 1.96,
           uprCI = pred + se * 1.96) #step4
  
  emm2_DF <- emm2$contrasts %>% 
    data.frame() %>% 
    tibble() %>% 
    mutate(lwrCI = ratio - SE * 1.96,
           uprCI = ratio + SE * 1.96)
  
  write_xlsx(HE_carmen_DF,"D:\\HE_carmen_DF.xlsx") #step 7
  
  # Obtain the estimated marginal means on the link scale
  emmeans_link <- emmeans(model2, ~ repro)
  
  # Back-transform the estimated marginal means to the response scale
  emmeans_response <- regrid(emmeans_link, type = "response")
  
  # Obtain delta method contrasts of the estimated marginal means
  contrasts <- pairs(emmeans_response, type = "response")
  CI <- confint(contrasts, level = 0.95)
  contrast_tableHA <- cbind(contrasts, CI)
  
  write_xlsx(contrast_tableHA,"D:\\contrast_tableHA.xlsx") #step 7
  
  contrastHA %>%
    ggplot() +
    geom_point(aes(x = estimate, y = contrast), size=2.5) +
    geom_errorbarh(aes(y = contrast, xmin = lower.CL, xmax = upper.CL), size= 3,height = 0, color = "blue", alpha=0.4) +
    geom_vline(xintercept=1, size=2, color="red", alpha = 0.3) #DONE
  
  ggsave("contrastC_HA.tiff", units="mm", width=250, 
         height=180, 
         dpi=800, compression = 'lzw')
  
  ggsave("contrastC_HA.svg", units="mm", width=250, 
         height=180, 
         dpi=800)
  
  #####roosting#####
 roost$repro<-factor(roost$repro, levels = c ("lactating", "non","pregnant"))
 r<-unique(roost$roosting)
 
 roost$repro<- factor(roost$repro, levels = levels(roost$repro))
 
 model3<-glm(roosting ~ repro, family=Gamma, data=roost) 
 r<-unique(roost$repro)
 summary(model3)
 #step 1
 new_r <- expand.grid(repro = r) %>% 
   mutate(id = NA, year = NA) #do all the possible combinations
 
 #E1 is the best model, PASO 2, do predictions
 newD_r_pred <- predict(model3, newdata = new_r,  re.form = NA, se.fit = TRUE, type = "response")
 
 new_r$pred <- newD_r_pred$fit
 new_r$se <- newD_r_pred$se.fit #PASO 3
 
 new_r_Plt <- new_r %>% 
   mutate(lwrCI = pred - se * 1.96,
          uprCI = pred + se * 1.96) %>% 
   mutate(repro = factor(repro, levels = repro)) #PASO 4

 roost$repro<-factor(roost$repro, levels = c ("non","pregnant", "lactating"))
 repro_order <- c("non","pregnant", "lactating")

   ggplot() +
     geom_violin(data = roost, aes(x = repro, y = roosting, fill = repro), alpha = 0.3, size = 1) +
     geom_point(data = new_r_Plt, aes(x = repro, y = pred, color = repro), size = 4, position = position_dodge(width = 1)) +
     geom_errorbar(data = new_r_Plt, aes(x = repro, ymin = lwrCI, ymax = uprCI, color = repro), width = 0.15, size=2, position = position_dodge(width = 0.9)) +
     ylab("Hours of roosting") +
     xlab("Food availability seasons") +
     scale_fill_manual(values = c("white", "white", "white")) +
     scale_color_manual(values = c("#000000", "#000000", "#000000"))+
     theme(legend.position = "none",
           axis.line = element_line(size = 2)) 
   #seguir, le falta corregir el geom_violin
   #geom_violin do not have a dist.
   
   ggplot() +
     geom_violin(data = roost, aes(x = factor(repro, levels = repro_order), y = roosting, fill = repro), alpha = 0.3, size = 1) +
     geom_point(data = new_r_Plt, aes(x = factor(repro, levels = repro_order), y = pred, color = repro), size = 4, position = position_dodge(width = 0.9)) +
     geom_errorbar(data = new_r_Plt, aes(x = factor(repro, levels = repro_order), ymin = lwrCI, ymax = uprCI, color = repro), width = 0.15, size = 2, position = position_dodge(width = 0.9)) +
     ylab("Time of emergence") +
     xlab("Food availability seasons") +
     scale_fill_manual(values = c("white", "white", "white")) +
     scale_color_manual(values = c("#000000", "#000000", "#000000")) +
     theme(legend.position = "none", axis.line = element_line(size = 2)) +
     scale_y_continuous(limits = c(0, max(roost$roosting) * 1.1))
#seguir, 
   
   ggsave("violin_carmen_HR.tiff", units="mm", width=250, 
          height=180, 
          dpi=800, compression = 'lzw')
  
  ggsave("violin_carmen_HR.svg", units="mm", width=250, 
         height=180, 
         dpi=800)  
  
  # Obtain the estimated marginal means on the link scale
  emmeans_link <- emmeans(model3, ~ repro)
  
  # Back-transform the estimated marginal means to the response scale
  emmeans_response <- regrid(emmeans_link, type = "response")
  
  # Obtain delta method contrasts of the estimated marginal means
  contrasts <- pairs(emmeans_response, type = "response")
  CI <- confint(contrasts, level = 0.95)
  contrast_tableHR <- cbind(contrasts, CI)
  
  write_xlsx(contrast_tableHR,"D:\\contrast_tableHR.xlsx") #step 7
  
  contrastHR %>%
    ggplot() +
    geom_point(aes(x = estimate, y = contrast), size=2.5) +
    geom_errorbarh(aes(y = contrast, xmin = lower.CL, xmax = upper.CL), size= 3,height = 0, color = "blue", alpha=0.4) +
    geom_vline(xintercept=1, size=2, color="red", alpha = 0.3) #DONE
  
  ggsave("contrastC_HR.tiff", units="mm", width=250, 
         height=180, 
         dpi=800, compression = 'lzw')
  
  ggsave("contrastC_HR.svg", units="mm", width=250, 
         height=180, 
         dpi=800)
  
  
 ##########LRT##########
  
  # Fit the null model
  null_model <- glm(y ~ 1)
  null_model_n <- glm(n ~ 1, family = poisson, data = carmen)
  null_model_e<- glm(emergence ~ 1, family = Gamma, data = carmen)
  null_model_r<- glm(roosting ~ 1, family=Gamma, data=roost)
  null_model_a<- glm(horas ~ 1, family=Gamma, data=carmen)
  
  # Fit the single model
  single_model <- lm(y ~ x)

  single_model_n<-glm(n ~ repro, family=poisson, data=carmen) 
  single_model_e<-glm(emergence ~ repro, family=Gamma, data=carmen)
  single_model_r<-glm(roosting ~ repro, family=Gamma, data=roost)
  single_model_a<-glm(horas ~ repro, family=Gamma, data=carmen)
  
  # Calculate log-likelihood for both models
  null_logLik <- logLik(null_model_a)
  single_logLik <- logLik(single_model_a)
  
  # Compute the likelihood ratio statistic
  LR_statistic <- 2 * (single_logLik - null_logLik)
  
  # Degrees of freedom for the chi-squared test
  df <- attr(LR_statistic, "df")
  
  # Perform the LRT and calculate p-value
  p_value <- 1 - pchisq(LR_statistic, df)
  
  # Output the results
  cat("Likelihood Ratio Statistic:", LR_statistic, "\n")
  cat("Degrees of Freedom:", df, "\n")
  cat("P-value:", p_value, "\n")
#if p-value is 0, it means that the single model is better than the null.   