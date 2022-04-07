###########################################################
############### LOADING PACKAGES ##########################
###########################################################
library(tidyverse)
library(ggplot2)
library(rstudioapi)
library(dplyr)
library(RColorBrewer)
library(mefa) # for repeating rows
# for correlation
library(corrplot)
library(psych)
library(Matrix)
# for map
library(udunits2)
library(sf)
library(ggspatial)
library(scatterpie)
library(gridExtra)
library(rnaturalearth)
# for Plotting Likert Scales
library(likert)
library(sjPlot)
library(sjmisc)
library(parameters)
library(plyr) # !!as.name does not work in map generation when this is loaded
# for gbm modeling
library(gbm)
library(caret)
library(pROC)
library(recipes)
library(plotROC)
library(precrec)
library(yardstick)
library(Metrics)

###########################################################
#################### READ DATA ############################
###########################################################
actdir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(actdir)

load("formatted_questionnaire.RData")

###########################################################
####################### Figure 1 MAP ######################
#!!as.name does not work in map generation when this is loaded
###########################################################
load("formatted_questionnaire.RData")
barplot_func <- function(pp, factor, data, group, df, coord_df){
  #factor = "sex"
  #data = "n"
  #group = "county"
  #df = szerk_kerdoiv
  #coord_df = centroidok_2
  
  input_df <- df %>% count(!!as.name(factor), !!as.name(group)) %>%
    group_by(county) %>%
    mutate(freq = n / sum(n)*100)
  input_df <- as.data.frame(input_df)
  
  input_df[,factor]<-as.factor(input_df[,factor])
  input_centroid <- merge(input_df, coord_df, all = T, by = group)
  levels(input_centroid$sex)
  
  szinek <- c("khaki2", "salmon", "lightgreen")
  names(szinek) <- levels(input_centroid$sex)
  
  plotok <- lapply(unique(input_centroid$county), function(x)
  {
    temp <- input_centroid[input_centroid$county == x, ]
    
    
    kisplot <- ggplot(temp, aes(!!as.name(factor), freq, fill = !!as.name(factor)))+
      geom_bar(stat="identity") + 
      scale_y_continuous(breaks=c(0,20,50,80))+
      scale_x_discrete(labels=c("M", "F", "O"))+
      theme(plot.title =  element_text(size=10, vjust=4), 
            plot.title.position = "plot",
            plot.background = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(), 
            axis.line.y = element_line(colour = "white"),
            axis.line.x = element_line(colour = "white"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size=8, color = "white"),
            axis.text.x = element_text(size=8, color = "white"))+
      scale_fill_manual(values=szinek)+
      geom_text(aes(label=!!as.name(data)), position=position_dodge(width=0.9), vjust=1, 
                size=6)+
      ggtitle(x)+
      guides(fill = "none")
    
    lon = unique(temp$X)
    lat = unique(temp$Y)
    list(annotation_custom(ggplotGrob(kisplot),
                           xmin = lon - 40000,       
                           ymin = lat - 30000,      
                           xmax = lon + 45000,   	
                           ymax = lat + 55000))
  })
  
  for (k in 1:length(plotok)) {
    pp <- pp + plotok[[k]]
  }
  
  pp
  
}


multiple_regions<-szerk_kerdoiv[sapply(strsplit(as.character(szerk_kerdoiv$region), ";"), length)>1, ]
multiple_regions$region<-as.character(multiple_regions$region)

multiple_regions_list<-lapply(1:nrow(multiple_regions), function(x) {dat<-multiple_regions[x,]
regs<-unlist(strsplit(as.character(dat["region"]), ";"))
out<-mefa:::rep.data.frame(dat, length(regs))
out[,"region"]<-regs
out})

multiple_regions_merged<-do.call("rbind", multiple_regions_list)

non_county_regions<-multiple_regions_merged[!multiple_regions_merged$region %in%
                                              unique(szerk_kerdoiv$county),]
multiple_regions_merged<-multiple_regions_merged[multiple_regions_merged$region %in%
                                                   unique(szerk_kerdoiv$county),]

multiple_regions_merged$county<-multiple_regions_merged$region

szerk_kerdoiv<-szerk_kerdoiv[!szerk_kerdoiv$ID %in% unique(multiple_regions_merged$ID), ]
colnames(szerk_kerdoiv)
szerk_kerdoiv<-rbind(szerk_kerdoiv, multiple_regions_merged)

megyek_kitoltoi <- table(szerk_kerdoiv$county)

megyek_kitoltoi_vektor <- c(megyek_kitoltoi)

stand_megyek <- round(megyek_kitoltoi_vektor/nepesseg_tabla$nepesseg*100000,2)
stand_megyek_szazalek <- round(prop.table(megyek_kitoltoi)*100,1)

megyek_df <- data.frame(stand_megyek, stand_megyek_szazalek)

megyek_df$county <- rownames(megyek_df)

# Merging of Budapest and Pest
pest_megye <- st_union(mo_terkep[mo_terkep$NAME == "Budapest", ], 
                       mo_terkep[mo_terkep$NAME == "Pest megye", ])

pest_megye$NAME <- NULL
pest_megye$ADMIN_LEVE.1 <- NULL

colnames(pest_megye)[2] <- "NAME"             

pest_megye <- pest_megye[, c(2,1,3)]

mo_terkep[mo_terkep$NAME == "Pest megye", ] <-  pest_megye

# "megye" word delete
mo_terkep$NAME <- substring(mo_terkep$NAME, 1, (nchar(mo_terkep$NAME)-6))

colnames(mo_terkep)[1] <- "county"   
mo_terkep_2 <- merge(mo_terkep, megyek_df, all = F, all.x = F, by = "county")

centroidok <- st_centroid(mo_terkep_2)
centroidok_2 <- cbind(mo_terkep_2$county, st_coordinates(st_centroid(mo_terkep_2$geometry)))
centroidok_2 <- as.data.frame(centroidok_2)
colnames(centroidok_2)[1] <- "county"

centroidok_2$X <- as.numeric(centroidok_2$X)
centroidok_2$Y <- as.numeric(centroidok_2$Y)

# plotting
p <- ggplot(data = mo_terkep_2) +
  geom_sf(aes(fill = stand_megyek)) +
  scale_fill_gradient2(name = "The number of respondents for 100k inhabitants") +
  theme_classic()+
  theme(legend.position=c(1,0.1),legend.justification=c(1,1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top"), 
        legend.background = element_rect(fill=adjustcolor('white', 0.1)),
        plot.margin = margin(2, 0, 0.1, 0, "cm"),
  )

barplot_func(pp = p, factor = "sex", data = "n", group = "county", df = szerk_kerdoiv, 
             coord_df =  centroidok_2)

# Europe map

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
Europe$col<-"light grey"
Europe[Europe$name=="Hungary", "col"]<-"red"
europe_plot<-ggplot(Europe, aes(fill = col)) +
  geom_sf() +
  coord_sf(xlim = c(-25,45), ylim = c(35,75), expand = FALSE)+
  scale_fill_manual(values = c("light grey", "red"))+
  theme_classic()+
  theme(axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

ggplot2::set_last_plot(NULL)

p_new<-barplot_func(p, "sex", "n", "county", szerk_kerdoiv, centroidok_2)
full_map<-p_new +
  annotation_custom(
    grob = ggplotGrob(europe_plot),
    xmin = 1760000,
    xmax = 1960000,
    ymin = 6110000,
    ymax = 6310000
  ) 


###########################################################
## Figure 2 and Electronic Suplementary Material S6a,b,c ##
###########################################################
load("formatted_questionnaire.RData")

szerk_kerdoiv[szerk_kerdoiv$sex == "other", "sex"] <- NA
szerk_kerdoiv$sex <- droplevels(szerk_kerdoiv$sex)

analysis_table <- szerk_kerdoiv[ , c("sex", 
                                     "age", 
                                     "education_level", 
                                     "residence_type",
                                     "area_size",
                                     "indiv_or_organi",
                                     "consulting",
                                     "host",
                                     "training",
                                     "prediction",
                                     "biocontrol",
                                     "risk_f_pesticide",
                                     "support_pollinators",
                                     "pesticide_use")]

colnames(analysis_table)[1:14] <- c("Sex",
                                    "Age",
                                    "Education level",
                                    "Residence type",
                                    "Farming area size",
                                    "Individual or association",
                                    "Expert knowledge",
                                    "Member of an association",
                                    "Participated agrotraining",
                                    "Uses prediction system",
                                    "Promotes biocontrol",
                                    "Aware of pesticide risks",  
                                    "Supports pollinators",
                                    "pesticide_use")  

print(str(analysis_table))
analysis_table$`Expert consulting` <- as.factor(analysis_table$`Expert consulting`)

#generalize outcome and predictor variables#
prop.table(table(analysis_table$pesticide_use))
outcomeName <- 'pesticide_use'
predictorsNames <- names(analysis_table)[names(analysis_table) != outcomeName]

levels(analysis_table$pesticide_use) <- c("uses_pesticides", "pesticide_free")

set.seed(0123)
splitIndex <- createDataPartition(analysis_table[,"pesticide_use"], 
                                  p = .80, 
                                  list = FALSE, 
                                  times = 1)
trainDF <- analysis_table[ splitIndex,]
testDF  <- analysis_table[-splitIndex,]

objControl <- trainControl(method='cv', 
                           number=10, 
                           returnResamp='none', 
                           savePredictions = "final",
                           summaryFunction = twoClassSummary, 
                           classProbs = TRUE)

garbage <- capture.output(
  objModel <- train(trainDF[ ,-14], trainDF[ ,14], 
                    method='gbm', 
                    trControl=objControl,  
                    distribution = "bernoulli",
                    metric = "ROC",
                    tuneLength = 5,
                    preProc = c("center", "scale")))
summary(objModel)
print(objModel)

#plot#
fig_rel_inf <- tibble::as_tibble(summary(objModel, plotit = FALSE))
szinek <- c("beige", "azure4", "antiquewhite4", "burlywood2", "darksalmon")

pdf(file = "../figures/Fig2.pdf", width = 15, height = 10)
#png(file = "../figures/Fig2.png", width = 3000, height = 2000)
fig_rel_inf %>% 
  dplyr::arrange(desc(rel.inf)) %>%
  ggplot(aes(x = forcats::fct_reorder(.f = var, 
                                      .x = rel.inf), 
             y = rel.inf, 
             fill = rel.inf)) +
  ylim(0, 30) +
  geom_col() +
  coord_flip() +
  scale_fill_gradientn(colours = szinek) +
  theme_classic(base_size = 20) +
  #theme_classic(base_size = 60) + #for png
  theme(axis.title = element_text()) + 
  theme(legend.position = 'none')+
  xlab('Variables') +
  ylab('Relative Influence')
dev.off()

objModel$bestTune

pdf(file = "../figures/S6a.pdf", width = 15, height = 10)
#png(file = "../figures/S6a.png", width = 3000, height = 2000)
ggplot(objModel) +
  geom_line(aes(colour = interaction.depth), size=1)+
  #geom_line(aes(colour = interaction.depth), size=2)+ #for png
  theme_classic(base_size = 20)
#theme_classic(base_size = 60) #for png
dev.off()

ggplot(varImp(objModel), "Gradient Boosting Variable Importance")+
  theme_classic()

#Resampling Performance#
confusionMatrix(objModel)

#Confusion Matrix and Statistics#
preds_gbm <- bind_cols(
  predict(objModel, newdata = testDF, type = "prob"),
  Predicted = predict(objModel, newdata = testDF, type = "raw"), 
  Actual = testDF[,14]
)

confusionMatrix(preds_gbm$Predicted, reference = preds_gbm$Actual, positive = "uses_pesticides")

#ROC corve, test data#
pdf(file = "../figures/S6b.pdf", width = 10, height = 8)
#png(file = "../figures/S6b.png", width = 3000, height = 2400)
mdl_auc <- Metrics::auc(actual = preds_gbm$Actual == "uses_pesticides", preds_gbm$uses_pesticides)
yardstick::roc_curve(preds_gbm, Actual, uses_pesticides) %>%
  mutate(b = 1-specificity) %>% 
  arrange(b, sensitivity) %>% 
  #plot(sensitivity~b, data=., type="l")
  autoplot()+
  geom_line(aes(x= b, y= sensitivity), size = 1)+
  theme_classic(base_size = 20)
#theme_classic(base_size = 60) #for png
#labs(subtitle = paste0("AUC = ", round(mdl_auc, 4))
dev.off()

#gain curve#
pdf(file = "../figures/S6c.pdf", width = 10, height = 8)
#png(file = "../figures/S6c.png", width = 3000, height = 2400)
yardstick::gain_curve(preds_gbm, Actual, uses_pesticides, event_level = "first") %>%
  autoplot()+
  geom_line(aes(x= .percent_tested, y= .percent_found), size = 1)+
  theme_classic(base_size = 20)
#theme_classic(base_size = 60) #for png
dev.off()

###########################################################
###################### Figure 3 ###########################
###########################################################
load("formatted_questionnaire.RData")

pesticide_users <- szerk_kerdoiv[!szerk_kerdoiv$pesticide_use == "pesticide free", ]
round(prop.table(table(pesticide_users$use_categories))*100,1)

table_for_plotting_likert <- pesticide_users[ , c("decision_price", 
                                                  "decision_wide_effects",
                                                  "decision_long_effects", 
                                                  "decision_experience", 
                                                  "decision_daytime_use",
                                                  "decision_use_flowering_c", 
                                                  "decision_air_use", 
                                                  "decision_no_harm_bees",
                                                  "decision_small_harm_humans", 
                                                  "decision_no_protective_clothing")]

table_for_plotting_likert$decision_price<-ordered(table_for_plotting_likert$decision_price, 
                                                  levels = c("not important",
                                                             "negligible importance", "moderately",
                                                             "important", "crucial"))
table_for_plotting_likert$decision_wide_effects<-ordered(table_for_plotting_likert$decision_wide_effects, 
                                                         levels = c("not important",
                                                                    "negligible importance", "moderately",
                                                                    "important", "crucial"))
table_for_plotting_likert$decision_long_effects<-ordered(table_for_plotting_likert$decision_long_effects, 
                                                         levels = c("not important",
                                                                    "negligible importance", "moderately",
                                                                    "important", "crucial"))
table_for_plotting_likert$decision_experience<-ordered(table_for_plotting_likert$decision_experience, 
                                                       levels = c("not important",
                                                                  "negligible importance", "moderately",
                                                                  "important", "crucial"))
table_for_plotting_likert$decision_daytime_use<-ordered(table_for_plotting_likert$decision_daytime_use, 
                                                        levels = c("not important",
                                                                   "negligible importance", "moderately",
                                                                   "important", "crucial"))
table_for_plotting_likert$decision_use_flowering_c<-ordered(table_for_plotting_likert$decision_use_flowering_c, 
                                                            levels = c("not important",
                                                                       "negligible importance", "moderately",
                                                                       "important", "crucial"))
table_for_plotting_likert$decision_air_use<-ordered(table_for_plotting_likert$decision_air_use, 
                                                    levels = c("not important",
                                                               "negligible importance", "moderately",
                                                               "important", "crucial"))
table_for_plotting_likert$decision_no_harm_bees<-ordered(table_for_plotting_likert$decision_no_harm_bees, 
                                                         levels = c("not important",
                                                                    "negligible importance", "moderately",
                                                                    "important", "crucial"))
table_for_plotting_likert$decision_small_harm_humans<-ordered(table_for_plotting_likert$decision_small_harm_humans, 
                                                              levels = c("not important",
                                                                         "negligible importance", "moderately",
                                                                         "important", "crucial"))
table_for_plotting_likert$decision_no_protective_clothing<-ordered(table_for_plotting_likert$decision_no_protective_clothing, 
                                                                   levels = c("not important",
                                                                              "negligible importance", "moderately",
                                                                              "important", "crucial"))

colnames(table_for_plotting_likert)[1:10] <- c("Price", 
                                               "Broad spectrum",
                                               "Long-lasting effects", 
                                               "Previous experience", 
                                               "Can be used daytime",
                                               "Spraying in flowering crops", 
                                               "Spraying from air", 
                                               "Low risk on bees",
                                               "Low risk on humans", 
                                               "Protective clothing needed")

myColor <- c("azure4", "azure3", "beige", "burlywood2", "darksalmon")

Result = likert(table_for_plotting_likert)
pdf(file = "../figures/Fig3.pdf", width = 15, height = 10)
#png(file = "../figures/Fig3.png", width = 3000, height = 2000)
plot(Result,
     type="bar",
     col= myColor,
     centered = FALSE, 
     include.histogram = FALSE,
     text.size=5
     #text.size=15 #for png
)+
  theme_classic(base_size = 20)+
  #theme_classic(base_size = 50)+ #for png
  theme(legend.position="top", legend.title = element_blank())
dev.off()

###########################################################
###################### Figure 4 ###########################
###########################################################
load("formatted_questionnaire.RData")

szerk_kerdoiv_szurt <- szerk_kerdoiv[szerk_kerdoiv$area_size == "small than 1 h", ]
levels(szerk_kerdoiv_szurt$area_size)
szerk_kerdoiv_szurt$area_size <- NULL

table_risk_w_poll <- szerk_kerdoiv_szurt[ , c("risk_f_hab_lost_agri", 
                                              "risk_f_hab_lost_urban", 
                                              "risk_f_pesticide", 
                                              "risk_f_intensive_p", 
                                              "risk_f_invasive_s",
                                              "risk_f_disease")]

levels(table_risk_w_poll$risk_f_hab_lost_agri)[2] <- "not at all"
levels(table_risk_w_poll$risk_f_hab_lost_urban)[2] <- "not at all"
levels(table_risk_w_poll$risk_f_pesticide)[2] <- "not at all"
levels(table_risk_w_poll$risk_f_intensive_p)[2] <- "not at all"
levels(table_risk_w_poll$risk_f_invasive_s)[2] <- "not at all"
levels(table_risk_w_poll$risk_f_disease)[2] <- "not at all"

table_risk_w_poll$risk_f_hab_lost_agri<-ordered(table_risk_w_poll$risk_f_hab_lost_agri, 
                                                levels = c("not at all",
                                                           "negligible",
                                                           "crucial"))
table_risk_w_poll$risk_f_hab_lost_urban<-ordered(table_risk_w_poll$risk_f_hab_lost_urban, 
                                                 levels = c("not at all",
                                                            "negligible",
                                                            "crucial"))
table_risk_w_poll$risk_f_pesticide<-ordered(table_risk_w_poll$risk_f_pesticide, 
                                            levels = c("not at all",
                                                       "negligible",
                                                       "crucial"))
table_risk_w_poll$risk_f_intensive_p<-ordered(table_risk_w_poll$risk_f_intensive_p, 
                                              levels = c("not at all",
                                                         "negligible",
                                                         "crucial"))
table_risk_w_poll$risk_f_disease<-ordered(table_risk_w_poll$risk_f_disease, 
                                          levels = c("not at all",
                                                     "negligible",
                                                     "crucial"))
table_risk_w_poll$risk_f_invasive_s<-ordered(table_risk_w_poll$risk_f_invasive_s, 
                                             levels = c("not at all",
                                                        "negligible",
                                                        "crucial"))

colnames(table_risk_w_poll)[1:6] <-c("Habitat loss in agricultural areas", 
                                     "Habitat loss in urban areas", 
                                     "Widespread use of pesticides", 
                                     "Agricultural intensification", 
                                     "Spread of invasive species",
                                     "Diseases and pathogens")

myColor <- c("azure4", "beige", "darksalmon")

Result = likert(table_risk_w_poll)
pdf(file = "../figures/Fig4.pdf", width = 15, height = 10)
#png(file = "../figures/Fig4.png", width = 3000, height = 2000)
plot(Result,
     type="bar",
     col= myColor,
     centered = FALSE, 
     include.histogram = FALSE,
     wrap = 25,
     text.size=5
     #text.size=15 #for png
)+
  theme_classic(base_size = 20)+
  #theme_classic(base_size = 50)+ #for png
  theme(legend.position="top", legend.title = element_blank())
dev.off()

###########################################################
## Figure 5 and Electronic Suplementary Material S8a,b,c ##
###########################################################
load("formatted_questionnaire.RData")

szerk_kerdoiv_szurt <- szerk_kerdoiv[szerk_kerdoiv$area_size == "small than 1 h", ]
levels(szerk_kerdoiv_szurt$area_size)
szerk_kerdoiv_szurt$area_size <- NULL

szerk_kerdoiv_szurt[szerk_kerdoiv_szurt$sex == "other", "sex"] <- NA
szerk_kerdoiv_szurt$sex <- droplevels(szerk_kerdoiv_szurt$sex)

analysis_table <- szerk_kerdoiv_szurt[ , c("sex", 
                                           "age", 
                                           "education_level", 
                                           "residence_type",
                                           "indiv_or_organi",
                                           "consulting",
                                           "host",
                                           "training",
                                           "prediction",
                                           "biocontrol",
                                           "risk_f_pesticide",
                                           "pesticide_use",
                                           "support_pollinators")]

colnames(analysis_table)[1:13] <- c("Sex",
                                    "Age",
                                    "Education level",
                                    "Residence type",
                                    "Individual or association",
                                    "Expert knowledge",
                                    "Member of an association",
                                    "Participated agrotraining",
                                    "Uses prediction system",
                                    "Promotes biocontrol",
                                    "Aware of pesticide risks", 
                                    "Uses pesticides",
                                    "support_pollinators")  

print(str(analysis_table))
analysis_table$`Expert consulting` <- as.factor(analysis_table$`Expert consulting`)

#generalize outcome and predictor variables
prop.table(table(analysis_table$support_pollinators))
outcomeName <- 'support_pollinators'
predictorsNames <- names(analysis_table)[names(analysis_table) != outcomeName]

levels(analysis_table$support_pollinators) <- c("support", "no")

set.seed(0123)
splitIndex <- createDataPartition(analysis_table[,"support_pollinators"], 
                                  p = .80, 
                                  list = FALSE, 
                                  times = 1)
trainDF <- analysis_table[ splitIndex,]
testDF  <- analysis_table[-splitIndex,]

objControl <- trainControl(method='cv', 
                           number=10, 
                           returnResamp='none', 
                           savePredictions = "final",
                           summaryFunction = twoClassSummary, 
                           classProbs = TRUE)

garbage <- capture.output(
  objModel <- train(trainDF[ ,-13], trainDF[ ,13], 
                    method='gbm', 
                    trControl=objControl,  
                    distribution = "bernoulli",
                    metric = "ROC",
                    tuneLength = 5,
                    preProc = c("center", "scale")))
summary(objModel)
print(objModel)

#plot#
fig_rel_inf <- tibble::as_tibble(summary(objModel, plotit = FALSE))
szinek <- c("beige", "azure4", "antiquewhite4", "burlywood2", "darksalmon")

pdf(file = "../figures/Fig5.pdf", width = 15, height = 10)
#png(file = "../figures/Fig5.png", width = 3000, height = 2000)
fig_rel_inf %>% 
  dplyr::arrange(desc(rel.inf)) %>%
  ggplot(aes(x = forcats::fct_reorder(.f = var, 
                                      .x = rel.inf), 
             y = rel.inf, 
             fill = rel.inf)) +
  ylim(0, 40) +
  geom_col() +
  coord_flip() +
  scale_fill_gradientn(colours = szinek) +
  theme_classic(base_size = 20) +
  #theme_classic(base_size = 60) + #for png
  theme(axis.title = element_text()) + 
  theme(legend.position = 'none')+
  xlab('Variables') +
  ylab('Relative Influence')
dev.off()

objModel$bestTune

pdf(file = "../figures/S8a.pdf", width = 15, height = 10)
#png(file = "../figures/S8a.png", width = 3000, height = 2000)
ggplot(objModel) +
  geom_line(aes(colour = interaction.depth), size=1)+
  #geom_line(aes(colour = interaction.depth), size=2)+ #for png
  theme_classic(base_size = 20)
#theme_classic(base_size = 60) #for png
dev.off()

ggplot(varImp(objModel), "Gradient Boosting Variable Importance")+
  theme_classic()

#Resampling Performance#
confusionMatrix(objModel)

#Confusion Matrix and Statistics#
preds_gbm <- bind_cols(
  predict(objModel, newdata = testDF, type = "prob"),
  Predicted = predict(objModel, newdata = testDF, type = "raw"), 
  Actual = testDF[,13]
)

confusionMatrix(preds_gbm$Predicted, reference = preds_gbm$Actual, positive = "support")

#ROC corve, test data#
pdf(file = "../figures/S8b.pdf", width = 10, height = 8)
#png(file = "../figures/S8b.png", width = 3000, height = 2400)
mdl_auc <- Metrics::auc(actual = preds_gbm$Actual == "support", preds_gbm$support)
yardstick::roc_curve(preds_gbm, Actual, support) %>%
  mutate(b = 1-specificity) %>% 
  arrange(b, sensitivity) %>% 
  #plot(sensitivity~b, data=., type="l")
  autoplot()+
  geom_line(aes(x= b, y= sensitivity), size = 1)+
  theme_classic(base_size = 20)
#theme_classic(base_size = 60) #for png
#labs(subtitle = paste0("AUC = ", round(mdl_auc, 4))
dev.off()

#gain curve#
pdf(file = "../figures/S8c.pdf", width = 10, height = 8)
#png(file = "../figures/S8c.png", width = 3000, height = 2400)
yardstick::gain_curve(preds_gbm, Actual, support, event_level = "first") %>%
  autoplot()+
  geom_line(aes(x= .percent_tested, y= .percent_found), size = 1)+
  theme_classic(base_size = 20)
#theme_classic(base_size = 60) #for png
dev.off()

###########################################################
########### Electronic Suplementary Material S2 ###########
###########################################################
load("formatted_questionnaire.RData")

cultivated_plants <- szerk_kerdoiv
cultivated_plants <- apply(cultivated_plants, 1, function (x) 
{unlist(strsplit(x["cultivated_plants"], ";"))})

max(sapply(cultivated_plants, length))

temp_list<-lapply(names(cultivated_plants), function(x)
{dat<-data.frame(cultivated_plants[[x]])
dat$ID = x
rownames(dat)<-NULL
dat
})

cultivated_plants_df <- do.call("rbind", temp_list)
unique(cultivated_plants_df$cultivated_plants..x..)

#rename
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Száraz hüvelyesek", "cultivated_plants..x.."] <- "Pulses"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Gyökér- és gumós növények", "cultivated_plants..x.."] <- "Roots and tubers"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Zöldségek", "cultivated_plants..x.."] <- "Vegetables"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Gyümölcsök", "cultivated_plants..x.."] <- "Fruits"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Szőlő", "cultivated_plants..x.."] <- "Grapes"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Olajos magvak", "cultivated_plants..x.."] <- "Oil-bearing crops"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Gabonafélék", "cultivated_plants..x.."] <- "Cereals"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Takarmánynövények", "cultivated_plants..x.."] <- "Fodder crops"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Egyéb ipari növények", "cultivated_plants..x.."] <- "Other industrial crops"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "tök, uborka, paradicsom, batáta, burgonya", "cultivated_plants..x.."] <- "Vegetables"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "fűszer- és gyógynövények", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Főleg díszítő növények", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "A zöldségeket nem tudom kiválasztani", "cultivated_plants..x.."] <- "Vegetables"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "gyepgazdálkodás", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Gyógynövények", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Dísznövény", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "gyógy- és fűszernövények", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Fűszer és gyógynövények", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Pattogatni való kukorica", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "gyógynövények", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "dísznövények", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "fűszernövények, dísznövény", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "erdőgazdálkodás, erdő", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Kapor, kender vetőmag előállítás ", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "dísznövény", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Kígyóuborka", "cultivated_plants..x.."] <- "Vegetables"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Kiskertbe a konyhára", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Közterület", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Erdészeti csemete", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Virágok", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Mandula", "cultivated_plants..x.."] <- "Oil-bearing crops"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Gyógynövény, fűszer növény, dísz növény", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Nemes és vad bogyósok, fűszer- és gyógyhatású növények, erdei és agzóta fás növények...", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Homoktövis ", "cultivated_plants..x.."] <- "Fruits"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Gyepek", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Gyógynövények és fűszernövények", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Disznoveny ", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Elsősorban nyulakat tartok, abból van eladásra is. Nekik kaszáljuk a szénát. A zöldség, gyümölcs magunknak van házikerben.", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "erdő", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "gyógy- és fűszernöványek", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"
cultivated_plants_df[cultivated_plants_df$cultivated_plants..x.. == "Gyógynövények ", "cultivated_plants..x.."] <- "Spices, herbs, ornamental, other"

table(cultivated_plants_df$cultivated_plants..x..)
round(table(cultivated_plants_df$cultivated_plants..x..)*100/463,1)

###########################################################
########### Electronic Suplementary Material S3 ###########
###########################################################
load("formatted_questionnaire.RData")

round(prop.table(table(szerk_kerdoiv$consulting))*100,1)
round(prop.table(table(szerk_kerdoiv$consulting, szerk_kerdoiv$area_size))*100,1)

round(prop.table(table(szerk_kerdoiv$host))*100,1)
round(prop.table(table(szerk_kerdoiv$training))*100,1)
round(prop.table(table(szerk_kerdoiv$prediction))*100,1)
round(prop.table(table(szerk_kerdoiv$biocontrol))*100,1)

###########################################################
########### Electronic Suplementary Material S4 ###########
###########################################################
load("formatted_questionnaire.RData")

pesticide_users <- szerk_kerdoiv[!szerk_kerdoiv$pesticide_use == "pesticide free", ]

round(prop.table(table(pesticide_users$use_importance))*100,1)
round(prop.table(table(pesticide_users$spraying_diary))*100,1)
round(prop.table(table(pesticide_users$register_book))*100,1)
round(prop.table(table(pesticide_users$additives))*100,1)
round(prop.table(table(pesticide_users$flowering_culture))*100,1)
round(prop.table(table(pesticide_users$emergency_exemptions))*100,1)

round(prop.table(table(pesticide_users$harm_pesticides))*100,1)

###########################################################
########### Electronic Suplementary Material S5 ###########
###########################################################
load("formatted_questionnaire.RData")

szerk_kerdoiv2 <- szerk_kerdoiv[ , c("sex",
                                     "age",
                                     "education_level",
                                     "residence_type",
                                     "area_size",
                                     "indiv_or_organi",
                                     "consulting",
                                     "host",
                                     "training",
                                     "prediction",
                                     "biocontrol",
                                     "pesticide_use",
                                     "risk_f_pesticide", 
                                     "support_pollinators")]

szerk_kerdoiv2$sex <- as.numeric(as.factor(szerk_kerdoiv2$sex))
szerk_kerdoiv2$age <- as.numeric(as.factor(szerk_kerdoiv2$age))
szerk_kerdoiv2$education_level <- as.numeric(as.factor(szerk_kerdoiv2$education_level))
szerk_kerdoiv2$residence_type <- as.numeric(as.factor(szerk_kerdoiv2$residence_type))
szerk_kerdoiv2$area_size <- as.numeric(as.factor(szerk_kerdoiv2$area_size))
szerk_kerdoiv2$indiv_or_organi <- as.numeric(as.factor(szerk_kerdoiv2$indiv_or_organi))
szerk_kerdoiv2$consulting <- as.numeric(as.factor(szerk_kerdoiv2$consulting))
szerk_kerdoiv2$host <- as.numeric(as.factor(szerk_kerdoiv2$host))
szerk_kerdoiv2$training <- as.numeric(as.factor(szerk_kerdoiv2$training))
szerk_kerdoiv2$prediction <- as.numeric(as.factor(szerk_kerdoiv2$prediction))
szerk_kerdoiv2$biocontrol <- as.numeric(as.factor(szerk_kerdoiv2$biocontrol))
szerk_kerdoiv2$pesticide_use <- as.numeric(as.factor(szerk_kerdoiv2$pesticide_use))
szerk_kerdoiv2$risk_f_pesticide <- as.numeric(as.factor(szerk_kerdoiv2$risk_f_pesticide))
szerk_kerdoiv2$support_pollinators <- as.numeric(as.factor(szerk_kerdoiv2$support_pollinators))

colnames(szerk_kerdoiv2) [1:14] <- c("Sex",
                                     "Age",
                                     "Education level",
                                     "Residence type",
                                     "Farming area size",
                                     "Individual or association",
                                     "Expert consulting",
                                     "Member of an association",
                                     "Participated agrotraining",
                                     "Uses prediction system",
                                     "Promotes biocontrol",
                                     "Uses pesticides",
                                     "Aware of pesticide risks",  
                                     "Supports pollinators")

head(szerk_kerdoiv2)
apply(szerk_kerdoiv2, 2, is.numeric)

M <- corr.test(szerk_kerdoiv2,method = "spearman",  adjust = "holm") 

corrected_pvals <- matrix(NA, ncol = dim(M[[1]])[1], nrow = dim(M[[1]])[1])
diag(corrected_pvals)<-0
corrected_pvals[lower.tri(corrected_pvals, diag = F)]<-M$p.adj
corrected_pvals<-as.matrix(forceSymmetric(corrected_pvals, uplo = "L"))
colnames(corrected_pvals)<-colnames(M$r)
rownames(corrected_pvals)<-rownames(M$r)

pdf(file = "../figures/S5.pdf", width = 10, height = 8)
#png(file = "../figures/S5.png", width = 3000, height = 2400)
#par(cex=4)
corrplot(M$r, type="upper", order="hclust", method="square", 
         addCoef.col = 'black', diag = FALSE,
         insig='blank',
         pch.col = "skyblue",
         number.cex = .9, 
         tl.col = "black", tl.srt = 45,
         p.mat = corrected_pvals, sig.level = 0.05,
         col = COL2('PiYG'))
dev.off()

###########################################################
########### Electronic Suplementary Material S7 ###########
###########################################################
load("formatted_questionnaire.RData")

szerk_kerdoiv_szurt <- szerk_kerdoiv[szerk_kerdoiv$area_size == "small than 1 h", ]
szerk_kerdoiv_szurt$area_size <- NULL

szerk_kerdoiv2 <- szerk_kerdoiv_szurt[ , c("sex",
                                           "age",
                                           "education_level",
                                           "residence_type",
                                           "indiv_or_organi",
                                           "consulting",
                                           "host",
                                           "training",
                                           "prediction",
                                           "biocontrol",
                                           "pesticide_use",
                                           "risk_f_pesticide", 
                                           "support_pollinators")]

szerk_kerdoiv2$sex <- as.numeric(as.factor(szerk_kerdoiv2$sex))
szerk_kerdoiv2$age <- as.numeric(as.factor(szerk_kerdoiv2$age))
szerk_kerdoiv2$education_level <- as.numeric(as.factor(szerk_kerdoiv2$education_level))
szerk_kerdoiv2$residence_type <- as.numeric(as.factor(szerk_kerdoiv2$residence_type))
szerk_kerdoiv2$indiv_or_organi <- as.numeric(as.factor(szerk_kerdoiv2$indiv_or_organi))
szerk_kerdoiv2$consulting <- as.numeric(as.factor(szerk_kerdoiv2$consulting))
szerk_kerdoiv2$host <- as.numeric(as.factor(szerk_kerdoiv2$host))
szerk_kerdoiv2$training <- as.numeric(as.factor(szerk_kerdoiv2$training))
szerk_kerdoiv2$prediction <- as.numeric(as.factor(szerk_kerdoiv2$prediction))
szerk_kerdoiv2$biocontrol <- as.numeric(as.factor(szerk_kerdoiv2$biocontrol))
szerk_kerdoiv2$pesticide_use <- as.numeric(as.factor(szerk_kerdoiv2$pesticide_use))
szerk_kerdoiv2$risk_f_pesticide <- as.numeric(as.factor(szerk_kerdoiv2$risk_f_pesticide))
szerk_kerdoiv2$support_pollinators <- as.numeric(as.factor(szerk_kerdoiv2$support_pollinators))

colnames(szerk_kerdoiv2) [1:13] <- c("Sex",
                                     "Age",
                                     "Education level",
                                     "Residence type",
                                     "Individual or association",
                                     "Expert consulting",
                                     "Member of an association",
                                     "Participated agrotraining",
                                     "Uses prediction system",
                                     "Promotes biocontrol",
                                     "Uses pesticides",
                                     "Aware of pesticide risks",  
                                     "Supports pollinators")

head(szerk_kerdoiv2)
apply(szerk_kerdoiv2, 2, is.numeric)

M <- corr.test(szerk_kerdoiv2,method = "spearman",  adjust = "holm") #Holm correction is strict in the sense that it ensures strong control of family wise type I error <=0.05. 

corrected_pvals <- matrix(NA, ncol = dim(M[[1]])[1], nrow = dim(M[[1]])[1])
diag(corrected_pvals)<-0
corrected_pvals[lower.tri(corrected_pvals, diag = F)]<-M$p.adj
corrected_pvals<-as.matrix(forceSymmetric(corrected_pvals, uplo = "L"))
colnames(corrected_pvals)<-colnames(M$r)
rownames(corrected_pvals)<-rownames(M$r)

pdf(file = "../figures/S7.pdf", width = 10, height = 8)
#png(file = "../figures/S7.png", width = 3000, height = 2400)
#par(cex=4)
corrplot(M$r, type="upper", order="hclust", method="square", 
         addCoef.col = 'black', diag = FALSE,
         insig='blank',
         pch.col = "skyblue",
         number.cex = .9, 
         tl.col = "black", tl.srt = 45,
         p.mat = corrected_pvals, sig.level = 0.05,
         col = COL2('PiYG'))
dev.off()
