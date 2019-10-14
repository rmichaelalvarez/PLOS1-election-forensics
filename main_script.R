########################## File Info ##########################################
### Author: Mali Zhang and Ines Levin
### Date: 06/17/2019
### Desc: Conduct analyses and generate output for the paper

########################## Libraries ##########################################
library(MASS)
library(plyr)
library(nlme)
library(truncnorm)
library(stringr)
library(randomForest)
library(dummies)
library(reshape2)
library(grid)
library(gridExtra)
library(xtable)
library(caret)
library(tidyverse)
library(rgdal)
library(RColorBrewer)
library(classInt)
library(maptools)
library(foreign)

set.seed(1000)

########################## Functions ##########################################

source("functions_script.R")

########################## Executions #########################################

########################## Step 1: create master dataset ######################

### read in and combine all presidental election data from 2015
filelist <- c("EG15pres_y_vice_0101.csv", "EG15pres_y_vice_0202.csv", 
              "EG15pres_y_vice_0313.csv", "EG15pres_y_vice_1424.csv")
data_all <- wide(filelist) 

### create vote related variables
data_all <- data_all %>%
  rename(VOTOS.FPV = VOTOS.0131, 
         VOTOS.Pro = VOTOS.0132,
         VOTOS.Com = VOTOS.0133, 
         VOTOS.Cam = VOTOS.0135, 
         VOTOS.Fre = VOTOS.0137, 
         VOTOS.UNA = VOTOS.0138) %>%
  mutate(residual = VOTOS.impugnados + VOTOS.blancos + VOTOS.nulos + VOTOS.recurridos, 
         total.votes = rowSums(.[, c("VOTOS.impugnados", "VOTOS.blancos", 
                                     "VOTOS.nulos", "VOTOS.recurridos", 
                                     "VOTOS.FPV", "VOTOS.Pro", "VOTOS.Com", 
                                     "VOTOS.Cam", "VOTOS.Fre", "VOTOS.UNA")]),
         turnout = total.votes/VOTOS.electores, 
         abstention = VOTOS.electores - total.votes, 
         VOTOS.other = VOTOS.Pro + VOTOS.Com + VOTOS.Fre, 
         p.abstention = abstention/VOTOS.electores,
         p.impugnados = VOTOS.impugnados/total.votes, 
         p.blancos = VOTOS.blancos/total.votes, 
         p.nulos = VOTOS.nulos/total.votes,
         p.recurridos = VOTOS.recurridos/total.votes, 
         p.residual = residual/total.votes, 
         p.byn = p.blancos + p.nulos, 
         p.votes.FPV = VOTOS.FPV/VOTOS.electores, 
         p.votes.Pro = VOTOS.Pro/VOTOS.electores,
         p.votes.Com = VOTOS.Com/VOTOS.electores,
         p.votes.Cam = VOTOS.Cam/VOTOS.electores,
         p.votes.Fre = VOTOS.Fre/VOTOS.electores,
         p.votes.UNA = VOTOS.UNA/VOTOS.electores, 
         p.votes.other = VOTOS.other/VOTOS.electores)


### merge in provincias code, demographics, and past election info
cod_prov <- read.csv("EG15cod_PROVINCIAS.csv", sep = ";",
                     colClasses = c("character", "character"))
cod_dept <- read.csv("EG15cod_DEPARTAMENTOS.csv", sep = ";",
                     colClasses = c("character", "character"))
demographic <- read.csv("demographics.csv") %>%
  rename(CODIGO.PROVINCIA = codigo_provincia_electoral, 
         CODIGO.DEPARTAMENTO = codigo_departamento_electoral) 
electoral_hist <- read.csv("results2011.csv") %>%
  rename(CODIGO.PROVINCIA = provinciaId, 
         CODIGO.DEPARTAMENTO = departamentoId)

data_all2 <- data_all %>%
  left_join(., cod_prov, by = "CODIGO.PROVINCIA") %>%
  rename(provincia.nombre = NOMBRE) %>%
  left_join(., cod_dept, by = c("CODIGO.DEPARTAMENTO", "CODIGO.PROVINCIA")) %>%
  rename(departamento.nombre = NOMBRE) %>%
  mutate(CODIGO.PROVINCIA = as.numeric(CODIGO.PROVINCIA), 
         CODIGO.DEPARTAMENTO = as.numeric(CODIGO.DEPARTAMENTO)) %>%
  left_join(., demographic, by = c("CODIGO.PROVINCIA", "CODIGO.DEPARTAMENTO")) %>%
  left_join(., electoral_hist, by = c("CODIGO.PROVINCIA", "CODIGO.DEPARTAMENTO")) %>%
  mutate(fpv2011_major = 1*(pres_fpv_share_2011>=0.5))

### Filter out mesas
# 1. mesas with votes less than or equal to 100, and those with mesa numbers equal to 9009 and 9075
data_filt <- data_all2 %>%
  filter(total.votes > 100) 

# 2. mesas with 0 votes for parties
data_filt2 <- data_filt %>%
  filter(VOTOS.FPV>0, VOTOS.Cam>0, VOTOS.UNA>0, VOTOS.other>0)

#################### Step 2: generate clean simulated data ####################

depvars <- c("p.votes.FPV", "p.votes.Cam", 
             "p.votes.UNA", "p.votes.other", "p.residual")
covars <- c("CODIGO.PROVINCIA","CODIGO.DEPARTAMENTO", "fpv2011_major", "nbi", 
            "masculinidad", "extranjeros", "analfabetismo", "no_usa_pc", 
            "menor_15", "mayor_65", "desocupados", 
            "universitarios", "per_propietario", "per_urban")

#---------------------- Output Table 1 in the paper --------------------------#
xtable(t(sapply(data_filt2[, c("VOTOS.electores", "turnout", 
                        "p.votes.FPV", "p.votes.Cam", "p.votes.UNA", 
                        "p.votes.other", "p.residual")], 
         summary, digits = 2))[, c(1, 3, 4, 6)], digits = 2)

clean_table <- data_filt2[, c("CODIGO.PROVINCIA", "CODIGO.CIRCUITO",
                              "CODIGO.DEPARTAMENTO", "CODIGO.MESA")]
# run regressions on each dependent variable
for (dep in depvars){
  col <-  str_replace(paste("log.p.abst",substr(dep, 3, 11), sep = "."), 
                      ".votes", "")
  col2 <-  str_replace(paste("pred.p.abst",substr(dep, 3, 11), sep = "."), 
                       ".votes", "")
  dep_var <- data.frame(dep_var = log(data_filt2[, dep]/data_filt2$p.abstention))
  dep_var[dep_var == Inf,] <- max(dep_var[dep_var != Inf,])
  dep_var[dep_var == -Inf,] <- min(dep_var[dep_var != -Inf,])
  reg_data <- bind_cols(dep_var, data_filt2[, covars])
  reg_data <- reg_data[complete.cases(reg_data),]
  # run linear mixed effect model
  mlr_reg0 <- lme(dep_var ~ -1 + fpv2011_major + nbi + masculinidad +
                    extranjeros + analfabetismo + no_usa_pc + 
                    menor_15 + mayor_65 + desocupados + 
                    universitarios + per_propietario + per_urban, 
                  random = list(~1|CODIGO.PROVINCIA, 
                                ~1|CODIGO.DEPARTAMENTO), data = reg_data, 
                  control = lmeControl(opt = "optim"))
  # re-estimate excluding points with outlier residuals from the previous reg
  mlr_reg <- lme(dep_var ~ -1 + fpv2011_major + nbi + masculinidad +
                   extranjeros + analfabetismo + no_usa_pc + 
                   menor_15 + mayor_65 + desocupados + 
                   universitarios + per_propietario + per_urban, 
                 random = list(~1|CODIGO.PROVINCIA, 
                               ~1|CODIGO.DEPARTAMENTO),  
                 data = reg_data[-which(abs(residuals(mlr_reg0, 
                                                      type = "normalized"))>qnorm(0.975)),],
                 control = lmeControl(opt = "optim"))
  # calculate the standard deviation of the residuals
  sd_resid <- sd(residuals(mlr_reg))
  # predict outcome variable for each mesa + randomly generated noise
  pred_mlr <- predict(mlr_reg, newdata = reg_data)
  pred_mlr[is.na(pred_mlr)] <- mean(pred_mlr, na.rm = T)
  noise_pred <- rnorm(nrow(reg_data), 0, sd_resid)
  pred_mlr_noisy <- pred_mlr + noise_pred
  clean_table[[col]] <- pred_mlr_noisy
  clean_table[[col2]] <- exp(pred_mlr_noisy)
}

clean_table$tot.p.abst <- rowSums(clean_table[,grep("pred.p.abst", 
                                                    names(clean_table))])

# calculate predicted votes and vote shares
for (dep in depvars){
  col <-  str_replace(paste("pred.share",substr(dep, 3, 11), sep = "."), 
                      ".votes", "")
  col2 <-  str_replace(paste("pred.p.abst",substr(dep, 3, 11), sep = "."), 
                       ".votes", "")
  col3 <-  str_replace(paste("pred.votos",substr(dep, 3, 11), sep = "."), 
                       ".votes", "")
  clean_table[[col]] <- clean_table[[col2]]/(clean_table$tot.p.abst + 1)
  clean_table[[col3]] <- round(clean_table[[col]] * data_filt2$VOTOS.electores, 0)
}
clean_table$pred.share.abst <- 1 - rowSums(clean_table[,grep("pred.share", 
                                                             names(clean_table))])
clean_table$pred.votos.abst <- round(clean_table$pred.share.abst * 
                                       data_filt2$VOTOS.electores, 0) 

# get observed data to be used for prediction later
observed_data <- bind_cols(data_filt2[, c("CODIGO.PROVINCIA", "provincia.nombre", 
                                          "CODIGO.CIRCUITO", 
                                          "CODIGO.DEPARTAMENTO", "CODIGO.MESA")],
                           data_filt2[, c("VOTOS.electores", "turnout",
                                          "p.residual")],
                           data_filt2[, grep("p.votes", names(data_filt2))])

####### Step 3: generate manipulated simulated data & output predictions ######
## Parameters for baseline fraud simulation
baseline <- c(1/3, 1/3, 1/2, 3/4, 1/2, 1/2, 0.005, 0.0025, 0.005, 0)
names(baseline) <- c("base.stolen.prop", "base.bboxstuff.prop", 
                     "stolen.coef", "bboxstuff.coef", "red.steal.large.party",
                     "base.transf.prop", "base.transf.sd", "stolen.var", 
                     "bboxstuff.var", "stolen.bboxstuff.cov")
base_output <- Sim_Classify_v2(baseline)

### output the results from estimating the baseline parameter
# output for Fig 1
ggsave("synth_plot.jpg", grid.arrange(base_output$synth_plot,
                                      nrow=2,heights=c(10, 1)), 
       width = 6,
       height = 8, units = "in")

# output for Table 2
tab2 <- round(base_output$rf_trial_perf,4)[c(2, 1, 3), c(2, 1, 3)]
colnames(tab2) <- rownames(tab2) <- c("Clean", "BBS risk", "VS risk")
xtable(tab2, digits = 3)

# output for Fig 2
imp <- data.frame(var = rownames(importance(base_output$rf_model, class = NULL, 
                             scale = TRUE, type = 2)), 
                  MeanDecreaseGini = importance(base_output$rf_model, class = NULL, 
                                                scale = TRUE, type = 2)[,"MeanDecreaseGini"]) %>%
  arrange(desc(MeanDecreaseGini)) %>%
  mutate(var2 = str_replace(var, "p.votes.", "vote share - "), 
         Variable = str_replace(var2, "Cam", "Cambiemos"))
varimpplot <- ggplot(imp, aes(x=MeanDecreaseGini, y=Variable)) + 
  geom_point(size = 3, shape=21, stroke = 1.5) +
  scale_y_discrete(limits=c("vote share - other",  "vote share - Cambiemos",
                            "vote share - FPV", "vote share - UNA", 
                            "turnout")) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(size = 12), 
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 12)) +
  geom_segment(aes(x = 0, y = c(1, 2, 3, 4, 5), xend = 40000, yend = c(1, 2, 3, 4, 5)), linetype="dashed") +
  geom_segment(aes(x = 0, y = 1, xend = 0, yend = 5))
ggsave("varimpplot.jpg", varimpplot, 
       width = 6, 
       height = 4)

# output for Table 3
tab3 <- base_output$rf_pred_prob[, c("Clean", "Tainted", "BBS_FPV", "VS_FPV")] * 100
colnames(tab3) <- c("Clean", "At risk", "BBS risk", "VS risk")
xtable(tab3)

# output for Figure 3
ggsave("pred_plot.jpg", grid.arrange(base_output$pred_plot,
                                     nrow=2,heights=c(10, 1)), 
       width = 6,
       height = 8, units = "in")

# output dataset with prediction, demographics, and past election for analysis
analysis_data <- base_output$data_pred %>%
  left_join(., data_filt2[, c("CODIGO.PROVINCIA", "CODIGO.CIRCUITO",
                                "CODIGO.DEPARTAMENTO", "CODIGO.MESA",
                              names(data_filt2)[c(37:64)])],
            by = c("CODIGO.PROVINCIA", "CODIGO.CIRCUITO",
                   "CODIGO.DEPARTAMENTO", "CODIGO.MESA"))

save(analysis_data, file = "prediction_data_w_covars.Rda")

############################### Step 4: Correlates of classsification ##########

#\/\/\/\/\/\/\/ output for Table A1
props_by_province <- prop.table(table(analysis_data$provincia.nombre, analysis_data$classif.rf), 1)[, c(2, 1, 3)]
xtable(props_by_province * 100, digits = 1)

#\/\/\/\/\/\/\/ create data frame with summaries by province

province_codes_census <- apply(table(analysis_data$provincia.nombre, analysis_data$CODIGO.PROVINCIA), 1, which.max)
sumstats_by_pronvice <- data.frame(cbind(props_by_province, Tainted = 1 - props_by_province[, 1], CODIGO.PROVINCIA = province_codes_census))

#\/\/\/\/\/\/\/ make maps with proportions by province

tmp <- tempdir()
url <- "http://biogeo.ucdavis.edu/data/diva/adm/ARG_adm.zip"
file <- basename(url)
download.file(url, file)
unzip(file, exdir = tmp)
argentina <- readOGR(dsn = tmp, layer = "ARG_adm1", use_iconv=TRUE, encoding='UTF-8')

codigos.provincias <- read.csv("codigos_provincias.csv")

argentina@data <- join(argentina@data, codigos.provincias, by = "ID_1")
argentina@data <- join(argentina@data, sumstats_by_pronvice, by = "CODIGO.PROVINCIA")

colours <- brewer.pal(6, "Reds")

IDs.map <- as.numeric(as.character(argentina@data$ID_1))

vars.to.plot <- cbind(argentina@data$Tainted[IDs.map], argentina@data$BBS_FPV[IDs.map], argentina@data$VS_FPV[IDs.map])
statlab <- c("fraud", "BBS", "VS")

for(i in 1:3) {
brks <- classIntervals(vars.to.plot[, i], n = 7, style = "fixed", fixedBreaks = c(-1, 0.025,0.05,0.10,0.15,0.20,1.1))
brks <- brks$brks
  
jpeg(paste("map_", statlab[i], sep = ""), width = 500, height = 800)
plot(argentina, col = colours[findInterval(vars.to.plot[, i], brks, all.inside = TRUE)], axes = F)
box()
legend(x = -60, y = -44, legend = leglabs(round(brks, digits = 2)), fill = colours, bty = "n")
dev.off()
}

#\/\/\/\/\/\/\/ make boxplots with demographics by classification

# reorder classif.rf levels
analysis_data$classif.rf2 <- factor(analysis_data$classif.rf, levels = c("Clean", "BBS_FPV", "VS_FPV"))
analysis_data$classif.rf2 <- revalue(analysis_data$classif.rf2, c("Clean"="Clean", "BBS_FPV"="BBS", "VS_FPV"="VS"))

# select demographics to plot
demos.list <- list(c("per_urban", "Urbanization"), c("nbi", "Need"), c("analfabetismo", "Illiteracy"))

plot_demos_function <- function(data, demo) {
 ggplot(data, aes(x=classif.rf2, y=eval(parse(text=demo[1])), color=classif.rf2)) + 
    geom_boxplot() + 
    theme_bw() +
    stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
    scale_color_manual(values=c("#00CC00", "#FF0000", "#0000FF")) +
    ggtitle(demo[2]) +
    xlab("Classification") +
    ylab("Proportion") +
    theme(legend.position="none", plot.title = element_text(hjust = 0.5))
}

demo_plots <- lapply(demos.list, plot_demos_function, data = analysis_data)

jpeg("demos.jpg", width = 400, height = 800)
grid.arrange(grobs = demo_plots, ncol=1)
dev.off()

############################### Step 5: Sensitivity Analysis ###################
## -------------- vary proportions of vote stealing ------------------###
vary_steal <- data.frame(base.stolen.prop = rep(baseline["base.stolen.prop"],
                                            nrow(base_output$pred_fraud_prov)+1),
                         rbind.fill(base_output$pred_fraud_prov,
                         base_output$rf_pred_prob))
par <- baseline
for (val in c(1/10, 1/5, 4/10, 1/2, 3/5)){
  par["base.stolen.prop"] <- val
  new_output <- Sim_Classify_Simp(par)
  new_steal <- data.frame(base.stolen.prop = rep(val,
                                              nrow(base_output$pred_fraud_prov)+1),
                           rbind.fill(new_output$pred_fraud_prov,
                           new_output$rf_pred_prob))
  vary_steal <- bind_rows(vary_steal, new_steal)
}
vary_steal <- vary_steal %>%
  arrange(base.stolen.prop)


### -------------- vary proportions of ballot box stuffing ------------###
vary_bboxstuff <- data.frame(base.bboxstuff.prop = rep(baseline["base.bboxstuff.prop"],
                                            nrow(base_output$pred_fraud_prov)+1),
                         rbind.fill(base_output$pred_fraud_prov,
                         base_output$rf_pred_prob))
par <- baseline
for (val in c(1/10, 1/5, 4/10, 1/2, 3/5)){
  par["base.bboxstuff.prop"] <- val
  new_output <- Sim_Classify_Simp(par)
  new_bboxstuff <- data.frame(base.bboxstuff.prop = rep(val,
                                              nrow(base_output$pred_fraud_prov)+1),
                           rbind.fill(new_output$pred_fraud_prov,
                           new_output$rf_pred_prob))
  vary_bboxstuff <- bind_rows(vary_bboxstuff, new_bboxstuff)
}
vary_bboxstuff <- vary_bboxstuff %>%
  arrange(base.bboxstuff.prop)


### -------------- vary different levels of stolen coef ------------###
vary_stolen_coef <- data.frame(stolen.coef = rep(baseline["stolen.coef"],
                                            nrow(base_output$pred_fraud_prov)+1),
                         rbind.fill(base_output$pred_fraud_prov,
                         base_output$rf_pred_prob))
par <- baseline
for (val in c(1/10, 1/5, 3/10, 4/10, 3/5, 7/10, 4/5, 9/10)){
  par["stolen.coef"] <- val
  new_output <- Sim_Classify_Simp(par)
  new_stcoef <- data.frame(stolen.coef = rep(val,
                                              nrow(base_output$pred_fraud_prov)+1),
                           rbind.fill(new_output$pred_fraud_prov,
                           new_output$rf_pred_prob))
  vary_stolen_coef <- bind_rows(vary_stolen_coef, new_stcoef)
}
vary_stolen_coef <- vary_stolen_coef %>%
  arrange(stolen.coef)

### -------------- vary different levels of bbox stuffing coef ------------###
vary_bboxstuff_coef <- data.frame(bboxstuff.coef = rep(baseline["bboxstuff.coef"],
                                            nrow(base_output$pred_fraud_prov)+1),
                         rbind.fill(base_output$pred_fraud_prov,
                         base_output$rf_pred_prob))
par <- baseline
for (val in c(1/10, 1/5, 3/10, 4/10, 1/2, 3/5, 7/10, 4/5, 9/10)){
  par["bboxstuff.coef"] <- val
  new_output <- Sim_Classify_Simp(par)
  new_bboxcoef <- data.frame(bboxstuff.coef = rep(val,
                                              nrow(base_output$pred_fraud_prov)+1),
                           rbind.fill(new_output$pred_fraud_prov,
                           new_output$rf_pred_prob))
  vary_bboxstuff_coef <- bind_rows(vary_bboxstuff_coef, new_bboxcoef)
}
vary_bboxstuff_coef <- vary_bboxstuff_coef %>%
  arrange(bboxstuff.coef)


par_list <- list(vary_steal, vary_bboxstuff, vary_stolen_coef, vary_bboxstuff_coef)
names(par_list) <- c("Probability of at-risk mesa tainted with VS",
                     "Probability of at-risk mesa tainted with BBS", 
                     "Extent of VS in tainted mesas", 
                     "Extent of BBS in tainted mesas")
par_name <- c("base.stolen.prop", "base.bboxstuff.prop", "stolen.coef", "bboxstuff.coef")

# output for Figure 6
jpeg("sensitivity_all.jpg", width=800, height=600)
par(mfrow=c(2,2))
for (i in 1:4){
  df <- par_list[[i]]
  plot(df[df$Name == "ALL", 1], 
       df$Clean[df$Name == "ALL"], 
       xlab = names(par_list)[i],
       ylab = "% Mesas Clean" ,
       ylim = c(0.45, 1),
       cex.axis = 1.3, 
       cex.lab = 1.6)
  lines(df[df$Name == "ALL",1], 
        df$Clean[df$Name == "ALL"])
}
dev.off()

############################### APPENDIX ######################################
# plot for one province for all paramaters
par(mfrow=c(2,2))
for (i in 1:4){
  df <- par_list[[i]]
  plot(df[df$Name == "BUENOS AIRES", 1], 
       df$Clean[df$Name == "BUENOS AIRES"], 
       main = names(par_list)[i], 
       xlab = par_name[i],
       ylab = "% Mesas Clean" ,
       ylim = c(0.45, 1),
       cex.main = 0.8)
  lines(df[df$Name == "BUENOS AIRES",1], 
        df$Clean[df$Name == "BUENOS AIRES"])
}

# plot for all provinces for just one parameter (change vary_steal)

#all_province_base_stolen_prop
par(mfrow=c(5,5))
par(oma=c(1,1,0,0),mar=c(2,2,2,2))
for (prov in unique(vary_steal$Name)){
  plot(vary_steal$base.stolen.prop[vary_steal$Name == prov],
       vary_steal$Clean[vary_steal$Name == prov],
       main = prov, xlab = "base.stolen.prop",ylab = "Clean" ,
       ylim = c(0.45, 1),
       cex.main = 0.8)
  lines(vary_steal$base.stolen.prop[vary_steal$Name == prov], vary_steal$Clean[vary_steal$Name == prov])
}

# all_province_base_bboxstuff_prop
par(mfrow=c(5,5))
par(oma=c(1,1,0,0),mar=c(2,2,2,2))
for (prov in unique(vary_bboxstuff$Name)){
  plot(vary_bboxstuff$base.bboxstuff.prop[vary_bboxstuff$Name == prov],
       vary_bboxstuff$Clean[vary_bboxstuff$Name == prov],
       main = prov, xlab = "base.bboxstuff.prop",ylab = "Clean" ,
       ylim = c(0.45, 1),
       cex.main = 0.8)
  lines(vary_bboxstuff$base.bboxstuff.prop[vary_bboxstuff$Name == prov], vary_bboxstuff$Clean[vary_steal$Name == prov])
}

# all_province_stolen_coef
par(mfrow=c(5,5))
par(oma=c(1,1,0,0),mar=c(2,2,2,2))
for (prov in unique(vary_stolen_coef$Name)){
  plot(vary_stolen_coef$stolen.coef[vary_stolen_coef$Name == prov],
       vary_stolen_coef$Clean[vary_stolen_coef$Name == prov],
       main = prov, xlab = "stolen.coef",ylab = "Clean" ,
       ylim = c(0.45, 1),
       cex.main = 0.8)
  lines(vary_stolen_coef$stolen.coef[vary_stolen_coef$Name == prov], vary_stolen_coef$Clean[vary_steal$Name == prov])
}

# all_province_bboxstuff_coef
par(mfrow=c(5,5))
par(oma=c(1,1,0,0),mar=c(2,2,2,2))
for (prov in unique(vary_bboxstuff_coef$Name)){
  plot(vary_bboxstuff_coef$bboxstuff.coef[vary_bboxstuff_coef$Name == prov],
       vary_bboxstuff_coef$Clean[vary_bboxstuff_coef$Name == prov],
       main = prov, xlab = "bboxstuff.coef",ylab = "Clean" ,
       ylim = c(0.45, 1),
       cex.main = 0.8)
  lines(vary_bboxstuff_coef$bboxstuff.coef[vary_bboxstuff_coef$Name == prov], vary_bboxstuff_coef$Clean[vary_steal$Name == prov])
}

