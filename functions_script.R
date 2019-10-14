########################## File Info ##########################################
### Author: Mali Zhang and Ines Levin
### Date: 06/17/2019
### Desc: Functions used in the script for the paper

########################## Functions ##########################################
wide <- function(filelist){ # Reshape the data with party code as columns
  ##-------- read in the data -----------
  data0101 <- read.csv(filelist[1], 
                       sep = ";", 
                       colClasses = c("character", "character", "character", "character",
                                      "character", "character", "numeric"))
  data0202 <- read.csv(filelist[2],
                       sep = ";", 
                       colClasses = c("character", "character", "character", "character",
                                      "character", "character", "numeric"))
  data0313 <- read.csv(filelist[3],
                       sep = ";", 
                       colClasses = c("character", "character", "character", "character",
                                      "character", "character", "numeric"))
  data1424 <- read.csv(filelist[4],
                       sep = ";", 
                       colClasses = c("character", "character", "character", "character",
                                      "character", "character", "numeric"))
  # Note: missing the electoral data
  ##-------- reshape the data -----------
  widedf <- lapply(list(data0101, data0202, data0313, data1424), 
                   reshape,
                   direction = "wide",
                   timevar = "CODIGO.VOTOS", 
                   idvar = c("CODIGO.PROVINCIA","CODIGO.DEPARTAMENTO",
                             "CODIGO.CIRCUITO","CODIGO.MESA"), 
                   drop = "AMBITO")
  data_all <- rbind.fill(widedf)
  ##-------- rename variables -----------
  data_all<-data_all %>%
    dplyr::rename(VOTOS.electores = VOTOS.9001,
            VOTOS.votantes = VOTOS.9002,
            VOTOS.impugnados = VOTOS.9003,
            VOTOS.blancos = VOTOS.9004,
            VOTOS.nulos = VOTOS.9005,
            VOTOS.recurridos = VOTOS.9006)
  return(data_all)
}

Sim_Classify_v2 <- function(par){ #generate synthetic data and prediction outputs
  # set the value of each individual parameter
  base.stolen.prop <- par["base.stolen.prop"] # var of interest
  base.bboxstuff.prop <- par["base.bboxstuff.prop"]   # var of interest
  stolen.coef <- par["stolen.coef"]
  bboxstuff.coef <- par["bboxstuff.coef"]
  red.steal.large.party <- par["red.steal.large.party"]
  base.transf.prop <- par["base.transf.prop"]      # var of interest
  base.transf.sd <- par["base.transf.sd"]
  
  stolen.var <- par["stolen.var"]
  bboxstuff.var <- par["bboxstuff.var"]
  stolen.bboxstuff.cov <- par["stolen.bboxstuff.cov"]
  Sigma <- matrix(c(stolen.var, stolen.bboxstuff.cov, 
                    stolen.bboxstuff.cov, bboxstuff.var ), 2, 2)
  
  # using multinomial distribution to determine if there's any kind of fraud
  # and assuming that vote stealing and ballot box stuffing are exclusive events
  set.seed(100)
  base_fraud <- t(rmultinom(nrow(clean_table), 1, 
                            c(base.stolen.prop, base.bboxstuff.prop, 
                              1- base.bboxstuff.prop - base.stolen.prop)))[, 1:2]
  proportion_fraud <- as.data.frame(base_fraud * mvrnorm(n = nrow(clean_table), 
                                                         mu = c(stolen.coef, bboxstuff.coef),
                                                         Sigma = Sigma,
                                                         empirical = TRUE))
  proportion_fraud[proportion_fraud < 0] <- 0 
  names(proportion_fraud) <- c("stolen.prop", "bboxstuff.prop")
  
  proportion_fraud[proportion_fraud > 1] <- 1
  
  prop_fraud_plot <- melt(proportion_fraud)
  
  # randomly generate the probability of transferring votes
  transf_prop <- rtruncnorm(nrow(clean_table), a = 0, b = 1, 
                            mean = base.transf.prop, sd = base.transf.sd)
  
  # generate synthetic manipulated data
  fraud_table <- data_filt2[, c("CODIGO.PROVINCIA", "CODIGO.CIRCUITO",
                                "CODIGO.DEPARTAMENTO", "CODIGO.MESA")]
  # for small parties
  for (party in c("Cam", "UNA", "oth")){
    col0 <- paste("pred.votos", party, sep = ".")
    col1 <- paste("votos", party, "loss", sep = ".")
    col2 <- paste("votos", party, "manip", sep = ".")
    fraud_table[[col1]] <- round(clean_table[[col0]] * proportion_fraud$stolen.prop)
    fraud_table[[col2]] <- clean_table[[col0]] - fraud_table[[col1]]
  }
  fraud_table$small.party.loss <- rowSums(
    fraud_table[, c("votos.Cam.loss", "votos.UNA.loss", "votos.oth.loss" )])
  # for FPV
  for (party in c("FPV")){
    col0 <- paste("pred.votos", party, sep = ".")
    col <- paste("votos",party, "manip", sep = ".")
    fraud_table[[col]] <-  round(clean_table[[col0]] + 
                                   transf_prop*fraud_table$small.party.loss +
                                   proportion_fraud$bboxstuff.prop*clean_table$pred.votos.abst)
  }
  fraud_table$abstention.manip <- round(clean_table$pred.votos.abst + 
                                          (1-transf_prop)*fraud_table$small.party.loss -
                                          proportion_fraud$bboxstuff.prop*clean_table$pred.votos.abst)
  
  training_data <- bind_cols(data_filt2[, c("CODIGO.PROVINCIA", "CODIGO.CIRCUITO", 
                                        "CODIGO.DEPARTAMENTO", "CODIGO.MESA", 
                                        "VOTOS.electores")],
                         data.frame(pred.votos.residual = clean_table[, "pred.votos.residual"]),
                         fraud_table[, grep("manip", names(fraud_table))],
                         proportion_fraud) %>% # generate relevant vote share vars
    mutate(sim.calc.total.votes = VOTOS.electores - abstention.manip, 
           sim.turnout = sim.calc.total.votes/VOTOS.electores,
           sim.p.votes.FPV = votos.FPV.manip/VOTOS.electores,
           sim.p.votes.Cam = votos.Cam.manip/VOTOS.electores,
           sim.p.votes.UNA = votos.UNA.manip/VOTOS.electores, 
           sim.p.votes.oth = votos.oth.manip/VOTOS.electores, 
           sim.p.residual = pred.votos.residual/VOTOS.electores)
  
  # -------------------- create risk type -------------------------------------#
  training_data$fraud.type <- factor(case_when(
    training_data$stolen.prop == 0 & training_data$bboxstuff.prop == 0 ~ "Clean", 
    training_data$stolen.prop > 0 & training_data$bboxstuff.prop == 0 ~ "VS_FPV",
    training_data$stolen.prop == 0 & training_data$bboxstuff.prop > 0 ~ "BBS_FPV"
  ))
  
  # -------------------------- Table/Plots to output -------------------------#
  
  #--- plot the turnout density of different risk types (Figure 1 in paper) --
  cond1 <- training_data$fraud.type == "VS_FPV" 
  
  cond2 <- training_data$fraud.type == "BBS_FPV" 
  
  ## plot turnout in clean vs. tainted data
  synth_plot_clean <- ggplot(training_data[training_data$fraud.type == "Clean",])+
    geom_density(aes(x = sim.turnout*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    ylim(0,0.4) +
    xlim(0,100) +
    geom_density(data = training_data[cond1,], 
                 aes(x = sim.turnout*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = training_data[cond2,], 
                 aes(x = sim.turnout*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) + 
    theme(legend.position = "right") + 
    labs(x = "Turnout (%)", 
         title = "(1)") +
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
          axis.title.y=element_blank(),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  mylegend <- g_legend(synth_plot_clean)
  
  ## plot share of the large parties in clean vs. tainted data
  # FPV
  synth_plot_FPV <- ggplot(training_data[training_data$fraud.type == "Clean",])+
    geom_density(aes(x = sim.p.votes.FPV*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    ylim(0,0.15) +
    xlim(0,100) + 
    geom_density(data = training_data[cond1,], 
                 aes(x = sim.p.votes.FPV*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = training_data[cond2,], 
                 aes(x = sim.p.votes.FPV*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2)+ 
    labs(x = "Vote share - FPV (%)", 
         title = "(2)")+
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
          axis.title.y=element_blank(),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  # Cam
  synth_plot_Cam <- ggplot(training_data[training_data$fraud.type == "Clean",])+
    geom_density(aes(x = sim.p.votes.Cam*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    ylim(0,0.15) +
    xlim(0,100) + 
    geom_density(data = training_data[cond1,], 
                 aes(x = sim.p.votes.Cam*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = training_data[cond2,], 
                 aes(x = sim.p.votes.Cam*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) + 
    labs(x = "Vote share - Cambiemos (%)", 
         title = "(3)")+
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
          axis.title.y=element_blank(),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  # UNA
  synth_plot_UNA <- ggplot(training_data[training_data$fraud.type == "Clean",])+
    geom_density(aes(x = sim.p.votes.UNA*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    ylim(0,0.15) +
    xlim(0,100) + 
    geom_density(data = training_data[cond1,], 
                 aes(x = sim.p.votes.UNA*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = training_data[cond2,], 
                 aes(x = sim.p.votes.UNA*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2)+ 
    labs(x = "Vote share-UNA (%)", 
         title = "(4)")+
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
          axis.title.y=element_blank(),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  
  # Other
  synth_plot_oth <- ggplot(training_data[training_data$fraud.type == "Clean",])+
    geom_density(aes(x = sim.p.votes.oth*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    geom_density(data = training_data[cond1,], 
                 aes(x = sim.p.votes.oth*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = training_data[cond2,], 
                 aes(x = sim.p.votes.oth*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2)+
    xlim(0,50)+
    ylim(0,0.4)+ 
    labs(x = "Vote share - Others (%)", 
         title = "(5)")+
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
          axis.title.y=element_blank(),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  synth_plot <- arrangeGrob(synth_plot_clean + theme(legend.position="none"),
                            mylegend,
                            synth_plot_FPV + theme(legend.position="none"),
                            synth_plot_Cam + theme(legend.position="none"),
                            synth_plot_UNA + theme(legend.position="none"),
                            synth_plot_oth + theme(legend.position="none"),
                            ncol=2)
  
  
  #----------------------------------------------------------------------------#
  
  # get the right data to build the model
  names(training_data) <- str_replace(names(training_data), "oth", "other")
  colname_share <- c("turnout", "p.votes.FPV", "p.votes.Cam", 
                     "p.votes.UNA", "p.votes.other")
  colname_share_train <- paste("sim.", colname_share, sep = "")
  training_short <- data.frame(
    sapply(training_data[, colname_share_train], function(x) (x-min(x))/(max(x)-min(x))),
    fraud.type = training_data[,"fraud.type"])
  
  observed_short <- data.frame(
    sapply(observed_data[, colname_share], 
           function(x) (x-min(x))/(max(x)-min(x))))
  names(training_short) <- str_replace(names(training_short), "sim.", "")
  
  
  # ---- use random forest to model on a subsample of 90% of the training data --#
  # ---------------------to see the performance of the model --------------------#
  flds <- createFolds(seq(1:nrow(training_short)), k = 10, list = TRUE, returnTrain = FALSE)
  rf_trial_perf <- matrix(rep(0, 9), nrow = 3)
  for (k in 1:10){
    test_row <- flds[[k]]
    test_sample <- training_short[test_row,]
    training_sample <- training_short[-test_row,]
    rf_model_trial <- randomForest(training_sample[, colname_share],training_sample$fraud.type, 
                                   ntree = 500, 
                                   mtry = sqrt(length(colname_share)),
                                   important = TRUE)
    rf_trial_perf <- prop.table(table(test_sample$fraud.type, 
                                      predict(rf_model_trial, 
                                              test_sample[, colname_share], type = "response")),1)+rf_trial_perf
  }
  
  
  # now build the model using the full training sample
  rf_model <- randomForest(training_short[, colname_share],training_short$fraud.type, 
                           ntree = 500, 
                           mtry = sqrt(length(colname_share)),
                           important = TRUE)
  observed_data$classif.rf <- predict(rf_model, observed_short)
  
  # turn the class assignment into dummy variables
  observed_data2 <- data.frame(observed_data,
                               dummy(observed_data$classif.rf))
  names(observed_data2) <- c(names(observed_data2)[1:16], "BBS_FPV", 
                             "Clean", "VS_FPV")
  observed_data2$Tainted <- 1 - observed_data2$Clean
  
  # ------------------------ things to output --------------------------------#
  rf_trial_perf <- rf_trial_perf/10
  rf_pred_prob <- prop.table(table((observed_data$classif.rf)))
  rf_pred_prob_names <- names(rf_pred_prob)
  rf_pred_prob <- data.frame(matrix(rf_pred_prob, nrow = 1))
  names(rf_pred_prob) <- rf_pred_prob_names
  rf_pred_prob["Tainted"] <- 1 - rf_pred_prob["Clean"]
  rf_pred_prob["Name"] <- "ALL"
  rf_pred_prob["Code"] <- 0
  
  pred_fraud_prov <- aggregate(observed_data2[,17:ncol(observed_data2)], 
                               FUN = mean,
                               by = list(Name = observed_data2$provincia.nombre,
                                         Code = observed_data2$CODIGO.PROVINCIA))
  # -------look at the distribution by risk type using random forest results ------# 
  cond1 <- observed_data2$classif.rf == "VS_FPV" 
  
  cond2 <- observed_data2$classif.rf == "BBS_FPV" 
  
  ## plot turnout in clean vs. tainted data
  pred_plot_clean <- ggplot(observed_data2[observed_data2$classif.rf == "Clean",])+
    geom_density(aes(x = turnout*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond1,], 
                 aes(x = turnout*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond2,], 
                 aes(x = turnout*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) +
    ylim(0,0.4) +
    xlim(0,100) +
    labs(x = "Turnout", 
         title = "(1)") +
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  ## plot share of the large parties in clean vs. tainted data
  # FPV
  pred_plot_FPV <- ggplot(observed_data2[observed_data2$classif.rf == "Clean",])+
    geom_density(aes(x = p.votes.FPV*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond1,], 
                 aes(x = p.votes.FPV*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond2,], 
                 aes(x = p.votes.FPV*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) +
    ylim(0, 0.15)+
    xlim(0,100) + 
    labs(x = "Vote share - FPV", 
         title = "(2)")+
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  # Cam
  pred_plot_Cam <- ggplot(observed_data2[observed_data2$classif.rf == "Clean",])+
    geom_density(aes(x = p.votes.Cam*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond1,], 
                 aes(x = p.votes.Cam*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond2,], 
                 aes(x = p.votes.Cam*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) +
    ylim(0, 0.15)+
    xlim(0,100) + 
    labs(x= "Vote share - Cambiemos", 
         title = "(3)") +
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  # UNA
  pred_plot_UNA <- ggplot(observed_data2[observed_data2$classif.rf == "Clean",])+
    geom_density(aes(x = p.votes.UNA*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond1,], 
                 aes(x = p.votes.UNA*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond2,], 
                 aes(x = p.votes.UNA*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) +
    ylim(0,0.15)+
    xlim(0,100) + 
    labs(x = "Vote share - UNA", 
         title = "(4)") +
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  # Others
  pred_plot_other <- ggplot(observed_data2[observed_data2$classif.rf == "Clean",])+
    geom_density(aes(x = p.votes.other*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond1,], 
                 aes(x = p.votes.other*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond2,], 
                 aes(x = p.votes.other*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) +
    xlim(0, 50) +
    ylim(0,0.4) + 
    labs(x = "Vote share - Other", 
         title = "(5)") +
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  
  pred_plot <- arrangeGrob(pred_plot_clean + theme(legend.position="none"),
                           mylegend,
                           pred_plot_FPV + theme(legend.position="none"),
                           pred_plot_Cam + theme(legend.position="none"),
                           pred_plot_UNA + theme(legend.position="none"),
                           pred_plot_other + theme(legend.position="none"),
                           ncol=2)
  # --------------------------------------------------------------------------#
  
  return(list(synth_plot = synth_plot,
              rf_trial_perf = rf_trial_perf,
              rf_model = rf_model, 
              rf_pred_prob = rf_pred_prob,
              pred_fraud_prov = pred_fraud_prov,
              pred_plot = pred_plot, 
              mylegend = mylegend, 
              data_pred = observed_data2))
}

g_legend<-function(a.gplot){ # get the legend for plots
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


Sim_Classify_Simp <- function(par){#generate synthetic data and prediction outputs 
  # set the value of each individual parameter
  base.stolen.prop <- par["base.stolen.prop"] # var of interest
  base.bboxstuff.prop <- par["base.bboxstuff.prop"]   # var of interest
  stolen.coef <- par["stolen.coef"]
  bboxstuff.coef <- par["bboxstuff.coef"]
  red.steal.large.party <- par["red.steal.large.party"]
  base.transf.prop <- par["base.transf.prop"]      # var of interest
  base.transf.sd <- par["base.transf.sd"]
  
  stolen.var <- par["stolen.var"]
  bboxstuff.var <- par["bboxstuff.var"]
  stolen.bboxstuff.cov <- par["stolen.bboxstuff.cov"]
  Sigma <- matrix(c(stolen.var, stolen.bboxstuff.cov, 
                    stolen.bboxstuff.cov, bboxstuff.var ), 2, 2)
  
  # using multinomial distribution to determine if there's any kind of fraud
  # and assuming that vote stealing and ballot box stuffing are exclusive events
  set.seed(100)
  base_fraud <- t(rmultinom(nrow(clean_table), 1, 
                            c(base.stolen.prop, base.bboxstuff.prop, 
                              1- base.bboxstuff.prop - base.stolen.prop)))[, 1:2]
  proportion_fraud <- as.data.frame(base_fraud * mvrnorm(n = nrow(clean_table), 
                                                         mu = c(stolen.coef, bboxstuff.coef),
                                                         Sigma = Sigma,
                                                         empirical = TRUE))
  proportion_fraud[proportion_fraud < 0] <- 0 
  names(proportion_fraud) <- c("stolen.prop", "bboxstuff.prop")
  
  proportion_fraud[proportion_fraud > 1] <- 1
  
  # randomly generate the probability of transferring votes
  transf_prop <- rtruncnorm(nrow(clean_table), a = 0, b = 1, 
                            mean = base.transf.prop, sd = base.transf.sd)
  
  # generate synthetic manipulated data
  fraud_table <- data_filt2[, c("CODIGO.PROVINCIA", "CODIGO.CIRCUITO",
                                "CODIGO.DEPARTAMENTO", "CODIGO.MESA")]
  # for small parties
  for (party in c("Cam", "UNA", "oth")){
    col0 <- paste("pred.votos", party, sep = ".")
    col1 <- paste("votos", party, "loss", sep = ".")
    col2 <- paste("votos", party, "manip", sep = ".")
    fraud_table[[col1]] <- round(clean_table[[col0]] * proportion_fraud$stolen.prop)
    fraud_table[[col2]] <- clean_table[[col0]] - fraud_table[[col1]]
  }
  fraud_table$small.party.loss <- rowSums(
    fraud_table[, c("votos.Cam.loss", "votos.UNA.loss", "votos.oth.loss" )])
  # for FPV
  for (party in c("FPV")){
    col0 <- paste("pred.votos", party, sep = ".")
    col <- paste("votos",party, "manip", sep = ".")
    fraud_table[[col]] <-  round(clean_table[[col0]] + 
                                   transf_prop*fraud_table$small.party.loss +
                                   proportion_fraud$bboxstuff.prop*clean_table$pred.votos.abst)
  }
  fraud_table$abstention.manip <- round(clean_table$pred.votos.abst + 
                                          (1-transf_prop)*fraud_table$small.party.loss -
                                          proportion_fraud$bboxstuff.prop*clean_table$pred.votos.abst)
  
  training_data <- bind_cols(data_filt2[, c("CODIGO.PROVINCIA", "CODIGO.CIRCUITO", 
                                        "CODIGO.DEPARTAMENTO", "CODIGO.MESA", 
                                        "VOTOS.electores")],
                         data.frame(pred.votos.residual = clean_table[, "pred.votos.residual"]),
                         fraud_table[, grep("manip", names(fraud_table))],
                         proportion_fraud) %>% # generate relevant vote share vars
    mutate(sim.calc.total.votes = VOTOS.electores - abstention.manip, 
           sim.turnout = sim.calc.total.votes/VOTOS.electores,
           sim.p.votes.FPV = votos.FPV.manip/VOTOS.electores,
           sim.p.votes.Cam = votos.Cam.manip/VOTOS.electores,
           sim.p.votes.UNA = votos.UNA.manip/VOTOS.electores, 
           sim.p.votes.oth = votos.oth.manip/VOTOS.electores, 
           sim.p.residual = pred.votos.residual/VOTOS.electores)
  
  # -------------------- create risk type -------------------------------------#
  training_data$fraud.type <- factor(case_when(
    training_data$stolen.prop == 0 & training_data$bboxstuff.prop == 0 ~ "Clean", 
    training_data$stolen.prop > 0 & training_data$bboxstuff.prop == 0 ~ "VS_FPV",
    training_data$stolen.prop == 0 & training_data$bboxstuff.prop > 0 ~ "BBS_FPV"
  ))
  
  
  # -------------------------- Table/Plots to output -------------------------#
  
  #--- plot the turnout density of different risk types (Figure 1 in paper) --
  cond1 <- training_data$fraud.type == "VS_FPV" 
  
  cond2 <- training_data$fraud.type == "BBS_FPV" 
  
  ## plot turnout in clean vs. tainted data
  synth_plot_clean <- ggplot(training_data[training_data$fraud.type == "Clean",])+
    geom_density(aes(x = sim.turnout*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    ylim(0,0.4) +
    xlim(0,100) + 
    geom_density(data = training_data[cond1,], 
                 aes(x = sim.turnout*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = training_data[cond2,], 
                 aes(x = sim.turnout*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) + 
    theme(legend.position = "right") + 
    labs(x = "Turnout (%)", 
         title = "(1)") +
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  mylegend <- g_legend(synth_plot_clean)
  
  ## plot share of the large parties in clean vs. tainted data
  # FPV
  synth_plot_FPV <- ggplot(training_data[training_data$fraud.type == "Clean",])+
    geom_density(aes(x = sim.p.votes.FPV*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    ylim(0,0.15) +
    xlim(0,100) + 
    geom_density(data = training_data[cond1,], 
                 aes(x = sim.p.votes.FPV*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = training_data[cond2,], 
                 aes(x = sim.p.votes.FPV*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2)+ 
    labs(x = "Vote share - FPV (%)", 
         title = "(2)")+
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  # Cam
  synth_plot_Cam <- ggplot(training_data[training_data$fraud.type == "Clean",])+
    geom_density(aes(x = sim.p.votes.Cam*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    ylim(0,0.15) +
    xlim(0,100) + 
    geom_density(data = training_data[cond1,], 
                 aes(x = sim.p.votes.Cam*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = training_data[cond2,], 
                 aes(x = sim.p.votes.Cam*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) + 
    labs(x = "Vote share - Cambiemos (%)", 
         title = "(3)")+
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  # UNA
  synth_plot_UNA <- ggplot(training_data[training_data$fraud.type == "Clean",])+
    geom_density(aes(x = sim.p.votes.UNA*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    ylim(0,0.15) +
    xlim(0,100) + 
    geom_density(data = training_data[cond1,], 
                 aes(x = sim.p.votes.UNA*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = training_data[cond2,], 
                 aes(x = sim.p.votes.UNA*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2)+ 
    labs(x = "Vote share-UNA (%)", 
         title = "(4)")+
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  
  # Other
  synth_plot_oth <- ggplot(training_data[training_data$fraud.type == "Clean",])+
    geom_density(aes(x = sim.p.votes.oth*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    geom_density(data = training_data[cond1,], 
                 aes(x = sim.p.votes.oth*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = training_data[cond2,], 
                 aes(x = sim.p.votes.oth*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2)+
    xlim(0, 50) +
    ylim(0, 0.4) + 
    labs(x = "Vote share - Others (%)", 
         title = "(5)")+
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  synth_plot <- arrangeGrob(synth_plot_clean + theme(legend.position="none"),
                            mylegend,
                            synth_plot_FPV + theme(legend.position="none"),
                            synth_plot_Cam + theme(legend.position="none"),
                            synth_plot_UNA + theme(legend.position="none"),
                            synth_plot_oth + theme(legend.position="none"),
                            ncol=2)
  
  
  #----------------------------------------------------------------------------#
  
  # get the right data to build the model
  names(training_data) <- str_replace(names(training_data), "oth", "other")
  colname_share <- c("turnout", "p.votes.FPV", "p.votes.Cam", 
                     "p.votes.UNA", "p.votes.other")
  colname_share_train <- paste("sim.", colname_share, sep = "")
  training_short <- data.frame(
    sapply(training_data[, colname_share_train], function(x) (x-min(x))/(max(x)-min(x))),
    fraud.type = training_data[,"fraud.type"])
  
  observed_short <- data.frame(
    sapply(observed_data[, colname_share], 
           function(x) (x-min(x))/(max(x)-min(x))))
  names(training_short) <- str_replace(names(training_short), "sim.", "")
  
  
  # ---- use random forest to model on a subsample of 90% of the training data --#
  # ---------------------to see the performance of the model --------------------#
  flds <- createFolds(seq(1:nrow(training_short)), k = 10, list = TRUE, returnTrain = FALSE)
  rf_trial_perf <- matrix(rep(0, 9), nrow = 3)
  for (k in 1:10){
    test_row <- flds[[k]]
    test_sample <- training_short[test_row,]
    training_sample <- training_short[-test_row,]
    rf_model_trial <- randomForest(training_sample[, colname_share],training_sample$fraud.type, 
                                   ntree = 500, 
                                   mtry = sqrt(length(colname_share)),
                                   important = TRUE)
    rf_trial_perf <- prop.table(table(test_sample$fraud.type, 
                                      predict(rf_model_trial, 
                                              test_sample[, colname_share], type = "response")),1)+rf_trial_perf
  }
  
  
  # now build the model using the full training sample
  rf_model <- randomForest(training_short[, colname_share],training_short$fraud.type, 
                           ntree = 500, 
                           mtry = sqrt(length(colname_share)),
                           important = TRUE)
  observed_data$classif.rf <- predict(rf_model, observed_short)
  
  # turn the class assignment into dummy variables
  observed_data2 <- data.frame(observed_data,
                               dummy(observed_data$classif.rf))
  names(observed_data2) <- c(names(observed_data2)[1:16], "BBS_FPV", 
                             "Clean", "VS_FPV")
  observed_data2$Tainted <- 1 - observed_data2$Clean
  
  # ------------------------ things to output --------------------------------#
  rf_trial_perf <- rf_trial_perf/10
  rf_pred_prob <- prop.table(table((observed_data$classif.rf)))
  rf_pred_prob_names <- names(rf_pred_prob)
  rf_pred_prob <- data.frame(matrix(rf_pred_prob, nrow = 1))
  names(rf_pred_prob) <- rf_pred_prob_names
  rf_pred_prob["Tainted"] <- 1 - rf_pred_prob["Clean"]
  rf_pred_prob["Name"] <- "ALL"
  rf_pred_prob["Code"] <- 0
  
  pred_fraud_prov <- aggregate(observed_data2[,17:ncol(observed_data2)], 
                               FUN = mean,
                               by = list(Name = observed_data2$provincia.nombre,
                                         Code = observed_data2$CODIGO.PROVINCIA))
  # -------look at the distribution by risk type using random forest results ------# 
  cond1 <- observed_data2$classif.rf == "VS_FPV" 
  
  cond2 <- observed_data2$classif.rf == "BBS_FPV" 
  
  ## plot turnout in clean vs. tainted data
  pred_plot_clean <- ggplot(observed_data2[observed_data2$classif.rf == "Clean",])+
    geom_density(aes(x = turnout*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond1,], 
                 aes(x = turnout*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond2,], 
                 aes(x = turnout*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) +
    ylim(0,0.4)+
    xlim(0,100) + 
    labs(x = "Turnout", 
         title = "(1)") +
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  ## plot share of the large parties in clean vs. tainted data
  # FPV
  pred_plot_FPV <- ggplot(observed_data2[observed_data2$classif.rf == "Clean",])+
    geom_density(aes(x = p.votes.FPV*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond1,], 
                 aes(x = p.votes.FPV*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond2,], 
                 aes(x = p.votes.FPV*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) +
    ylim(0, 0.15)+
    xlim(0,100) + 
    labs(x = "Vote share - FPV", 
         title = "(2)")+
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  # Cam
  pred_plot_Cam <- ggplot(observed_data2[observed_data2$classif.rf == "Clean",])+
    geom_density(aes(x = p.votes.Cam*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond1,], 
                 aes(x = p.votes.Cam*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond2,], 
                 aes(x = p.votes.Cam*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) +
    ylim(0, 0.15)+
    xlim(0,100) + 
    labs(x= "Vote share - Cambiemos", 
         title = "(3)") +
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  # UNA
  pred_plot_UNA <- ggplot(observed_data2[observed_data2$classif.rf == "Clean",])+
    geom_density(aes(x = p.votes.UNA*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond1,], 
                 aes(x = p.votes.UNA*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond2,], 
                 aes(x = p.votes.UNA*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) +
    ylim(0,0.15)+
    xlim(0,100) + 
    labs(x = "Vote share - UNA", 
         title = "(4)") +
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  # Others
  pred_plot_other <- ggplot(observed_data2[observed_data2$classif.rf == "Clean",])+
    geom_density(aes(x = p.votes.other*100, y = ..density.., color = "clean  "), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond1,], 
                 aes(x = p.votes.other*100, y = ..density.., color = "vote stealing"), 
                 adjust = 1.2) +
    geom_density(data = observed_data2[cond2,], 
                 aes(x = p.votes.other*100, y = ..density.., color = "ballot box stuffing  "), 
                 adjust = 1.2) +
    xlim(0, 50)+
    ylim(0,0.4)+ 
    labs(x = "Vote share - Other", 
         title = "(5)") +
    theme(
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 13, hjust = 0.5), 
      axis.title.y=element_blank(),
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13)) +
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"))
  
  
  pred_plot <- arrangeGrob(pred_plot_clean + theme(legend.position="none"),
                           mylegend,
                           pred_plot_FPV + theme(legend.position="none"),
                           pred_plot_Cam + theme(legend.position="none"),
                           pred_plot_UNA + theme(legend.position="none"),
                           pred_plot_other + theme(legend.position="none"),
                           ncol=2)
  # --------------------------------------------------------------------------#
  
  return(list(synth_plot = synth_plot,
              rf_trial_perf = rf_trial_perf,
              rf_model = rf_model, 
              rf_pred_prob = rf_pred_prob,
              pred_fraud_prov = pred_fraud_prov,
              pred_plot = pred_plot, 
              mylegend = mylegend, 
              data_pred = observed_data2))
}

g_legend<-function(a.gplot){ # get the legend for plots
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


Sim_Classify_Simp <- function(par){#generate synthetic data and prediction outputs 
  # set the value of each individual parameter
  base.stolen.prop <- par["base.stolen.prop"] # var of interest
  base.bboxstuff.prop <- par["base.bboxstuff.prop"]   # var of interest
  stolen.coef <- par["stolen.coef"]
  bboxstuff.coef <- par["bboxstuff.coef"]
  red.steal.large.party <- par["red.steal.large.party"]
  base.transf.prop <- par["base.transf.prop"]      # var of interest
  base.transf.sd <- par["base.transf.sd"]
  
  stolen.var <- par["stolen.var"]
  bboxstuff.var <- par["bboxstuff.var"]
  stolen.bboxstuff.cov <- par["stolen.bboxstuff.cov"]
  Sigma <- matrix(c(stolen.var, stolen.bboxstuff.cov, 
                    stolen.bboxstuff.cov, bboxstuff.var ), 2, 2)
  
  # using multinomial distribution to determine if there's any kind of fraud
  # and assuming that vote stealing and ballot box stuffing are exclusive events
  set.seed(100)
  base_fraud <- t(rmultinom(nrow(clean_table), 1, 
                            c(base.stolen.prop, base.bboxstuff.prop, 
                              1- base.bboxstuff.prop - base.stolen.prop)))[, 1:2]
  proportion_fraud <- as.data.frame(base_fraud * mvrnorm(n = nrow(clean_table), 
                                                         mu = c(stolen.coef, bboxstuff.coef),
                                                         Sigma = Sigma,
                                                         empirical = TRUE))
  proportion_fraud[proportion_fraud < 0] <- 0 
  names(proportion_fraud) <- c("stolen.prop", "bboxstuff.prop")
  
  proportion_fraud[proportion_fraud > 1] <- 1
  
  # randomly generate the probability of transferring votes
  transf_prop <- rtruncnorm(nrow(clean_table), a = 0, b = 1, 
                            mean = base.transf.prop, sd = base.transf.sd)
  
  # generate synthetic manipulated data
  fraud_table <- data_filt2[, c("CODIGO.PROVINCIA", "CODIGO.CIRCUITO",
                                "CODIGO.DEPARTAMENTO", "CODIGO.MESA")]
  # for small parties
  for (party in c("Cam", "UNA", "oth")){
    col0 <- paste("pred.votos", party, sep = ".")
    col1 <- paste("votos", party, "loss", sep = ".")
    col2 <- paste("votos", party, "manip", sep = ".")
    fraud_table[[col1]] <- round(clean_table[[col0]] * proportion_fraud$stolen.prop)
    fraud_table[[col2]] <- clean_table[[col0]] - fraud_table[[col1]]
  }
  fraud_table$small.party.loss <- rowSums(
    fraud_table[, c("votos.Cam.loss", "votos.UNA.loss", "votos.oth.loss" )])
  # for FPV
  for (party in c("FPV")){
    col0 <- paste("pred.votos", party, sep = ".")
    col <- paste("votos",party, "manip", sep = ".")
    fraud_table[[col]] <-  round(clean_table[[col0]] + 
                                   transf_prop*fraud_table$small.party.loss +
                                   proportion_fraud$bboxstuff.prop*clean_table$pred.votos.abst)
  }
  fraud_table$abstention.manip <- round(clean_table$pred.votos.abst + 
                                          (1-transf_prop)*fraud_table$small.party.loss -
                                          proportion_fraud$bboxstuff.prop*clean_table$pred.votos.abst)
  
  training_data <- bind_cols(data_filt2[, c("CODIGO.PROVINCIA", "CODIGO.CIRCUITO", 
                                            "CODIGO.DEPARTAMENTO", "CODIGO.MESA", 
                                            "VOTOS.electores")],
                             data.frame(pred.votos.residual = clean_table[, "pred.votos.residual"]),
                             fraud_table[, grep("manip", names(fraud_table))],
                             proportion_fraud) %>% # generate relevant vote share vars
    mutate(sim.calc.total.votes = VOTOS.electores - abstention.manip, 
           sim.turnout = sim.calc.total.votes/VOTOS.electores,
           sim.p.votes.FPV = votos.FPV.manip/VOTOS.electores,
           sim.p.votes.Cam = votos.Cam.manip/VOTOS.electores,
           sim.p.votes.UNA = votos.UNA.manip/VOTOS.electores, 
           sim.p.votes.oth = votos.oth.manip/VOTOS.electores, 
           sim.p.residual = pred.votos.residual/VOTOS.electores)
  
  # -------------------- create risk type -------------------------------------#
  training_data$fraud.type <- factor(case_when(
    training_data$stolen.prop == 0 & training_data$bboxstuff.prop == 0 ~ "Clean", 
    training_data$stolen.prop > 0 & training_data$bboxstuff.prop == 0 ~ "VS_FPV",
    training_data$stolen.prop == 0 & training_data$bboxstuff.prop > 0 ~ "BBS_FPV"
  ))
  
  # get the right data to build the model
  names(training_data) <- str_replace(names(training_data), "oth", "other")
  colname_share <- c("turnout", "p.votes.FPV", "p.votes.Cam", 
                     "p.votes.UNA", "p.votes.other")
  colname_share_train <- paste("sim.", colname_share, sep = "")
  training_short <- data.frame(
    sapply(training_data[, colname_share_train], function(x) (x-min(x))/(max(x)-min(x))),
    fraud.type = training_data[,"fraud.type"])
  
  observed_short <- data.frame(
    sapply(observed_data[, colname_share], 
           function(x) (x-min(x))/(max(x)-min(x))))
  names(training_short) <- str_replace(names(training_short), "sim.", "")
  
  
  # now build the model using the full training sample
  rf_model <- randomForest(training_short[, colname_share],training_short$fraud.type, 
                           ntree = 500, 
                           mtry = sqrt(length(colname_share)),
                           important = TRUE)
  observed_data$classif.rf <- predict(rf_model, observed_short)
  
  # turn the class assignment into dummy variables
  observed_data2 <- data.frame(observed_data,
                               dummy(observed_data$classif.rf))
  names(observed_data2) <- c(names(observed_data2)[1:16], "BBS_FPV", 
                             "Clean", "VS_FPV")
  observed_data2$Tainted <- 1 - observed_data2$Clean
  
  # ------------------------ things to output --------------------------------#
  rf_pred_prob <- prop.table(table((observed_data$classif.rf)))
  rf_pred_prob_names <- names(rf_pred_prob)
  rf_pred_prob <- data.frame(matrix(rf_pred_prob, nrow = 1))
  names(rf_pred_prob) <- rf_pred_prob_names
  rf_pred_prob["Tainted"] <- 1 - rf_pred_prob["Clean"]
  rf_pred_prob["Name"] <- "ALL"
  rf_pred_prob["Code"] <- 0
  
  pred_fraud_prov <- aggregate(observed_data2[,17:ncol(observed_data2)], 
                               FUN = mean,
                               by = list(Name = observed_data2$provincia.nombre,
                                         Code = observed_data2$CODIGO.PROVINCIA))
  
  return(list(rf_pred_prob = rf_pred_prob,
              pred_fraud_prov = pred_fraud_prov))
}
