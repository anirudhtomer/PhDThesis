library(ggplot2)
library(splines)
library(JMbayes)
library(doParallel)

reset = function(){
  parent_env = parent.env(environment())
  rm(list = setdiff(ls(name=parent_env), "reset"), envir = parent_env)
  load("Rdata/colors.Rdata", envir = parent_env)
  load("Rdata/shapes.Rdata", envir = parent_env)
  load("Rdata/chapter2/plotDataList.Rdata", envir = parent_env)
  load("Rdata/chapter2/cutoffValues_PRIAS.Rdata", envir = parent_env)
  load("Rdata/chapter2/cleandata.Rdata", envir = parent_env)
  
  FONT_SIZE = 10
  MEDIUM_LINE = 0.5
  NORMAL_POINT_SIZE = 2
  BIG_POINT_SIZE = 4
  LABEL_SIZE = 3.5
  
  assign("FONT_SIZE", FONT_SIZE, envir = parent_env)
  assign("MEDIUM_LINE", MEDIUM_LINE, envir = parent_env)
  assign("BIG_POINT_SIZE", BIG_POINT_SIZE, envir = parent_env)
  assign("NORMAL_POINT_SIZE", NORMAL_POINT_SIZE, envir = parent_env)
  assign("LABEL_SIZE", LABEL_SIZE, envir = parent_env)
  
  NORMAL_PLOT_SIZE = list(width=130, height=115)
  #Normal size uses 4:3 aspect ratio, but after adjusting for legend etc roughly
  assign("NORMAL_PLOT_SIZE", NORMAL_PLOT_SIZE, envir = parent_env)
  assign("FULL_PLOT_SIZE", list(width=130, height=130), envir = parent_env)
  
  assign("saveFigure", function(object, filename, size=NORMAL_PLOT_SIZE){
    ggsave(plot=object, filename = filename,
           device = cairo_pdf, units = "mm", width = size$width, 
           height = size$height, dpi = 1200)
  }, envir = parent_env)
  
  
  baseggplot = ggplot() + 
    theme_bw() + 
    theme(plot.margin = margin(0,0,0,0, "mm"), 
          axis.text = element_text(size=FONT_SIZE),
          axis.title = element_text(size=FONT_SIZE),
          plot.title = element_text(size=FONT_SIZE),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=FONT_SIZE-2))
  
  baseggplot_no_xticks = baseggplot + 
    theme(axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())
  
  baseggplot_no_yticks = baseggplot + 
    theme(axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
  
  assign("baseggplot_no_xticks", baseggplot_no_xticks, envir = parent_env)
  assign("baseggplot_no_yticks", baseggplot_no_yticks, envir = parent_env)
  assign("baseggplot", baseggplot, envir = parent_env)
}

########################################
#Figure 1 is a tikzpicture made in latex
########################################

########################################
#Figure 2
########################################
reset()
source("src/expectedCondFailureTime.R")
source("src/varCondFailureTime.R")
load("Rdata/chapter2/fitted_jm.Rdata")

modifyScheduledBiopsyTime = function(proposedTime, curVisitTime, lastBiopsyTime){
  if(proposedTime < curVisitTime){
    if(curVisitTime - lastBiopsyTime <= 1){
      return(lastBiopsyTime + 1)
    }else{
      return(curVisitTime)
    }
  }else{
    if(proposedTime - lastBiopsyTime <= 1){
      return(lastBiopsyTime + 1)
    }else{
      return(proposedTime)
    }
  }
}

prias_long_911 = prias_long[prias_long$P_ID==911,]

maxPossibleFailureTime = 20
plotList = vector("list", nrow(prias_long_911))
#########################
#Figure 2a

prias_long_911_12obs = prias_long_911[1:12, ]
curVisitTime_a = tail(prias_long_911_12obs$visitTimeYears, 1)
lastBiopsyTime_a = tail(prias_long_911_12obs$visitTimeYears[!is.na(prias_long_911_12obs$gleason)],1)
survTimes = seq(lastBiopsyTime_a, maxPossibleFailureTime, 0.1)
survFitObj_a = survfitJM(mvjoint_log2psa_plus1_spline_pt1pt54_pt1, prias_long_911_12obs[!is.na(prias_long_911_12obs$psa),],
                         idVar="P_ID", last.time = lastBiopsyTime_a,
                         survTimes = survTimes)
survProbs = c(1, survFitObj_a$summaries[[1]][, "Median"])

nearest_time_index = which(abs(dynamicCutoffTimes_PRIAS-lastBiopsyTime_a)==min(abs(dynamicCutoffTimes_PRIAS-lastBiopsyTime_a)))[1]
survProbF1Score = cutoffValues_PRIAS[[nearest_time_index]]["f1score"]

survTimeF1Score_a = survTimes[which(abs(survProbs-survProbF1Score)==min(abs(survProbs-survProbF1Score)))[1]]
survTimeF1Score_a = modifyScheduledBiopsyTime(survTimeF1Score_a, curVisitTime_a, lastBiopsyTime_a)
expectedFailureTime_a = expectedCondFailureTime(mvjoint_log2psa_plus1_spline_pt1pt54_pt1, idVar = "P_ID", 
                                                prias_long_911_12obs[!is.na(prias_long_911_12obs$psa),], 
                                                last.time = lastBiopsyTime_a, maxPossibleFailureTime = maxPossibleFailureTime)
expectedFailureTime_a = modifyScheduledBiopsyTime(expectedFailureTime_a, curVisitTime_a, lastBiopsyTime_a)

figure2a = baseggplot_no_xticks + geom_segment(aes(x=lastBiopsyTime_a, xend=lastBiopsyTime_a, 
                                                   y=-Inf, yend=2),
                                               linetype="solid", color=GREEN, size = MEDIUM_LINE) + 
  geom_label(aes(x=lastBiopsyTime_a, y=2, label="Latest\nBiopsy"), 
             color=WHITE, fill=GREEN, size=LABEL_SIZE) +
  geom_segment(aes(x=expectedFailureTime_a, xend=expectedFailureTime_a, 
                   y=-Inf, yend=1),
               linetype="solid", color=ORANGE, size = MEDIUM_LINE) + 
  geom_label(aes(x=expectedFailureTime_a, y=1, label="Exp. GR\nTime"), 
             color=WHITE, fill=ORANGE, size=LABEL_SIZE) +
  geom_segment(aes(x=survTimeF1Score_a, xend=survTimeF1Score_a, 
                   y=-Inf, yend=0.7),
               linetype="solid", color=ORANGE, size = MEDIUM_LINE) + 
  geom_label(aes(x=survTimeF1Score_a, y=0.7, label="Dyn. Risk\nGR"), 
             color=WHITE, fill=ORANGE, size=LABEL_SIZE) +
  geom_line(data=prias_long_911_12obs,
            aes(x=visitTimeYears, y=log2psa_plus1),
            color=GREY, linetype="dashed") +
  geom_point(data=prias_long_911_12obs,
             aes(x=visitTimeYears, y=log2psa_plus1),
             size=NORMAL_POINT_SIZE) +
  geom_line(aes(x=survFitObj_a$fitted.times[[1]], 
                y=survFitObj_a$fitted.y[[1]]$log2psa_plus1))+
  scale_x_continuous(breaks=seq(0,16,2), 
                     labels=c("0\n(Start AS)", seq(2, 16, 2)),
                     limits=c(0,17)) +
  scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
  xlab("Follow-up time (years)") + 
  ylab(expression('log'[2]*' (PSA + 1)'))

#########################
#Figure 2b

prias_long_911_15obs = prias_long_911[1:15, ]
curVisitTime_b = tail(prias_long_911_15obs$visitTimeYears, 1)
lastBiopsyTime_b = tail(prias_long_911_15obs$visitTimeYears[!is.na(prias_long_911_15obs$gleason)],1)
survTimes = seq(lastBiopsyTime_b, maxPossibleFailureTime, 0.1)
survFitObj_b = survfitJM(mvjoint_log2psa_plus1_spline_pt1pt54_pt1, prias_long_911_15obs[!is.na(prias_long_911_15obs$psa),],
                         idVar="P_ID", last.time = lastBiopsyTime_b,
                         survTimes = survTimes)
survProbs = c(1, survFitObj_b$summaries[[1]][, "Median"])

nearest_time_index = which(abs(dynamicCutoffTimes_PRIAS-lastBiopsyTime_b)==min(abs(dynamicCutoffTimes_PRIAS-lastBiopsyTime_b)))[1]
survProbF1Score = cutoffValues_PRIAS[[nearest_time_index]]["f1score"]

survTimeF1Score_b = survTimes[which(abs(survProbs-survProbF1Score)==min(abs(survProbs-survProbF1Score)))[1]]
survTimeF1Score_b = modifyScheduledBiopsyTime(survTimeF1Score_b, curVisitTime_b, lastBiopsyTime_b)
expectedFailureTime_b = expectedCondFailureTime(mvjoint_log2psa_plus1_spline_pt1pt54_pt1, idVar = "P_ID", 
                                                prias_long_911_15obs[!is.na(prias_long_911_15obs$psa),], 
                                                last.time = lastBiopsyTime_b, maxPossibleFailureTime = maxPossibleFailureTime)
expectedFailureTime_b = modifyScheduledBiopsyTime(expectedFailureTime_b, curVisitTime_b, lastBiopsyTime_b)

figure2b = baseggplot + geom_segment(aes(x=lastBiopsyTime_b, xend=lastBiopsyTime_b, 
                                         y=-Inf, yend=1),
                                     linetype="solid", color=GREEN, size = MEDIUM_LINE) + 
  geom_label(aes(x=lastBiopsyTime_b, y=1, label="Latest\nBiopsy"), 
             color=WHITE, fill=GREEN, size=LABEL_SIZE) +
  geom_segment(aes(x=survTimeF1Score_b, xend=survTimeF1Score_b, 
                   y=-Inf, yend=0.8),
               linetype="solid", color=ORANGE, size = MEDIUM_LINE) + 
  geom_label(aes(x=survTimeF1Score_b, y=0.8, label="Dyn. Risk\nGR"), 
             color=WHITE, fill=ORANGE, size=LABEL_SIZE) +
  geom_segment(aes(x=expectedFailureTime_b, xend=expectedFailureTime_b, 
                   y=-Inf, yend=3),
               linetype="solid", color=ORANGE, size = MEDIUM_LINE) + 
  geom_label(aes(x=expectedFailureTime_b, y=3, label="Exp. GR\nTime"), 
             color=WHITE, fill=ORANGE, size=LABEL_SIZE) +
  geom_line(data=prias_long_911_15obs,
            aes(x=visitTimeYears, y=log2psa_plus1),
            color=GREY, linetype="dashed") +
  geom_point(data=prias_long_911_15obs,
             aes(x=visitTimeYears, y=log2psa_plus1),
             size=NORMAL_POINT_SIZE) +
  geom_line(aes(x=survFitObj_b$fitted.times[[1]], 
                y=survFitObj_b$fitted.y[[1]]$log2psa_plus1))+
  scale_x_continuous(breaks=seq(0,16,2), 
                     labels=c("0\n(Start AS)", seq(2, 16, 2)),
                     limits=c(0,17)) +
  scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
  xlab("Follow-up time (years)") + 
  ylab(expression('log'[2]*' (PSA + 1)'))

figure2ab = ggpubr::ggarrange(figure2a, figure2b, nrow = 2, ncol=1,
                              align = "v", labels = "AUTO",
                              heights = c(1, 1.1), hjust = -15)
saveFigure(figure2ab, filename = "latex/contents/c2/images/c2_fig2ab.pdf", size = FULL_PLOT_SIZE)

##############################
#Figure 2c
ct= makeCluster(detectCores())
registerDoParallel(ct)
prias_long_911$variances=foreach(k=1:nrow(prias_long_911),.combine='c', .export=c("varCondFailureTime"),
                                 .packages=c("JMbayes", "splines")) %dopar%{
                                   subset = prias_long_911[1:k,]
                                   last.time = tail(subset$visitTimeYears[!is.na(subset$gleason)],1)
                                   return(varCondFailureTime(mvjoint_log2psa_plus1_spline_pt1pt54_pt1, subset[!is.na(subset$psa),], "P_ID", 
                                                             last.time, maxPossibleFailureTime = 20))
                                 }
stopCluster(ct)

biopsyTimes = prias_long_911$visitTimeYears[!is.na(prias_long_911$gleason)]

figure2c = baseggplot + 
  geom_segment(aes(x=biopsyTimes, xend=biopsyTimes, y=-Inf, yend=1.5),
               linetype="solid", color=GREEN, size = MEDIUM_LINE) + 
  geom_label(aes(x=biopsyTimes[1:2], y=rep(1.5,2), label="Biopsy"), 
             color=WHITE, fill=GREEN, size=LABEL_SIZE) +
  geom_label(aes(x=biopsyTimes[3], y=1.5, label="Latest\nBiopsy"), 
             color=WHITE, fill=GREEN, size=LABEL_SIZE) +
  geom_line(data=prias_long_911, 
            aes(x=visitTimeYears, y=sqrt(variances))) + 
  xlab("Follow-up time (years)") + 
  ylab(expression('SD'[g]*'(T'^{'*'}*''[j]*')')) +
  scale_y_continuous(breaks = 0:8, limits = c(0,8)) +
  scale_x_continuous(breaks = 0:6, 
                     labels = c("0\n(Start AS)", as.character(1:6)),
                     limits = c(-0.5,6))

saveFigure(figure2c, filename = "latex/contents/c2/images/c2_fig2c.pdf", size = NORMAL_PLOT_SIZE)

########################################
#Figure 3
########################################
reset()
personalized_indices  = c(1:3, 6)
fixed_indices = 4:5

figure3 = baseggplot + 
  geom_point(aes(x=plotDataList$nbVsOffset_all$meanNb_all,
                 y=plotDataList$nbVsOffset_all$meanOffset_all,
                 shape=plotDataList$nbVsOffset_all$Type, 
                 color=plotDataList$nbVsOffset_all$Type), 
             size=BIG_POINT_SIZE) +
  geom_label(aes(x=plotDataList$nbVsOffset_all$meanNb_all[personalized_indices],
                 y=plotDataList$nbVsOffset_all$meanOffset_all[personalized_indices],
                 label=plotDataList$nbVsOffset_all$labels[personalized_indices]),
             fill=GREEN, color=WHITE, size=LABEL_SIZE, 
             nudge_y = c(1.1, 0, -1, 0), nudge_x = c(0, -0.7, 0, -0.45)) +
  geom_label(aes(x=plotDataList$nbVsOffset_all$meanNb_all[fixed_indices],
                 y=plotDataList$nbVsOffset_all$meanOffset_all[fixed_indices],
                 label=plotDataList$nbVsOffset_all$labels[fixed_indices]),
             fill=RED, color=WHITE, size=LABEL_SIZE,
             nudge_y = c(1,-1)) +
  scale_color_manual(values=c(RED, GREEN)) + 
  scale_shape_manual(values=c(SQUARE, TRIANGLE)) + 
  scale_y_continuous(labels=seq(0,16.5,2), breaks=seq(0,16.5,2), limits=c(0,16.5))+
  scale_x_continuous(labels = 1:6, breaks = 1:6, limits = c(1,6)) +
  xlab("Mean number of biopsies") + 
  ylab("Mean biopsy offset (months)")

saveFigure(figure3, filename = "latex/contents/c2/images/c2_fig3.pdf", size = NORMAL_PLOT_SIZE)

########################################
#Figure 4
########################################
reset()

figure4a = baseggplot + 
  geom_boxplot(data=plotDataList$boxplotDf,
               aes(ymin=nb.all.ymin, 
                   lower=nb.all.lower, 
                   middle=nb.all.middle, 
                   upper=nb.all.upper, 
                   ymax=nb.all.ymax, 
                   x=reorder(methodName, methodName, 
                             FUN = function(x){nchar(as.character(x))})),
               stat="identity") + 
  xlab("Schedule") + ylab("Number of biopsies") + 
  scale_y_continuous(breaks = seq(1, 16, 3), limits = c(0,16))

figure4b = baseggplot_no_xticks +
  geom_boxplot(data=plotDataList$boxplotDf,
               aes(ymin=offset.all.ymin, 
                   lower=offset.all.lower, 
                   middle=offset.all.middle, 
                   upper=offset.all.upper, 
                   ymax=offset.all.ymax, 
                   x=reorder(methodName, methodName, 
                             FUN = function(x){nchar(as.character(x))})),
               stat="identity") + 
  xlab("Schedule") + ylab("Biopsy offset (months)") 

figure4 = ggpubr::ggarrange(figure4b, figure4a,
                            align = "v", ncol = 1, nrow=2, 
                            heights = c(1, 1.15),
                            hjust = -3.5, labels = "AUTO")

saveFigure(figure4, filename = "latex/contents/c2/images/c2_fig4.pdf",
           size = FULL_PLOT_SIZE)

########################################
#Figure Appendix 1
########################################
reset()
load("Rdata/chapter2/fitted_jm.Rdata")
#First we do the PSA fit
visitTimeYears = seq(0,10, 0.5)
df = data.frame(Age = 70, visitTimeYears)
betas_psa_matrix = mvjoint_log2psa_plus1_spline_pt1pt54_pt1$mcmc$betas1

fixedPSAFormula = ~ 1 +I(Age - 70) +  I((Age - 70)^2) + ns(visitTimeYears, knots=c(0.1, 0.5, 4), Boundary.knots=c(0, 7))

xbeta = model.matrix(fixedPSAFormula, df) %*% t(betas_psa_matrix)

meanXbeta = apply(xbeta, 1, mean)
upperXbeta = apply(xbeta, 1, quantile, probs=0.975)
lowerXbeta = apply(xbeta, 1, quantile, probs=0.025)

figure_app1 = baseggplot + 
  geom_line(aes(x=visitTimeYears, y=meanXbeta)) +
  geom_ribbon(aes(x=visitTimeYears, ymin = lowerXbeta, ymax = upperXbeta), 
              fill="grey", alpha=0.5) + 
  xlab("Follow-up time (years)") + ylab(expression('log'[2]*'(PSA + 1)'))

saveFigure(figure_app1, filename = "latex/contents/c2/images/c2_fig_app1.pdf",
           size = NORMAL_PLOT_SIZE)

#####################
# Figure Appendix 2
#####################
reset()
load("Rdata/chapter2/fitted_jm.Rdata")

plotFittedPSASubject = function(){
  data.id = mvjoint_log2psa_plus1_spline_pt1pt54_pt1$model_info$coxph_components$data
  repeat{
    pid = sample(data.id$P_ID, size = 1)
    print(paste("Choosing patient", pid, "randomly because pid not provided"))
    
    data = mvjoint_log2psa_plus1_spline_pt1pt54_pt1$model_info$mvglmer_components$data
    data = data[data$P_ID == pid,]
    lastBiopsyTime =  max(data$visitTimeYears[!is.na(data$gleason)])
    data = data[!is.na(data$log2psa_plus1),]
    if(nrow(data)>4){
      break
    }
  }
  
  rowNums = mvjoint_log2psa_plus1_spline_pt1pt54_pt1$model_info$mvglmer_components$id1
  psaFit = fitted(mvjoint_log2psa_plus1_spline_pt1pt54_pt1, process = "Longitudinal", type="Subject")[[1]]
  psaFit = psaFit[rowNums==which(data.id$P_ID==pid)]
  
  log2psaplus1Observed = data$log2psa_plus1[data$P_ID==pid]
  
  xrange = range(data$visitTimeYears)
  
  plotdf = data.frame(time = data$visitTimeYears, log2psaplus1Observed=log2psaplus1Observed, psaFit = psaFit)
  plot = baseggplot + 
    geom_point(data=plotdf, aes(x=time,y=log2psaplus1Observed, shape="Observed PSA"), 
               size=NORMAL_POINT_SIZE) + 
    geom_line(data=plotdf, aes(x=time, y=psaFit, linetype="Fitted PSA")) + 
    geom_vline(aes(xintercept=lastBiopsyTime, linetype="Latest biopsy"), show.legend =  F) + 
    scale_shape_manual(name="", values=16, labels=expression('Observed log'[2]*'(PSA + 1)')) +
    scale_linetype_manual(name="", values=c("dashed","solid"), 
                          labels=c(expression('Fitted log'[2]*'(PSA + 1)'), "Latest biopsy")) +
    scale_x_continuous(breaks = seq(xrange[1], xrange[2], length.out = 4),
                       labels = round(seq(xrange[1], xrange[2], length.out = 4),1),
                       limits = xrange) +
    theme(axis.text.x = element_text(size=FONT_SIZE, angle = 25, hjust = 1)) +
    xlab("Follow-up time\n(years)") + ylab(expression('log'[2]*'(PSA + 1)')) 
  
  return(plot)
}

set.seed(2019)
temp = lapply(1:9, FUN = function(i){
  res = plotFittedPSASubject()
  if(i %in% 1:6){
    res = res + theme(axis.title.x = element_blank())
  }
  if(i %in% c(2,3,5,6,8,9)){
    res = res + theme(axis.title.y = element_blank())
  }
  return(res)
})

figure_app2 = ggpubr::ggarrange(plotlist = temp, nrow = 3, ncol=3, 
                            common.legend = T, legend = "bottom",
                            align = "hv")

saveFigure(figure_app2, filename = "latex/contents/c2/images/c2_fig_app2.pdf",
           size = FULL_PLOT_SIZE)

########################################
#Figure Appendix 3
########################################
reset()
load("Rdata/chapter2/fitted_jm.Rdata")

log2psaplus1Observed = mvjoint_log2psa_plus1_spline_pt1pt54_pt1$model_info$mvglmer_components$data$log2psa_plus1
log2psaplus1Observed = log2psaplus1Observed[!is.na(log2psaplus1Observed)] 

psaFit = fitted(mvjoint_log2psa_plus1_spline_pt1pt54_pt1, process = "Longitudinal", type="Subject")[[1]]
residualPSA = log2psaplus1Observed - psaFit

residualPSA_quantiles <- quantile(residualPSA, c(0.25, 0.75), names = FALSE, type = 7, na.rm = TRUE)
theoretical_quantiles = qt(c(0.25, 0.75), df=3)  

slope <- diff(residualPSA_quantiles)/diff(theoretical_quantiles)
intercept = residualPSA_quantiles[1L] - slope * theoretical_quantiles[1L]

figure_app3 = baseggplot + geom_qq(aes(sample=residualPSA), 
                   dparams = list(df=3),
                   distribution = qt) + 
  geom_abline(intercept = intercept, slope = slope) + 
  xlab("t-distribution (df=3) quantiles") + ylab("Residual quantiles")

saveFigure(figure_app3, filename = "latex/contents/c2/images/c2_fig_app3.pdf",
           size = NORMAL_PLOT_SIZE)
