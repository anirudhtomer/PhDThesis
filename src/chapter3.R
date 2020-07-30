library(ggplot2)
library(splines)

reset = function(){
  parent_env = parent.env(environment())
  rm(list = setdiff(ls(name=parent_env), "reset"), envir = parent_env)
  load("Rdata/colors.Rdata", envir = parent_env)
  load("Rdata/shapes.Rdata", envir = parent_env)
  load("Rdata/chapter3/cleandata.Rdata", envir = parent_env)
  load("Rdata/chapter3/npmle_prias.Rdata", envir = parent_env)
  load("Rdata/chapter3/fitted_jm.Rdata", envir = parent_env)
  load("Rdata/chapter3/simres.Rdata", envir = parent_env)
  
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
#Figure 1
########################################
reset()

patient_df = prias_long[prias_long$P_ID == 2340 & prias_long$visitTimeYears<=4,]
current_visit_time = max(patient_df$visitTimeYears)
infinity = ceiling(current_visit_time + 1)
last_biopsy_time = max(patient_df$visitTimeYears[!is.na(patient_df$gleason)])

DRE_PSA_Y_GAP = 0.1
minYLeft = 0
maxYleft = 2 + DRE_PSA_Y_GAP * 2

dreDs = patient_df[!is.na(patient_df$high_dre), c("visitTimeYears", "high_dre")]
psaDs = patient_df[!is.na(patient_df$psa), c("visitTimeYears", "psa")]
maxPSA = max(psaDs$psa)
psaDs$psa = (psaDs$psa / maxPSA) + maxYleft/2 + DRE_PSA_Y_GAP

xTicks = c(0, last_biopsy_time, current_visit_time, infinity)
xLabels = c("0\n(Start AS)", paste0("t = ", round(last_biopsy_time,1), "\n(Latest\nbiopsy)"),
            paste0("s = ", round(current_visit_time,1), "\n(Current\nvisit)"),
            "\u221E")

drebreaks = c(0, maxYleft/2 - DRE_PSA_Y_GAP)
psabreaks = seq(maxYleft/2 + DRE_PSA_Y_GAP, maxYleft, length.out = 4)
psalabels = round(seq(0, maxPSA, length.out = 4),1)

figure1 = baseggplot +
  geom_segment(aes(x=-Inf, xend=Inf, y=maxYleft/2, 
                   yend=maxYleft/2),
               linetype="solid") +
  geom_ribbon(aes(x=seq(last_biopsy_time, infinity, length.out = 10),
                  ymin=-Inf, ymax=Inf, 
                  fill="Risk of cancer\nprogression"), 
              alpha=0.5) +
  scale_fill_manual(name="", values=LIGHTRED) + 
  geom_segment(aes(x=current_visit_time, xend=current_visit_time, 
                   y=-Inf, yend=maxYleft/2),
               linetype="solid", color=ORANGE, size=MEDIUM_LINE) + 
  geom_label(aes(x=current_visit_time, y=maxYleft/2,
                 label="Biopsy\nnow?"), color=WHITE, fill=ORANGE, size=LABEL_SIZE) +
  geom_segment(aes(x=last_biopsy_time, xend=last_biopsy_time, 
                   y=-Inf, yend=maxYleft/2),
               linetype="solid", color=GREEN, size = MEDIUM_LINE) + 
  geom_label(aes(x=last_biopsy_time, y=maxYleft/2,
                 label="Latest\nBiopsy"), color=WHITE, fill=GREEN, size=LABEL_SIZE) +
  geom_point(data = psaDs, size=NORMAL_POINT_SIZE, aes(x = visitTimeYears, y=psa, shape="Observed PSA")) +
  geom_point(data = dreDs, size=NORMAL_POINT_SIZE, aes(x = visitTimeYears, y=high_dre, shape="Observed DRE")) +
  scale_shape_manual(name="",
                     labels=c("Observed DRE\n(T1c / above T1c)", "Observed PSA\n(ng/mL)"),
                     values = c(TRIANGLE, CIRCLE)) +
  ylab("DRE (binary)               PSA (ng/mL)") +
  xlab("Follow-up time (years)") + 
  scale_x_continuous(breaks=xTicks, labels = xLabels, limits = c(0, max(xTicks))) +
  scale_y_continuous(limits = c(minYLeft, maxYleft),
                     breaks = c(drebreaks, psabreaks),
                     labels = c("T1c", "above\nT1c", psalabels)) 

saveFigure(figure1, filename = "latex/contents/c3/images/c3_fig1.pdf")

########################################
#Figure 2
########################################
reset()

survProb = 1 - cumsum(npmle_prias$pf)
survProb = c(1, survProb)

survIntervals = npmle_prias$intmap
survIntervals = cbind(c(0,0), survIntervals)

timePoints = as.numeric(survIntervals)
survProbs = c(1,as.numeric(rep(survProb, each=2)))[1:length(timePoints)]

figure2 = baseggplot + geom_line(aes(x=timePoints, y=1-survProbs)) +  
  coord_cartesian(xlim=c(0,10)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25), 
                     labels = paste0(seq(0, 1, 0.25)*100, "%"),
                     limits = c(0,1)) + 
  ylab("Cumulative risk of cancer progression (%)") +
  xlab("Follow-up time (years)")

saveFigure(figure2, filename = "latex/contents/c3/images/c3_fig2.pdf")

###################
# Figure 3
###################
reset()

generateTruePSAProfile = function(visitTimeYears, randomEff_psa){
  betas_psa = mvJoint_dre_psa_dre_value$statistics$postMeans$betas2
  
  fixedPSAFormula = ~ 1 +I(Age - 70) +  I((Age - 70)^2) + ns(visitTimeYears, knots=c(0.1, 0.7, 4), Boundary.knots=c(0, 5.42))
  randomPSAFormula = ~ 1 + ns(visitTimeYears, knots=c(0.1, 0.7, 4), Boundary.knots=c(0, 5.42))
  
  df = data.frame(Age, visitTimeYears)
  model.matrix(fixedPSAFormula, df) %*% betas_psa + model.matrix(randomPSAFormula, df) %*% as.numeric(randomEff_psa)
}

generateTruePSASlope = function(visitTimeYears, randomEff_psa_slope){
  betas_psa_time = mvJoint_dre_psa_dre_value$statistics$postMeans$betas2[4:7]
  
  fixedPSASlopeFormula = ~ 0 + JMbayes::dns(visitTimeYears, knots=c(0.1, 0.7, 4), Boundary.knots=c(0, 5.42))
  randomPSASlopeFormula = ~ 0 + JMbayes::dns(visitTimeYears, knots=c(0.1, 0.7, 4), Boundary.knots=c(0, 5.42))
  
  df = data.frame(visitTimeYears)
  model.matrix(fixedPSASlopeFormula, df) %*% betas_psa_time + model.matrix(randomPSASlopeFormula, df) %*% as.numeric(randomEff_psa_slope)
}

generateTrueDRELogOdds = function(visitTimeYears, randomEff_dre){
  betas_dre = mvJoint_dre_psa_dre_value$statistics$postMeans$betas1
  
  fixedDREFormula = ~ 1 + I(Age - 70) +  I((Age - 70)^2) + visitTimeYears
  randomDREFormula = ~ 1 + visitTimeYears
  
  df = data.frame(Age, visitTimeYears)
  
  model.matrix(fixedDREFormula, df) %*% betas_dre + model.matrix(randomDREFormula, df) %*% as.numeric(randomEff_dre)
}

hazardFunc = function(visitTimeYears) {
  alphas = mvJoint_dre_psa_dre_value$statistics$postMeans$alphas
  
  gammas = mvJoint_dre_psa_dre_value$statistics$postMeans$gammas
  survivalFormula = ~ 0 + I(Age - 70) + I((Age - 70)^2)
  wGamma = as.numeric(model.matrix(survivalFormula, data = data.frame(Age=Age)) %*% gammas)
  
  baselinehazard = exp(splineDesign(mvJoint_dre_psa_dre_value$control$knots, visitTimeYears,
                                    ord = mvJoint_dre_psa_dre_value$control$ordSpline, outer.ok = T) %*% mvJoint_dre_psa_dre_value$statistics$postMeans$Bs_gammas)
  
  truePSA = generateTruePSAProfile(visitTimeYears, b_subject[3:7])
  truePSASlope = generateTruePSASlope(visitTimeYears, b_subject[4:7])
  trueDRELogOdds = generateTrueDRELogOdds(visitTimeYears, b_subject[1:2])
  
  y_Alpha = cbind(trueDRELogOdds, truePSA, truePSASlope) %*% alphas
  
  exp(wGamma + y_Alpha)
}

patient_df = prias_long[prias_long$P_ID == 113 & prias_long$visitTimeYears<=4,]

b_subject = mvJoint_dre_psa_dre_value$statistics$postMeans$b[which(prias.id$P_ID == 113),]
Age = patient_df$Age[1]

dre_fixed_eff = mvJoint_dre_psa_dre_value$statistics$postMeans$betas1
psa_fixed_eff = mvJoint_dre_psa_dre_value$statistics$postMeans$betas2

times = seq(0, 4, length.out = 20)
truePSA = generateTruePSAProfile(times, b_subject[3:7])
truePSASlope = generateTruePSASlope(times, b_subject[4:7])
trueDREProbHighDRE = plogis(generateTrueDRELogOdds(times, b_subject[1:2]))
trueHazard = hazardFunc(times)

hazardPlotYbreaks = seq(min(trueHazard),
                        max(trueHazard), length.out = 3)
hazardPlot = baseggplot + 
  geom_line(aes(x=times, y=trueHazard), color=RED) +
  ylab("Hazard of cancer\nprogression") + 
  xlab("Follow-up time (years)") +
  theme(axis.title.y = element_text(colour = RED),
        axis.text.y = element_text(colour = RED, size=FONT_SIZE)) +
  scale_y_continuous(breaks = hazardPlotYbreaks, 
                     labels = round(hazardPlotYbreaks,1))

drePlot = baseggplot_no_xticks +
  geom_point(data = patient_df, size=NORMAL_POINT_SIZE,
             aes(x = visitTimeYears, y=high_dre, shape="Observed DRE")) +
  geom_point(aes(x = 0, y=10, shape="Observed PSA")) +
  geom_line(aes(x = times, y=trueDREProbHighDRE, linetype="Fitted DRE")) +
  geom_line(aes(x = 0, y=10, linetype="Fitted PSA")) +
  scale_shape_manual(name="",
                     labels= c(expression(atop('Observed', 'DRE')), expression(atop('Observed', 'log'[2]*'(PSA + 1)'))),
                     values = c(TRIANGLE, CIRCLE)) +
  scale_linetype_manual(name="",
                        labels= c(expression(atop('Fitted', 'Pr (DRE > T1c)')), expression(atop('Fitted', 'log'[2]*'(PSA + 1)'))),
                        values = c("dotted", "dashed")) +
  ylab("Pr (DRE > T1c)") +
  scale_y_continuous(breaks = seq(0,1, length.out = 3),
                     labels=paste0(seq(0,1,length.out = 3)*100, "%"), limits = c(0,1),
                     sec.axis = dup_axis(trans = ~.,
                                         breaks = c(0,1),
                                         labels = c("T1c", "\nabove\nT1c"),
                                         name = "Observed\nDRE"))

psaPlotYbreaks = seq(min(c(truePSA,patient_df$log2psaplus1), na.rm = T),
                     max(c(truePSA,patient_df$log2psaplus1), na.rm = T),
                     length.out = 3)

psaPlot = baseggplot_no_xticks +
  geom_point(data = patient_df, size=NORMAL_POINT_SIZE,
             aes(x = visitTimeYears, y=log2psaplus1),
             shape=CIRCLE) +
  geom_line(aes(x = times, y=truePSA), linetype="dashed") +
  ylab(bquote(atop('log'[2]*'(PSA + 1)', 'value'))) +
  scale_y_continuous(breaks = psaPlotYbreaks,
                     labels = round(psaPlotYbreaks,1))

psaVelocityPlotYbreaks = seq(min(truePSASlope),
                             max(truePSASlope), length.out = 3)

psaVelocityPlot = baseggplot_no_xticks +
  geom_line(aes(x = times, y=truePSASlope)) +
  ylab(bquote(atop('log'[2]*'(PSA + 1)', 'velocity'))) +
  scale_y_continuous(breaks = psaVelocityPlotYbreaks,
                     labels = round(psaVelocityPlotYbreaks,1))

figure3 = ggpubr::ggarrange(drePlot, psaPlot, psaVelocityPlot, hazardPlot, 
                            labels = "AUTO",hjust = -8, 
                            common.legend = T, 
                            legend = "bottom",
                            heights = c(1,1,1,1.30),
                            nrow=4, ncol=1, align = "v")

saveFigure(figure3, filename = "latex/contents/c3/images/c3_fig3.pdf", size = FULL_PLOT_SIZE)

###################
# Figure 4a
###################
#do not reset
#reset()

patient_df = prias_long[prias_long$P_ID == 2340 & prias_long$visitTimeYears<=4,]
current_visit_time = max(patient_df$visitTimeYears)
last_biopsy_time = max(patient_df$visitTimeYears[!is.na(patient_df$gleason)])
infinity = 8

b_subject = mvJoint_dre_psa_dre_value$statistics$postMeans$b[which(prias.id$P_ID == 2340),]
Age = patient_df$Age[1]

dre_fixed_eff = mvJoint_dre_psa_dre_value$statistics$postMeans$betas1
psa_fixed_eff = mvJoint_dre_psa_dre_value$statistics$postMeans$betas2

patient_df$truePSA = generateTruePSAProfile(patient_df$visitTimeYears, b_subject[3:7])
patient_df$trueProbHighDRE = plogis(generateTrueDRELogOdds(patient_df$visitTimeYears, b_subject[1:2]))

DRE_PSA_Y_GAP = 0.1
minYLeft = 0
maxYleft = 2 + DRE_PSA_Y_GAP * 2

dreDs = patient_df[!is.na(patient_df$high_dre), c("visitTimeYears", "high_dre", "trueProbHighDRE")]
psaDs = patient_df[!is.na(patient_df$psa), c("visitTimeYears", "log2psaplus1", "truePSA")]
maxPSA = max(psaDs[,-1], na.rm = T)
minPSA = min(psaDs[,-1], na.rm = T)

newMinPSA = (maxYleft/2 + DRE_PSA_Y_GAP)
newMaxPSA = maxYleft

psaDs[,-1] = ((psaDs[,-1] - minPSA) * (newMaxPSA - newMinPSA))/ (maxPSA - minPSA) + newMinPSA

xTicks = c(0, last_biopsy_time, current_visit_time)
xLabels = c("0\n(Start AS)", paste0("t = ", round(last_biopsy_time,1), "\n(Latest\nbiopsy)"),
            paste0("s = ", round(current_visit_time,1), "\n(Current\nvisit)"))

drebreaks = seq(0, maxYleft/2 - DRE_PSA_Y_GAP, length.out = 3)
drelabels = paste0(drebreaks * 100, "%")
psabreaks = seq(maxYleft/2 + DRE_PSA_Y_GAP, maxYleft, length.out = 4)
psalabels = round(seq(minPSA, maxPSA, length.out = 4), 1)

risk4a = 0.078
threshold = 0.1

maxMeanRiskScaled = risk4a * (maxYleft - minYLeft) + minYLeft
thresholdScaled = threshold *  (maxYleft - minYLeft) + minYLeft

risk_col_width = 0.75

figure4a = baseggplot +
  geom_segment(aes(x=-Inf, xend=Inf, y=maxYleft/2, 
                   yend=maxYleft/2),
               linetype="solid") +
  geom_col(aes(x=c(current_visit_time,current_visit_time), y=c(maxMeanRiskScaled, maxYleft-maxMeanRiskScaled)),
           fill=c(GREEN, WHITE), color=GREEN, width = risk_col_width) +
  geom_segment(aes(x=current_visit_time-risk_col_width/2, xend=current_visit_time + risk_col_width*2.25,
                   y=thresholdScaled, yend=thresholdScaled),
               linetype="dashed", color=RED, size=MEDIUM_LINE) +
  geom_label(aes(x=current_visit_time, y=1.6,
                 label=paste0("Cumulative risk\n of cancer progression:\n", round(risk4a * 100,2), "%")),
             color = WHITE, fill=GREEN, size=LABEL_SIZE)+
  geom_text(aes(x=current_visit_time + risk_col_width*2.25, y=thresholdScaled,
                label=paste0("k = ",threshold*100, "%\n(Biopsy threshold)")),
            color = RED, size=LABEL_SIZE)+
  geom_segment(aes(x=last_biopsy_time, xend=last_biopsy_time, 
                   y=-Inf, yend=maxYleft/2),
               linetype="solid", color=GREEN, size = MEDIUM_LINE) + 
  geom_label(aes(x=last_biopsy_time, y=maxYleft/2,
                 label="Latest\nBiopsy"), color=WHITE, fill=GREEN, size=LABEL_SIZE) +
  geom_point(data = psaDs, size=NORMAL_POINT_SIZE, aes(x = visitTimeYears, y=log2psaplus1, shape="Observed PSA")) +
  geom_line(data = psaDs, aes(x = visitTimeYears, y=truePSA, linetype="Fitted PSA")) +
  geom_point(data = dreDs, size=NORMAL_POINT_SIZE, aes(x = visitTimeYears, y=high_dre, shape="Observed DRE")) +
  geom_line(data = dreDs, aes(x = visitTimeYears, y=trueProbHighDRE, linetype="Fitted DRE")) +
  scale_shape_manual(name="",
                     labels= c(expression(atop('Observed', 'DRE')), expression(atop('Observed', 'log'[2]*'(PSA + 1)'))),
                     values = c(TRIANGLE, CIRCLE)) +
  scale_linetype_manual(name="",
                        labels= c(expression(atop('Fitted', 'Pr (DRE > T1c)')), expression(atop('Fitted', 'log'[2]*'(PSA + 1)'))),
                        values = c("dotted", "dashed")) +
  ylab(expression('Pr (DRE > T1c)            '*'log'[2]*'(PSA + 1)')) +
  xlab("Follow-up time (years)") + 
  scale_x_continuous(breaks=xTicks, labels = xLabels, 
                     limits = c(0, infinity)) +
  scale_y_continuous(limits = c(minYLeft, maxYleft),
                     breaks = c(drebreaks, psabreaks),
                     labels = c(drelabels, psalabels)) 

saveFigure(figure4a, filename = "latex/contents/c3/images/c3_fig4a.pdf", size = NORMAL_PLOT_SIZE)

###################
# Figure 4b
###################
#Do not reset
#reset()

patient_df = prias_long[prias_long$P_ID == 2340 & prias_long$visitTimeYears<=8,]
current_visit_time = max(patient_df$visitTimeYears)
last_biopsy_time = 2.56
infinity = 8

b_subject = mvJoint_dre_psa_dre_value$statistics$postMeans$b[which(prias.id$P_ID == 2340),]
Age = patient_df$Age[1]

dre_fixed_eff = mvJoint_dre_psa_dre_value$statistics$postMeans$betas1
psa_fixed_eff = mvJoint_dre_psa_dre_value$statistics$postMeans$betas2

patient_df$truePSA = generateTruePSAProfile(patient_df$visitTimeYears, b_subject[3:7])
patient_df$trueProbHighDRE = plogis(generateTrueDRELogOdds(patient_df$visitTimeYears, b_subject[1:2]))

DRE_PSA_Y_GAP = 0.1
minYLeft = 0
maxYleft = 2 + DRE_PSA_Y_GAP * 2

dreDs = patient_df[!is.na(patient_df$high_dre), c("visitTimeYears", "high_dre", "trueProbHighDRE")]
psaDs = patient_df[!is.na(patient_df$psa), c("visitTimeYears", "log2psaplus1", "truePSA")]
maxPSA = max(psaDs[,-1], na.rm = T)
minPSA = min(psaDs[,-1], na.rm = T)

newMinPSA = (maxYleft/2 + DRE_PSA_Y_GAP)
newMaxPSA = maxYleft

psaDs[,-1] = ((psaDs[,-1] - minPSA) * (newMaxPSA - newMinPSA))/ (maxPSA - minPSA) + newMinPSA

xTicks = c(0, last_biopsy_time, current_visit_time)
xLabels = c("0\n(Start AS)", paste0("t = ", round(last_biopsy_time,1), "\n(Latest\nbiopsy)"),
            paste0("s = ", round(current_visit_time,1), "\n(Current\nvisit)"))

drebreaks = seq(0, maxYleft/2 - DRE_PSA_Y_GAP, length.out = 3)
drelabels = paste0(drebreaks * 100, "%")
psabreaks = seq(maxYleft/2 + DRE_PSA_Y_GAP, maxYleft, length.out = 4)
psalabels = round(seq(minPSA, maxPSA, length.out = 4),1)

risk4b = 0.135
threshold = 0.1

maxMeanRiskScaled = risk4b * (maxYleft - minYLeft) + minYLeft
thresholdScaled = threshold *  (maxYleft - minYLeft) + minYLeft

risk_col_width = 0.75

figure4b = baseggplot +
  geom_segment(aes(x=-Inf, xend=Inf, y=maxYleft/2, 
                   yend=maxYleft/2),
               linetype="solid") +
  geom_col(aes(x=c(current_visit_time,current_visit_time), y=c(maxMeanRiskScaled, maxYleft-maxMeanRiskScaled)),
           fill=c(RED, WHITE), color=RED, width = risk_col_width) +
  geom_segment(aes(x=current_visit_time-risk_col_width/2, xend=current_visit_time + risk_col_width*2.25,
                   y=thresholdScaled, yend=thresholdScaled),
               linetype="dashed", color=RED, size=MEDIUM_LINE) +
  geom_label(aes(x=current_visit_time, y=1.6,
                 label=paste0("Cumulative risk\n of cancer progression:\n", round(risk4b * 100,2), "%")),
             color = WHITE, fill=RED, size=LABEL_SIZE)+
  geom_text(aes(x=current_visit_time + risk_col_width*2.25, y=thresholdScaled,
                label=paste0("k = ",threshold*100, "%\n(Biopsy threshold)")),
            color = RED, size=LABEL_SIZE)+
  geom_segment(aes(x=last_biopsy_time, xend=last_biopsy_time, 
                   y=-Inf, yend=maxYleft/2),
               linetype="solid", color=GREEN, size = MEDIUM_LINE) + 
  geom_label(aes(x=last_biopsy_time, y=maxYleft/2,
                 label="Latest\nBiopsy"), color=WHITE, fill=GREEN, size=LABEL_SIZE) +
  geom_point(data = psaDs, size=NORMAL_POINT_SIZE, aes(x = visitTimeYears, y=log2psaplus1, shape="Observed PSA")) +
  geom_line(data = psaDs, aes(x = visitTimeYears, y=truePSA, linetype="Fitted PSA")) +
  geom_point(data = dreDs, size=NORMAL_POINT_SIZE, aes(x = visitTimeYears, y=high_dre, shape="Observed DRE")) +
  geom_line(data = dreDs, aes(x = visitTimeYears, y=trueProbHighDRE, linetype="Fitted DRE")) +
  scale_shape_manual(name="",
                     labels= c(expression(atop('Observed', 'DRE')), expression(atop('Observed', 'log'[2]*'(PSA + 1)'))),
                     values = c(TRIANGLE, CIRCLE)) +
  scale_linetype_manual(name="",
                        labels= c(expression(atop('Fitted', 'Pr (DRE > T1c)')), expression(atop('Fitted', 'log'[2]*'(PSA + 1)'))),
                        values = c("dotted", "dashed")) +
  ylab(expression('Pr (DRE > T1c)            '*'log'[2]*'(PSA + 1)')) +
  xlab("Follow-up time (years)") + 
  scale_x_continuous(breaks=xTicks, labels = xLabels, 
                     limits = c(0, infinity)) +
  scale_y_continuous(limits = c(minYLeft, maxYleft),
                     breaks = c(drebreaks, psabreaks),
                     labels = c(drelabels, psalabels)) 

saveFigure(figure4b, filename = "latex/contents/c3/images/c3_fig4b.pdf", size = NORMAL_PLOT_SIZE)

#################
# Figure 5
#################
reset()

medianOffset = by(scheduleResCombined, INDICES = scheduleResCombined$methodName, function(x){
  median(x$offset[x$progression_time!=10])
})
medianNb = by(scheduleResCombined$nb, INDICES = scheduleResCombined$methodName, median)

fixedScheduleIndices = c(1, 2, 3, 9, 10)
fixedScheduleLabels = c("Biopsy every\n1.5 years", "Annual\nbiopsies", 
                        "Biopsy every\n2 years", "Biopsy every\n3 years", "Biopsy every\n4 years")

priasIndex = 4
priasLabel = "PRIAS"

persScheduleIndices = c(5,6,7,8)
persScheduleLabels = c("Risk: 10%", "Risk: 15%", "Risk: 5%", "Risk: F1") 

figure5 = baseggplot + 
  geom_ribbon(aes(x=c(1:3, medianNb[fixedScheduleIndices]), 
                  ymin=0,
                  ymax=c(Inf,Inf,Inf, medianOffset[fixedScheduleIndices]), 
                  fill="Better balance in median\nnumber of biopsies & delay,\nthan fixed schedules"), 
              alpha=0.5) +
  scale_fill_manual(name="", values=LIGHTGREEN) + 
  geom_point(aes(x=medianNb[fixedScheduleIndices], 
                 y=medianOffset[fixedScheduleIndices], 
                 shape="Fixed", color="Fixed"),
             size=BIG_POINT_SIZE) +
  geom_line(aes(x=medianNb[fixedScheduleIndices], 
                y=medianOffset[fixedScheduleIndices]), 
            linetype="dashed", color=RED) +
  geom_point(aes(x=medianNb[persScheduleIndices], 
                 y=medianOffset[persScheduleIndices], 
                 shape="Personalized", color="Personalized"), 
             size=BIG_POINT_SIZE) +
  geom_line(aes(x=medianNb[persScheduleIndices], 
                y=medianOffset[persScheduleIndices]), 
            linetype="dashed", color=GREEN) +
  geom_point(aes(x=medianNb[priasIndex], y=medianOffset[priasIndex], 
                 shape="PRIAS", color="PRIAS"), 
             size=BIG_POINT_SIZE, alpha=0.8) +
  geom_label(aes(x=medianNb[fixedScheduleIndices], 
                 y=medianOffset[fixedScheduleIndices], 
                 label=fixedScheduleLabels),
             color=WHITE, fill=RED, size=LABEL_SIZE,
             nudge_x = c(0.6,-0.6,1.15,1.2,0.0), 
             nudge_y = c(0.185, 0.25, 0.15,0.135,0.125)) +
  geom_label(aes(x=medianNb[persScheduleIndices], 
                 y=medianOffset[persScheduleIndices], 
                 label=persScheduleLabels),
             color=WHITE, fill=GREEN, size=LABEL_SIZE,
             nudge_x = c(0, 0.5,0,0), 
             nudge_y = c(-0.15, 0.15, -0.15, -0.15)) +
  geom_label(aes(x=medianNb[priasIndex],
                 y=medianOffset[priasIndex]), label=priasLabel, 
             color=WHITE, fill=BLUE, size=LABEL_SIZE, 
             nudge_x = 0.3, nudge_y = 0.125) +
  scale_color_manual(name="", values=c(RED, GREEN, BLUE)) + 
  scale_shape_manual(name="", values=c(SQUARE,TRIANGLE,RHOMBUS)) + 
  coord_cartesian(ylim=c(0,2)) +
  scale_x_continuous(breaks=1:10, limits = c(1,10)) +
  xlab("Median number of biopsies") + 
  ylab("Median delay in detection of\ncancer progression (years)") 

saveFigure(figure5, filename = "latex/contents/c3/images/c3_fig5.pdf", size = NORMAL_PLOT_SIZE)

#################
# Figure 6
#################
reset()

scheduleResCombined = scheduleResCombined[scheduleResCombined$methodName %in% levels(scheduleResCombined$methodName)[c(2, 4, 5, 7, 8)],]
scheduleResCombined$methodName = droplevels(scheduleResCombined$methodName)

getBoxplotStatsDf=function(progression_time_low, progression_time_high, attribute){
  temp = scheduleResCombined[scheduleResCombined$progression_time>=progression_time_low & scheduleResCombined$progression_time<=progression_time_high,]
  res = unlist(by(temp$methodName, data = temp[, attribute], FUN = function(x){
    boxplot.stats(x)$stats
  }))
  
  resDf = data.frame(matrix(res, ncol=5, byrow = T))
  resDf$methodName = levels(scheduleResCombined$methodName)
  return(resDf)
}

MAX_DELAY_Y = 5.5

gfastNb = baseggplot_no_xticks + 
  geom_boxplot(data=getBoxplotStatsDf(0,3.5, "nb"),
               aes(ymin = X1, lower = X2, middle = X3, upper = X4, ymax = X5, x=methodName), 
               stat = "identity") + coord_flip() + 
  scale_y_continuous(breaks=seq(1,10, length.out = 4), limits = c(1,10)) +
  xlab("Schedule") + ylab("Number of Biopsies") 

gfastOffset = baseggplot_no_xticks +
  geom_boxplot(data=getBoxplotStatsDf(0,3.5, "offset"),
               aes(ymin = X1, lower = X2, middle = X3, upper = X4, ymax = X5, x=methodName),
               stat = "identity") + coord_flip() + 
  scale_y_continuous(breaks=0:5, limits = c(0,MAX_DELAY_Y)) + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  xlab("Schedule") + 
  ylab("Delay in detection of\ncancer progression (years)")

gFast = ggpubr::ggarrange(gfastNb, gfastOffset, ncol=2, widths = c(1.4,1), align="h")

gIntermediateNb = baseggplot_no_xticks + 
  geom_boxplot(data=getBoxplotStatsDf(3.50001,9.999999, "nb"),
               aes(ymin = X1, lower = X2, middle = X3, upper = X4, ymax = X5, x=methodName),
               stat = "identity") + coord_flip() + 
  scale_y_continuous(breaks=seq(1,10, length.out = 4)) +
  xlab("Schedule") + ylab("Number of Biopsies") 
#ggtitle("Intermediate progressing (20% patients)")

gIntermediateOffset = baseggplot_no_xticks +
  geom_boxplot(data=getBoxplotStatsDf(3.50001,9.999999, "offset"),
               aes(ymin = X1, lower = X2, middle = X3, upper = X4, ymax = X5, x=methodName),
               stat = "identity") + coord_flip() + 
  scale_y_continuous(breaks=0:5, limits = c(0,MAX_DELAY_Y)) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  xlab("Schedule") + ylab("Delay in detection of\ncancer progression (years)")

gIntermediate = ggpubr::ggarrange(gIntermediateNb, gIntermediateOffset, ncol=2, widths = c(1.4,1), align = "h")

gSlowNb = baseggplot +
  geom_boxplot(data=getBoxplotStatsDf(10,10, "nb"),
               aes(ymin = X1, lower = X2, middle = X3, upper = X4, ymax = X5, x=methodName),
               stat = "identity") + coord_flip() + 
  scale_y_continuous(breaks=c(1,4,7,10)) +
  xlab("Schedule") + ylab("Number of Biopsies") 
#ggtitle("Slow progressing (50% patients)")

gSlowOffset = baseggplot + 
  geom_text(aes(x=MAX_DELAY_Y/2, y=MAX_DELAY_Y/2, 
                label="No cancer progression\nand hence no delay in detection\nof cancer progression."),
            size=LABEL_SIZE) + 
  coord_flip() + 
  scale_y_continuous(breaks=0:5, limits = c(0,MAX_DELAY_Y), labels = 0:5) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  xlab("Schedule") + ylab("Delay in detection of\ncancer progression (years)")


gSlow = ggpubr::ggarrange(gSlowNb, gSlowOffset, ncol=2, widths = c(1.4,1), align="h")

figure6 = ggpubr::ggarrange(gFast, gIntermediate,gSlow, nrow=3, ncol=1, align="v",
                            heights = c(1,1,1.3), labels = "AUTO")

saveFigure(figure6, filename = "latex/contents/c3/images/c3_fig6.pdf", size = NORMAL_PLOT_SIZE)

################
# Figure Appendix 1
################
plotFittedDREMarginal = function(logOdds = F){
  visitTimeYears = seq(0, 10, 0.5)
  df = data.frame(Age = 70, visitTimeYears)
  
  betas_dre_matrix = mvJoint_dre_psa_dre_value$mcmc$betas1
  D_matrix = mvJoint_dre_psa_dre_value$mcmc$D
  
  fixedDREFormula = ~ 1 + I(Age - 70) +  I((Age - 70)^2) + visitTimeYears
  randomDREFormula = ~ 1 + visitTimeYears
  
  totalMCMC = dim(D_matrix)[1]
  
  xbetaZb_mcmc = sapply(1:totalMCMC, function(m){
    beta_m = betas_dre_matrix[m,]
    D_m = D_matrix[m,,]
    
    nSub = 10000
    b_m = MASS::mvrnorm(nSub, mu = rep(0, nrow(D_m)), D_m)[,1:2]
    
    xbeta = model.matrix(fixedDREFormula, df) %*% beta_m
    Zb = model.matrix(randomDREFormula, df) %*% t(b_m)
    
    xbetaZb = matrix(rep(c(xbeta), nSub), ncol=nSub, byrow = F) + Zb
    apply(plogis(xbetaZb), 1, mean)
  })
  
  if(logOdds==T){
    xbetaZb_mcmc = log(xbetaZb_mcmc / (1-xbetaZb_mcmc))
  }
  
  meanXbetaZb = apply(xbetaZb_mcmc, 1, mean)
  upperXbetaZb = apply(xbetaZb_mcmc, 1, quantile, probs=0.975)
  lowerXbetaZb = apply(xbetaZb_mcmc, 1, quantile, probs=0.025)
  
  plot = baseggplot + 
    geom_line(aes(x=visitTimeYears, y=meanXbetaZb)) +
    geom_ribbon(aes(x=visitTimeYears, ymin = lowerXbetaZb, ymax = upperXbetaZb), fill="grey", alpha=0.5) + 
    xlab("Follow-up time (years)")
  
  if(logOdds==F){
    plot = plot + scale_y_continuous(breaks = seq(0,1, by = 0.25), labels=paste0(seq(0,1,by=0.25)*100, "%"),limits = c(0,1)) +
      ylab(expression('Probability (DRE > T1c)'))
  }else{
    plot = plot + ylab(expression('log odds (DRE > T1c)'))
  }
}

figure_app1 = ggpubr::ggarrange(plotFittedDREMarginal(F),plotFittedDREMarginal(T),
                                labels = "AUTO")
saveFigure(figure_app1, filename = "latex/contents/c3/images/c3_fig_app1.pdf",
           size = NORMAL_PLOT_SIZE)

###################
# Figure Appendix 2
###################
reset()

visitTimeYears = seq(0,10, 0.5)
df = data.frame(Age = 70, visitTimeYears)
betas_psa_matrix = mvJoint_dre_psa_dre_value$mcmc$betas2

fixedPSAFormula = ~ 1 +I(Age - 70) +  I((Age - 70)^2) + ns(visitTimeYears, knots=c(0.1, 0.7, 4), Boundary.knots=c(0, 5.42))

xbeta = model.matrix(fixedPSAFormula, df) %*% t(betas_psa_matrix)

meanXbeta = apply(xbeta, 1, mean)
upperXbeta = apply(xbeta, 1, quantile, probs=0.975)
lowerXbeta = apply(xbeta, 1, quantile, probs=0.025)

figure_app2 = baseggplot +
  geom_line(aes(x=visitTimeYears, y=meanXbeta)) +
  geom_ribbon(aes(x=visitTimeYears, ymin = lowerXbeta, ymax = upperXbeta), fill="grey", alpha=0.5) + 
  xlab("Follow-up time (years)") + ylab(expression('log'[2]*'(PSA + 1)'))

saveFigure(figure_app2, filename = "latex/contents/c3/images/c3_fig_app2.pdf",
           size = NORMAL_PLOT_SIZE)

###################
# Figure Appendix 3
###################
reset()
load("Rdata/chapter3/fitted_jm_large.Rdata")

plotFittedDRESubject = function(){
  data.id = mvJoint_dre_psa_dre_value$model_info$coxph_components$data
  
  repeat{
    pid = sample(data.id$P_ID, size = 1)
    print(paste("Choosing patient", pid, "randomly because pid not provided"))
    
    data = mvJoint_dre_psa_dre_value$model_info$mvglmer_components$data
    data = data[data$P_ID == pid,]
    lastBiopsyTime =  max(data$visitTimeYears[!is.na(data$gleason)])
    data = data[!is.na(data$high_dre),] 
    if(nrow(data)>=2){
      break
    }
  }
  rowNums = mvJoint_dre_psa_dre_value$model_info$mvglmer_components$id1
  dreFit = fitted(mvJoint_dre_psa_dre_value, process = "Longitudinal", type="Subject")[[1]]
  dreFit = dreFit[rowNums==which(data.id$P_ID==pid)]
  dreFit = plogis(dreFit)
  
  dreObserved = data$high_dre[data$P_ID==pid]
  
  xrange = range(data$visitTimeYears)
  
  plotdf = data.frame(time = data$visitTimeYears, dreObserved=dreObserved, dreFit = dreFit)
  
  plot = baseggplot + 
    geom_point(data=plotdf, aes(x=time,y=dreObserved, shape="Observed DRE"), size=3) + 
    geom_line(data=plotdf, aes(x=time, y=dreFit, linetype="Fitted Pr (DRE > T1c)")) +
    geom_vline(aes(xintercept=lastBiopsyTime, linetype="Latest biopsy"), show.legend =  F) + 
    scale_shape_manual(name="", values=17, labels="Observed DRE") +
    scale_linetype_manual(name="", values=c("dashed","solid"), 
                          labels=c("Fitted Pr (DRE > T1c)", "Latest biopsy")) +
    scale_y_continuous(breaks = seq(0,1, by = 0.25), labels=paste0(seq(0,1,by=0.25)*100, "%"),limits = c(0,1)) +
    scale_x_continuous(breaks = seq(xrange[1], xrange[2], length.out = 4),
                       labels = round(seq(xrange[1], xrange[2], length.out = 4),1),
                       limits = xrange) +
    theme(axis.text.x = element_text(size=FONT_SIZE, angle = 25, hjust = 1)) +
    xlab("Follow-up time (years)") + ylab("Pr (DRE > T1c)")
  
  return(plot)
}

set.seed(2019)
temp = lapply(1:9, FUN = function(i){
  res = plotFittedDRESubject()
  if(i %in% 1:6){
    res = res + theme(axis.title.x = element_blank())
  }
  if(i %in% c(2,3,5,6,8,9)){
    res = res + theme(axis.title.y = element_blank())
  }
  return(res)
})

figure_app3 = ggpubr::ggarrange(plotlist = temp, nrow = 3, ncol=3, 
                                common.legend = T, legend = "bottom",
                                align = "hv")

saveFigure(figure_app3, filename = "latex/contents/c3/images/c3_fig_app3.pdf",
           size = FULL_PLOT_SIZE)


###################
# Figure Appendix 4
###################
reset()
load("Rdata/chapter3/fitted_jm_large.Rdata")

plotFittedPSASubject = function(){
  data.id = mvJoint_dre_psa_dre_value$model_info$coxph_components$data
  repeat{
    pid = sample(data.id$P_ID, size = 1)
    print(paste("Choosing patient", pid, "randomly because pid not provided"))
    
    data = mvJoint_dre_psa_dre_value$model_info$mvglmer_components$data
    data = data[data$P_ID == pid,]
    lastBiopsyTime =  max(data$visitTimeYears[!is.na(data$gleason)])
    data = data[!is.na(data$log2psaplus1),]
    if(nrow(data)>4){
      break
    }
  }
  
  rowNums = mvJoint_dre_psa_dre_value$model_info$mvglmer_components$id2
  psaFit = fitted(mvJoint_dre_psa_dre_value, process = "Longitudinal", type="Subject")[[2]]
  psaFit = psaFit[rowNums==which(data.id$P_ID==pid)]
  
  log2psaplus1Observed = data$log2psaplus1[data$P_ID==pid]
  
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

figure_app4 = ggpubr::ggarrange(plotlist = temp, nrow = 3, ncol=3, 
                                common.legend = T, legend = "bottom",
                                align = "hv")

saveFigure(figure_app4, filename = "latex/contents/c3/images/c3_fig_app4.pdf",
           size = FULL_PLOT_SIZE)


###################
# Figure Appendix 5
###################
reset()
load("Rdata/chapter3/fitted_jm_large.Rdata")

qqplotPSA = function(df = 3, normalDist = F){
  data = mvJoint_dre_psa_dre_value$model_info$mvglmer_components$data
  data = data[!is.na(data$log2psaplus1),] 
  
  psaFit = fitted(mvJoint_dre_psa_dre_value, process = "Longitudinal", type="Subject")[[2]]
  
  log2psaplus1Observed = data$log2psaplus1
  residualPSA = log2psaplus1Observed - psaFit
  
  probs=c(0.25, 0.75)
  residualPSA_quantiles <- quantile(residualPSA, probs, names = FALSE, type = 7, na.rm = TRUE)
  if(normalDist==T){
    theoretical_quantiles = qnorm(probs)
  }else{
    theoretical_quantiles = qt(probs, df=df)  
  }
  
  slope <- diff(residualPSA_quantiles)/diff(theoretical_quantiles)
  intercept = residualPSA_quantiles[1L] - slope * theoretical_quantiles[1L]
  
  if(normalDist==T){
    plot = ggplot() + geom_qq(aes(sample=residualPSA), 
                              distribution = qnorm) + 
      geom_abline(intercept = intercept, slope = slope) + 
      theme_bw() + 
      theme(text = element_text(size=FONT_SIZE), axis.text=element_text(size=FONT_SIZE),
            axis.line = element_line(),
            panel.grid.minor = element_blank()) +  
      xlab("Normal distribution quantiles") + ylab("Residual quantiles")
  }else{
    plot = ggplot() + geom_qq(aes(sample=residualPSA), 
                              dparams = list(df=df),
                              distribution = qt) + 
      geom_abline(intercept = intercept, slope = slope) + 
      theme_bw() + 
      theme(text = element_text(size=FONT_SIZE), axis.text=element_text(size=FONT_SIZE),
            axis.line = element_line(),
            panel.grid.minor = element_blank()) +  
      xlab("t-distribution (df=3) quantiles") + ylab("Residual quantiles")
  }
  return(plot)
}

figure_app5 = ggpubr::ggarrange(qqplotPSA() + ggtitle("Error distribution: t (df=3)"),
                                qqplotPSA(normalDist = T) + ggtitle("Error distribution: normal"), labels = "AUTO")

saveFigure(figure_app5, filename = "latex/contents/c3/images/c3_fig_app5.pdf",
           size = NORMAL_PLOT_SIZE)
