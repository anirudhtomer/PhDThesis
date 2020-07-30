library(ggplot2)
library(ggpubr)
library(splines)

reset = function(){
  parent_env = parent.env(environment())
  rm(list = setdiff(ls(name=parent_env), "reset"), envir = parent_env)
  load("Rdata/colors.Rdata", envir = parent_env)
  load("Rdata/shapes.Rdata", envir = parent_env)
  load("Rdata/chapter4/sim_res.Rdata", envir = parent_env)
  load("Rdata/chapter4/fig2_data.Rdata", envir = parent_env)
  load("Rdata/chapter4/fig3_data.Rdata", envir = parent_env)
  load("Rdata/chapter4/fig4_data.Rdata", envir = parent_env)
  load("Rdata/chapter4/fig5_data.Rdata", envir = parent_env)
  
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
  assign("MEDIUM_LABEL_SIZE", LABEL_SIZE-0.5, envir = parent_env)
  assign("SMALL_LABEL_SIZE", LABEL_SIZE-1.0, envir = parent_env)
  
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
    theme(plot.margin = margin(1,1,1,1, unit = "mm"), 
          axis.text = element_text(size=FONT_SIZE),
          axis.title = element_text(size=FONT_SIZE),
          plot.title = element_text(size=FONT_SIZE),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=FONT_SIZE-2))
  
  baseggplot_no_xticks = baseggplot + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank())
  
  baseggplot_no_yticks = baseggplot + 
    theme(axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
  
  baseggplot_no_xyticks = baseggplot + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
  
  baseggplot_event_plot = baseggplot + 
    theme(panel.background = element_blank(),
          axis.text = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank())
  
  assign("baseggplot_no_xticks", baseggplot_no_xticks, envir = parent_env)
  assign("baseggplot_no_yticks", baseggplot_no_yticks, envir = parent_env)
  assign("baseggplot_no_xyticks", baseggplot_no_xyticks, envir = parent_env)
  assign("baseggplot_event_plot", baseggplot_event_plot, envir = parent_env)
  assign("baseggplot", baseggplot, envir = parent_env)
}

########################################
#Figure 1
########################################
reset()
TRUE_TIME_Prog = 4.5
TRUE_TIME_Prog_HT = 0.85
ARROW_SIZE = 2.0

delay_explanation_plot_a = baseggplot_no_xyticks + 
  geom_ribbon(aes(x=c(TRUE_TIME_Prog, 5), ymin=-Inf, ymax=Inf), fill=RED, alpha=0.25) +
  geom_segment(aes(x=0:4, y=rep(-Inf,5), xend=0:4, yend=rep(0.5,5)),
               color=c(ORANGE, rep(GREEN, 4)))+
  geom_segment(aes(x=c(TRUE_TIME_Prog,5), y=rep(-Inf,2), xend=c(TRUE_TIME_Prog,5), yend=c(TRUE_TIME_Prog_HT,0.5)),
               color=RED)+
  geom_label(aes(x=TRUE_TIME_Prog, y=TRUE_TIME_Prog_HT, label = "True time of\nprogression"),
             size=SMALL_LABEL_SIZE, color=RED,
             fill=WHITE) +
  geom_label(aes(x=0:5, y=rep(0.5,6),
                 label = c("Start\nsurveillance", 
                           "1st negative\ntest", 
                           "2nd negative\ntest", 
                           "3rd negative\ntest", 
                           "4th negative\ntest", 
                           "5th test\nprogression\ndetected")),
             size=SMALL_LABEL_SIZE, color=WHITE,
             fill=c(ORANGE, rep(GREEN,4), RED)) +
  geom_text(aes(x=0.5 * (TRUE_TIME_Prog + 5), y=0.2, label="6 months delay\n in detecting progression"), size=MEDIUM_LABEL_SIZE) + 
  geom_segment(aes(x=TRUE_TIME_Prog, xend = 5, y=0.07, yend = 0.07),
               arrow = arrow(length = unit(ARROW_SIZE,"mm"), ends="both", type="closed"))+
  scale_x_continuous(breaks = c(0,1,2,3,TRUE_TIME_Prog,4,5,6), limits=c(-0.3,6.25)) + 
  ylim(0,1) + 
  ggtitle("    Annual tests, shorter delay") +
  theme(plot.title = element_text(vjust = -10, hjust = 0.025))

delay_explanation_plot_b = baseggplot_no_yticks + 
  geom_ribbon(aes(x=c(TRUE_TIME_Prog, 6), ymin=-Inf, ymax=Inf), fill=RED, alpha=0.25) +
  geom_segment(aes(x=c(0,2,4), y=rep(-Inf,3), xend=c(0,2,4), yend=rep(0.5,3)),
               color=c(ORANGE, rep(GREEN, 2)))+
  geom_segment(aes(x=c(TRUE_TIME_Prog,6), y=rep(-Inf,2), xend=c(TRUE_TIME_Prog,6), yend=c(TRUE_TIME_Prog_HT,0.5)),
               color=RED)+
  geom_label(aes(x=TRUE_TIME_Prog, y=TRUE_TIME_Prog_HT, label = "True time of\nprogression"),
             size=SMALL_LABEL_SIZE, color=RED,
             fill=WHITE) +
  geom_label(aes(x=c(0,2,4,6), y=rep(0.5,4),
                 label = c("Start\nsurveillance", 
                           "1st negative\ntest", 
                           "2nd negative\ntest", 
                           "3rd test\nprogression\ndetected")),
             size=SMALL_LABEL_SIZE, color=WHITE,
             fill=c(ORANGE, rep(GREEN,2), RED)) +
  annotate("text", x=0.5 * (TRUE_TIME_Prog + 6), y=0.2, label="18 months delay\n in detecting progression", size=MEDIUM_LABEL_SIZE)+
  geom_segment(aes(x=TRUE_TIME_Prog, xend = 6, y=0.07, yend = 0.07),
               arrow = arrow(length = unit(ARROW_SIZE,"mm"), ends="both", type="closed"))+
  xlab("Time of test visits") + 
  scale_x_continuous(breaks = c(0,1,2,3,4,TRUE_TIME_Prog, 5,6), 
                     labels = c("Jan 2000","Jan 2001", "Jan 2002",
                                "Jan 2003", "Jan 2004", "Jul 2004", 
                                "Jan 2005", "Jan 2006"), limits=c(-0.3,6.25)) + 
  ylim(0,1) + theme(axis.text.x = element_text(size=FONT_SIZE, angle = 30, hjust = 1)) +
  ggtitle("    Biannual tests, larger delay") +
  theme(plot.title = element_text(vjust = -10, hjust = 0.025))


figure1 = ggpubr::ggarrange(delay_explanation_plot_a, delay_explanation_plot_b, 
                            ncol = 1, nrow=2, align = "v", labels = "AUTO",
                            heights = c(1, 1.2), vjust = 3.25, hjust = -0.75)
print(figure1)
saveFigure(figure1, filename = "latex/contents/c4/images/c4_fig1.pdf",
           size = NORMAL_PLOT_SIZE)

########################################
#Figure 2
########################################
reset()
max_follow_up = 6.5
psa_breaks = fig2_data$psa_breaks
pat_data = fig2_data$pat_data
transformRiskToPSA = function(x){
  x*(tail(psa_breaks,1) - psa_breaks[1]) + psa_breaks[1]
}
riskAxisBreaks = transformRiskToPSA(c(0, 0.5, 1))
riskAxisLabels = c("0%", "50%", "100%")

figure2_plots = lapply(list(fig2_data$panelA_data, fig2_data$panelB_data),
                       FUN = function(panel_data){
                         exp_fut = panel_data$exp_fut
                         latest_survival_time = panel_data$latest_survival_time
                         max_psa_time = panel_data$max_psa_time
                         psa_predict_times = panel_data$psa_predict_times
                         survival_predict_times = panel_data$survival_predict_times
                         mean_psa = rowMeans(exp_fut$predicted_psa)
                         mean_psa_velocity = rowMeans(exp_fut$predicted_psa_slope)
                         
                         mean_cum_risk = transformRiskToPSA(1-c(1, rowMeans(exp_fut$predicted_surv_prob)))
                         lower_cum_risk = transformRiskToPSA(1-c(1, apply(exp_fut$predicted_surv_prob, 1, quantile, probs=0.025)))
                         upper_cum_risk = transformRiskToPSA(1-c(1, apply(exp_fut$predicted_surv_prob, 1, quantile, probs=0.975)))
                         
                         panel_plot = baseggplot +  
                           scale_x_continuous(breaks = panel_data$xbreaks, labels=panel_data$xlabs,
                                              limits = c(0, max_follow_up), 
                                              minor_breaks = seq(0, max_follow_up, 1)) +
                           xlab("Follow-up time (years)")+
                           geom_vline(xintercept = latest_survival_time, color=GREEN) + 
                           geom_vline(xintercept = max_psa_time, linetype="dashed") +
                           geom_point(aes(x=pat_data$year_visit[pat_data$year_visit<=max_psa_time],
                                          y=pat_data$log2psaplus1[pat_data$year_visit<=max_psa_time]),
                                      size=NORMAL_POINT_SIZE, color=BLUE) +
                           geom_line(aes(x=psa_predict_times, y=mean_psa), color=BLUE) + 
                           geom_line(aes(x=survival_predict_times, y=mean_cum_risk), color=RED) +
                           geom_ribbon(aes(x=survival_predict_times, ymin=lower_cum_risk,
                                           ymax=upper_cum_risk), alpha=0.15, fill=RED) + 
                           scale_y_continuous(breaks = psa_breaks, 
                                              labels = round(psa_breaks,1), 
                                              limits = range(psa_breaks),
                                              sec.axis = sec_axis(trans=~., 
                                                                  breaks= riskAxisBreaks,
                                                                  labels = riskAxisLabels,
                                                                  name = "Cumulative-risk of\n progression")) +
                           geom_point(aes(x=-5,y=-5, color="Observed PSA"), size=NORMAL_POINT_SIZE) +
                           scale_color_manual(values = c(BLUE), labels="Observed longitudinal biomarker")+
                           ylab("Biomarker") +
                           theme(axis.title.y = element_text(size=FONT_SIZE, color=BLUE),
                                 axis.text.y = element_text(size=FONT_SIZE, color=BLUE),
                                 axis.title.y.right  = element_text(size=FONT_SIZE, color=RED),
                                 axis.text.y.right = element_text(size=FONT_SIZE, color=RED))
                         return(panel_plot)
                       })

figure2_plots[[1]] = figure2_plots[[1]] + theme(axis.title.x = element_blank())

figure2 = ggarrange(plotlist = figure2_plots, ncol = 1, nrow=2, 
                    align = "v", legend = "bottom", common.legend = T,
                    labels = "AUTO",heights = c(1,1.075), hjust = 0.025)

saveFigure(figure2, filename = "latex/contents/c4/images/c4_fig2.pdf",
           size = FULL_PLOT_SIZE)

########################################
#Figure 3
########################################
reset()
test_decision_times = seq(2.5, 6.5, by = 1)
psa_breaks = fig3_data$psa_breaks
pat_df = fig3_data$pat_data
latest_survival_time = 1.5
threshold = 0.12
xbreaks = c(0,1.5,2.5,3.5,4.5,5.5,6.5)
xlabs = c("0","t=1.5",
          expression("v=u"[1]*"=2.5"),
          expression("  u"[2]*"=3.5"),
          expression("u"[3]*"=4.5"),
          expression("u"[4]*"=5.5"),
          expression("u"[5]*"=6.5"))
max_follow_up = 6.5
max_psa_time = 2.5
transformRiskToPSA = function(x){
  x*(tail(psa_breaks,1) - psa_breaks[1]) + psa_breaks[1]
}

fig3_top = baseggplot +  
  geom_vline(xintercept = latest_survival_time, color=GREEN)+
  geom_vline(xintercept = max_psa_time, linetype='dashed')+
  geom_segment(aes(x = latest_survival_time, xend=max_follow_up,
                   y=transformRiskToPSA(threshold), yend=transformRiskToPSA(threshold)),
               linetype="dashed", color=RED)+
  geom_line(aes(x=fig3_data$psa_predict_times, y=fig3_data$mean_psa), color=BLUE) + 
  geom_line(data=fig3_data$new_test_df, 
            aes(x=survtime, y=mean_cum_risk_scaled), color=RED) +
  geom_ribbon(data=fig3_data$new_test_df, 
              aes(x=survtime, ymin=lower_cum_risk_scaled,
                  ymax=upper_cum_risk_scaled), alpha=0.15, fill=RED) + 
  geom_vline(xintercept = test_decision_times[fig3_data$test_decisions=="B"],
             color=GREEN) +
  geom_point(aes(x=pat_df$year_visit,y=pat_df$log2psaplus1),
             size=NORMAL_POINT_SIZE, color=BLUE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=FONT_SIZE, color=BLUE),
        axis.text.y = element_text(size=FONT_SIZE, color=BLUE),
        axis.title.y.right  = element_text(size=FONT_SIZE, color=RED),
        axis.text.y.right = element_text(size=FONT_SIZE, color=RED)) +
  scale_x_continuous(breaks = xbreaks, labels=xlabs,
                     limits = c(-0.35, max_follow_up), 
                     minor_breaks = seq(0, max_follow_up, 1)) +
  xlab("Follow-up time (years)")+
  scale_y_continuous(breaks = psa_breaks, 
                     labels = round(psa_breaks,1), 
                     limits = range(psa_breaks),
                     sec.axis = sec_axis(trans=~., 
                                         breaks= fig3_data$riskAxisBreaks,
                                         labels = fig3_data$riskAxisLabels,
                                         name = "Cumulative-risk of progression")) +
  ylab("Biomarker") +
  geom_point(aes(x=-5,y=-5, color="Observed PSA"), size=NORMAL_POINT_SIZE) +
  scale_color_manual(values = c(BLUE), labels="Observed longitudinal biomarker")

fig3_label = baseggplot + 
  geom_vline(xintercept = c(0,1.5, test_decision_times[fig3_data$test_decisions=="B"]),
             color=c(ORANGE, GREEN, GREEN, GREEN),
             linetype=c("solid", "solid", "solid", "solid")) +
  geom_vline(xintercept = test_decision_times[fig3_data$test_decisions!="B"], 
             linetype="dashed") +
  geom_label(aes(x=c(0,1.5,2.5), y=c(0,0,0.03), 
                 label = c("Start\nsurveillance", 
                           "Last\ntest",
                           "Current\nvisit")), color=WHITE,
             size= SMALL_LABEL_SIZE,
             fill=c(ORANGE, GREEN, BLACK)) +
  geom_label(aes(x=test_decision_times[fig3_data$test_decisions!="B"],
                 y=c(-0.07,0,0), 
                 label = c("No test","No\ntest", "No\ntest")),
             size= SMALL_LABEL_SIZE) +
  geom_label(aes(x=test_decision_times[fig3_data$test_decisions=="B"], 
                 y=c(0,0), 
                 label = c("Test\nplanned", 
                           "Test\nplanned")), 
             color=GREEN, size=SMALL_LABEL_SIZE, fill=WHITE) +
  theme(text = element_text(size = FONT_SIZE),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(0,0,0,0, unit = "mm")) + 
  xlab("Follow-up time (years)") + ylim(-0.1,0.1) + 
  xlim(-0.35,6.5)

figure3 = ggpubr::ggarrange(fig3_top, fig3_label,
                            nrow=2, ncol=1, align="v",
                            heights = c(4,1.2),
                            common.legend = T, legend = "bottom")

saveFigure(figure3, filename = "latex/contents/c4/images/c4_fig3.pdf",
           size = NORMAL_PLOT_SIZE)

########################################
#Figure 4
########################################
reset()
total_schedules = length(fig4_data$risk_schedules$all_schedules)
risk_thresholds = sapply(fig4_data$risk_schedules$all_schedules, "[[", "cumulative_risk_threshold")
expected_delays = sapply(fig4_data$risk_schedules$all_schedules, "[[", "expected_detection_delay")
expected_total_tests = sapply(fig4_data$risk_schedules$all_schedules, "[[", "expected_num_tests")
euclidean_distance = sapply(fig4_data$risk_schedules$all_schedules, "[[", "euclidean_distance")

ceiling_expected_delay = ceiling(max(expected_delays))
ceiling_expected_total_tests = ceiling(max(expected_total_tests))

min_dist_schedule_index = which.min(euclidean_distance)[1]

delay_threshold = 1.5

figure4 = baseggplot + 
  geom_hline(yintercept = delay_threshold, linetype='dashed', color=ORANGE) +
  geom_label(aes(x=3.5, y=delay_threshold, label="Clinically acceptable limit for maximum time delay (example)"),
             color=ORANGE, size=SMALL_LABEL_SIZE) +
  geom_segment(aes(x=1,xend=expected_total_tests[-min_dist_schedule_index], 
                   y=0,yend=expected_delays[-min_dist_schedule_index]), 
               alpha=0.175, color='gray') +
  geom_segment(aes(x=1,xend=expected_total_tests[min_dist_schedule_index], 
                   y=0,yend=expected_delays[min_dist_schedule_index]),
               color=GREEN) +
  geom_point(aes(x=expected_total_tests[-min_dist_schedule_index], 
                 y=expected_delays[-min_dist_schedule_index]), 
             size=NORMAL_POINT_SIZE) +
  geom_point(aes(x=expected_total_tests[min_dist_schedule_index], 
                 y=expected_delays[min_dist_schedule_index]), 
             size=BIG_POINT_SIZE, color=GREEN, shape=TRIANGLE) +
  geom_label(aes(x=expected_total_tests[min_dist_schedule_index], 
                 y=expected_delays[min_dist_schedule_index], 
                 label=paste0("Optimal\nPersonalized\nSchedule\n\u03BA*(v) = ", 
                              round(risk_thresholds[min_dist_schedule_index]*100,1), "%")), 
             nudge_x = 0, nudge_y = -0.6, fill=GREEN, color=WHITE, size=SMALL_LABEL_SIZE)+
  geom_label(aes(x=expected_total_tests[c(1, total_schedules)], 
                 y=expected_delays[c(1, total_schedules)], 
                 label=paste0("Personalized\nSchedule\n\u03BA = ", 
                              round(risk_thresholds[c(1, total_schedules)]*100,1), "%")), 
             nudge_x = c(0.4, 0.4), nudge_y=c(0.2, 0), fill=BLACK, color=WHITE, size=SMALL_LABEL_SIZE)+
  geom_point(aes(x=1, y=0), shape=SQUARE, size=BIG_POINT_SIZE, 
             color=BLUE) +
  geom_label(aes(x=1,y=0, label="Ideal Schedule"), 
             nudge_y = -0.25,
             fill=BLUE, color=WHITE,
             size=SMALL_LABEL_SIZE)+
  scale_x_continuous(breaks=1:ceiling_expected_total_tests, 
                     limits = c(0.5,ceiling_expected_total_tests)) +
  scale_y_continuous(breaks=seq(0, ceiling_expected_delay, 1), 
                     limits = c(-0.5,ceiling_expected_delay)) +
  xlab("Expected number of tests") +
  ylab("Expected time delay in detecting progression (e.g., months,years)")

saveFigure(figure4, filename = "latex/contents/c4/images/c4_fig4.pdf",
           size = NORMAL_PLOT_SIZE)

########################################
#Figure 5
########################################
reset()
patient_df = fig5_data$patient_df
last_biopsy_time = 3.5
current_visit_time = 5
MAX_FOLLOW_UP = 10
surv_prob_times = seq(last_biopsy_time, MAX_FOLLOW_UP, length.out = 100)
future = fig5_data$future
schedule_df = fig5_data$schedule_df

risk_probs = 1 - future$predicted_surv_prob

patient_df$truePSA = rowMeans(future$predicted_psa)
patient_df$trueProbPalpableDRE = rowMeans(future$predicted_dre_prob)
min_x = -0.4
DRE_PSA_Y_GAP = 0.1
minYLeft = 0
maxYleft = 2 + DRE_PSA_Y_GAP * 2

dreDs = patient_df[!is.na(patient_df$palpable_dre), c("year_visit", "palpable_dre", "trueProbPalpableDRE")]
psaDs = patient_df[!is.na(patient_df$psa), c("year_visit", "log2psaplus1", "truePSA")]
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
psabreaks = seq(maxYleft/2 + DRE_PSA_Y_GAP, maxYleft, length.out = 3)
psalabels = round(seq(minPSA, maxPSA, length.out = 3),1)

mean_risk_probs = c(0, apply(risk_probs, 1, mean))
upper_risk_probs = c(0, apply(risk_probs, 1, quantile, probs=0.975))
lower_risk_probs = c(0,apply(risk_probs, 1, quantile, probs=0.025))

scaled_mean_risk_probs = mean_risk_probs * (maxYleft - minYLeft) + minYLeft
scaled_upper_risk_probs = upper_risk_probs * (maxYleft - minYLeft) + minYLeft
scaled_lower_risk_probs = lower_risk_probs * (maxYleft - minYLeft) + minYLeft

riskAxisBreaks = c(0, 0.25, 0.5, 0.75, 1) * (maxYleft - minYLeft) + minYLeft
riskAxisLabels = c("0%", "25%", "50%", "75%", "100%")

fig5_risk_plot = baseggplot_no_xticks +
  geom_line(aes(x=surv_prob_times, y=scaled_mean_risk_probs), color=RED) +
  geom_line(aes(x=surv_prob_times, y=scaled_mean_risk_probs), color=RED) +
  geom_ribbon(aes(x=surv_prob_times, ymin=scaled_lower_risk_probs,
                  ymax=scaled_upper_risk_probs), alpha=0.15, fill=RED) + 
  geom_segment(aes(x=-Inf, xend=current_visit_time, 
                   y=maxYleft/2, yend=maxYleft/2), linetype="solid") +
  geom_vline(xintercept = last_biopsy_time, color=GREEN) + 
  geom_vline(xintercept = current_visit_time, color=BLACK, linetype='dashed') + 
  geom_point(data = psaDs, size=NORMAL_POINT_SIZE, color=BLUE,
             aes(x = year_visit, y=log2psaplus1, shape="Observed PSA")) +
  geom_line(data = psaDs,  color=BLUE,
            aes(x = year_visit, y=truePSA, linetype="Fitted PSA")) +
  geom_point(data = dreDs, size=NORMAL_POINT_SIZE, color=BLUE,
             aes(x = year_visit, y=palpable_dre, shape="Observed DRE")) +
  geom_line(data = dreDs, color=BLUE,
            aes(x = year_visit, y=trueProbPalpableDRE, linetype="Fitted DRE")) +
  scale_shape_manual(name="",
                     labels= c(expression(atop('Observed', 'DRE')), expression(atop('Observed', 'log'[2]*'(PSA + 1)'))),
                     values = c(TRIANGLE, CIRCLE)) +
  scale_linetype_manual(name="",
                        labels= c(expression(atop('Fitted', 'Pr (Palpable DRE)')), expression(atop('Fitted', 'log'[2]*'(PSA + 1)'))),
                        values = c("dotted", "dashed")) +
  #ylab(expression('Pr (DRE Palpable)   '*'log'[2]*'(PSA + 1)')) +
  #ylab('DRE              PSA') +
  ylab(expression('DRE     '*'log'[2]*'(PSA + 1)')) +
  xlab("Follow-up time (years)") + 
  xlim(min_x,MAX_FOLLOW_UP) +
  theme(axis.text.y = element_text(size=FONT_SIZE, color=BLUE),
        axis.title.y = element_text(size=FONT_SIZE, color=BLUE),
        axis.text.y.right = element_text(size=FONT_SIZE, color=RED),
        axis.title.y.right = element_text(size=FONT_SIZE, color=RED),
        plot.margin = margin(0,0,0,0, unit = "mm")) +
  scale_y_continuous(limits = c(minYLeft, maxYleft),
                     breaks = c(drebreaks, psabreaks),
                     labels = c(drelabels, psalabels),
                     sec.axis = sec_axis(trans=~., 
                                         breaks= riskAxisBreaks,
                                         labels = riskAxisLabels,
                                         name = "Cumulative-risk\nof progression")) 

xbreaks = c(0:2,3.5,5:MAX_FOLLOW_UP)
xlabs = c("0","1","2","t=3.5","v=5","6","7","8","9","10")
fig5_planned_schedule_plot = baseggplot +
  geom_vline(xintercept = last_biopsy_time, color=GREEN) + 
  geom_vline(xintercept = current_visit_time, color=BLACK, linetype='dashed') + 
  geom_segment(aes(x=rep(current_visit_time,4), xend=rep(MAX_FOLLOW_UP,4), y=1:4, yend=1:4), color=GREEN, linetype='dotted')+
  geom_label(data=schedule_df,
             aes(x=times, y=number, label="B", group=name), 
             color=GREEN, fill=WHITE, size=SMALL_LABEL_SIZE)+
  xlab("Follow-up time (years)") + 
  ylab('Future biopsy\nschedule') +
  scale_x_continuous(breaks= xbreaks, labels=xlabs,
                     limits = c(min_x, MAX_FOLLOW_UP)) +
  scale_y_continuous(limits = c(1 - 0.25, 4 + 0.25),
                     breaks = 1:4,
                     labels = schedule_df$name[!duplicated(schedule_df$name)],
                     sec.axis = dup_axis()) +
  theme(axis.title.x = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        axis.text.y.right = element_text(size=FONT_SIZE),
        axis.title.y.right = element_text(size=FONT_SIZE),
        plot.margin = margin(0,0,0,0, unit = "mm"))

##############
fig5_label_plot = baseggplot + 
  geom_vline(xintercept = c(0,last_biopsy_time,current_visit_time),
             color=c(ORANGE, GREEN, "black"),
             linetype=c("solid", "solid", "dashed")) +
  geom_label(aes(x=c(0.1,last_biopsy_time,current_visit_time), 
                 y=c(0,0,0), 
                 label = c("Start\nsurveillance", 
                           "Last\nbiopsy",
                           "Current\nvisit")), color=WHITE,
             size= SMALL_LABEL_SIZE,
             fill=c(ORANGE, GREEN, BLACK)) +
  theme(text = element_text(size = FONT_SIZE),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(0,0,b = 1,0, unit = "mm")) + 
  xlab("Follow-up time (years)") + ylim(-0.1,0.1) + 
  xlim(min_x,MAX_FOLLOW_UP)

#########################
consequences_df = schedule_df[!duplicated(schedule_df$name),]
max_tests_possible = sum(schedule_df$name=='Annual')

fig5_num_test_plot = baseggplot + geom_col(aes(x=rep(consequences_df$name,2), 
                                        y=c(consequences_df$expected_num_tests, max_tests_possible - consequences_df$expected_num_tests)),
                                    color=BLACK, fill=c(rep(c('darkgrey',WHITE), nrow(consequences_df))),
                                    width=0.5)+
  ylab("Expected number of tests") + 
  xlab("Schedule") + scale_y_continuous(breaks = seq(0, max_tests_possible, by=2),
                                        labels = seq(0, max_tests_possible, by=2),
                                        limits = c(0, max_tests_possible)) + 
  coord_flip()

max_delay_limit = 3
fig5_delay_plot = baseggplot_no_yticks + geom_col(aes(x=rep(consequences_df$name,2), 
                                     y=c(consequences_df$expected_detection_delay, 
                                         max_delay_limit - consequences_df$expected_detection_delay)),
                                 color=BLACK, fill=c(rep(c('darkgrey',WHITE), nrow(consequences_df))),
                                 width=0.5)+
  ylab("Expected time delay (years)\nin detecting progression") + 
  xlab("Schedule") +
  scale_y_continuous(breaks = seq(0, max_delay_limit, by = 0.5),
                     labels = seq(0, max_delay_limit,  by = 0.5),
                     limits= c(0, max_delay_limit))+
  coord_flip()

fig5_consequences_plot = ggpubr::ggarrange(fig5_num_test_plot, fig5_delay_plot, 
                                      ncol=2, nrow=1, align = "h",
                                      labels = c("C", "D"),
                                      widths = c(1, 0.8))

fig5_upper_plot = ggpubr::ggarrange(fig5_risk_plot, fig5_planned_schedule_plot, 
                               fig5_label_plot,
                               ncol=1, align = "v",
                               hjust = -1.5,
                               heights = c(1,0.8,0.35),
                               labels = c('A', 'B', ''), 
                               legend = "none")

figure5 = ggpubr::ggarrange(fig5_upper_plot, fig5_consequences_plot,
                                  ncol=1, align = "v",
                                  hjust = -1.5,
                                  heights = c(1,0.4),
                                  legend = "none")

saveFigure(figure5, filename = "latex/contents/c4/images/c4_fig5.pdf",
           size = FULL_PLOT_SIZE)

########################################
#Figure 6
########################################
reset()
fig6_topleft = baseggplot_no_xticks +
  geom_boxplot(data = sim_res[sim_res$progressed==1,], 
               mapping = aes(x=schedule, y=nb), outlier.shape = NA) +
  stat_summary(data = sim_res[sim_res$progressed==1,], 
               mapping = aes(x=schedule, y=nb), 
               fun.y=mean, geom="point", size=NORMAL_POINT_SIZE, color=ORANGE) +
  scale_y_continuous(breaks = c(1,4,7,10), 
                     limits = c(1,10), minor_breaks = 1:10) +
  xlab("Schedule") + ylab("Number of biopsies") +
  coord_flip()

fig6_topright = baseggplot_no_xyticks +
  geom_boxplot(data = sim_res[sim_res$progressed==1,], 
               mapping = aes(x=schedule, y=delay), outlier.shape = NA)+
  stat_summary(data = sim_res[sim_res$progressed==1,], 
               mapping = aes(x=schedule, y=delay),
               fun.y=mean, geom="point", size=NORMAL_POINT_SIZE, color=ORANGE) +
  scale_y_continuous(breaks = 0:3, limits = c(0,3)) +
  xlab("Schedule") + ylab("Time delay in detecting\nprogression (years)")+
  coord_flip()

fig6_bottomleft = baseggplot + 
  geom_boxplot(data = sim_res[sim_res$progressed==0,], 
               mapping = aes(x=schedule, y=nb), outlier.shape = NA) +
  stat_summary(data = sim_res[sim_res$progressed==0,], 
               mapping = aes(x=schedule, y=nb),
               fun.y=mean, geom="point", size=NORMAL_POINT_SIZE, color=ORANGE) +
  scale_y_continuous(breaks = c(1,4,7,10), limits = c(1,10),
                     minor_breaks = 1:10) +
  xlab("Schedule") + ylab("Number of biopsies") +
  coord_flip()

fig6_bottomright = baseggplot_no_yticks + 
  geom_text(aes(x=1, y=1.5), size=MEDIUM_LABEL_SIZE,
            label="Time delay not available for\nnon-progressing patients")+
  scale_y_continuous(breaks = 0:3, limits = c(0,3)) +
  xlab("Schedule") + ylab("Time delay in detecting\nprogression (years)")+ 
  coord_flip()

fig6_top_plot = ggarrange(fig6_topleft, fig6_topright, nrow=1, ncol=2, 
                          align = "h", widths = c(1.7,1))
fig6_bottom_plot = ggarrange(fig6_bottomleft, fig6_bottomright, nrow=1, ncol=2, align = "h", widths = c(1.7,1))

figure6 = ggpubr::ggarrange(fig6_top_plot, fig6_bottom_plot,
                            heights = c(1, 1.2), hjust = -5,
                            nrow=2, ncol=1, align = "v", labels = "AUTO")
saveFigure(figure6, filename = "latex/contents/c4/images/c4_fig6.pdf",
           size = NORMAL_PLOT_SIZE)

########################################
#Figure 2app
########################################
reset()
total_schedules = length(fig4_data$tree_schedules$all_schedules)
expected_delays = sapply(fig4_data$tree_schedules$all_schedules, "[[", "expected_detection_delay")
expected_total_tests = sapply(fig4_data$tree_schedules$all_schedules, "[[", "expected_num_tests")
euclidean_distance = sapply(fig4_data$tree_schedules$all_schedules, "[[", "euclidean_distance")

ceiling_expected_delay = ceiling(max(expected_delays))
ceiling_expected_total_tests = ceiling(max(expected_total_tests))

min_dist_schedule_index = which.min(euclidean_distance)[1]

risk_thresholds = sapply(fig4_data$risk_schedules$all_schedules, "[[", "cumulative_risk_threshold")
expected_risk_delays = sapply(fig4_data$risk_schedules$all_schedules, "[[", "expected_detection_delay")
expected_risk_total_tests = sapply(fig4_data$risk_schedules$all_schedules, "[[", "expected_num_tests")
euclidean_risk_distance = sapply(fig4_data$risk_schedules$all_schedules, "[[", "euclidean_distance")
min_dist_risk_schedule_index = which.min(euclidean_risk_distance)[1]

delay_threshold = 1.5

figure2app = baseggplot + 
  geom_segment(aes(x=1,xend=expected_total_tests[-min_dist_schedule_index], 
                   y=0,yend=expected_delays[-min_dist_schedule_index]), 
               alpha=0.175, color=GREY) +
  geom_segment(aes(x=1,xend=expected_total_tests[min_dist_schedule_index], 
                   y=0,yend=expected_delays[min_dist_schedule_index]),
               color=RED) +
  geom_segment(aes(x=1,xend=expected_risk_total_tests[min_dist_risk_schedule_index], 
                   y=0,yend=expected_risk_delays[min_dist_risk_schedule_index]),
               color=GREEN) +
  geom_point(aes(x=expected_total_tests[-min_dist_schedule_index], 
                 y=expected_delays[-min_dist_schedule_index], 
                 color="All possible schedules", shape="All possible schedules"), 
             size=BIG_POINT_SIZE, alpha=0.35) +
  geom_point(aes(x=expected_risk_total_tests, 
                 y=expected_risk_delays, 
                 color="Risk based schedules", shape="Risk based schedules"), 
             size=NORMAL_POINT_SIZE) +
  geom_point(aes(x=expected_total_tests[min_dist_schedule_index], 
                 y=expected_delays[min_dist_schedule_index]), 
             size=BIG_POINT_SIZE, color=RED, shape=RHOMBUS) + 
  geom_point(aes(x=expected_risk_total_tests[min_dist_risk_schedule_index], 
                 y=expected_risk_delays[min_dist_risk_schedule_index]), 
             size=BIG_POINT_SIZE, color=GREEN, shape=TRIANGLE)+
  geom_label(aes(x=expected_risk_total_tests[min_dist_risk_schedule_index], 
                 y=expected_risk_delays[min_dist_risk_schedule_index], 
                 label=paste0("Optimal\nAmong Risk-based\nSchedules\n\u03BA*(v) = ", 
                              round(risk_thresholds[min_dist_risk_schedule_index]*100,1), "%")), 
             nudge_x = 0, nudge_y = -0.7, fill=GREEN, color=WHITE, size=SMALL_LABEL_SIZE) +
  geom_label(aes(x=expected_total_tests[min_dist_schedule_index], 
                 y=expected_delays[min_dist_schedule_index], 
                 label="Optimal\nAmong All\nSchedules"), 
             nudge_x = 0, nudge_y = 0.5, fill=RED, color=WHITE, 
             size=SMALL_LABEL_SIZE) +
  geom_point(aes(x=1, y=0), shape=SQUARE, size=BIG_POINT_SIZE, 
             color=BLUE) +
  geom_label(aes(x=1,y=0, label="Ideal Schedule"), 
             nudge_y = -0.25,
             fill=BLUE, color=WHITE,
             size=SMALL_LABEL_SIZE)+
  scale_x_continuous(breaks=1:ceiling_expected_total_tests, 
                     limits = c(0.5,ceiling_expected_total_tests)) +
  scale_y_continuous(breaks=seq(0, ceiling_expected_delay, 1), 
                     limits = c(-0.5,ceiling_expected_delay)) +
  scale_color_manual(name="Type", 
                     labels=c("All possible schedules","Risk based schedules"), 
                     values=c(ORANGE, BLACK))+
  scale_shape_manual(name="Type",
                     labels=c("All possible schedules","Risk based schedules"), 
                     values=c(RHOMBUS, CIRCLE))+
  theme(legend.position = 'bottom', legend.direction = 'horizontal')+
  xlab("Expected number of tests") +
  ylab("Expected time delay in detecting progression (e.g., months,years)")

saveFigure(figure2app, filename = "latex/contents/c4/images/c4_fig_app2.pdf",
           size = FULL_PLOT_SIZE)

