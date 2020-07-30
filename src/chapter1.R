library(ggplot2)
library(splines)
library(JMbayes)
library(doParallel)
library(dplyr)
library(tidyr)
library(ggpubr)

reset = function(){
  parent_env = parent.env(environment())
  rm(list = setdiff(ls(name=parent_env), "reset"), envir = parent_env)
  load("Rdata/colors.Rdata", envir = parent_env)
  load("Rdata/shapes.Rdata", envir = parent_env)
  
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
    theme(plot.margin = margin(0,0,0,0, "mm"), 
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
  
  # assign("npmle_all", npmle_all, envir = parent_env)
  # assign("prias.id", prias.id, envir = parent_env)
  # assign("prias_long", prias_long, envir = parent_env)
  # assign("reclassification_df", reclassification_df, envir = parent_env)
  # assign("colormap", colormap, envir = parent_env)
  # assign("mvJoint_psa_time_scaled", mvJoint_psa_time_scaled, envir = parent_env)
}


###############
# Figure 1
###############
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
             fill='white') +
  geom_label(aes(x=0:5, y=rep(0.5,6),
                 label = c("Start\nsurveillance", 
                           "1st negative\ntest", 
                           "2nd negative\ntest", 
                           "3rd negative\ntest", 
                           "4th negative\ntest", 
                           "5th test\nprogression\ndetected")),
             size=SMALL_LABEL_SIZE, color='white',
             fill=c(ORANGE, rep(GREEN,4), RED)) +
  geom_text(aes(x=0.5 * (TRUE_TIME_Prog + 5), y=0.2, label="6 months delay\n in detecting progression"), size=SMALL_LABEL_SIZE) + 
  geom_segment(aes(x=TRUE_TIME_Prog, xend = 5, y=0.09, yend = 0.09),
               arrow = arrow(length = unit(ARROW_SIZE,"mm"), ends="both", type="closed"))+
  scale_x_continuous(breaks = c(0,1,2,3,TRUE_TIME_Prog,4,5,6), limits=c(-0.3,6.25)) + 
  ylim(0,1) + 
  ggtitle("    More tests, shorter delay") +
  theme(plot.title = element_text(vjust = -10, hjust = 0.025))

delay_explanation_plot_b = baseggplot_no_yticks + 
  geom_ribbon(aes(x=c(TRUE_TIME_Prog, 6), ymin=-Inf, ymax=Inf), fill=RED, alpha=0.25) +
  geom_segment(aes(x=c(0,2,4), y=rep(-Inf,3), xend=c(0,2,4), yend=rep(0.5,3)),
               color=c(ORANGE, rep(GREEN, 2)))+
  geom_segment(aes(x=c(TRUE_TIME_Prog,6), y=rep(-Inf,2), xend=c(TRUE_TIME_Prog,6), yend=c(TRUE_TIME_Prog_HT,0.5)),
               color=RED)+
  geom_label(aes(x=TRUE_TIME_Prog, y=TRUE_TIME_Prog_HT, label = "True time of\nprogression"),
             size=SMALL_LABEL_SIZE, color=RED,
             fill='white') +
  geom_label(aes(x=c(0,2,4,6), y=rep(0.5,4),
                 label = c("Start\nsurveillance", 
                           "1st negative\ntest", 
                           "2nd negative\ntest", 
                           "3rd test\nprogression\ndetected")),
             size=SMALL_LABEL_SIZE, color='white',
             fill=c(ORANGE, rep(GREEN,2), RED)) +
  #geom_text(aes(x=0.5 * (TRUE_TIME_Prog + 6), y=0.2, label="18 months delay\n in detecting progression"), size=LABEL_SIZE) + 
  annotate("text", x=0.5 * (TRUE_TIME_Prog + 6), y=0.2, label="18 months delay\n in detecting progression", size=SMALL_LABEL_SIZE)+
  geom_segment(aes(x=TRUE_TIME_Prog, xend = 6, y=0.09, yend = 0.09),
               arrow = arrow(length = unit(ARROW_SIZE,"mm"), ends="both", type="closed"))+
  xlab("Time of test visits") + 
  scale_x_continuous(breaks = c(0,1,2,3,4,TRUE_TIME_Prog, 5,6), 
                     labels = c("Jan 2000","Jan 2001", "Jan 2002",
                                "Jan 2003", "Jan 2004", "Jul 2004", 
                                "Jan 2005", "Jan 2006"), limits=c(-0.3,6.25)) + 
  ylim(0,1) + theme(axis.text.x = element_text(size=FONT_SIZE, angle = 30, hjust = 1)) +
  ggtitle("    Fewer tests, larger delay") +
  theme(plot.title = element_text(vjust = -10, hjust = 0.025))


figure1 = ggpubr::ggarrange(delay_explanation_plot_a, delay_explanation_plot_b, 
                            ncol = 1, nrow=2, align = "v", labels = "AUTO",
                            heights = c(1, 1.2), vjust = 3.25)
print(figure1)
saveFigure(figure1, filename = "latex/contents/c1/images/c1_fig1.pdf",
           size = FULL_PLOT_SIZE)



# ###############
# # Figure 2
# ###############
# reset()
# 
# npmle_plotdf_all=do.call('rbind', 
#                          lapply(c("Hopkins", "KCL", "MSKCC", "PRIAS", "Toronto", "MUSIC"), FUN = function(name){
#                            survProb = 1 - cumsum(npmle_all[[name]]$pf)
#                            survProb = c(1, survProb)
#                            
#                            survIntervals = npmle_all[[name]]$intmap
#                            survIntervals = cbind(c(0,0), survIntervals)
#                            
#                            timePoints = as.numeric(survIntervals)
#                            survProbs = c(1,as.numeric(rep(survProb, each=2)))[1:length(timePoints)]
#                            
#                            return(data.frame('Cohort'=name,timePoints=timePoints, riskProbs=1-survProbs))
#                          }))
# 
# npmle_plotdf_all$time_10pat_risk_set = sapply(npmle_plotdf_all$Cohort, function(x){
#   reclassification_df$time_10pat_risk_set[reclassification_df$Cohort==x]
# })
# npmle_plotdf_all = npmle_plotdf_all[npmle_plotdf_all$timePoints <= npmle_plotdf_all$time_10pat_risk_set,]
# 
# cohort_names = unique(npmle_plotdf_all$Cohort)
# cohort_labpos_x = as.numeric(by(npmle_plotdf_all$Cohort, data = npmle_plotdf_all$timePoints, max))
# cohort_labpos_y = as.numeric(by(npmle_plotdf_all$Cohort, data = npmle_plotdf_all$riskProbs, max))
# 
# figure4 = baseggplot + 
#   geom_line(aes(x=npmle_plotdf_all$timePoints, 
#                 y=npmle_plotdf_all$riskProbs, 
#                 group=npmle_plotdf_all$Cohort, 
#                 color=npmle_plotdf_all$Cohort)) +  
#   geom_label(aes(x=cohort_labpos_x, 
#                  y=cohort_labpos_y, 
#                  label=cohort_names,
#                  fill=cohort_names), color='white')+
#   scale_color_manual(values=colormap)+
#   scale_fill_manual(values=colormap)+
#   coord_cartesian(xlim=c(0,8)) + 
#   theme(legend.position = "none") +
#   scale_y_continuous(breaks = seq(0, 1, 0.25), labels = paste0(seq(0, 1, 0.25)*100, "%"),
#                      limits = c(0,1)) + 
#   ylab("Cumulative risk of reclassification (%)") +
#   xlab("Follow-up time (years)")
# 
# saveFigure(figure4, filename = "latex/contents/c1/images/c1_fig4.pdf",
#            size = NORMAL_PLOT_SIZE)

# ########################################
# #Figure 2
# ########################################
# reset()
# 
# df <- data.frame(
#   Schedule = c("1.0 year", "1.5 years",
#                "2.0 years", "3.0 years"),
#   prop = c(52, 12, 30, 6))
# df$label = paste0(df$prop, "%")
# 
# df <- df %>%
#   arrange(desc(Schedule)) %>%
#   mutate(lab.ypos = cumsum(prop) - 0.5*prop)
# df
# 
# mycols <- c(RED, GREEN, BLUE, BLACK)
# figure2 = ggplot(df, aes(x = 2, y = prop, fill = Schedule)) +
#   geom_bar(stat = "identity", color = WHITE) +
#   coord_polar(theta = "y", start = 1.65)+
#   geom_text(aes(y = lab.ypos, label = label), size=LABEL_SIZE, color = WHITE)+
#   scale_fill_manual(name="Biopsy Frequency", values = mycols) +
#   theme_void()+ theme(text = element_text(size=FONT_SIZE), legend.position = "right",
#                       legend.direction = "vertical", 
#                       plot.margin = unit(c(0,0,0,0),"pt"))+
#   xlim(0.5, 2.5)
# 
# plot(figure2)
# 
# saveFigure(figure2, filename = "latex/contents/c1/images/c1_fig2.pdf",
#            size = NORMAL_PLOT_SIZE)
# 
# 

# ###############
# # PSA observed
# ###############
# reset()
# 
# plotObsPSASubject = function(){
#   repeat{
#     pid = sample(unique(prias_long$P_ID), size = 1)
#     print(paste("Choosing patient", pid, "randomly because pid not provided"))
#     
#     plotdf = prias_long[prias_long$P_ID == pid & !is.na(prias_long$psa),]
#     if(nrow(plotdf)>4){
#       break
#     }
#   }
#   
#   yrange = range(plotdf$psa)
#   xrange = range(plotdf$visitTimeYears)
#   
#   plot = baseggplot + 
#     geom_point(aes(x=plotdf$visitTimeYears,y=plotdf$psa), size=NORMAL_POINT_SIZE) + 
#     geom_line(aes(x=plotdf$visitTimeYears,y=plotdf$psa), alpha=0.2) + 
#     scale_y_continuous(breaks = seq(yrange[1], yrange[2], length.out = 4),
#                        labels = round(seq(yrange[1], yrange[2], length.out = 4),1),
#                        limits = yrange) +
#     scale_x_continuous(breaks = seq(xrange[1], xrange[2], length.out = 4),
#                        labels = round(seq(xrange[1], xrange[2], length.out = 4),1),
#                        limits = xrange) +
#     theme(axis.text.x = element_text(size=FONT_SIZE, angle = 25, hjust = 1)) +
#     xlab("Follow-up time\n(years)") + ylab("PSA")
#   return(plot)
# }
# 
# set.seed(2019)
# temp = lapply(1:9, FUN = function(i){
#   res = plotObsPSASubject()
#   if(i %in% 1:6){
#     res = res + theme(axis.title.x = element_blank())
#   }
#   if(i %in% c(2,3,5,6,8,9)){
#     res = res + theme(axis.title.y = element_blank())
#   }
#   return(res)
# })
# 
# figure5 = ggpubr::ggarrange(plotlist = temp, nrow = 3, ncol=3, 
#                             common.legend = T, legend = "bottom",
#                             align = "hv")
# saveFigure(figure5, filename = "latex/contents/c1/images/c1_fig5.pdf",
#            size = FULL_PLOT_SIZE)
# 
# 
# ##############
# # Figure 6
# ##############
# reset()
# source("src/prediction_only_psa.R")
# MAX_FOLLOW_UP = 6
# 
# pat_data = prias_long[prias_long$P_ID==113 & prias_long$year_visit<=3,]
# set.seed(2019)
# pat_data$log2psaplus1 = pat_data$log2psaplus1 - 2
# 
# pat_data$log2psaplus1[pat_data$year_visit > 2] = pat_data$log2psaplus1[pat_data$year_visit > 2] + 
#   rnorm(n = length(pat_data$log2psaplus1[pat_data$year_visit > 2]), 2.25, 1)
# 
# pat_data$psa = 2^pat_data$log2psaplus1 - 1
# 
# latest_survival_time = 1
# 
# cur_visit_time = max(pat_data$year_visit)
# 
# accuracy = 100
# psa_predict_times = seq(0, MAX_FOLLOW_UP, length.out = accuracy)
# survival_predict_times = seq(latest_survival_time, MAX_FOLLOW_UP, length.out = accuracy)
# 
# exp_fut = getExpectedFutureOutcomes(mvJoint_psa_time_scaled, pat_data, latest_survival_time, Inf,
#                                     survival_predict_times, psa_predict_times, psaDist = "Tdist")
# mean_psa = rowMeans(exp_fut$predicted_psa)
# mean_psa_velocity = rowMeans(exp_fut$predicted_psa_slope)
# mean_cum_risk = 1-c(1, rowMeans(exp_fut$predicted_surv_prob))
# lower_cum_risk = 1-c(1, apply(exp_fut$predicted_surv_prob, 1, quantile, probs=0.025))
# upper_cum_risk = 1-c(1, apply(exp_fut$predicted_surv_prob, 1, quantile, probs=0.975))
# 
# A_y_breaks = seq(min(pat_data$log2psaplus1, na.rm = T), 
#                  max(pat_data$log2psaplus1, na.rm = T), 
#                  length.out = 3)
# 
# B_y_breaks = seq(min(mean_psa_velocity, na.rm = T), 
#                  max(mean_psa_velocity, na.rm = T), 
#                  length.out = 3)
# 
# common = baseggplot +
#   scale_x_continuous(breaks = 0:MAX_FOLLOW_UP,
#                      limits = c(-0.3, MAX_FOLLOW_UP)) +
#   geom_vline(xintercept = cur_visit_time, linetype="dashed") +
#   geom_vline(xintercept = latest_survival_time, color=GREEN) +
#   xlab("Follow-up time (years)")
# 
# blank_common = common + 
#   theme(axis.text.x = element_blank(), 
#         axis.title.x = element_blank(),
#         axis.title.y = element_text(size=FONT_SIZE, color=BLUE),
#         axis.text.y = element_text(size=FONT_SIZE, color=BLUE))
# 
# #c(0,latest_survival_time,cur_visit_time)
# A = blank_common + 
#   geom_point(aes(x=-50, y=Inf, color='Observed PSA (log scale)')) +
#   scale_color_manual(values = BLUE) + 
#   geom_point(aes(x=pat_data$year_visit,y=pat_data$log2psaplus1),
#              size=NORMAL_POINT_SIZE, color=BLUE) +
#   geom_line(aes(x=psa_predict_times[psa_predict_times<=cur_visit_time], 
#                 y=mean_psa[psa_predict_times<=cur_visit_time]), color=BLUE) + 
#   scale_y_continuous(breaks = A_y_breaks, labels = round(A_y_breaks,1)) +
#   ylab("PSA (log scale)\nvalue")
# #ylab(expression(atop('log'[2]*'(PSA + 1)', 'value')))
# 
# B = blank_common + 
#   geom_line(aes(x=psa_predict_times[psa_predict_times<=cur_visit_time], 
#                 y=mean_psa_velocity[psa_predict_times<=cur_visit_time]), 
#             color=BLUE) + 
#   scale_y_continuous(breaks = B_y_breaks, labels = round(B_y_breaks,1)) +
#   ylab('PSA (log scale)\nVelocity')
# #ylab(expression(atop('log'[2]*'(PSA + 1)', 'instantaneous velocity')))
# 
# C = common + geom_line(aes(x=survival_predict_times, y=mean_cum_risk), color=RED) +
#   geom_ribbon(aes(x=survival_predict_times, ymin=lower_cum_risk,
#                   ymax=upper_cum_risk), alpha=0.15, fill=RED) + 
#   scale_y_continuous(breaks = seq(0,1, 0.5), 
#                      labels = c("0%", "50%", "100%"), 
#                      limits = c(0,1)) + ylab("Cumulative risk\nof reclassification") +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_text(color=RED, size=FONT_SIZE),
#         axis.text.y = element_text(color=RED, size=FONT_SIZE),
#         plot.margin = margin(b = 0, unit = "pt"))
# 
# D = common +
#   geom_label(aes(x=c(0,1,cur_visit_time), y=c(0,0,0), 
#                  label = c("Start\nsurveillance", 
#                            "negative\ntest",
#                            "Current\nVisit")), color='white',
#              size= SMALL_LABEL_SIZE, nudge_x = c(-0.17, 0,0),
#              fill=c(ORANGE, GREEN, 'black')) +
#   theme(text = element_text(size = FONT_SIZE),
#         panel.background = element_blank(),
#         axis.text = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks = element_blank(),
#         panel.border = element_blank(),
#         panel.grid = element_blank()) + 
#   ylim(-0.25,0.25)
# 
# figure6 = ggarrange(A,B, C, D, ncol=1, nrow=4, align = "v",
#                                 heights = c(1.1,1,1.1,0.6), common.legend = T,
#                                 hjust = -9, vjust = 2,
#                                 legend = "none", labels = c("A","B", "C", ""))
# 
# saveFigure(figure6, filename = "latex/contents/c1/images/c1_fig6.pdf",
#            size = FULL_PLOT_SIZE)
