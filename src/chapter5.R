library(ggplot2)
library(ggpubr)
library(splines)

reset = function(){
  parent_env = parent.env(environment())
  rm(list = setdiff(ls(name=parent_env), "reset"), envir = parent_env)
  load("Rdata/colors.Rdata", envir = parent_env)
  load("Rdata/shapes.Rdata", envir = parent_env)
  load("Rdata/rocresults.Rdata", envir = parent_env)
  load("Rdata/chapter5/cleandata.Rdata", envir=parent_env)
  load("Rdata/chapter5/fig3_data.Rdata", envir=parent_env)
  load("Rdata/chapter5/fig4_data.Rdata", envir=parent_env)
  load("Rdata/chapter5/fig5_data.Rdata", envir=parent_env)
  load("Rdata/chapter5/calib_in_small_plotdf.Rdata", envir=parent_env)
  load("Rdata/chapter5/npmle_plotdf_all.Rdata", envir=parent_env)
  
  FONT_SIZE = 10
  MEDIUM_LINE = 0.5
  NORMAL_POINT_SIZE = 2
  BIG_POINT_SIZE = 4
  LABEL_SIZE = 3.5
  
  assign("MAX_FOLLOW_UP", 6, envir = parent_env)
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
  
  assign("reclassification_df", reclassification_df, envir = parent_env)
  assign("colormap", colormap, envir = parent_env)
  
}

########################################
#Figure 1
########################################
reset()
ARROW_SIZE = 2.0
TRUE_TIME_GS7 = 3.5
TRUE_TIME_GS7_HT = 0.85

delay_explanation_plot_a = baseggplot_no_xyticks + 
  geom_ribbon(aes(x=c(TRUE_TIME_GS7, 4), ymin=-Inf, ymax=Inf), fill=RED, alpha=0.25) +
  geom_segment(aes(x=0:3, y=rep(-Inf,4), xend=0:3, yend=rep(0.5,4)),
               color=c(ORANGE, rep(GREEN, 3)))+
  geom_segment(aes(x=c(TRUE_TIME_GS7,4), y=rep(-Inf,2), xend=c(TRUE_TIME_GS7,4), yend=c(TRUE_TIME_GS7_HT,0.5)),
               color=RED)+
  geom_label(aes(x=TRUE_TIME_GS7, y=TRUE_TIME_GS7_HT, label = "True time of\nGleason grade \u2265 2"),
             size=SMALL_LABEL_SIZE, color=RED,
             fill=WHITE) +
  geom_label(aes(x=0:4, y=rep(0.5,5),
                 label = c("Start AS\nGleason\ngrade 1", 
                           "1st Biopsy\nGleason\ngrade 1", 
                           "2nd Biopsy\nGleason\ngrade 1", 
                           "3rd Biopsy\nGleason\ngrade 1", 
                           "4th Biopsy\nGleason\ngrade \u2265 2")),
             size=SMALL_LABEL_SIZE, color=WHITE,
             fill=c(ORANGE, rep(GREEN,3), RED)) +
  geom_text(aes(x=0.5 * (TRUE_TIME_GS7 + 4), y=0.2, label="6 months delay\n in detecting upgrading"), size=MEDIUM_LABEL_SIZE) + 
  geom_segment(aes(x=TRUE_TIME_GS7, xend = 4, y=0.07, yend = 0.07),
               arrow = arrow(length = unit(ARROW_SIZE,"mm"), ends="both", type="closed"))+
  scale_x_continuous(breaks = c(0,1,2,3,TRUE_TIME_GS7,4), limits=c(-0.2,5.25)) + 
  ylim(0,1) + 
  ggtitle("    Biopsy every year") +
  theme(plot.title = element_text(vjust = -10, hjust = 0.025))

delay_explanation_plot_b = baseggplot_no_yticks + 
  geom_ribbon(aes(x=c(TRUE_TIME_GS7, 5), ymin=-Inf, ymax=Inf), fill=RED, alpha=0.25) +
  geom_segment(aes(x=c(0,1,3), y=rep(-Inf,3), xend=c(0,1,3), yend=rep(0.5,3)),
               color=c(ORANGE, rep(GREEN, 2)))+
  geom_segment(aes(x=c(TRUE_TIME_GS7,5), y=rep(-Inf,2), xend=c(TRUE_TIME_GS7,5), yend=c(TRUE_TIME_GS7_HT,0.5)),
               color=RED)+
  geom_label(aes(x=TRUE_TIME_GS7, y=TRUE_TIME_GS7_HT, label = "True time of\nGleason grade \u2265 2"),
             size=SMALL_LABEL_SIZE, color=RED,
             fill=WHITE) +
  geom_label(aes(x=c(0,1,3,5), y=rep(0.5,4),
                 label = c("Start AS\nGleason\ngrade 1", 
                           "1st Biopsy\nGleason\ngrade 1", 
                           "2nd Biopsy\nGleason\ngrade 1", 
                           "3rd Biopsy\nGleason\ngrade \u2265 2")),
             size=SMALL_LABEL_SIZE, color=WHITE,
             fill=c(ORANGE, rep(GREEN,2), RED)) +
  annotate("text", x=0.5 * (TRUE_TIME_GS7 + 5), y=0.2, label="18 months delay\n in detecting upgrading", size=MEDIUM_LABEL_SIZE)+
  geom_segment(aes(x=TRUE_TIME_GS7, xend = 5, y=0.07, yend = 0.07),
               arrow = arrow(length = unit(ARROW_SIZE,"mm"), ends="both", type="closed"))+
  xlab("Time of biopsy visits") + 
  scale_x_continuous(breaks = c(0,1,2,3,TRUE_TIME_GS7,4, 5), 
                     labels = c("Jan 2005","Jan 2006", "Jan 2007","Jan 2008",
                                "Jul 2008", "Jan 2009", "Jan 2010"), limits=c(-0.2,5.25))+
  ylim(0,1) + theme(axis.text.x = element_text(size=FONT_SIZE, angle = 30, hjust = 1)) +
  ggtitle("    Biopsy every 2 years") +
  theme(plot.title = element_text(vjust = -10, hjust = 0.025))


figure1 = ggpubr::ggarrange(delay_explanation_plot_a, delay_explanation_plot_b, 
                            ncol = 1, nrow=2, align = "v", labels = "AUTO",
                            heights = c(1, 1.2), vjust = 3.25, hjust = -0.75)
print(figure1)
saveFigure(figure1, filename = "latex/contents/c5/images/c5_fig1.pdf",
           size = NORMAL_PLOT_SIZE)

###############
# Figure 2
###############
reset()

riskGaugeGraph = function(mean_risk_prob, RED_threshold = 0.2, gauge_color=RED){
  
  risk_label = paste0("\n\n\nUpgrading-risk\n at current visit: ", round(mean_risk_prob*100), "%")
  
  gauge_ticks_colors = sapply(seq(0,1,0.25), FUN = function(prop){
    if(prop > RED_threshold){
      return(RED)
    }else{
      col=colorRamp(c(GREEN, ORANGE, RED))(prop/RED_threshold)
      return(rgb(col[1], col[2], col[3], maxColorValue = 255))
    }
  })
  
  riskGauge = ggplot(data = NULL,
                     aes(ymax = mean_risk_prob, ymin = 0, xmax = 2, xmin = 1,
                         fill="Risk")) +
    geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill =WHITE, color=gauge_color) +
    geom_rect() +
    geom_segment(aes(x=2.0, xend=2.1, y=0, yend=0), color=gauge_ticks_colors[1])+
    geom_segment(aes(x=2.0, xend=2.1, y=0.25, yend=0.25), color=gauge_ticks_colors[2])+
    geom_segment(aes(x=2.0, xend=2.1, y=0.5, yend=0.5), color=gauge_ticks_colors[3])+
    geom_segment(aes(x=2.0, xend=2.1, y=0.75, yend=0.75), color=gauge_ticks_colors[4])+
    geom_segment(aes(x=2.0, xend=2.1, y=1, yend=1), color=gauge_ticks_colors[5])+
    geom_text(aes(x = 2.55, y = 0, label = "0%"), size=SMALL_LABEL_SIZE, color=gauge_ticks_colors[1]) +
    geom_text(aes(x = 2.55, y = 0.25, label = "25%"), size=SMALL_LABEL_SIZE, color=gauge_ticks_colors[2]) +
    geom_text(aes(x = 2.55, y = 0.5, label = "50%"), size=SMALL_LABEL_SIZE, color=gauge_ticks_colors[3]) +
    geom_text(aes(x = 2.55, y = 0.75, label = "75%"), size=SMALL_LABEL_SIZE, color=gauge_ticks_colors[4]) +
    geom_text(aes(x = 2.55, y = 1, label = "100%"), size=SMALL_LABEL_SIZE, color=gauge_ticks_colors[5]) +
    coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2.6)) + ylim(c(0,2)) +
    geom_text(aes(x = 0, y = 0, label = risk_label),
              color=gauge_color, size=SMALL_LABEL_SIZE) +
    geom_segment(aes(x=0, xend=1, y=mean_risk_prob, yend=mean_risk_prob),
                 color=gauge_color,
                 arrow = arrow(length = unit(0.25,"cm")))+
    geom_point(aes(x=0, y=0), color=gauge_color, size=2)+
    scale_fill_manual("", values=gauge_color)+
    theme_void() +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          title = element_text(size=FONT_SIZE, color=gauge_color)) +
    guides(fill=FALSE) +
    guides(colour=FALSE)
  return(riskGauge)
}

pat1 = prias_long_final[prias_long_final$P_ID==3682,]
pat1 = pat1[!is.na(pat1$psa) & pat1$year_visit<=3,]
pat1$year_visit[nrow(pat1)] = 3
p1 = baseggplot_no_xticks +
  geom_point(aes(x=pat1$year_visit, y=pat1$psa), size=NORMAL_POINT_SIZE) +
  geom_line(aes(x=pat1$year_visit, y=pat1$psa), alpha=0.2) +
  geom_vline(xintercept = 1, color=GREEN) +
  scale_x_continuous(breaks = c(0,1,2,3), limits = c(-0.35,2.25),
                     labels = c("0", "1","2", "3")) +
  ylim(0,15) +
  ylab("PSA (ng/mL)") + xlab("Follow-up time (years)") +
  ggtitle("Should a biopsy be conducted at current visit?")

set.seed(100)
pat2 = prias_long_final[prias_long_final$P_ID==1931,]
pat2 = pat2[!is.na(pat2$psa) & pat2$year_visit<=3,]
pat2$psa[pat2$year_visit>1] = pat2$psa[pat2$year_visit>1] +
  2*pat2$year_visit[pat2$year_visit>1] +
  rnorm(n=sum(pat2$year_visit>2), mean = 0, sd=0.18)
pat2$year_visit[nrow(pat2)] = 3
p2 = baseggplot +
  geom_point(aes(x=pat2$year_visit, y=pat2$psa), size=NORMAL_POINT_SIZE) +
  geom_line(aes(x=pat2$year_visit, y=pat2$psa), alpha=0.2) +
  geom_vline(xintercept = 1, color=GREEN) +
  scale_x_continuous(breaks = c(0,1,2,3), limits = c(-0.35,2.25),
                     labels = c("0", "1","2", "3")) +
  ylim(0,15) + theme(axis.title.x = element_blank())+
  ylab("PSA (ng/mL)")

p3 = baseggplot_event_plot +
  geom_label(aes(x=c(0,1.1,3), y=c(0,0,0),
                 label = c("Start AS\nGleason grade 1",
                           "Time of last biopsy\nGleason grade 1",
                           "Current\nvisit")), color=WHITE,
             size= SMALL_LABEL_SIZE,
             fill=c(ORANGE, GREEN, BLACK)) +
  xlab("Follow-up time (years)") + ylim(-0.25,0.25) +
  scale_x_continuous(breaks = c(0, 1, 2, 3), limits = c(-0.35,2.25),
                     labels = c("0", "1","2", "3"))


psa_plot = ggpubr::ggarrange(p1, p2, p3,
                             align = "v", labels = c("A", "B", ""),
                             ncol=1, nrow=3, heights = c(1, 1, 0.4))

risk_plot = ggpubr::ggarrange(ggplot() + theme_void(),
                              riskGaugeGraph(mean_risk_prob = 0.05, gauge_color = GREEN),
                              riskGaugeGraph(mean_risk_prob = 0.2, gauge_color = RED),
                              ggplot() + theme_void(),
                              ncol = 1, nrow = 4, align = "v", heights = c(0.35,1,1, 0.15))

figure2 = ggpubr::ggarrange(psa_plot, risk_plot, widths = c(2,1))

saveFigure(figure2, filename = "latex/contents/c5/images/c5_fig2.pdf",
           size = NORMAL_PLOT_SIZE)

###############
# Figure 3
###############
reset()

psa_predict_times = fig3_data$psa_predict_times
survival_predict_times = fig3_data$survival_predict_times
mean_psa = rowMeans(fig3_data$exp_fut$predicted_psa)
mean_psa_velocity = rowMeans(fig3_data$exp_fut$predicted_psa_slope)
mean_cum_risk = 1-c(1, rowMeans(fig3_data$exp_fut$predicted_surv_prob))
lower_cum_risk = 1-c(1, apply(fig3_data$exp_fut$predicted_surv_prob, 1, quantile, probs=0.025))
upper_cum_risk = 1-c(1, apply(fig3_data$exp_fut$predicted_surv_prob, 1, quantile, probs=0.975))

A_y_breaks = seq(min(fig3_data$pat_data$log2psaplus1, na.rm = T), 
                 max(fig3_data$pat_data$log2psaplus1, na.rm = T), 
                 length.out = 3)

B_y_breaks = seq(min(mean_psa_velocity, na.rm = T), 
                 max(mean_psa_velocity, na.rm = T), 
                 length.out = 3)

common = baseggplot +  
  scale_x_continuous(breaks = 0:MAX_FOLLOW_UP,
                     limits = c(-0.3, MAX_FOLLOW_UP)) +
  geom_vline(xintercept = fig3_data$cur_visit_time, linetype="dashed") +
  geom_vline(xintercept = fig3_data$latest_survival_time, color=GREEN) +
  xlab("Follow-up time (years)")

blank_common = common + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=FONT_SIZE, color=BLUE),
        axis.text.y = element_text(size=FONT_SIZE, color=BLUE))

fig3_A = blank_common + 
  geom_point(aes(x=-50, y=Inf, color='Observed PSA (log scale of ng/mL)')) +
  scale_color_manual(values = BLUE) + 
  geom_point(aes(x=fig3_data$pat_data$year_visit,
                 y=fig3_data$pat_data$log2psaplus1),
             size=NORMAL_POINT_SIZE, color=BLUE) +
  geom_line(aes(x=psa_predict_times[psa_predict_times<=fig3_data$cur_visit_time], 
                y=mean_psa[psa_predict_times<=fig3_data$cur_visit_time]), color=BLUE) + 
  scale_y_continuous(breaks = A_y_breaks, labels = round(A_y_breaks,1)) +
  ylab("PSA (log scale)\nvalue")

fig3_B = blank_common + 
  geom_line(aes(x=psa_predict_times[psa_predict_times<=fig3_data$cur_visit_time], 
                y=mean_psa_velocity[psa_predict_times<=fig3_data$cur_visit_time]), 
            color=BLUE) + 
  scale_y_continuous(breaks = B_y_breaks, labels = round(B_y_breaks,1)) +
  ylab('PSA (log scale)\nInstantaneous\nvelocity')

fig3_C = common + geom_line(aes(x=survival_predict_times, y=mean_cum_risk), color=RED) +
  geom_ribbon(aes(x=survival_predict_times, ymin=lower_cum_risk,
                  ymax=upper_cum_risk), alpha=0.15, fill=RED) + 
  scale_y_continuous(breaks = seq(0,1, 0.5), 
                     labels = c("0%", "50%", "100%"), 
                     limits = c(0,1)) + ylab("Cause-specific\ncumulative\nupgrading-risk (%)") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(color=RED, size=FONT_SIZE),
        axis.text.y = element_text(color=RED, size=FONT_SIZE))

fig3_D = common +
  geom_label(aes(x=c(0,1,fig3_data$cur_visit_time), y=c(0,0,0), 
                 label = c("Start AS\nGleason\ngrade 1", 
                           "Biopsy\nGleason\ngrade 1",
                           "Current\nVisit")), color=WHITE,
             size=SMALL_LABEL_SIZE, nudge_x = c(-0.1, 0, 0),
             fill=c(ORANGE, GREEN, BLACK)) +
  theme(text = element_text(size = FONT_SIZE),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank()) + 
  ylim(-0.25,0.25)

figure3 = ggarrange(fig3_A, fig3_B, fig3_C, fig3_D, 
                    ncol=1, nrow=4, align = "v",
                    heights = c(1,1,1.2,0.6), common.legend = T,
                    hjust=-7.5, vjust = 2,
                    legend = "bottom", labels = c("A","B", "C", ""))

saveFigure(figure3, filename = "latex/contents/c5/images/c5_fig3.pdf",
           size = FULL_PLOT_SIZE)

###############
# Figure 4
###############
reset()

cohort_labpos_x = as.numeric(by(npmle_plotdf_all$Cohort, data = npmle_plotdf_all$timePoints, max))
cohort_labpos_y = as.numeric(by(npmle_plotdf_all$Cohort, data = npmle_plotdf_all$riskProbs, max))

figure4_base = baseggplot + 
  geom_line(aes(x=npmle_plotdf_all$timePoints, 
                y=npmle_plotdf_all$riskProbs, 
                group=npmle_plotdf_all$Cohort, 
                color=npmle_plotdf_all$Cohort)) +  
  geom_label(aes(x=cohort_labpos_x, y=cohort_labpos_y, 
                 label=cohortnames, fill=cohortnames), 
             color=WHITE, size=SMALL_LABEL_SIZE)+
  scale_color_manual(values=colormap)+
  scale_fill_manual(values=colormap)+
  scale_x_continuous(breaks=c(0,2,4,6,8), limits = c(0,9)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25), 
                     labels = paste0(seq(0, 1, 0.25)*100, "%"),
                     limits = c(0,1)) + 
  theme(legend.position = "none")+
  ylab("Cause-specific cumulative upgrading-risk (%)") +
  xlab("Follow-up time (years)")

fig4_auc_recalib = baseggplot + 
  geom_line(data=fig4_data$auc_recalibdf, 
            aes(x=t_horiz, y=mean, group=cohort, color=cohort)) + 
  geom_point(aes(x=2, y=0.5991967), size=3, color=colormap["MUSIC"]) +
  scale_color_manual(values = colormap)+
  scale_x_continuous(breaks = seq(1,9, by=1), limits = c(2,9.1)) + 
  theme(legend.title = element_blank()) +
  ylab("AUC (higher is better)") + xlab("Follow-up time (years)") +
  ylim(0.5,1)

fig4_before_recalib = figure4_base +
  geom_line(data=fig4_data$calib_df, aes(x=pred_time, y=cum_risk, color=cohort), 
            linetype="dashed") +
  geom_label(aes(x=cohort_labpos_x, y=cohort_labpos_y, 
                 label=cohortnames, fill=cohortnames), 
             color=WHITE, size=SMALL_LABEL_SIZE)+
  scale_color_manual(values=colormap)

figure4 = ggarrange(fig4_auc_recalib, fig4_before_recalib, 
                    nrow=1, ncol = 2,
                    align = "h", common.legend = T, 
                    legend = "bottom", labels = "AUTO")

saveFigure(figure4, filename = "latex/contents/c5/images/c5_fig4.pdf",
           size = NORMAL_PLOT_SIZE)

###############
# Figure 5
###############
reset()

survival_predict_times = fig3_data$survival_predict_times
mean_cum_risk = 1-c(1, rowMeans(fig3_data$exp_fut$predicted_surv_prob))
lower_cum_risk = 1-c(1, apply(fig3_data$exp_fut$predicted_surv_prob, 1, quantile, probs=0.025))
upper_cum_risk = 1-c(1, apply(fig3_data$exp_fut$predicted_surv_prob, 1, quantile, probs=0.975))

common = baseggplot +  
  scale_x_continuous(breaks = 0:MAX_FOLLOW_UP,
                     limits = c(-0.3, MAX_FOLLOW_UP)) +
  geom_vline(xintercept = fig3_data$cur_visit_time, linetype="dashed") +
  geom_vline(xintercept = fig3_data$latest_survival_time, color=GREEN) +
  xlab("Follow-up time (years)")

fig5_A = common + geom_line(aes(x=survival_predict_times, 
                                y=mean_cum_risk), color=RED) +
  geom_ribbon(aes(x=survival_predict_times, ymin=lower_cum_risk,
                  ymax=upper_cum_risk), alpha=0.15, fill=RED) + 
  scale_y_continuous(breaks = seq(0,1, 0.5), 
                     labels = c("0%", "50%", "100%"), 
                     limits = c(0,1)) + ylab("Cause-specific\ncumulative\nupgrading-risk (%)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(color=RED, size=FONT_SIZE),
        axis.text.y = element_text(color=RED, size=FONT_SIZE))

fig5_B = common +
  geom_line(data = fig5_schedule_df, aes(x=biopsy_times, 
                                         y=as.numeric(fig5_schedule_df$Schedule),
                                         group=as.numeric(fig5_schedule_df$Schedule)),
            linetype='dotted')+
  geom_label(data = fig5_schedule_df, 
             aes(x=biopsy_times, y=as.numeric(fig5_schedule_df$Schedule)),
             label="B",size=SMALL_LABEL_SIZE, fill=RED, color=WHITE) + 
  ylab("Biopsy schedule") + 
  theme(axis.title.x = element_blank())+
  scale_y_continuous(breaks = 1:length(levels(fig5_schedule_df$Schedule)),
                     labels = levels(fig5_schedule_df$Schedule),
                     limits = c(0.5, length(levels(fig5_schedule_df$Schedule)) + 0.5)) 

fig5_C = common +
  geom_label(aes(x=c(0,1,fig3_data$cur_visit_time), y=c(0,0,0), 
                 label = c("Start AS\nGleason\ngrade 1", 
                           "Biopsy\nGleason\ngrade 1",
                           "Current\nVisit")), color=WHITE,
             size=SMALL_LABEL_SIZE, nudge_x = c(-0.1, 0, 0),
             fill=c(ORANGE, GREEN, BLACK)) +
  theme(text = element_text(size = FONT_SIZE),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank()) + 
  ylim(-0.25,0.25)

consequences_df = fig5_schedule_df[!duplicated(fig5_schedule_df$Schedule),]
max_delay_limit = 3
fig5_D = baseggplot + geom_col(aes(x=rep(consequences_df$Schedule,2), 
                                   y=c(consequences_df$expected_detection_delay, 
                                       max_delay_limit - consequences_df$expected_detection_delay)),
                               color=BLACK, fill=c(rep(c(GREY,WHITE), nrow(consequences_df))),
                               width=0.5)+
  ylab("Expected time delay (years) in detecting upgrading") + 
  xlab("Biopsy Schedule") +
  scale_y_continuous(breaks = seq(0, max_delay_limit, by = 0.5),
                     labels = seq(0, max_delay_limit,  by = 0.5),
                     limits= c(0, max_delay_limit))+
  coord_flip()

figure5 = ggarrange(fig5_A, fig5_B, fig5_C, fig5_D, 
                    ncol=1, nrow=4, align = "v",
                    labels=c("A", "B", "", "C"), 
                    heights = c(0.8,1,0.5,1))

saveFigure(figure5, filename = "latex/contents/c5/images/c5_fig5.pdf",
           size = FULL_PLOT_SIZE)

###############
# Figure 1app
###############
reset()

cohort_labpos_x = as.numeric(by(npmle_plotdf_all$Cohort, data = npmle_plotdf_all$timePoints, max))
cohort_labpos_y = as.numeric(by(npmle_plotdf_all$Cohort, data = npmle_plotdf_all$riskProbs, max))

figure1app = baseggplot + 
  geom_line(aes(x=npmle_plotdf_all$timePoints, 
                y=npmle_plotdf_all$riskProbs, 
                group=npmle_plotdf_all$Cohort, 
                color=npmle_plotdf_all$Cohort)) +  
  geom_label(aes(x=cohort_labpos_x, y=cohort_labpos_y, 
                 label=cohortnames, fill=cohortnames), 
             color=WHITE, size=SMALL_LABEL_SIZE)+
  scale_color_manual(values=colormap)+
  scale_fill_manual(values=colormap)+
  scale_x_continuous(breaks=c(0,2,4,6,8), limits = c(0,9)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25), 
                     labels = paste0(seq(0, 1, 0.25)*100, "%"),
                     limits = c(0,1)) + 
  theme(legend.position = "none")+
  ylab("Cause-specific cumulative upgrading-risk (%)") +
  xlab("Follow-up time (years)")

saveFigure(figure1app, filename = "latex/contents/c5/images/c5_fig_app1.pdf",
           size = NORMAL_PLOT_SIZE)

###############
# Figure 2app
###############
reset()
cohort_labpos_x = as.numeric(by(npmle_plotdf_all$Cohort, data = npmle_plotdf_all$timePoints, max))
cohort_labpos_y = as.numeric(by(npmle_plotdf_all$Cohort, data = npmle_plotdf_all$riskProbs, max))

figure2app_base = baseggplot + 
  geom_line(aes(x=npmle_plotdf_all$timePoints, 
                y=npmle_plotdf_all$riskProbs, 
                group=npmle_plotdf_all$Cohort, 
                color=npmle_plotdf_all$Cohort)) +  
  geom_label(aes(x=cohort_labpos_x, y=cohort_labpos_y, 
                 label=cohortnames, fill=cohortnames), 
             color=WHITE, size=SMALL_LABEL_SIZE)+
  scale_color_manual(values=colormap)+
  scale_fill_manual(values=colormap)+
  scale_x_continuous(breaks=c(0,2,4,6,8), limits = c(0,9)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25), 
                     labels = paste0(seq(0, 1, 0.25)*100, "%"),
                     limits = c(0,1)) + 
  theme(legend.position = "none")+
  ylab("Cause-specific cumulative upgrading-risk (%)") +
  xlab("Follow-up time (years)")

fig2app_before_recalib = figure2app_base +
  geom_line(data=fig4_data$calib_df, aes(x=pred_time, y=cum_risk, color=cohort), 
            linetype="dashed") +
  geom_label(aes(x=cohort_labpos_x, y=cohort_labpos_y, 
                 label=cohortnames, fill=cohortnames), 
             color=WHITE, size=SMALL_LABEL_SIZE)+
  scale_color_manual(values=colormap) + ggtitle("Before recalibration")

fig2app_after_recalib = figure2app_base +
  geom_line(data=fig4_data$recalib_df, aes(x=pred_time, y=cum_risk, color=cohort), 
            linetype="dashed") +
  geom_label(aes(x=cohort_labpos_x, y=cohort_labpos_y, 
                 label=cohortnames, fill=cohortnames), 
             color=WHITE, size=SMALL_LABEL_SIZE)+
  theme(axis.text.y = element_blank(), axis.title.y=element_blank())+
  scale_color_manual(values=colormap) + ggtitle("    After recalibration")

figure2app =  ggarrange(fig2app_before_recalib, fig2app_after_recalib,
                        nrow=1, ncol = 2,
                        align = "h", widths = c(1.25,1),
                        common.legend = F, legend = "none", labels = "AUTO")

saveFigure(figure2app, filename = "latex/contents/c5/images/c5_fig_app2.pdf",
           size = NORMAL_PLOT_SIZE)


###############
# Figure 3app
###############
reset()
figure3app = baseggplot + 
  geom_boxplot(data=calib_in_small_plotdf,
               aes(y=diff,x=cohort), outlier.shape = NA) +
  scale_y_continuous(breaks = seq(-0.5,0.5,by=0.1), 
                     limits = c(-0.5,0.5),
                     labels = paste0(seq(-0.5,0.5,by = 0.1)*100, "%")) + 
  xlab("Cohort") + ylab("Difference in predicted\ncumulative-risk")
saveFigure(figure3app, filename = "latex/contents/c5/images/c5_fig_app3.pdf",
           size = NORMAL_PLOT_SIZE)

###############
# Figure 4app
###############
reset()

fig4app_auc_recalib = baseggplot + 
  geom_line(data=fig4_data$auc_recalibdf, 
            aes(x=t_horiz, y=mean, group=cohort, color=cohort)) + 
  geom_point(aes(x=2, y=0.5991967), size=3, color=colormap["MUSIC"]) +
  scale_color_manual(values = colormap)+
  scale_x_continuous(breaks = seq(1,9, by=1), limits = c(2,9.1)) + 
  theme(legend.title = element_blank()) +
  ylab("AUC (higher is better)") + xlab("Follow-up time (years)") +
  ylim(0.5,1)

fig4app_mapeplot = baseggplot + 
  geom_line(data=fig4_data$pe_df,
            aes(x=t_horiz, y=mean_mape, group=cohort, color=cohort)) + 
  geom_point(aes(x=2, y=0.3309), size=3, color=colormap["MUSIC"]) +
  scale_color_manual(values = colormap)+
  scale_y_continuous(breaks = seq(0,1,0.25), limits=c(0,1)) + 
  scale_x_continuous(breaks = seq(1,9, by=1), limits = c(2,9.1)) + 
  theme(legend.title = element_blank()) +
  ylab("MAPE (lower is better)") + xlab("Follow-up time (years)")

figure4app = ggarrange(fig4app_auc_recalib, fig4app_mapeplot, nrow=1, ncol = 2, 
                       common.legend = T, legend = "bottom",labels = "AUTO")

saveFigure(figure4app, filename = "latex/contents/c5/images/c5_fig_app4.pdf",
           size = NORMAL_PLOT_SIZE)
