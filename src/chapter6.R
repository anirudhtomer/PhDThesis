library(ggplot2)
library(ggpubr)
library(splines)

reset = function(){
  parent_env = parent.env(environment())
  rm(list = setdiff(ls(name=parent_env), "reset"), envir = parent_env)
  load("Rdata/colors.Rdata", envir = parent_env)
  load("Rdata/shapes.Rdata", envir = parent_env)
  load("Rdata/chapter6/sim_res.Rdata", envir = parent_env)
  load("Rdata/chapter6/fitted_jm.Rdata", envir = parent_env)
  
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
    theme(axis.text = element_text(size=FONT_SIZE),
          axis.title = element_text(size=FONT_SIZE),
          plot.title = element_text(size=FONT_SIZE),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=FONT_SIZE-2))
  
  assign("baseggplot", baseggplot, envir = parent_env)
}

########################################
#Figure 1
########################################
reset()
set.seed(2019)
long_time = seq(0,2, length.out = 10)
true_long_marker = long_time^(1/3) 
obs_long_marker = pmax(0, true_long_marker +  rnorm(n = length(long_time), sd = 0.5))
risk_time = seq(2,4, length.out = 10)

info_time = seq(2, 3.222, length.out = 10)
info = sin(info_time-1.25)
max_info = max(info)
max_info_time = info_time[which.max(info)]

max_y = 1.5

fig1a = baseggplot + 
  geom_vline(xintercept = 2, size=MEDIUM_LINE) +
  geom_point(aes(x=long_time, y=obs_long_marker), 
             size=NORMAL_POINT_SIZE, color=BLUE) +
  geom_line(aes(x=long_time, y=true_long_marker), color=BLUE) + 
  geom_line(aes(x=risk_time, y=((risk_time-2)^2)/10), color=RED) +
  geom_segment(aes(x=2, y=max_y/10, xend=Inf, yend=max_y/10), 
               color=RED,linetype='dotted') + 
  
  geom_vline(xintercept = 3.222, linetype='dashed') +
  ylab("NT-proBNP (pmol/L)") + xlab("Follow-up time (years)") +
  scale_x_continuous(breaks = c(0, 2, 3.222), labels = c(0, 2, 3.2), limits = c(0,4)) + 
  scale_y_continuous(limits = c(0,max_y),
                     sec.axis = dup_axis(name = "Cumulative-risk of PE (%)",
                                         breaks = c(max_y/10, seq(0,max_y, length.out = 3)),
                                         labels = paste0(c(10, seq(0,100, by = 50)), "%"))) + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.left = element_text(color = BLUE, size=FONT_SIZE),
        axis.title.y.right = element_text(color = RED, size=FONT_SIZE),
        axis.text.y.right = element_text(color = RED, size=FONT_SIZE))

fig1b = baseggplot + 
  geom_vline(xintercept = 2, size=MEDIUM_LINE) +
  geom_point(aes(x=long_time, y=obs_long_marker), 
             size=NORMAL_POINT_SIZE, color=BLUE) +
  geom_line(aes(x=long_time, y=true_long_marker), color=BLUE) + 
  geom_line(aes(x=info_time, y=info), color=GREEN) +
  geom_segment(aes(x=max_info_time, y=0, xend=max_info_time, yend=max_info + 0.25),
               color=GREEN, linetype='dotted') + 
  geom_vline(xintercept = 3.222, linetype='dashed') +
  geom_label(aes(x=max_info_time, y=max_info + 0.25, 
                 label="Time of maximum\nexpected information"), 
             fill=GREEN, color='white', size=SMALL_LABEL_SIZE)+
  ylab("NT-proBNP (pmol/L)") + xlab("Follow-up time (years)") +
  scale_x_continuous(breaks = c(0, 2, max_info_time, 3.222),
                     labels = c(0, 2, round(max_info_time,1), 3.2), limits = c(0,4)) + 
  scale_y_continuous(limits = c(0,max_y),
                     sec.axis = dup_axis(name = "Expected information gain\nregarding PE")) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(color = BLUE, size=FONT_SIZE),
        axis.title.y.right = element_text(color = GREEN, size=FONT_SIZE),
        axis.text.y.right = element_blank())


fig1c =  baseggplot +
  scale_x_continuous(limits = c(0,4)) + 
  xlab("Follow-up time (years)") +
  geom_label(aes(x=c(0,2,3.222), y=c(0,0,0),
                 label = c("Start\nsurveillance",
                           "Current visit\n(PE not observed)",
                           "Time of maximum\nacceptable risk (10%)")),
             color='white', nudge_x = c(0.15,0,0),
             size= SMALL_LABEL_SIZE,
             fill=c(ORANGE, BLACK, RED)) +
  theme(text = element_text(size = FONT_SIZE),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank()) +
  ylim(-0.25,0.25)

figure_1 = ggarrange(fig1a, fig1b, fig1c, 
                     align = "v", nrow = 3, ncol=1,
                     heights = c(0.95,1,0.35),
                     labels = c("A", "B", ""))
print(figure_1)

saveFigure(figure_1, filename = "latex/contents/c6/images/c6_fig1.pdf",
           size = NORMAL_PLOT_SIZE)

########################################
#Figure 2
########################################
reset()

figure_2a = baseggplot +
  geom_boxplot(data=sim_res[sim_res$type=='n_measurements' & sim_res$threshold==7.5,],
               aes(x=Schedule, ymin = ymin, lower = lower, 
                   middle = middle, upper = upper, ymax = ymax), stat = "identity") + 
  ylim(0, 15) + ylab("Number of measurements")

figure_2b = baseggplot +
  geom_hline(yintercept = 0, linetype='dashed', color=RED) + 
  geom_boxplot(data=sim_res[sim_res$type=='high_risk_interval' & sim_res$threshold==7.5,],
               aes(x=Schedule, ymin = ymin, lower = lower, 
                   middle = middle, upper = upper, ymax = ymax), stat = "identity") + 
  ylab("High-risk interval (months)") +
  scale_y_continuous(breaks = seq(-20,5, by = 5), limits = c(-21,5))

figure_2 = ggpubr::ggarrange(figure_2a, figure_2b, nrow=1, ncol=2, labels = "AUTO")

saveFigure(figure_2, filename = "latex/contents/c6/images/c6_fig2.pdf",
           size = NORMAL_PLOT_SIZE)

########################################
#Figure 1 Appendix
########################################
reset()

figure_1app_a = baseggplot +
  geom_boxplot(data=sim_res[sim_res$type=='n_measurements' & sim_res$threshold==5,],
               aes(x=Schedule, ymin = ymin, lower = lower, 
                   middle = middle, upper = upper, ymax = ymax), stat = "identity") + 
  ylim(0, 15) + ylab("Number of measurements")

figure_1app_b = baseggplot +
  geom_hline(yintercept = 0, linetype='dashed', color=RED) + 
  geom_boxplot(data=sim_res[sim_res$type=='high_risk_interval' & sim_res$threshold==5,],
               aes(x=Schedule, ymin = ymin, lower = lower, 
                   middle = middle, upper = upper, ymax = ymax), stat = "identity") + 
  ylab("High-risk interval (months)") +
  scale_y_continuous(breaks = seq(-20,5, by = 5), limits = c(-25,5))

figure_1app = ggpubr::ggarrange(figure_1app_a, figure_1app_b, nrow=1, ncol=2, labels = "AUTO")

saveFigure(figure_1app, filename = "latex/contents/c6/images/c6_fig_app1.pdf",
           size = NORMAL_PLOT_SIZE)

########################################
#Figure 3 Appendix
########################################
reset()

figure_2app_a = baseggplot +
  geom_boxplot(data=sim_res[sim_res$type=='n_measurements' & sim_res$threshold==10,],
               aes(x=Schedule, ymin = ymin, lower = lower, 
                   middle = middle, upper = upper, ymax = ymax), stat = "identity") + 
  ylim(0, 15) + ylab("Number of measurements")

figure_2app_b = baseggplot +
  geom_hline(yintercept = 0, linetype='dashed', color=RED) + 
  geom_boxplot(data=sim_res[sim_res$type=='high_risk_interval' & sim_res$threshold==10,],
               aes(x=Schedule, ymin = ymin, lower = lower, 
                   middle = middle, upper = upper, ymax = ymax), stat = "identity") + 
  ylab("High-risk interval (months)") +
  scale_y_continuous(breaks = seq(-20,5, by = 5), limits = c(-25,5))

figure_2app = ggpubr::ggarrange(figure_2app_a, figure_2app_b, nrow=1, ncol=2, labels = "AUTO")

saveFigure(figure_2app, filename = "latex/contents/c6/images/c6_fig_app2.pdf",
           size = NORMAL_PLOT_SIZE)
