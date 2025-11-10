# load libraries
library(data.table)
library(ggpubr)
library(ggthemes)
library(tidyverse)
library(stringr)
library(cowplot)
source(file.path("my_theme.R"))

# assign experiment label
exp <- 'multi'

# load data
avg_multi <- fread('data/exp-multi_avg.csv')
avg_multi_subRT <- fread('data/exp-multi_avg_by_subRT.csv', stringsAsFactors = T)
avg_multi_memgrp <- fread('data/exp-multi_avg_by_memgrp.csv', stringsAsFactors = T)
avg_multi_memgrp_subRT <- fread('data/exp-multi_avg_by_memgrp_by_subRT.csv', stringsAsFactors = T)
avg_multi_mts <- fread('data/exp-multi_mts_avg.csv')

# change factor level names
avg_multi[, switch := ifelse(switch==0, 'non-switch', 'switch')]
avg_multi[, train_type := ifelse(train_type==1,'stable', 'variable')]
avg_multi[, ses := ifelse(ses==2, 'training', 'testing')]
avg_multi$multi_cond <- factor(avg_multi$multi_cond , 
                               levels = c('none', 
                                          'neither',
                                          'other'))

avg_multi_subRT[, switch := ifelse(switch==0, 'non-switch', 'switch')]
avg_multi_subRT[, train_type := ifelse(train_type==1, 'variable', 'stable')]
avg_multi_subRT[, ses := ifelse(ses==2, 'training', 'testing')]
avg_multi_subRT$multi_cond <- factor(avg_multi_subRT$multi_cond , 
                                            levels = c('none', 
                                                       'neither',
                                                       'other'))


avg_multi_memgrp[, switch := ifelse(switch==0, 'non-switch', 'switch')]
avg_multi_memgrp[, train_type := ifelse(train_type==1, 'variable', 'stable')]
avg_multi_memgrp[, ses := ifelse(ses==2, 'training', 'testing')]

avg_multi_memgrp_subRT[, switch := ifelse(switch==0, 'non-switch', 'switch')]
avg_multi_memgrp_subRT[, train_type := ifelse(train_type==1, 'variable', 'stable')]
avg_multi_memgrp_subRT[, ses := ifelse(ses==2, 'training', 'testing')]

avg_multi_mts <- avg_multi_mts[unique(avg_multi[, .(sub, train_type)]), on='sub']
avg_multi_mts[, cond := ifelse(cond=='nc', 'neither', 'other')]
avg_multi_mts[, stage := ifelse(stage=='3', 'testing', 'initial')]
setnames(avg_multi_mts, 'stage', 'ses') 

### Test Session #### ==========================================================
### Plot accuracy and error rates ----------------------------------------------
avg_multi_gav_tst_acc <- avg_multi[ses=='testing',
                                   .(Accuracy=mean(accuracy_mean),
                                     SettingErrors=mean(setting_errors_mean),
                                     GeneralErrors=mean(general_errors_mean)),
                                   by=c('train_type', 'multi_trial', 'multi_cond')]

# plot_tst_acc <- ggplot() +
#   geom_point(data=avg_multi[ses=='testing'],
#              aes(x=multi_cond,
#                  y=accuracy_mean,
#                  color=train_type),
#              alpha=0.3,
#              position=position_jitterdodge(jitter.width = 0.1,
#                                            dodge.width = 0.5)) +
#   geom_line(data=avg_multi_gav_tst_acc,
#             aes(x=multi_cond, 
#                 y=Accuracy, 
#                 color=train_type,
#                 group=interaction(multi_trial, train_type)),
#             linetype=2, 
#             position=position_dodge(width = 0.5)) +
#   geom_point(data=avg_multi_gav_tst_acc,
#              aes(x=multi_cond, 
#                  y=Accuracy, 
#                  color=train_type),
#              fill='white',
#              alpha=1,
#              size=3,
#              stroke=1.25,
#              shape=23,
#              position=position_dodge(width = 0.5)) +
#   facet_wrap('multi_trial') +
#   
#   # customise
#   scale_color_tableau() +
#   scale_y_continuous(name='Accuracy (%)',
#                      breaks=c(0.9, 0.95, 1.0),
#                      labels=c('90', '95', '100')) +
#   scale_x_discrete(name='Multitasking Condition') +
#   geom_rangeframe(data=data.frame(x=c(1, 3), y=c(0.9, 1)),
#                   aes(x, y), size=1, color='black') +
#   my_theme() 

plot_tst_seterr <- ggplot() +
  geom_point(data=avg_multi[ses=='testing'],
             aes(x=multi_cond,
                 y=setting_errors_mean,
                 color=train_type),
             alpha=0.3,
             position=position_jitterdodge(jitter.width = 0.1,
                                           dodge.width = 0.5)) +
  geom_line(data=avg_multi_gav_tst_acc,
            aes(x=multi_cond, 
                y=SettingErrors, 
                color=train_type,
                group=interaction(multi_trial, train_type)),
            linetype=2, 
            position=position_dodge(width = 0.5)) +
  geom_point(data=avg_multi_gav_tst_acc,
             aes(x=multi_cond, 
                 y=SettingErrors, 
                 color=train_type),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23,
             position=position_dodge(width = 0.5)) +
  facet_wrap('multi_trial') +
  
  # customise
  scale_color_tableau() +
  scale_y_continuous(name='Setting Errors (%)',
                     breaks=c(0, 0.05, 0.1),
                     labels=c('0', '5', '10')) +
  scale_x_discrete(name='Multitasking Condition',
                   labels=c('s', 'm-n', 'm-o')) +
  geom_rangeframe(data=data.frame(x=c(1, 3), y=c(0, 0.1)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = c(0.8, 0.5))


plot_tst_generr <- ggplot() +
  geom_point(data=avg_multi[ses=='testing'],
             aes(x=multi_cond,
                 y=general_errors_mean,
                 color=train_type),
             alpha=0.3,
             position=position_jitterdodge(jitter.width = 0.1,
                                           dodge.width = 0.5)) +
  geom_line(data=avg_multi_gav_tst_acc,
            aes(x=multi_cond, 
                y=GeneralErrors, 
                color=train_type,
                group=interaction(multi_trial, train_type)),
            linetype=2, 
            position=position_dodge(width = 0.5)) +
  geom_point(data=avg_multi_gav_tst_acc,
             aes(x=multi_cond, 
                 y=GeneralErrors, 
                 color=train_type),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23,
             position=position_dodge(width = 0.5)) +
  facet_wrap('multi_trial') +
  
  # customise
  scale_color_tableau() +
  scale_y_continuous(name='General Errors (%)',
                     breaks=c(0, 0.05, 0.1),
                     labels=c('0', '5', '10')) +
  scale_x_discrete(name='Multitasking Condition',
                   labels=c('s', 'm-n', 'm-o')) +
  geom_rangeframe(data=data.frame(x=c(1, 3), y=c(0, 0.1)),
                  aes(x, y), size=1, color='black') +
  my_theme() 

fig_tst_acc <- plot_grid(#plot_tst_acc, 
                     plot_tst_seterr, 
                     plot_tst_generr,
                     ncol=3, nrow=1,
                     axis='tlbr',
                     rel_heights=c(1, 1),
                     align="v")

svg('figs/fig_tst_acc.svg',
    width=15, height=3)
plot(fig_tst_acc)
dev.off()

### Plot RTs -------------------------------------------------------------------
avg_multi_subRT_melt <- melt(avg_multi_subRT[ses=='testing'], 
                             id.vars=c('sub', 'ses', 'multi_trial', 'multi_cond', 'train_type'), # train_type
                             measure.vars=c('rt_first_correct_mean',
                                            'rt_subs_correct_1_mean',
                                            'rt_subs_correct_2_mean',
                                            'rt_subs_correct_3_mean'),
                             variable.name='door_selection',
                             value.name='rt_correct_mean')

avg_multi_gav_tst_rt <- avg_multi_subRT_melt[, .(RT=mean(rt_correct_mean)), 
                                                 by=c('multi_trial', 
                                                      'multi_cond',
                                                      'door_selection')] # train_type

plot_tst_rt <- ggplot() +
  geom_point(data=avg_multi_subRT_melt,
             aes(x=door_selection,
                 y=rt_correct_mean,
                 color=multi_cond),
             alpha=0.3,
             position=position_jitterdodge(jitter.width = 0.1,
                                           dodge.width = 0.5)) +
  geom_line(data=avg_multi_gav_tst_rt,
            aes(x=door_selection, 
                y=RT, 
                color=multi_cond,
                group=interaction(multi_trial, multi_cond)),
            linetype=2, 
            position=position_dodge(width = 0.5)) +
  geom_point(data=avg_multi_gav_tst_rt,
             aes(x=door_selection, 
                 y=RT, 
                 color=multi_cond),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23,
             position=position_dodge(width = 0.5)) +
  facet_wrap('multi_trial') +
  
  # customise
  #scale_color_tableau() +
  scale_colour_manual(values=c('#a2b627', '#1BA3C6','#ce69be')) + 
  scale_y_continuous(name='Reaction Time (s)',
                     breaks=c(0, 0.5, 1, 1.5, 2),
                     labels=c('0.0', '0.5', '1.0', '1.5', '2.0')) +
  scale_x_discrete(name='Door Selection',
                   labels=c('first', 'second', 'third', 'fourth')) +
  geom_rangeframe(data=data.frame(x=c(1, 4), y=c(0, 2)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = c(0.8, 0.8))



svg('figs/fig_tst_rt.svg',
    width=8, height=4.5)
plot(plot_tst_rt)
dev.off()

### DVs by Search Context ------------------------------------------------------
avg_multi_memgrp <- avg_multi_memgrp[ses=='testing', 
                         .(accuracy_mean=mean(accuracy_mean, na.rm=T),
                           setting_errors_mean=mean(setting_errors_mean, na.rm=T),
                           general_errors_mean=mean(general_errors_mean, na.rm=T),
                           rt_first_correct_mean=mean(rt_first_correct_mean, na.rm=T),
                           rt_subs_correct_mean=mean(rt_subs_correct_mean, na.rm=T)),
                         by=c('train_type', 'multi_trial', 'mem_group', 'sub')]

avg_multi_gav_tst_memgrp <- avg_multi_memgrp[,
                         .(accuracy_mean=mean(accuracy_mean, na.rm=T),
                           setting_errors_mean=mean(setting_errors_mean, na.rm=T),
                           general_errors_mean=mean(general_errors_mean, na.rm=T),
                           rt_first_correct_mean=mean(rt_first_correct_mean, na.rm=T),
                           rt_subs_correct_mean=mean(rt_subs_correct_mean, na.rm=T)),
                         by=c('train_type', 'multi_trial', 'mem_group')]


# plot_tst_acc_memgrp <- ggplot() +
#   geom_point(data=avg_multi_memgrp,
#              aes(x=multi_trial,
#                  y=accuracy_mean,
#                  color=train_type),
#              alpha=0.3,
#              position=position_jitterdodge(jitter.width = 0.1,
#                                            dodge.width = 0.5)) +
#   geom_line(data=avg_multi_gav_tst_memgrp,
#             aes(x=multi_trial, 
#                 y=accuracy_mean, 
#                 color=train_type,
#                 group=interaction(mem_group, train_type)),
#             linetype=2, 
#             position=position_dodge(width = 0.5)) +
#   geom_point(data=avg_multi_gav_tst_memgrp,
#              aes(x=multi_trial, 
#                  y=accuracy_mean, 
#                  color=train_type),
#              fill='white',
#              alpha=1,
#              size=3,
#              stroke=1.25,
#              shape=23,
#              position=position_dodge(width = 0.5)) +
#   facet_wrap('mem_group') +
#   
#   # customise
#   scale_color_tableau() +
#   scale_y_continuous(name='Accuracy (%)',
#                      breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
#                      labels=c('0', '20', '40', '60', '80', '100')) +
#   scale_x_discrete(name='Multitasking Trial',
#                    labels=c('first', 'subs')) +
#   geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0, 1)),
#                   aes(x, y), size=1, color='black') +
#   my_theme() 

plot_tst_seterr_memgrp <- ggplot() +
  geom_point(data=avg_multi_memgrp,
             aes(x=multi_trial,
                 y=setting_errors_mean,
                 color=train_type),
             alpha=0.3,
             position=position_jitterdodge(jitter.width = 0.1,
                                           dodge.width = 0.5)) +
  geom_line(data=avg_multi_gav_tst_memgrp,
            aes(x=multi_trial, 
                y=setting_errors_mean, 
                color=train_type,
                group=interaction(mem_group, train_type)),
            linetype=2, 
            position=position_dodge(width = 0.5)) +
  geom_point(data=avg_multi_gav_tst_memgrp,
             aes(x=multi_trial, 
                 y=setting_errors_mean, 
                 color=train_type),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23,
             position=position_dodge(width = 0.5)) +
  facet_wrap('mem_group') +
  
  # customise
  scale_color_tableau() +
  scale_y_continuous(name='Setting Errors (%)',
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8),
                     labels=c('0', '20', '40', '60', '80')) +
  scale_x_discrete(name='Multitasking Trial',
                   labels=c('first', 'subs')) +
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0, 0.8)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = c(0.8, 0.5))


plot_tst_generr_memgrp <- ggplot() +
  geom_point(data=avg_multi_memgrp,
             aes(x=multi_trial,
                 y=general_errors_mean,
                 color=train_type),
             alpha=0.3,
             position=position_jitterdodge(jitter.width = 0.1,
                                           dodge.width = 0.5)) +
  geom_line(data=avg_multi_gav_tst_memgrp,
            aes(x=multi_trial, 
                y=general_errors_mean, 
                color=train_type,
                group=interaction(mem_group, train_type)),
            linetype=2, 
            position=position_dodge(width = 0.5)) +
  geom_point(data=avg_multi_gav_tst_memgrp,
             aes(x=multi_trial, 
                 y=general_errors_mean, 
                 color=train_type),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23,
             position=position_dodge(width = 0.5)) +
  facet_wrap('mem_group') +
  
  # customise
  scale_color_tableau() +
  scale_y_continuous(name='General Errors (%)',
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8),
                     labels=c('0', '20', '40', '60', '80')) +
  scale_x_discrete(name='Multitasking Trial',
                   labels=c('first', 'subs')) +
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0, 0.8)),
                  aes(x, y), size=1, color='black') +
  my_theme() 

fig_tst_acc_memgrp <- plot_grid(#plot_tst_acc_memgrp, 
                                plot_tst_seterr_memgrp, 
                                plot_tst_generr_memgrp,
                                ncol=3, nrow=1,
                                axis='tlbr',
                                rel_heights=c(1, 1),
                                align="v")

svg('figs/fig_tst_acc_searchcontext.svg',
    width=15, height=3)
plot(fig_tst_acc_memgrp)
dev.off()

plot_tst_rt_fc_memgrp <- ggplot() +
  geom_point(data=avg_multi_memgrp,
             aes(x=multi_trial,
                 y=rt_first_correct_mean,
                 color=train_type),
             alpha=0.3,
             position=position_jitterdodge(jitter.width = 0.1,
                                           dodge.width = 0.5)) +
  geom_line(data=avg_multi_gav_tst_memgrp,
            aes(x=multi_trial, 
                y=rt_first_correct_mean, 
                color=train_type,
                group=interaction(mem_group, train_type)),
            linetype=2, 
            position=position_dodge(width = 0.5)) +
  geom_point(data=avg_multi_gav_tst_memgrp,
             aes(x=multi_trial, 
                 y=rt_first_correct_mean, 
                 color=train_type),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23,
             position=position_dodge(width = 0.5)) +
  facet_wrap('mem_group') +
  
  # customise
  scale_color_tableau() +
  scale_y_continuous(name='Reaction Time (s)',
                     breaks=c(0, 0.5, 1, 1.5, 2, 2.5),
                     labels=c('0.0', '0.5', '1.0', '1.5', '2.0', '2.5'),
                     limits=c(0, 2.7)) +
  scale_x_discrete(name='Multitasking Trial',
                   labels=c('first', 'subs')) +
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0, 2.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = c(0.8, 0.5))


plot_tst_rt_sc_memgrp <- ggplot() +
  geom_point(data=avg_multi_memgrp,
             aes(x=multi_trial,
                 y=rt_subs_correct_mean,
                 color=train_type),
             alpha=0.3,
             position=position_jitterdodge(jitter.width = 0.1,
                                           dodge.width = 0.5)) +
  geom_line(data=avg_multi_gav_tst_memgrp,
            aes(x=multi_trial, 
                y=rt_subs_correct_mean, 
                color=train_type,
                group=interaction(mem_group, train_type)),
            linetype=2, 
            position=position_dodge(width = 0.5)) +
  geom_point(data=avg_multi_gav_tst_memgrp,
             aes(x=multi_trial, 
                 y=rt_subs_correct_mean, 
                 color=train_type),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23,
             position=position_dodge(width = 0.5)) +
  facet_wrap('mem_group') +
  
  # customise
  scale_color_tableau() +
  scale_y_continuous(name='Reaction Time (s)',
                     breaks=c(0, 0.5, 1, 1.5, 2, 2.5),
                     labels=c('0.0', '0.5', '1.0', '1.5', '2.0', '2.5'),
                     limits=c(0, 2.7)) +
  scale_x_discrete(name='Multitasking Trial',
                   labels=c('first', 'subs')) +
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0, 2.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() 

fig_tst_rt_memgrp <- plot_grid(plot_tst_rt_fc_memgrp,
                               plot_tst_rt_sc_memgrp,
                               ncol=3, nrow=1,
                               axis='tlbr',
                               rel_heights=c(1, 1),
                               align="v")

svg('figs/fig_tst_rt_searchcontext.svg',
    width=15, height=3)
plot(fig_tst_rt_memgrp)
dev.off()

# avg_multi_memgrp_subRT <- avg_multi_memgrp_subRT[ses=='testing', 
#                                      .(rt_first_correct_mean=mean(rt_first_correct_mean, na.rm=T),
#                                        rt_subs_correct_1_mean=mean(rt_subs_correct_1_mean, na.rm=T),
#                                        rt_subs_correct_2_mean=mean(rt_subs_correct_2_mean, na.rm=T),
#                                        rt_subs_correct_3_mean=mean(rt_subs_correct_3_mean, na.rm=T)),
#                                      by=c('train_type', 'multi_trial', 'mem_group', 'sub')]
# 
# avg_multi_memgrp_subRT_melt <- melt(avg_multi_memgrp_subRT,
#                                     id.vars=c('sub', 'multi_trial', 'mem_group', 'train_type'), # train_type
#                                     measure.vars=c('rt_first_correct_mean',
#                                                    'rt_subs_correct_1_mean',
#                                                    'rt_subs_correct_2_mean',
#                                                    'rt_subs_correct_3_mean'),
#                                     variable.name='door_selection',
#                                     value.name='rt_correct_mean')
# 
# avg_multi_gav_tst_rt_memgrp <- avg_multi_memgrp_subRT_melt[, 
#                                              .(RT=mean(rt_correct_mean, na.rm=T)), 
#                                              by=c('multi_trial', 
#                                                   'train_type',
#                                                   'mem_group',
#                                                   'door_selection')]
# 
# plot_tst_rt_memgrp <- ggplot() +
#   geom_point(data=avg_multi_memgrp_subRT_melt,
#              aes(x=door_selection,
#                  y=rt_correct_mean,
#                  color=train_type),
#              alpha=0.3,
#              position=position_jitterdodge(jitter.width = 0.1,
#                                            dodge.width = 0.5)) +
#   geom_line(data=avg_multi_gav_tst_rt_memgrp,
#             aes(x=door_selection, 
#                 y=RT, 
#                 color=train_type,
#                 group=interaction(multi_trial, train_type)),
#             linetype=2, 
#             position=position_dodge(width = 0.5)) +
#   geom_point(data=avg_multi_gav_tst_rt_memgrp,
#              aes(x=door_selection, 
#                  y=RT, 
#                  color=train_type),
#              fill='white',
#              alpha=1,
#              size=3,
#              stroke=1.25,
#              shape=23,
#              position=position_dodge(width = 0.5)) +
#   facet_grid(rows=vars(mem_group), 
#              cols=vars(multi_trial)) +
#   
#   # customise
#   scale_color_tableau() +
#   #scale_colour_manual(values=c('#a2b627', '#1BA3C6','#ce69be')) + 
#   scale_y_continuous(name='Reaction Time (s)',
#                      breaks=c(0, 0.5, 1, 1.5, 2),
#                      labels=c('0.0', '0.5', '1.0', '1.5', '2.0')) +
#   scale_x_discrete(name='Door Selection',
#                    labels=c('first', 'second', 'third', 'fourth')) +
#   geom_rangeframe(data=data.frame(x=c(1, 4), y=c(0, 2)),
#                   aes(x, y), size=1, color='black') +
#   my_theme() + theme(legend.position = c(0.8, 0.8))


### Training Session #### ======================================================
### Plot accuracy and error rates ----------------------------------------------
avg_multi_gav_trn_acc <- avg_multi[ses=='training' & switch =='non-switch',
                                   .(Accuracy=mean(accuracy_mean),
                                     SettingErrors=mean(setting_errors_mean),
                                     GeneralErrors=mean(general_errors_mean)),
                                   by=c('train_type')]

# plot_trn_acc <- ggplot() +
#   geom_point(data=avg_multi[ses=='training' & switch=='non-switch'],
#              aes(x=train_type,
#                  y=accuracy_mean,
#                  color=train_type),
#              alpha=0.3,
#              position=position_jitter(width = 0.1)) +
#   geom_point(data=avg_multi_gav_trn_acc,
#              aes(x=train_type, 
#                  y=Accuracy, 
#                  color=train_type),
#              fill='white',
#              alpha=1,
#              size=3,
#              stroke=1.25,
#              shape=23) +
#   
#   # customise
#   scale_color_tableau() +
#   scale_y_continuous(name='Accuracy (%)',
#                      breaks=c(0.9, 0.95, 1.0),
#                      labels=c('90', '95', '100')) +
#   scale_x_discrete(name='Training Group') +
#   geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0.9, 1)),
#                   aes(x, y), size=1, color='black') +
#   my_theme() 

plot_trn_seterr <- ggplot() +
  geom_point(data=avg_multi[ses=='training' & switch=='non-switch'],
             aes(x=train_type,
                 y=setting_errors_mean,
                 color=train_type),
             alpha=0.3,
             position=position_jitter(width = 0.1)) +
  geom_point(data=avg_multi_gav_trn_acc,
             aes(x=train_type, 
                 y=SettingErrors, 
                 color=train_type),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23) +
  
  # customise
  scale_color_tableau() +
  scale_y_continuous(name='Setting Errors (%)',
                     breaks=c(0, 0.02, 0.04),
                     labels=c('0', '2', '4')) +
  scale_x_discrete(name='Training Group') +
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0, 0.04)),
                  aes(x, y), size=1, color='black') +
  my_theme() 


plot_trn_generr <- ggplot() +
  geom_point(data=avg_multi[ses=='training' & switch=='non-switch'],
             aes(x=train_type,
                 y=general_errors_mean,
                 color=train_type),
             alpha=0.3,
             position=position_jitter(width = 0.1)) +
  geom_point(data=avg_multi_gav_trn_acc,
             aes(x=train_type, 
                 y=GeneralErrors, 
                 color=train_type),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23) +
  
  # customise
  scale_color_tableau() +
  scale_y_continuous(name='General Errors (%)',
                     breaks=c(0, 0.02, 0.04),
                     labels=c('0', '2', '4'),
                     limits=c(0, 0.04)) +
  scale_x_discrete(name='Training Group') +
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0, 0.04)),
                  aes(x, y), size=1, color='black') +
  my_theme() # one sub excluded from visualisation for being an extreme outlier

fig_trn_acc <- plot_grid(#plot_trn_acc, 
                         plot_trn_seterr, 
                         plot_trn_generr,
                         ncol=3, nrow=1,
                         axis='tlbr',
                         rel_heights=c(1, 1),
                         align="v")

svg('figs/fig_trn_acc.svg',
    width=7.5, height=3)
plot(fig_trn_acc)
dev.off()

### Working Memory Task #### ===================================================
avg_multi_mts[, error_mean := 1 - accuracy_mean]
avg_multi_gav_mts <- avg_multi_mts[, 
                                   .(Accuracy=mean(accuracy_mean),
                                     Error=mean(error_mean),
                                     RT=mean(rt_mean)),
                                   by=c('train_type', 'cond', 'ses')]

plot_mts_acc <- ggplot() +
  
  geom_point(data=avg_multi_mts,
             aes(x=cond,
                 y=error_mean,
                 color=train_type),
             alpha=0.3,
             position=position_jitterdodge(jitter.width = 0.1,
                                           dodge.width = 0.5)) +
  geom_line(data=avg_multi_gav_mts,
            aes(x=cond, 
                y=Error, 
                color=train_type,
                group=interaction(ses, train_type)),
            linetype=2, 
            position=position_dodge(width = 0.5)) +
  geom_point(data=avg_multi_gav_mts,
             aes(x=cond, 
                 y=Error, 
                 color=train_type),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23,
             position=position_dodge(width = 0.5)) +
  facet_wrap('ses') +
  
  # customise
  scale_color_tableau() +
  scale_y_continuous(name='Error rate (%)',
                     breaks=c(0, 0.25, 0.5),
                     labels=c('0', '25', '50')) +
  scale_x_discrete(name='Multitasking Condition',
                   labels=c('m-n', 'm-o')) +
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() 


plot_mts_rt <- ggplot() +
  
  geom_point(data=avg_multi_mts,
             aes(x=cond,
                 y=rt_mean,
                 color=train_type),
             alpha=0.3,
             position=position_jitterdodge(jitter.width = 0.1,
                                           dodge.width = 0.5)) +
  geom_line(data=avg_multi_gav_mts,
            aes(x=cond, 
                y=RT, 
                color=train_type,
                group=interaction(ses, train_type)),
            linetype=2, 
            position=position_dodge(width = 0.5)) +
  geom_point(data=avg_multi_gav_mts,
             aes(x=cond, 
                 y=RT, 
                 color=train_type),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23,
             position=position_dodge(width = 0.5)) +
  facet_wrap('ses') +
  
  # customise
  scale_color_tableau() +
  scale_y_continuous(name='Reaction Time (s)',
                     breaks=c(0, 2, 4, 6),
                     labels=c('0', '2', '4', '6')) +
  scale_x_discrete(name='Multitasking Condition',
                   labels=c('m-n', 'm-o')) +
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0, 6)),
                  aes(x, y), size=1, color='black') +
  my_theme() 


fig_mts <- plot_grid(plot_mts_acc, 
                     plot_mts_rt, 
                     ncol=1, nrow=2,
                     axis='tlbr',
                     rel_heights=c(1, 1),
                     align="v")

svg('figs/fig_mts.svg',
    width=5, height=5)
plot(fig_mts)
dev.off()