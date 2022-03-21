#===============================================================================
# Chapter 1: Figures 
#
# Date: January 26, 2022
# 
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: Figures for chapter one
#===============================================================================
# NOTES: 
#===============================================================================

# Load packages ################################################################
library(tidyverse)
library(RColorBrewer)
library(extrafont)
loadfonts(device = 'win')

# Set working directory ########################################################
setwd("C:/Users/lukeh/Desktop/School/Chapter_1")
setwd("C:/Users/lhhenslee/Desktop/Luke/School/Thesis/Chapter1")

# Data ####

# Data from 'chapter_1.R'
coho <- tags %>% 
  filter(species == 'coho')

# Color palettes ####
  # Final fate color palette
display.brewer.pal(4, 'Set2')

# Tags deployed by final fate, by week, by sd, and by year ####

  ## SD 5 ####
  coho.fig1 <- coho %>% 
  group_by(year, capture.loc, stat.week, final.fate) %>% 
  count()

  coho.fig1$`Final fate` <- coho.fig1$final.fate

  fig1 <- ggplot(subset(coho.fig1, capture.loc == '5'), 
                 aes(x = stat.week, y = n)) +
    geom_col(aes(fill = `Final fate`)) +
    facet_wrap(~year) +
    xlab("Statistical week") +
    ylab("Tags deployed") +
    scale_fill_brewer(palette = 'Set2') +
    #set the limits and tick breaks for the y-axis
    scale_y_continuous (limits = c(0,100), expand = c(0,0), breaks = seq(0,100,25)) +
    #adjust the order of the legend, make new labels, and select the symbol colors
    #makes the figure background white without grid lines
    theme_classic() +
    theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           text = element_text(family = "Times New Roman"),
           #modify plot title, the B in this case
           plot.title = element_text(face = "bold", family = "Times New Roman"),
           #position the legend on the figure
           legend.position = c(.65, .7),
           #adjust size of text for legend
           #margin for the plot
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           strip.background = element_blank(),
           strip.text = element_text(size = 14),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(size = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(size = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))
  
  fig1
  
  ggsave(fig1, file = "figs/tags.ff.sd5.png", width = 15, height = 12, units = "cm", dpi = 300)
  
  ## SD 5 ####
  
  fig2 <- ggplot(subset(coho.fig1, capture.loc == '6'), 
                 aes(x = stat.week, y = n)) +
    geom_col(aes(fill = `Final fate`)) +
    facet_wrap(~year) +
    xlab("Statistical week") +
    ylab("Tags deployed") +
    scale_fill_brewer(palette = 'Set2') +
    #set the limits and tick breaks for the y-axis
    scale_y_continuous (limits = c(0,100), expand = c(0,0), breaks = seq(0,100,25)) +
    #adjust the order of the legend, make new labels, and select the symbol colors
    #makes the figure background white without grid lines
    theme_classic() +
    theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
           axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
           #set the font type
           text = element_text(family = "Times New Roman"),
           #modify plot title, the B in this case
           plot.title = element_text(face = "bold", family = "Times New Roman"),
           #position the legend on the figure
           legend.position = c(.65, .7),
           #adjust size of text for legend
           #margin for the plot
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
           strip.background = element_blank(),
           strip.text = element_text(size = 14),
           #set size of the tick marks for y-axis
           axis.ticks.y = element_line(size = 0.5),
           #set size of the tick marks for x-axis
           axis.ticks.x = element_line(size = 0.5),
           #adjust length of the tick marks
           axis.ticks.length = unit(0.2,"cm"),
           #set size and location of the tick labels for the y axis
           axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                      margin = margin(t = 0, r = 5, b = 0, l = 0)),
           #set size and location of the tick labels for the x axis
           axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0, hjust = 0.5,
                                      margin = margin(t = 5, r = 0, b = 0, l = 0)),
           #set the axis size, color, and end shape
           axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))
  
  fig2
  
  ggsave(fig2, file = "figs/tags.ff.sd6.png", width = 15, height = 12, units = "cm", dpi = 300)
  



amss.fig.1 <- bb.dat.kvi %>% ggplot(aes(x=spawn, y=rec)) +
  geom_point(shape = 21, size = 2) +
  xlab("Spawning Abundance (millions)") +
  ylab("Recruitment (millions)") +
  ggtitle('Kvichak sockeye salmon 1968-2012') +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous (limits = c(0,50), expand = c(0,0), breaks = seq(10,50,10)) +
  #set the limits and tick spacing for the x-axis
  scale_x_continuous(limits = c(0,25), expand = c(0,0), breaks = seq(5,25,5)) +
  #adjust the order of the legend, make new labels, and select the symbol colors
  #makes the figure background white without grid lines
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Calibri"),
         #modify plot title, the B in this case
         plot.title = element_text(face = "bold", family = "Arial"),
         #position the legend on the figure
         legend.position = 'none',
         #adjust size of text for legend
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_line(size = 0.5),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))

ggsave(amss.fig.1, file = "ggplot_figure.png", width = 10, height = 7.62, units = "cm", dpi = 300)

# Figure 2 ####

ggplot(data = coho.fig.longer, aes(x = year, y = value)) +
  geom_col(aes(fill = stock)) +
  facet_wrap(~capture.loc)

amss.fig.2 <- ggplot(data = subset(coho.fig.longer, year %in% 1985:2017), aes(x = year, y = value)) +
  geom_col(aes(fill = Stock)) +
  facet_wrap(~capture.sd, scales = 'free_y') +
  xlab("Year") +
  ylab("Coho landed") +
  ggtitle("Stock composition of commercial harvests 1985-2019") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(breaks= c(1990, 1995, 2000, 2005, 2010, 2015)) +
  #set the limits and tick spacing for the x-axis
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73",
                              "#F0E442")) +
  #adjust the order of the legend, make new labels, and select the symbol colors
  #makes the figure background white without grid lines
  theme_classic() +
  theme (axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 10, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Calibri"),
         #modify plot title, the B in this case
         plot.title = element_text(face = "bold", family = "Calibri"),
         #position the legend on the figure
         legend.position = 'right',
         #adjust size of text for legend
         legend.text = element_text(size = 10, family = 'Calibri'),
         strip.text = element_text(size=14),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_line(size = 0.5),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
           panel.spacing.x = unit(1, 'lines'),
         strip.background = element_blank())
  
  amss.fig.2

ggsave(amss.fig.2, file = "ggplot_figure8.png", width = 20, height = 12, units = "cm", dpi = 300)

