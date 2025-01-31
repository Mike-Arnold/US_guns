# Attempt to set working directory
# setwd(getSrcDirectory()[1]) # if running entire file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # if running section
options(scipen=999) # don't use scientific notation
library(dplyr)
library(ggplot2)

# https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html
# choose link for data 2018-2023
# group by state gender intent
# name CDC - State Gender Intent External
# down to section 6, radio button for injury intent and mechanism
# in intent, select all but first and last
# check all but 'show totals'

CDC_import = function(file_name) {
  df = read.table(file_name, header=T, sep="\t", fill=T) %>%
    filter(Notes == "") %>% select(-Notes,-Crude.Rate) %>%
    mutate(Deaths = as.numeric(ifelse(Deaths == "Suppressed", NA, Deaths)))
  return(df)
}

state_ST = read.csv("https://worldpopulationreview.com/static/states/abbr-name.csv",
                    header=F, col.names=c("ST","State")) %>%
  mutate(State = ifelse(ST == "DC", "District of Columbia", State))

rand = read.csv("RAND_gun_ownership_2016.csv")

light.color = "#e0e0e0"
dark.color = "#202020"
red.color = "#d03030"

scatter.theme = theme(
  axis.text = element_text(color='#bfbfbf'),
  axis.title.x = element_text(size=15, face="bold", color='#e6e6e6'),
  axis.title.y = element_text(size=15, face="bold", color='#e6e6e6'),
  legend.title = element_text(size=12, face="bold", color='#e6e6e6'),
  panel.background = element_rect(fill='#202020', color='#404040'),
  plot.background = element_rect(fill='#181818', color='#404040'),
  panel.grid.minor = element_blank(), 
  panel.grid.major = element_line(color='#404040'),
  plot.margin = unit(c(.5,.8,.5,.8), "cm"))

left.label = paste0(
  "michaelarnoldgraphs.substack.com",
  "\nCircle size indicates state population. Correlation is weighted by population.",
  "\nIncludes non-gun deaths. Gun data includes men and women.")

right.label = paste0(
  "Mortality data from CDC, 2018-2023 (wonder.cdc.gov/Deaths-by-Underlying-Cause.html)",
  "\nGun data from RAND, 2016 (rand.org/pubs/tools/TL354.html)")

all_death = CDC_import("CDC/CDC - State Gender Intent External.txt") %>%
  filter(Injury.Intent %in% c("Suicide","Homicide")) %>%
  select(State, Gender, Injury.Intent, Deaths, Population) %>%
  mutate(Rate = Deaths / Population * 10^5) %>%
  merge(state_ST) %>%
  merge(rand)

###### general ######

make_graph = function(gender, intent, nudge, y1, y2, y3) {
  lower.gender = tolower(gender)
  lower.intent = tolower(intent)
  
  df = all_death %>%
    filter(Gender == gender, Injury.Intent == intent) %>%
    merge(read.csv(paste0("labels/",lower.gender,"_",lower.intent,".csv"))) %>%
    arrange(desc(Population)) %>%
    mutate(Color = ifelse(Color == "light.color", light.color, dark.color))
  
  lm_model = lm(Rate ~ HFR, data = df, weights = Population)
  slope = coef(lm_model)[2]
  correlation = cor(df$HFR, df$Rate)
  
  stats_h = min(df$Rate) + .8 * (max(df$Rate) - min(df$Rate))
  note_h = min(df$Rate) + -0.125 * (max(df$Rate) - min(df$Rate))
  
  g1 = ggplot(data = df, aes(x = HFR, y = Rate)) +
    geom_abline(slope = slope, intercept = coef(lm_model)[1], 
                color = red.color, linetype = "solid", linewidth=2) +
    geom_point(aes(size = Population^.5), color = "black", shape = 21, fill = "#c0c0c0", stroke = .5) +
    scale_size_continuous(range = c(0, 20), guide = "none") +
    scale_color_identity(guide="none") +
    scatter.theme +
    labs(x = "% live with gun", 
         y = paste(gender,lower.intent,"rate"))
  
  g2 = g1 +
    scale_y_continuous(breaks = seq(y1, y2, by = y3),
                       labels = function(x) stringr::str_pad(x, width = 2, side = "left", pad = "\u2007")) +
    geom_text(aes(label = ST, color = Color), size = 3,
              nudge_x = df$Label.x, 
              nudge_y = df$Label.y) +
    annotate("text", x = .1, y = note_h, size = 2, color = light.color, hjust=0, vjust=0.5,
             label = left.label) +
    annotate("text", x = .45, y = note_h, size = 2, color = light.color, hjust=0, vjust=0.5,
             label = right.label) +
    annotate("text", x = .09, y = stats_h, size=5,
             label = paste0("Slope: ", format(round(slope,2), nsmall=2, trim=T), "\n",
                            "Correlation: ", format(round(correlation,2), nsmall=2, trim=T)),
             hjust = 0, vjust = 0.5, color=red.color) +
    
    coord_cartesian(ylim = c(min(df$Rate), max(df$Rate)), clip = "off")
  
  g3 = g1 + 
    coord_cartesian(ylim = c(0,45)) +
    annotate("text", x = .1, y = 40, size=5,
             label = paste0("Slope: ", format(round(slope,2), nsmall=2, trim=T), "\n",
                            "Correlation: ", format(round(correlation,2), nsmall=2, trim=T)),
             hjust = 0, vjust = 0.5, color=red.color)
  
  ggsave(paste0("images/",lower.gender,"_",lower.intent,".png"), g2, width=10, height=6)
  return(g3)
}

fs = make_graph("Female","Suicide","fs_nudge.csv", 5, 11, 3)
ms = make_graph("Male","Suicide","ms_nudge.csv", 20, 40, 10)
fh = make_graph("Female","Homicide","fh_nudge.csv", 0, 6, 2)
mh = make_graph("Male","Homicide","mh_nudge.csv", 0, 30, 10)

compare = gridExtra::grid.arrange(fs, ms, fh, mh, ncol=2, nrow=2)

annotation = ggplot() + 
  annotate("text", x = 0, y = 0.5, label = left.label, size = 3, color = light.color, hjust=0) +
  annotate("text", x = 0.55, y = 0.5, label = right.label, size = 3, color = light.color, hjust=0) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = '#181818', color = NA),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

compare2 = gridExtra::grid.arrange(compare, annotation, ncol = 1, heights = c(16, 1))
ggsave("images/comparison.png", compare2, width=12, height=9)


# summary comparisons

# temp = CDC_import("CDC - State Gender Intent External.txt") %>%
#   group_by(Injury.Intent, Gender) %>%
#   summarize(Sum_Value = sum(Deaths))
#
# guns = CDC_import("CDC/CDC - State Gender Intent Gun.txt") %>%
#   mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>%
#   group_by(Injury.Intent, Gender) %>%
#   summarize(gun_deaths = sum(Deaths))
# 
# all = CDC_import("CDC/CDC - State Gender Intent External.txt") %>%
#   mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>%
#   group_by(Injury.Intent, Gender) %>%
#   summarize(all_deaths = sum(Deaths)) %>%
#   left_join(guns) %>%
#   mutate(gun.pct = round(gun_deaths / all_deaths * 100,1))
