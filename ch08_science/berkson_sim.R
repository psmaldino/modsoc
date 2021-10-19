# Selection bottlenecks lead to the promotion of less trustworthy science
# Coded by Paul Smaldino in 2021
# Modeling Social Behavior

# A simple model to show selection distortion.

# MODEL DESCRIPTION
# 1. Generate dataset of M papers, with normally distributed trustworthiness (T) and newsworthiness (W)
# 2. Show no correlation in pre-selection sample
# 3. Rank papers by sum of T + W. Select top 10% for publication
# 4. Show correlation in post-selection sample. 

#import libraries
library(ggplot2)
library (cowplot)
library(dplyr)

rm(list = ls()) 
M <- 500 #number of papers
M_top <- round(M/10) #number of selected papers

df <- data.frame("news" = rnorm(M, 0, 1), 
                 "trust" = rnorm(M, 0, 1))

#select on score
df$score <- df$news + df$trust
df <- df[order(df$score, decreasing = TRUE),]
df$selected <- rep(0, M)
df$selected[1:M_top] <- 1

#dataframe of just selected values
df.selected <- df[which(df$selected == 1),] 
cor.selected <- cor(df.selected$news, df.selected$trust) #correlation coefficient
cor.all <- cor(df$news, df$trust) #correlation coefficient

#plot of rejected and accepted papers
ggplot() +
  geom_point(data=df, aes(x=news, y=trust), size = 1.45, alpha = 0.65, shape = 21, color = "black", fill = "firebrick1") +
  geom_point(data=df.selected, aes(x=news, y=trust), size = 1.46, alpha = 1, shape = 19, color = "seagreen4") +
  geom_smooth(data=df.selected, aes(x=news, y=trust), method='lm', se = FALSE, fullrange = TRUE, color = "seagreen4", size = .6) + 
  scale_x_continuous(limits=c(-3.1, 3.1), breaks = c(-2, 0,  2)) +
  scale_y_continuous(limits=c(-3.1, 3.1), breaks = c(-2, 0,  2)) +
  labs(x = "newsworthiness", y="trustworthiness") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))
#3.44 x 3.52 inches

#plot of all papers
ggplot() +
  geom_point(data=df, aes(x=news, y=trust), size = 1.45, alpha = 0.65, shape = 21, color = "black", fill = "gray50") +
  #geom_smooth(data=df, aes(x=news, y=trust), method='lm', se = FALSE, fullrange = TRUE, color = "gray40", size = .6) + 
  scale_x_continuous(limits=c(-3.1, 3.1), breaks = c(-2, 0,  2)) +
  scale_y_continuous(limits=c(-3.1, 3.1), breaks = c(-2, 0,  2)) +
  labs(x = "newsworthiness", y="trustworthiness") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))
#3.44 x 3.52 inches




