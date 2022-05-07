packages = c('tidyverse', 'ggrepel', 'ggiraph', 'patchwork', 'plotly', 'hrbrthemes', 'heatmaply', 'viridis','ggridges')
for(p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

library(extrafont)
loadfonts(device = "win")

financeJ <- readRDS(file = "th_ex3/data/financeJ.rds")
participantsD <- readRDS(file = "th_ex3/data/participantsD.rds")

head (financeJ)
head (participantsD)

financeJ$timestamp <- format(as.POSIXct(financeJ$timestamp), format = "%Y-%m-%d")

age_group1 <- 18:23 #create a vector of age
age_group2 <- 24:29
age_group3 <- 30:35
age_group4 <- 36:42
age_group5 <- 43:48
age_group6 <- 49:54
age_group7 <- 55:60

participants_final <- participantsD %>%
  mutate (agegroup= case_when(
    age %in% age_group1 ~ "18-23",
    age %in% age_group2 ~ "24-29",
    age %in% age_group3 ~ "30-35",
    age %in% age_group4 ~ "36-42",
    age %in% age_group5 ~ "43-48",
    age %in% age_group6 ~ "49-54",
    age %in% age_group7 ~ "55-60")) %>%
  select (participantId,agegroup)

final <- merge(participants_final,financeJ,by=c("participantId","participantId"),all.x=T)

cuts <- bins(participantsD$age,target.bins = 6,minpts = 120)
cuts$breaks <- bins.getvals(cuts)
cuts$binct


scatter_data <- final %>%
  select(c("participantId","category","amount","agegroup")) %>%
  group_by(participantId,category,agegroup) %>%
  summarise (amount = round(sum(amount),2)) %>%
  pivot_wider(names_from = "category",values_from = "amount")
scatter_data[is.na(scatter_data)] = 0

scatter_data <- scatter_data %>%
  mutate(Expenses = -(Education + Food + Recreation + Shelter)) %>%
  mutate(Income = RentAdjustment + Wage) %>%
  mutate (Savings = Income - Expenses) %>%
  mutate (PctSavings = (Savings/Income) *100)


p <- ggplot (scatter_data, aes (Income, Expenses, color=agegroup))

p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0, 260000)) 
p <- p + scale_y_continuous(expand = c(0, 0), limits = c(0, 40000))

p <- p + labs(x="INCREASE OF INCOME",y="INCREASE OF EXPENDITURE")
p <- p + theme(axis.title.x = element_text(hjust = 0, vjust=4, colour="darkgrey",size=10,face="bold"))
p <- p + theme(axis.title.y = element_text(hjust = 0, vjust=0, colour="darkgrey",size=10,face="bold"))

p <- p + theme(
  axis.ticks.x=element_blank(), 
  axis.text.x=element_blank(),
  axis.ticks.y=element_blank(),
  axis.text.y=element_blank()
)

p <- p+ggtitle("Income Against Expenses using Magic Quandrant")

p <- p +
  annotate("rect", xmin = 130000, xmax = 260000, ymin = 20000, ymax = 40000, fill= "#F8F9F9")  + 
  annotate("rect", xmin = 0, xmax = 130000, ymin = 0, ymax = 20000 , fill= "#F8F9F9") + 
  annotate("rect", xmin = 130000, xmax = 260000, ymin = 0, ymax = 20000, fill= "white") + 
  annotate("rect", xmin = 0, xmax = 130000, ymin = 20000, ymax = 40000, fill= "white")

p <- p + theme(panel.border = element_rect(colour = "lightgrey", fill=NA, size=3))
p <- p + geom_hline(yintercept=20000, color = "lightgrey", size=1.5)
p <- p + geom_vline(xintercept=130000, color = "lightgrey", size=1.5)

p <- p + annotation_custom(
  grob = linesGrob(arrow=arrow(type="open", ends="last", length=unit(2,"mm")), 
                   gp=gpar(col="lightgrey", lwd=4)), 
  xmin = -2, xmax = -2, ymin = 25, ymax = 40
)
p <- p + annotation_custom(
  grob = linesGrob(arrow=arrow(type="open", ends="last", length=unit(2,"mm")), 
                   gp=gpar(col="lightgrey", lwd=4)), 
  xmin = 100000, xmax = 120000, ymin = -10, ymax = -20
)



p <- p + geom_point() 

gt = ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name=="panel"] = "off"
grid.draw(gt)


p <- p + geom_label(aes(x = 25, y = 97, label = "CALLENGERS"), 
                    label.padding = unit(2, "mm"),  fill = "lightgrey", color="white")
p <- p + geom_label(aes(x = 75, y = 97, label = "LEADERS"), 
                    label.padding = unit(2, "mm"), fill = "lightgrey", color="white")
p <- p + geom_label(aes(x = 25, y = 3, label = "NICHE PLAYERS"), 
                    label.padding = unit(2, "mm"),  fill = "lightgrey", color="white")
p <- p + geom_label(aes(x = 75, y = 3, label = "VISIONARIES"), 
                    label.padding = unit(2, "mm"), fill = "lightgrey", color="white")