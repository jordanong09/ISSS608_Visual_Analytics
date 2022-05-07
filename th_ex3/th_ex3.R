packages = c('tidyverse', 'ggrepel', 'ggiraph', 'patchwork', 'plotly', 'hrbrthemes', 'heatmaply', 'viridis','ggridges')
for(p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

library(extrafont)
loadfonts(device = "win")

'financeJ <- read_csv(file = "th_ex3/rawdata/FinancialJournal.csv")
participantsD <- read_csv(file = "th_ex3/rawdata/Participants.csv")

saveRDS(financeJ, "financeJ.rds")
saveRDS(participantsD, "participantsD.rds")'

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


cuts <- bins(participantsD$age,target.bins = 6,minpts = 120)
cuts$breaks <- bins.getvals(cuts)
cuts$binct


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

head(final)

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

mean(scatter_data$Expenses)

ggplot(data = scatter_data, aes (x=PctSavings)) +
  geom_histogram()



q <- ggplot(data = scatter_data, aes(x = Expenses, y = Income,color=agegroup)) +
  geom_point() + 
  geom_vline(aes(xintercept = mean(scatter_data$Expenses)),
             linetype= 'dashed',
             color= '#f08080',
             size= .6) + 
  geom_hline(aes(yintercept = mean(scatter_data$Income)),
             linetype= 'dashed',
             color= '#f08080',
             size= .6)
  


ggplot(data = scatter_data, aes(x = Income)) +
  geom_histogram()

ggplotly(q)

final$timestamp <- format(strptime(final$timestamp, "%Y-%m-%d"), "%B-%Y")


ridge_plot <- final %>%
  select(c("timestamp","participantId", "category","amount")) %>% #choose the columns to subset
  group_by(timestamp,category,participantId)%>% 
  summarise(amount = round(sum(amount),2)) %>%
  pivot_wider(names_from = "category",values_from = "amount")
ridge_plot[is.na(ridge_plot)] = 0

ridge_plot_final <- ridge_plot %>%
  mutate(Expenses = -(Education + Food + Recreation + Shelter))
  

financeJ_1 <- final %>% 
  select(c("timestamp","category","amount")) %>% #choose the columns to subset
  group_by(timestamp,category)%>% 
  summarise(amount = round(sum(amount),2)) %>%
  pivot_wider(names_from = "category",values_from = "amount")
financeJ_1[is.na(financeJ_1)] = 0

financeJ_2 <- financeJ_1 %>%
  mutate(Expenses = -(Education + Food + Recreation + Shelter))

financeJ_2[2:8] <- abs(financeJ_2[2:8])

financeJ_2$timestamp <- format(strptime(financeJ_2$timestamp, "%Y-%m-%d"), "%B-%Y")



fig <- plot_ly(financeJ_2, type = 'scatter')%>%
  add_trace(x = ~timestamp, y = ~Food)%>%
  layout(showlegend = F)
options(warn = -1)

fig

ggplot (participants_final, aes(x=participants_final$agegroup)) +
  geom_bar()

finance_bar <- final %>% 
  select(c("agegroup","category","amount")) %>% #choose the columns to subset
  group_by(agegroup,category)%>% 
  summarise(amount = round(sum(amount),2)) %>%
  pivot_wider(names_from = "category",values_from = "amount")
finance_bar[is.na(finance_bar)] = 0

finance_bar_2 <- finance_bar %>%
  mutate(Expenses = -(Education + Food + Recreation + Shelter))

finance_bar_2[2:8] <- abs(finance_bar_2[2:8])


final_list_bar <- finance_bar_2 %>%
  group_by(agegroup) %>%
  summarise(Wage = sum(Wage), Expenses = sum(Expenses), Education = sum(Education), Shelter = sum(Shelter), Food = sum(Food), Recreation=sum(Recreation)) %>% #reduces multiple values down to a single summary.
  mutate(
    tooltip_textWage = paste0(toupper(agegroup), "\n", #tooltip for Wage bar chart
                          "$", round(Wage,2) )
  ) %>%
  mutate(
    tooltip_textExpenses = paste0("Age Group: ",toupper(agegroup), "\n", #tooltip for Expenses bar chart
                           "Amount: $", round(Expenses,2) )
  ) %>%
  mutate(
    tooltip_textEducation = paste0(toupper(agegroup), "\n", #tooltip for Expenses bar chart
                           "$", round(Education,2) )
  ) %>%
  mutate(
    tooltip_textShelter = paste0(toupper(agegroup), "\n", #tooltip for Expenses bar chart
                           "$", round(Shelter,2) )
  ) %>%
  mutate(
    tooltip_textFood = paste0(toupper(agegroup), "\n", #tooltip for Expenses bar chart
                           "$", round(Food,2) )
  ) %>%
  mutate(
    tooltip_textRecreation = paste0(toupper(agegroup), "\n", #tooltip for Expenses bar chart
                           "$", round(Recreation,2) )
  )
  

Wage_bar <- ggplot(final_list_bar, 
                   aes(x = reorder(agegroup, Wage), 
                       y = Wage,
                       tooltip = tooltip_textWage, data_id = agegroup #<<
                   )) +
  geom_col_interactive(color = "black", fill="#0072B2", size = 0.5) +  #<<
  theme_minimal() +
  theme(axis.text=element_text(size = 6)) +  #<<
  labs(title = "Average Wage (USD) of Ohio Population by Age Group"
  ) +
  ylab("Age Group") +
  xlab("Amount ($)") +
  coord_flip()

Expenses_bar <- ggplot(final_list_bar, aes(x = reorder(agegroup, Expenses), y = Expenses, tooltip = tooltip_textExpenses, data_id = agegroup)) +
  geom_col_interactive(color = "black", fill="#0072B2", size = 0.5) +
  theme_minimal() +
  theme(axis.text=element_text(size = 6)) +
  labs(title = "Average Expenses (USD) of Ohio Population by Age Group"
  ) +
  ylab("") +
  xlab("") +
  coord_flip()

Education_bar <- ggplot(final_list_bar, aes(x = reorder(agegroup, Education), y = Education, tooltip = tooltip_textEducation, data_id = agegroup)) +
  geom_col_interactive(color = "black", fill="#0072B2", size = 0.5) +
  theme_minimal() +
  theme(axis.text=element_text(size = 6)) +
  labs(title = "Average Education Expenses (USD) of Ohio Population by Age Group"
  ) +
  ylab("") +
  xlab("") +
  coord_flip()
Shelter_bar <- ggplot(final_list_bar, aes(x = reorder(agegroup, Shelter), y = Shelter, tooltip = tooltip_textShelter, data_id = agegroup)) +
  geom_col_interactive(color = "black", fill="#0072B2", size = 0.5) +
  theme_minimal() +
  theme(axis.text=element_text(size = 6)) +
  labs(title = "Average Shelter Expenses (USD) of Ohio Population by Age Group"
  ) +
  ylab("") +
  xlab("") +
  coord_flip()

Food_bar <- ggplot(final_list_bar, aes(x = reorder(agegroup, Food), y = Food, tooltip = tooltip_textFood, data_id = agegroup)) +
  geom_col_interactive(color = "black", fill="#0072B2", size = 0.5) +
  theme_minimal() +
  theme(axis.text=element_text(size = 6)) +
  labs(title = "Average Food Expenses (USD) of Ohio Population by Age Group"
  ) +
  ylab("") +
  xlab("") +
  coord_flip()
Recreation_bar <- ggplot(final_list_bar, aes(x = reorder(agegroup, Recreation), y = Recreation, tooltip = tooltip_textRecreation, data_id = agegroup)) +
  geom_col_interactive(color = "black", fill="#0072B2", size = 0.5) +
  theme_minimal() +
  theme(axis.text=element_text(size = 6)) +
  labs(title = "Average Recreation Expenses (USD) of Ohio Population by Age Group"
  ) +
  ylab("") +
  xlab("") +
  coord_flip()


girafe(code = print(Wage_bar / Expenses_bar/(Education_bar + Food_bar)/ (Shelter_bar + Recreation_bar)), 
       width_svg = 10, height_svg = 7) %>% 
  girafe_options(opts_hover(css = "fill:cyan;"))

ridge_plot_final$timestamp <- format(strptime(ridge_plot_final$timestamp, "%Y-%m-%d"), "%B-%Y")

ggplot(ridge_plot_final, aes(x=Wage)) +
  geom_histogram()

ridge_plot_final$timestamp <- as.Date(ridge_plot_final$timestamp, format = "%m-%Y")

ridge_plot_final %>%
  arrange(desc(lubridate::my(timestamp))) %>% 
  ggplot(aes(x = Wage, y = fct_inorder(timestamp))) +
  geom_density_ridges(scale = 2, quantile_lines=TRUE,quantiles = 0.5 )

financeJ_2$timestamp <- format(strptime(financeJ_2$timestamp, "%Y-%m-%d"), "%B-%Y")

finance_heat<- financeJ_2 %>%
  group_by(timestamp) %>%
  summarise(Expenses = sum(Expenses), Education = sum(Education), Shelter = sum(Shelter), Food = sum(Food), Recreation=sum(Recreation)) %>%
  mutate(timestamp = factor(timestamp, levels = month.name[month.name %in% unique(timestamp)])) %>%
  select(timestamp, Expenses, Education, Shelter, Food, Recreation)

mat <- finance_heat
rownames(mat) <- mat[,1]
mat <- as.matrix(mat)

p <- ggplot(finance_heat, aes(agegroup,timestamp, fill = Expenses)) +
  geom_tile() +
  theme_ipsum() +
  theme(text=element_text(size=16,  family="TT Times New Roman"))

p
financeJ_1 <- financeJ %>% #load the financeJ data table
  group_by(category, timestamp)%>% 
  summarise(amount = round(sum(amount),2)) %>% #sum all the amount based on their ID and Category rounding off to 2 decimal place
  pivot_wider(names_from = "category",values_from = "amount") #%>% #pivot the table to have the categories in columns instead of rows
financeJ_1[is.na(financeJ_1)] = 0

financeJ_Final <- financeJ_1 %>%
  mutate(Expenses = Education + Food + Recreation + Shelter + RentAdjustment) %>%
  select(timestamp,Wage,Expenses)
head (financeJ_Final)

financeJ_Final$timestamp <- strptime(financeJ_Final$timestamp, format = "%Y-%m-%d")
p <- ggplot(financeJ_Final, aes(x=timestamp, y = -Expenses)) +
  geom_line(color="#69b3a2") +
  xlab("")

p+scale_x_date(date_labels = "%W")


dput(ridge_plot_final)
