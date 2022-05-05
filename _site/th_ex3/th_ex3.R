packages = c('tidyverse', 'ggrepel', 'ggiraph', 'patchwork', 'plotly', 'hrbrthemes')
for(p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

financeJ <- read_csv(file = "rawdata/FinancialJournal.csv")
participantsD <- read_csv(file = "rawdata/Participants.csv")

head (financeJ)
head (participantsD)


financeJ$timestamp <- format(as.POSIXct(financeJ$timestamp), format = "%Y-%m-%d")

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