########TASK 4 ###############
### Homework
# Conduct exploratory data analysisfor the variable EU_Sales and Publisher 
# from the dataset vgsales (Select TOP 10 publishers for the analysis)
# Include at least 3 different plots made with ggplot(). Provide description/interpretation for those plots. 
#install.packages("dplyr")
library(dplyr)
library(ggplot2)
vgsales <- read.csv("/Users/bartekgornicki/Desktop/CLASS_06_STAT/vgsales.csv")
View(vgsales)

str(vgsales)

best_publishers <- vgsales %>%
  group_by(Publisher) %>%
  summarise(Total_EU_Sales = sum(EU_Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_EU_Sales)) %>%
  slice(1:10)


vgsales_top <- vgsales %>%
  filter(Publisher %in% best_publishers$Publisher)

sales_by_publisher <- vgsales_top %>%
  group_by(Publisher) %>%
  summarise(Total_EU_Sales = sum(EU_Sales, na.rm = TRUE))

ggplot(sales_by_publisher, aes(x = reorder(Publisher, -Total_EU_Sales), y = Total_EU_Sales, fill = Publisher)) +
  geom_bar(stat = "identity") +
  labs(title = "Total EU Sales by Top 10 Publishers", x = "Publisher", y = "Total EU Sales (in millions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
