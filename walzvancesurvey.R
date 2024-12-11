library(tibble)

# Create the data frame
poll_data <- tibble::tribble(
  ~Survey_Date, ~Sample_Size, ~Pollster, ~Sponsor, ~Favorable_Percentage, ~Unfavorable_Percentage, ~Net_Result,
  "Nov. 1-3", 1242, "Ipsos", "Reuters", 43, 39, 4,
  "Oct. 30-Nov. 2", 1000, "Hart Research Associates/Public Opinion Strategies", "NBC News", 37, 38, -1,
  "Oct. 30-Nov. 2", 3759, "HarrisX", "Forbes", 41, 41, 0,
  "Oct. 30-Nov. 2", 4520, "HarrisX", "Forbes", 37, 39, -2,
  "Oct. 31", 671, "Kaplan Strategies", NA, 47, 53, -6,
  "Oct. 29-31", 1152, "YouGov", "Yahoo News", 48, 44, 4,
  "Oct. 29-31", 1267, "YouGov", "Yahoo News", 47, 44, 3,
  "Oct. 29-31", 1710, "YouGov", "Yahoo News", 43, 39, 4,
  "Oct. 28-31", 1328, "Echelon Insights", NA, 41, 40, 1,
  "Oct. 27-29", 3718, "HarrisX", "Forbes", 40, 41, -1,
  "Oct. 27-29", 4523, "HarrisX", "Forbes", 36, 38, -2,
  "Oct. 26-29", 1312, "YouGov", "The Economist", 47, 46, 1,
  "Oct. 26-29", 1446, "YouGov", "The Economist", 46, 46, 0,
  "Oct. 26-29", 1587, "YouGov", "The Economist", 41, 43, -2,
  "Oct. 25-28", 1475, "HarrisX", "Blockchain Association", 42, 42, 0,
  "Oct. 25-28", 1717, "HarrisX", "Blockchain Association", 40, 39, 1,
  "Oct. 24-28", 1000, "Navigator Research", NA, 39, 36, 3,
  "Oct. 25-27", 2205, "Morning Consult", NA, 47, 38, 9,
  "Oct. 25-27", 1150, "Ipsos", "Reuters", 41, 43, -2,
  "Oct. 14-27", 1007, "Gallup", NA, 45, 39, 6,
  "Oct. 24-26", 1507, "Cygnal", NA, 45, 45, 0,
  "Oct. 17-24", 3041, "Deltapoll", "The National", 42, 39, 3,
  "Oct. 20-23", 1704, "CNN/SSRS", NA, 34, 34, 0
  # Add more rows as needed...
)

# Adding additional polling data
new_poll_data <- data.frame(
  Date = c("Oct. 1", "Sept. 29-Oct. 1", "Sept. 29-Oct. 1", "Sept. 29-Oct. 1", "Sept. 29-Oct. 1", 
           "Sept. 27-29", "Sept. 27-29", "Sept. 16-28", "Sept. 23-25", "Sept. 23-25"),
  Sample_Size = c(3062, 1266, 1463, 1638, 1000, 2121, 1010, 1023, 1005, 1524),
  Type = c("A", "LV", "RV", "A", "LV", "LV", "A", "A", "LV", "LV"),
  Pollster = c("YouGov", "YouGov", "YouGov", "YouGov", "Emerson College", 
               "Morning Consult", "Leger", "Gallup", "Echelon Insights", "Big Village"),
  Sponsor = c(NA, "The Economist", "The Economist", "The Economist", NA, 
              NA, "The New York Post", NA, NA, NA),
  Favorable = c(43, 47, 47, 42, 46, 45, 36, 38, 41, 49),
  Unfavorable = c(31, 47, 46, 42, 46, 39, 28, 38, 33, 40),
  Margin = c("+12", "Even", "+1", "Even", "Even", "+6", "+8", "Even", "+8", "+9")
)

# Combine with existing data (assuming the dataset is named 'poll_data')
tim_walz_favor <- rbind(poll_data, new_poll_data)

# Check the updated dataset
print(tim_walz_favor)

# Align columns
new_poll_data <- new_poll_data %>%
  rename(Survey_Date = Date,
         Sample_Size = Sample_Size,
         Pollster = Pollster,
         Sponsor = Sponsor,
         Favorable_Percentage = Favorable,
         Unfavorable_Percentage = Unfavorable) %>%
  select(-Type)  # Drop extra 'Type' column

# Calculate Net_Result from Margin
new_poll_data$Net_Result <- as.numeric(gsub("[^0-9-]", "", new_poll_data$Margin))
new_poll_data$Net_Result[grep("Even", new_poll_data$Margin)] <- 0  # Handle "Even" cases
new_poll_data <- new_poll_data %>% select(-Margin)  # Drop 'Margin' after processing

# Combine with poll_data
tim_walz_favor <- bind_rows(poll_data, new_poll_data)

# Check the updated dataset
print(tim_walz_favor)

ggplot(tim_walz_favor, aes(x = reorder(Pollster, Net_Result), y = Net_Result, fill = Net_Result > 0)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), labels = c("Negative", "Positive")) +
  coord_flip() +
  labs(
    title = "Tim Walz Net Result by Pollster",
    x = "Pollster",
    y = "Net Result",
    fill = "Sentiment"
  ) +
  theme_minimal()

library(tibble)
library(dplyr)
library(ggplot2)

vance_poll_data <- data.frame(
  Survey_Date = c("Nov. 25-27", "Nov. 23-26", "Nov. 23-26", "Nov. 17-19", "Nov. 17-19", "Nov. 14-18", "Nov. 14-18", "Nov. 14-18", "Nov. 12-18", "Nov. 15-17", 
                  "Nov. 13-14", "Nov. 11-13", "Nov. 9-12", "Nov. 9-12", "Nov. 8-11", "Nov. 6-7", "Nov. 6-7", "Nov. 1-3", "Oct. 30-Nov. 2", "Oct. 30-Nov. 2", 
                  "Oct. 30-Nov. 2", "Oct. 31", "Oct. 29-31", "Oct. 29-31", "Oct. 29-31", "Oct. 28-31", "Oct. 27-29", "Oct. 27-29", "Oct. 26-29", "Oct. 26-29", 
                  "Oct. 26-29", "Oct. 25-28", "Oct. 25-28", "Oct. 24-28", "Oct. 25-27", "Oct. 25-27", "Oct. 25-27", "Oct. 14-27", "Oct. 24-26", "Oct. 20-23", 
                  "Oct. 18-23", "Oct. 18-23", "Oct. 18-23", "Oct. 17-23", "Oct. 21-22", "Oct. 21-22", "Oct. 19-22", "Oct. 19-22", "Oct. 19-22", "Oct. 19-22", 
                  "Oct. 18-21", "Oct. 18-21", "Oct. 18-20", "Oct. 12-17", "Oct. 14-16", "Oct. 13-15", "Oct. 12-15", "Oct. 12-15", "Oct. 11-14", "Oct. 11-14", 
                  "Oct. 11-14", "Oct. 11-14", "Oct. 11-13", "Oct. 11-13", "Oct. 11-13", "Oct. 9-11", "Oct. 9-11", "Oct. 9-11", "Oct. 1-10", "Oct. 1-10", 
                  "Oct. 7-8", "Oct. 4-8", "Oct. 6-7", "Oct. 6-7", "Oct. 6-7", "Oct. 3-7", "Oct. 4-6", "Oct. 2-5", "Oct. 2-4", "Oct. 2-4", "Oct. 2-4", 
                  "Oct. 2-4", "Oct. 2-4", "Oct. 2-3", "Oct. 2-3", "Oct. 2-3", "Oct. 2-3", "Oct. 2", "Oct. 1", "Sept. 29-Oct. 1", "Sept. 29-Oct. 1", 
                  "Sept. 29-Oct. 1", "Sept. 29-Oct. 1", "Sept. 27-29", "Sept. 27-29", "Sept. 27-29", "Sept. 16-28", "Sept. 23-25", "Sept. 23-25", "Sept. 23-25", 
                  "Sept. 23-25", "Sept. 21-24", "Sept. 21-24", "Sept. 21-24", "Sept. 19-24", "Sept. 20-23", "Sept. 20-22", "Sept. 20-22", "Sept. 19-22", 
                  "Sept. 19-22", "Sept. 17-19", "Sept. 15-17", "Sept. 15-17", "Sept. 14-17", "Sept. 13-17", "Sept. 13-17", "Sept. 12-17", "Sept. 12-17", 
                  "Sept. 12-16", "Sept. 12-16", "Sept. 12-16", "Sept. 12-16", "Sept. 12-16", "Sept. 11-15"),
  Sample_Size = c(1550, 1412, 1590, 1435, 1595, 1078, 1612, 1010, 800, 1031, 1732, 1500, 1557, 1743, 3475, 1590, 2200, 1242, 1000, 3759, 4520, 
                  671, 1152, 1267, 1710, 1328, 3718, 4523, 1312, 1446, 1587, 1475, 1717, 1000, 2205, 1150, 1007, 1507, 1704, 1592, 1739, 
                  2026, 1314, 1244, 1512, 1295, 1422, 1615, 1500, 1191, 1266, 2143, 4180, 800, 1248, 1317, 1457, 1624, 870, 1110, 957, 1072, 
                  2193, 938, 3145, 1622, 1692, 2010, 652, 780, 5500, 1000, 1235, 1414, 1604, 1000, 2122, 1106, 1033, 1148, 1714, 755, 841, 
                  1013, 1211, 1500, 2933, 3062, 1266, 1463, 1638, 1000, 2121, 1010, 672, 1531, 1233, 1592, 1530, 1418, 1685, 2507, 1207, 1054),
  Pollster = c("TIPP Insights", "YouGov", "YouGov", "YouGov", "YouGov", "YouGov", "YouGov", "Echelon Insights", "Lake Research Partners/The Tarrance Group", 
               "Ipsos", "HarrisX/Harris Poll", "Cygnal", "YouGov", "YouGov", "Morning Consult", "YouGov", "Morning Consult", "Ipsos", 
               "Hart Research Associates/Public Opinion Strategies", "HarrisX", "HarrisX", "Kaplan Strategies", "YouGov", "YouGov", "YouGov", "Echelon Insights", 
               "HarrisX", "HarrisX", "YouGov", "YouGov", "YouGov", "YouGov", "HarrisX", "Morning Consult", "Ipsos", "Morning Consult", "YouGov", "YouGov", 
               "YouGov", "Morning Consult", "YouGov", "Morning Consult", "YouGov", "YouGov", "YouGov", "YouGov", "Beacon Research/Shaw & Company Research", 
               "Beacon Research/Shaw & Company Research", "AP-NORC", "AP-NORC", "Morning Consult", "Ipsos", "HarrisX", "YouGov", "YouGov", "YouGov", 
               "YouGov", "Morning Consult", "Ipsos", "Morning Consult", "Morning Consult", "YouGov", "Morning Consult", "Morning Consult", "Morning Consult", 
               "YouGov", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", 
               "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", 
               "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", 
               "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult"),
  Sponsor = c("League of American Workers", "The Economist", "The Economist", "The Economist", "The Economist", "Yahoo News", "Yahoo News", "Echelon Insights", 
              "Georgetown Institute of Politics and Public Service", "Yahoo News", "Harvard University Center for American Political Studies", "American Principles Project", 
              "The Economist", "The Economist", "Morning Consult", "The Economist", "Forbes", "Forbes", "Harvard University Center for American Political Studies", 
              "AARP", "AARP", "AARP", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", 
              "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", 
              "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", 
              "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", 
              "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", 
              "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", 
              "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult", "Morning Consult"),
  JD_Vance_Favorable = c(0, 35, 35, 33, 35, 33, 32, 32, 32, 31, 31, 31, 32, 30, 30, 29, 29, 29, 29, 29, 29, 30, 31, 31, 33, 32, 30, 30, 30, 31, 
                         31, 32, 30, 32, 32, 31, 33, 33, 30, 31, 32, 33, 31, 31, 31, 32, 32, 31, 31, 30, 31, 31, 33, 33, 33, 33, 32, 30, 32, 30, 
                         30, 33, 33, 30, 33, 32, 33, 30, 32, 33, 32, 33, 33, 32, 32, 30, 32, 30, 32, 30, 31, 33, 33, 33, 32, 31, 32, 30, 33, 30),
  JD_Vance_Unfavorable = c(100, 50, 50, 50, 50, 50, 51, 50, 51, 49, 49, 49, 50, 50, 50, 51, 51, 50, 51, 51, 51, 51, 50, 51, 49, 50, 51, 50, 
                           50, 50, 50, 51, 50, 51, 51, 51, 50, 50, 51, 51, 50, 50, 51, 50, 51, 50, 50, 51, 51, 51, 50, 51, 51, 50, 51, 50, 50, 
                           50, 51, 50, 50, 51, 50, 50, 51, 51, 50, 50, 50, 50, 51, 51, 50, 50, 51, 51, 50, 50, 50, 51, 50, 50, 51, 50, 50, 51, 51, 
                           50, 51, 51, 51, 51, 51, 51, 50, 51, 51, 50, 51, 51, 50, 50, 50, 51, 50, 51, 51),
  Net_Result = c(NA, 35-50, 35-50, 33-50, 35-50, 33-51, 32-51, 32-50, 32-51, 31-49, 31-49, 31-49, 32-50, 30-50, 30-50, 29-51, 29-51, 29-50, 
                 29-51, 29-51, 29-51, 30-51, 31-50, 31-50, 33-49, 32-50, 30-51, 30-50, 30-50, 31-51, 31-50, 32-50, 30-51, 32-51, 32-51, 31-50, 
                 33-51, 33-51, 30-51, 31-51, 32-51, 33-50, 31-50, 31-51, 31-50, 32-51, 32-50, 31-50, 31-51, 30-50, 31-50, 31-51, 33-51, 33-51, 
                 33-50, 33-50, 32-50, 30-50, 32-51, 30-51, 30-51, 33-50, 33-51, 30-50, 33-50, 32-51, 33-50, 30-51, 32-51, 33-50, 32-51, 33-50, 
                 33-51, 32-50, 32-51, 30-50, 32-51, 30-51, 32-50, 30-51, 31-50, 33-50, 33-51, 33-51, 32-50, 31-51, 32-51, 30-51, 33-51, 30-51)
)

# Determine the shortest length of columns
min_length <- min(length(vance_poll_data$Survey_Date), length(vance_poll_data$Sample_Size), length(vance_poll_data$Pollster), length(vance_poll_data$Sponsor))

# Truncate each column to the minimum length
Survey_Date <- vance_poll_data$Survey_Date[1:min_length]
Sample_Size <- vance_poll_data$Sample_Size[1:min_length]
Pollster <- vance_poll_data$Pollster[1:min_length]
Sponsor <- vance_poll_data$Sponsor[1:min_length]

# Create the data frame with truncated columns
vance_poll_data <- data.frame(
  Survey_Date = Survey_Date,
  Sample_Size = Sample_Size,
  Pollster = Pollster,
  Sponsor = Sponsor
)

# Check the truncated data
head(vance_poll_data)

# Clean up the Net_Result column and handle "Even" cases
vance_poll_data$Net_Result <- ifelse(vance_poll_data$Net_Result == 0, "Even", vance_poll_data$Net_Result)

print(poll_data)

# Plot Net Favorability
library(ggplot2)

vance_poll_data %>%
  ggplot(aes(x = reorder(Pollster, Net_Favorability), y = Net_Favorability, fill = Pollster)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  xlab("Pollster") +
  ylab("Net Favorability") +
  ggtitle("JD Vance Favorability Polling Results by Pollster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggplot(vance_poll_data, aes(x = reorder(Pollster, Net_Result), y = Net_Result, fill = Net_Result > 0)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), labels = c("Positive", "Negative")) +
  coord_flip() +
  labs(
    title = "JD Vance Net Favorability by Pollster",
    x = "Pollster",
    y = "Net Favorability",
    fill = "Sentiment"
  ) +
  theme_minimal()

# Calculate the Net Result as the difference between favorable and unfavorable percentages
vance_poll_data$Net_Result <- vance_poll_data$JD_Vance_Favorable - vance_poll_data$JD_Vance_Unfavorable

# Check the first few rows of the data
head(vance_poll_data)

# Now create the plot
ggplot(vance_poll_data, aes(x = reorder(Pollster, Net_Result), y = Net_Result, fill = Net_Result > 0)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), labels = c("Positive", "Negative")) +
  coord_flip() +
  labs(
    title = "JD Vance Net Favorability by Pollster",
    x = "Pollster",
    y = "Net Favorability",
    fill = "Sentiment"
  ) +
  theme_minimal()

# Check lengths of each column
length(vance_poll_data$Survey_Date)
length(vance_poll_data$Sample_Size)
length(vance_poll_data$Pollster)
length(vance_poll_data$Sponsor)
length(vance_poll_data$JD_Vance_Favorable)
length(vance_poll_data$JD_Vance_Unfavorable)

# Get the minimum length of all columns to ensure truncation is correct
min_length <- min(length(vance_poll_data$Survey_Date), 
                  length(vance_poll_data$Sample_Size), 
                  length(vance_poll_data$Pollster), 
                  length(vance_poll_data$Sponsor), 
                  length(vance_poll_data$JD_Vance_Favorable), 
                  length(vance_poll_data$JD_Vance_Unfavorable))

# Truncate all columns to the minimum length
vance_poll_data <- data.frame(
  Survey_Date = vance_poll_data$Survey_Date[1:min_length],
  Sample_Size = vance_poll_data$Sample_Size[1:min_length],
  Pollster = vance_poll_data$Pollster[1:min_length],
  Sponsor = vance_poll_data$Sponsor[1:min_length],
  JD_Vance_Favorable = vance_poll_data$JD_Vance_Favorable[1:min_length],
  JD_Vance_Unfavorable = vance_poll_data$JD_Vance_Unfavorable[1:min_length]
)

# Check the data after truncation
head(vance_poll_data)


# Example bar plot of Net_Result, categorized by Pollster
ggplot(vance_poll_data, aes(x = reorder(Pollster, Net_Result), y = Net_Result, fill = Net_Result > 0)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), labels = c("Positive", "Negative")) +
  coord_flip() +
  labs(
    title = "JD Vance Net Favorability by Pollster",
    x = "Pollster",
    y = "Net Favorability",
    fill = "Sentiment"
  ) +
  theme_minimal()
