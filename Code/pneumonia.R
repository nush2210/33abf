library(tidyverse)
library(knitr)
library(scales)
library(here)

here::i_am("code/pneumonia.R")

# load data 
covid <- read.csv("raw_data/covid_sub.csv")   

# remove NA values
covid_clean <- covid |> 
  filter(!is.na(PNEUMONIA), !is.na(INTUBED))

covid_clean$PNEUMONIA <- factor(covid_clean$PNEUMONIA, levels = c("No", "Yes"))
covid_clean$INTUBED   <- factor(covid_clean$INTUBED,   levels = c("No", "Yes"))

## 2×2 Summary Table
table_raw <- table(covid_clean$PNEUMONIA, covid_clean$INTUBED)

# turn into a data frame for kable
pneumonia_table <- as.data.frame.matrix(table_raw) |>
  rownames_to_column(var = "Pneumonia status")

colnames(pneumonia_table) <- c("Pneumonia status", "Not intubated", "Intubated")

kable(
  pneumonia_table,
  caption = "2×2 Table of Pneumonia vs. Intubation"
)

# Save table 
saveRDS(
  pneumonia_table,
  file = here::here("output/pneumonia_table.rds")
)

# Proportion Bar Plot
pneumonia_plot <- ggplot(covid_clean, aes(x = PNEUMONIA, fill = INTUBED)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("No" = "#A8D5E6",   
               "Yes" = "#F7A8B8")  
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "Pneumonia",
    y = "Proportion",
    fill = "Intubated",
    title = "Proportion of Intubation by Pneumonia Status"
  ) +
  theme_minimal(base_size = 14)

# Save Plot
png(here::here("output/pneumonia_plot.png"))
print(pneumonia_plot)
dev.off()