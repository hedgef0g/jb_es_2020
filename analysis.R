install.packages("tidyverse")
install.packages("foreign")
library(tidyverse)
library(foreign)

data <- read_csv("./DevEcosystem 20 external data sharing/2020_sharing_data_outside.csv")
qre <- read_csv("./DevEcosystem 20 external data sharing/DevEcosystem 2020 questions_outside.csv")

gamedev_only <- data %>%
  filter(sw_types_developed.Games == "Games")

ru_gamedev <- gamedev_only %>%
  filter(country == "Russia")

# Base function for descriptive statistics
maketable <- function(dataset = data, t_country = "", sw_type = "", variable) {
  
  if(t_country == "") {dataset} else {dataset = filter(dataset, country == t_country)}
  
  if(sw_type == "") {dataset}
  else
  {
    sw_type_col <- paste("sw_types_developed.", sw_type, sep = "") 
    dataset <- filter(dataset, dataset[sw_type_col] == sw_type)
  }

  output <- tibble("value" = character(), "share" = numeric())

  for (i in which(colnames(dataset) %in% grep(variable, names(dataset), value = TRUE))) {
    v = as.character(unique(na.omit(dataset[i])))
    s = weighted.mean(!is.na(dataset[i]), weight = dataset$weight)
    output <- add_row(output, tibble_row(value = v, share = s))
  }
  
  output
} 

prof <- maketable(t_country = "Russia", sw_type = "Games", variable = "job_role")

prof %>%
  filter(share != 0) %>%
  ggplot(aes(x = reorder(value, share), y = share, fill = desc(share))) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent)
