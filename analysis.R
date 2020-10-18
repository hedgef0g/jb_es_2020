install.packages("tidyverse")
install.packages("foreign")
library(tidyverse)
library(foreign)

data <- read_csv("./DevEcosystem 20 external data sharing/2020_sharing_data_outside.csv")
qre <- read_csv("./DevEcosystem 20 external data sharing/DevEcosystem 2020 questions_outside.csv")

data <- data %>%
  mutate(years_exp = factor(code_yrs,
                            levels = c("Less than 1 year", "1–2 years", "3–5 years", "6–10 years", "11+ years", "I don't have any professional coding experience"),
                            labels = c(0.5, 1.5, 4, 8, 12, NA))) %>%
  mutate(new_age = factor(age_range,
                          levels = c("18–20", "21–29", "30–39", "40–49", "50–59", "60 or older"),
                          labels = c(19, 24, 34.5, 44.5, 54.5, 64.5))) %>%
    mutate(mobile_target_os_overall = ifelse((!is.na(mobile_target_os.Android) & !is.na(mobile_target_os.iOS)), "Android & iOS",
                                             ifelse(!is.na(mobile_target_os.Other), "Other",
                                                    ifelse(!is.na(mobile_target_os.Android), "Android",
                                                           ifelse(!is.na(mobile_target_os.iOS), "iOS", NA)))))

# Reworked function with additional options
maketable <- function(variable, dataset = data, t_country = "total", sw_type = "any", base = "weighted", sort = FALSE, filter = "none") {
  
  if(t_country == "total") {dataset} else {dataset = filter(dataset, country == t_country)}
  
  if(sw_type == "any") {dataset}
  else {    
    sw_type_col <- paste("sw_types_developed.", sw_type, sep = "") 
    dataset <- filter(dataset, dataset[sw_type_col] == sw_type)}
  
  dataset <- switch(filter,
                    "none" = dataset,
                    "employment" = filter(dataset, employment_status %in% unique(dataset$employment_status)[c(1,2,4,5,6)]),
                    "job_role" = filter_at(dataset, 
                                           vars(grep("job_role", names(dataset), value = FALSE)[c(1,2,3,5,6,7,8,10,12)]), 
                                           any_vars(!is.na(.))),
                    "desktop" = filter(dataset, !is.na(dataset$target_platforms.Desktop)),
                    "mobile" = filter(dataset, !is.na(dataset$target_platforms.Mobile)),
                    "pets" = filter(dataset, rowSums(is.na(dataset[grep("lifestyle_pet", names(dataset), value = FALSE)])) != 10))
  
  colnums <- which(colnames(dataset) %in% grep(variable, names(dataset), value = TRUE))
  
  if (length(colnums) > 1) {
    output <- tibble("value" = character(), "share" = numeric())
    for (i in colnums) {
      v = as.character(unique(na.omit(dataset[i])))
      s = switch(base,
                 "weighted" = weighted.mean(!is.na(dataset[i]), w = dataset$weight),
                 "non-weighted" = sum(dataset[i] == v, na.rm = TRUE) / nrow(dataset))
      output <- add_row(output, tibble_row(value = v, share = s))}
    output <- switch(base, 
                     "weighted" = add_row(output, tibble_row(value = "Base", share = sum(dataset$weight))),
                     "non-weighted" = add_row(output, tibble_row(value = "Base", share = nrow(dataset))))
  }
  else {
    v = unique(unlist(data[colnums]))
    v = v[!is.na(v)]
    s = numeric(length = length(v))
    for (i in v) {
      s[which(v == i)] = switch(base,
                                "weighted" = sum(filter(dataset, dataset[colnums] == i)$weight) / sum(filter(dataset, !is.na(dataset[colnums]))$weight),
                                "non-weighted" = nrow(filter(dataset, dataset[colnums] == i)) / nrow(filter(dataset, !is.na(dataset[colnums]))))
    }
    output <- tibble("value" = v, "share" = s)
    output <- switch(base,
                     "weighted" = add_row(output, tibble_row(value = "Base", share = sum(filter(dataset, !is.na(dataset[colnums]))$weight))),
                     "non-weighted" = add_row(output, tibble_row(value = "Base", share = nrow(filter(dataset, !is.na(dataset[colnums]))))))
  }
  if (sort == FALSE) {output} else {arrange(output, value)}
} 

percent_sig <- function(perc1, perc2, base1, base2, lev = 1.96) {
  if(base1 >= 75 & base2 >= 75) {
  perc1 = perc1 * 100
  perc2 = perc2 * 100
  p = (perc1 * base1 + perc2 * base2) / (base1 + base2)
  output <- tibble(sig = character())
  if((perc1 - perc2) / sqrt(p * (100 - p) * (1 / base1 + 1 /base2)) > lev) {sig = "high"} 
  else {
      if((perc1 - perc2) / sqrt(p * (100 - p) * (1 / base1 + 1 /base2)) < -lev) {sig = "low"} 
    else {sig = "no"}}
  sig}
  else {sig = "no"}
}

get_sig <- function(percent_table, level = 1.96) {
  add_column(percent_table, sig = vector("character", nrow(percent_table)))  
  for (i in 1:(nrow(percent_table) - 1)) {
    output = percent_sig(percent_table[i,3], 
                         percent_table[i,2], 
                         percent_table[nrow(percent_table),3], 
                         percent_table[nrow(percent_table),2],
                         lev = level)
    percent_table$sig[i] = output
  }
  percent_table$sig[nrow(percent_table)] = NA
  percent_table
}

sig_levels <- c("darkgreen", "darkred", "darkgrey")
names(sig_levels) <- levels(factor(c("high", "no", "low")))

age_range <- maketable(variable = "age_range", sort = TRUE) %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "age_range", sw_type = "Games", sort = TRUE)$share)

age_range <- get_sig(age_range)

age_range$value <- factor(c("18–20", "21–29", "30–39", "40–49", "50–59", "60 or older", "Base"), 
                          levels = c("60 or older", "50–59", "40–49", "30–39", "21–29", "18–20", "Base"))

t.test(as.numeric(as.character(filter(data, data$sw_types_developed.Games == "Games")$new_age)), as.numeric(as.character(data$new_age)))

weighted.mean(as.numeric(as.character(filter(data, data$sw_types_developed.Games == "Games")$new_age)), w = filter(data, data$sw_types_developed.Games == "Games")$weight)
weighted.mean(as.numeric(as.character(data$new_age)), w = data$weight)

png(filename = "age.png", width = 900, height = 500)
age_range %>%
  filter(value != "Base") %>%
  ggplot(aes(x = value, y = gamedev, fill = sig)) +
  coord_flip() +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Возраст разработчиков игр",
       subtitle = "Доля каждого возраста, %",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

code_yrs <- maketable(variable = "code_yrs") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "code_yrs", sw_type = "Games")$share) %>%
  filter(total != 0 & gamedev != 0)

code_yrs <- get_sig(code_yrs)

code_yrs$value <- factor(code_yrs$value, levels = c("I don't have any professional coding experience",
                                                    "11+ years", 
                                                    "6–10 years",
                                                    "3–5 years", 
                                                    "1–2 years", 
                                                    "Less than 1 year", 
                                                    "Base"))

png(filename = "exp.png", width = 900, height = 500)
code_yrs %>%
  filter(value != "Base") %>%
  ggplot(aes(x = value, y = gamedev, fill = sig)) +
  coord_flip() +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Опыт профессиональной разбработки",
       subtitle = "Число лет, %",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

employment <- maketable(variable = "employment_status") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "employment_status", sw_type = "Games")$share)

employment <- get_sig(employment)

png(filename = "employment.png", width = 900, height = 500)
employment %>%
  filter(gamedev != 0 & value != "Base") %>%
  mutate(value = c("Fully employed", "Freelancer", "Student", "Self-employed", "Partially employed", "Working student", "Other", "Retired")) %>%
  ggplot(aes(x = reorder(value, gamedev), y = gamedev, fill = sig)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Трудоустройство",
       subtitle = "Доля каждой позиции, %",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

prof <- maketable(variable = "job_role", filter = "employment") %>%
  rename(total = share) %>%
  add_column(gamedev = (maketable(variable = "job_role", sw_type = "Games", filter = "employment")$share))

prof <- get_sig(prof)

png("job_role.png", width = 900, height = 500)
prof %>%
  filter(gamedev != 0 & value != "Base") %>%
  ggplot(aes(x = reorder(value, gamedev), y = gamedev, fill = sig)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Что из перечисленного лучше всего описывает ваши\nдолжностные обязанности?",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")
dev.off()

position_level <- maketable(variable = "position_level", filter = "job_role") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "position_level", sw_type = "Games", filter = "job_role")$share)

position_level <- get_sig(position_level)

png("position.png", width = 900, height = 500)
position_level %>%
  filter(gamedev != 0 & value != "Base") %>%
  ggplot(aes(x = reorder(value, gamedev), y = gamedev, fill = sig)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Уровень, занимаемый в компании",
       subtitle = "Доля каждого уровня, %",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

activities <- maketable(variable = "activities_kind") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "activities_kind", sw_type = "Games")$share)

activities <- get_sig(activities)

png("activities.png", width = 900, height = 500)
activities %>%
  filter(gamedev != 0 & value != "Base") %>%
  ggplot(aes(x = reorder(value, gamedev), y = gamedev, fill = sig)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Типичные деловые задачи",
       subtitle = "% выполняющих задачи подобного рода на основной работе",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

lang_p12m <- maketable(variable = "^proglang\\.") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "^proglang\\.", sw_type = "Games")$share)

lang_p12m <- get_sig(lang_p12m)

png("lang_p12m.png", width = 900, height = 500)
lang_p12m %>%
  filter(gamedev != 0 & value != "Base") %>%
  mutate(value = replace(value, value == "SQL(PL/SQL, T-SQL and otherprogramming extensions of SQL)", "SQL")) %>%
  mutate(value = replace(value, value == "Shell scripting languages(bash/shell/powershell)", "Shell")) %>%
  mutate(value = replace(value, value == "I don't use programming languages", "Not any")) %>%
  ggplot(aes(x = reorder(value, -gamedev), y = gamedev, fill = sig)) +
  geom_col() +
  #coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Языки программирования, использованные за 12 месяцев",
       subtitle = "Доля каждого языка, %",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

primary_lang <- maketable(variable = "primary_proglang") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "primary_proglang", sw_type = "Games")$share) %>%
  filter(total != 0 & gamedev != 0)

primary_lang <- get_sig(primary_lang)

png("primary_lang.png", width = 900, height = 500)
primary_lang %>%
  filter(gamedev != 0 & value != "Base") %>%
  mutate(value = replace(value, value == "SQL(PL/SQL, T-SQL and otherprogramming extensions of SQL)", "SQL")) %>%
  mutate(value = replace(value, value == "Shell scripting languages(bash/shell/powershell)", "Shell")) %>%
  mutate(value = replace(value, value == "I don't use programming languages", "Not any")) %>%
  ggplot(aes(x = reorder(value, -gamedev), y = gamedev, fill = sig)) +
  geom_col() +
  #coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Основные языки программирования (не более трёх для респондента)",
       subtitle = "Доля каждого языка, %",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

lang_adopt <- maketable("adopt_proglang") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "adopt_proglang", sw_type = "Games")$share) %>%
  filter(total != 0 & gamedev != 0)

lang_adopt <- get_sig(lang_adopt)

png("lang_adopt.png", width = 900, height = 500)
lang_adopt %>%
  filter(gamedev != 0 & value != "Base") %>%
  mutate(value = replace(value, value == "SQL(PL/SQL, T-SQL and otherprogramming extensions of SQL)", "SQL")) %>%
  mutate(value = replace(value, value == "Shell scripting languages(bash/shell/powershell)", "Shell")) %>%
  mutate(value = replace(value, value == "No, I'm not planning to adopt / migrate", "Not any")) %>%
  mutate(value = replace(value, value == "Planning to adopt / migrate to other language(s) - Write In:", "Щерук")) %>%
  ggplot(aes(x = reorder(value, -gamedev), y = gamedev, fill = sig)) +
  geom_col() +
  #coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Языки, планируемые к изучению / миграции в следующие 12 месяцев",
       subtitle = "Доля каждого языка, %",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

os_used <- maketable(variable = "os_devenv") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "os_devenv", sw_type = "Games")$share)

os_used <- get_sig(os_used)

png("os_used.png", width = 900, height = 500)
os_used %>%
  filter(gamedev != 0 & value != "Base") %>%
  ggplot(aes(x = reorder(value, gamedev), y = gamedev, fill = sig)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Пользование операционными системами",
       subtitle = "Доля ОС, %",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

target_platforms <- maketable(variable = "target_platforms") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "target_platforms", sw_type = "Games")$share)

target_platforms <- get_sig(target_platforms)

png("target_platform.png", width = 900, height = 500)
target_platforms %>%
  filter(gamedev != 0 & value != "Base") %>%
  ggplot(aes(x = reorder(value, gamedev), y = gamedev, fill = sig)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Целевые платформы разработки",
       subtitle = "Доля каждой платформы, %",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

target_os <- maketable(variable = "^target_os\\.", filter = "desktop") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "^target_os\\.", sw_type = "Games", filter = "desktop")$share)
  
target_os <- get_sig(target_os)

png(filename="target_os.png", width = 900, height = 500)
target_os %>%
  filter(value != "Base") %>%
  ggplot(aes(x = reorder(value, gamedev), y = gamedev, fill = sig)) +
  coord_flip() +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Целевая ОС (для тех, кто разрабатывает приложения для ПК)",
       subtitle = "Доля каждой ОС, %",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

mobile_os <- maketable(variable = "mobile_target_os\\.", filter = "mobile") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "mobile_target_os\\.", sw_type = "Games", filter = "mobile")$share)

mobile_os <- get_sig(mobile_os)

mobile_os_2 <- maketable(variable = "mobile_target_os_overall", filter = "mobile") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "mobile_target_os_overall", sw_type = "Games", filter = "mobile")$share)

mobile_os_2 <- get_sig(mobile_os_2)

png(filename="mobile_os_2.png", width = 900, height = 500)
mobile_os_2 %>%
  filter(value != "Base") %>%
  ggplot(aes(x = reorder(value, gamedev), y = gamedev, fill = sig)) +
  coord_flip() +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Целевая ОС (для тех, кто разрабатывает приложения для смартфонов)",
       subtitle = "Доля каждой ОС, %",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

open_source <- maketable(variable = "contribute_os") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "contribute_os", sw_type = "Games")$share)

open_source <- get_sig(open_source)

png(filename="open_source.png", width = 900, height = 500)
open_source %>%
  filter(value != "Base") %>%
  ggplot(aes(x = reorder(value, gamedev), y = gamedev, fill = sig)) +
  coord_flip() +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Участие в проектах с открытым исходным кодом",
       subtitle = "% для варианта ответа",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

hours_code_job <- maketable(variable = "hours_code_job") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "hours_code_job", sw_type = "Games")$share)

hours_code_job <- get_sig(hours_code_job)

hours_code_job$value <- factor(hours_code_job$value, levels = c("32 hours a week or more",
                                                    "17-31 hours a week", 
                                                    "9-16 hours a week",
                                                    "3-8 hours a week", 
                                                    "1-2 hours a week", 
                                                    "Less than 1 hour a week", 
                                                    "Base"))

png(filename = "hours_code_job.png", width = 900, height = 500)
hours_code_job %>%
  filter(value != "Base") %>%
  ggplot(aes(x = value, y = gamedev, fill = sig)) +
  #coord_flip() +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Сколько часов в неделю вы программируете на работе?",
       subtitle = "% для варианта ответа",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust=1),
        legend.position = "none")
dev.off()

hours_code_hobby <- maketable(variable = "hours_code_hobby") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "hours_code_hobby", sw_type = "Games")$share)

hours_code_hobby <- get_sig(hours_code_hobby)

hours_code_hobby$value <- factor(hours_code_hobby$value, levels = c("I don’t have a side project",
                                                                "32 hours a week or more",
                                                                "17-32 hours a week", 
                                                                "9-16 hours a week",
                                                                "3-8 hours a week", 
                                                                "1-2 hours a week", 
                                                                "Less than 1 hour a week", 
                                                                "Base"))

png(filename = "hours_code_hobby.png", width = 900, height = 500)
hours_code_hobby %>%
  filter(value != "Base") %>%
  ggplot(aes(x = value, y = gamedev, fill = sig)) +
  #coord_flip() +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Сколько времени вы посвящаете разработке личных проектов или проектов,\nне связанных с основной работой?",
       subtitle = "% для варианта ответа",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")
dev.off()

lifestyle_infosource <- maketable(variable = "lifestyle_infosource") %>%
  rename(total = "share") %>%
  add_column(gamedev = maketable(variable = "lifestyle_infosource", sw_type = "Games")$share)

lifestyle_infosource <- get_sig(lifestyle_infosource)

png(filename = "lifestyle_infosource.png", width = 900, height = 500)
lifestyle_infosource %>%
  filter(value != "Base") %>%
  ggplot(aes(x = reorder(value, gamedev), y = gamedev, fill = sig)) +
  coord_flip() +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Какие источники информации вы используете?",
       subtitle = "% для варианта ответа",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")
dev.off()

laptop_or_desktop <- maketable(variable = "laptop_or_desktop") %>%
  rename(total = "share") %>%
  add_column(gamedev = maketable(variable = "laptop_or_desktop", sw_type = "Games")$share)

laptop_or_desktop <- get_sig(laptop_or_desktop)

png(filename = "laptop_or_desktop.png", width = 900, height = 500)
laptop_or_desktop %>%
  filter(value != "Base") %>%
  ggplot(aes(x = value, y = gamedev, fill = sig)) +
  #coord_flip() +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Вы предпочитаете ноутбук или десктоп?",
       subtitle = "% для варианта ответа",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")
dev.off()

lifestyle_hobbies <- maketable(variable = "lifestyle_hobbies") %>%
  rename(total = "share") %>%
  add_column(gamedev = maketable(variable = "lifestyle_hobbies", sw_type = "Games")$share)

lifestyle_hobbies <- get_sig(lifestyle_hobbies)

png(filename = "lifestyle_hobbies.png", width = 900, height = 500)
lifestyle_hobbies %>%
  filter(value != "Base") %>%
  ggplot(aes(x = reorder(value, gamedev), y = gamedev, fill = sig)) +
  coord_flip() +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "Чем вы занимаетесь в свободное время?",
       subtitle = "% для варианта ответа",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")
dev.off()

lifestyle_pet <- maketable(variable = "lifestyle_pet", filter = "pets") %>%
  rename(total = share) %>%
  add_column(gamedev = maketable(variable = "lifestyle_pet", sw_type = "Games", filter = "pets")$share) %>%
  filter(total != 0 & gamedev != 0)

lifestyle_pet <- get_sig(lifestyle_pet)

png(filename = "lifestyle_pet.png", width = 900, height = 500)
lifestyle_pet %>%
  filter(value != "Base") %>%
  ggplot(aes(x = reorder(value, gamedev), y = gamedev, fill = sig)) +
  coord_flip() +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = sig_levels) +
  geom_label(aes(label = round(gamedev * 100, 0)), fill = "white", size = 5) +
  labs(x = "", y = "",
       title = "У вас есть домашние животные?",
       subtitle = "% для варианта ответа",
       caption = "На основе исследования JetBrains Developer EcoSystem 2020") +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")
dev.off()
