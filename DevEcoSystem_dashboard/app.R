#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# install.packages("plotly")
# install.packages("shinythemes")

library(shiny)
library(tidyverse)
library(plotly)

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
maketable <- function(variable, dataset = data, t_country = "Total", sw_type = "Total", base = "weighted", sort = FALSE, filter = "none") {
    
    if("Total" %in% t_country) {dataset} else {dataset = subset(dataset, country %in% t_country)}
    
    if(sw_type == "Total") {dataset}
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
    else {sig = "N/A"}
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

sig_levels <- c("#00bc8c", "darkred", "#444444", "#444444")
names(sig_levels) <- levels(factor(c("high", "no", "low", "N/A")))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("JetBrains Dev EcoSystem dashboard"),
    
    theme = shinythemes::shinytheme("darkly"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("country",
                        "Choose country",
                        choices = c("Total", sort(unique(data$country))),
                        selected = "Total",
                        multiple = TRUE),
            selectInput("software",
                        "Choose developed software type:",
                        choices = c("Total", str_replace_all(grep("sw_types", names(data), value = TRUE)[2:18], "sw_types_developed.", "")),
                        selected = "Total"),
            checkboxInput("sig", "Show significant difference vs. Total"),
            actionButton('show_about', 'About')
        ),

        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(
            tabPanel(title = "Demographics", 
                     plotlyOutput("age_range"),
                     plotlyOutput("code_yrs")),
            tabPanel(title = "Employment",
                     plotlyOutput("employment_status")),
            tabPanel(title = "Software"),
            tabPanel(title = "Lifestyle")
            ))
    )
)

        
# Define server logic required to draw a histogram
server <- function(input, output) {
    age_range <- reactive({
        maketable(variable = "age_range") %>%
            rename(total = share) %>%
            add_column("Data" = maketable(variable = "age_range", sw_type = input$software, t_country = input$country)$share)
    })

    age_range_sig <- reactive({
        if(input$sig == TRUE) {get_sig(age_range())} else {mutate(age_range(), sig = "N/A")}
        })
    
    output$age_range <- plotly::renderPlotly({
        age_range_sig() %>%
            filter(value != "Base") %>%
            ggplot(aes(x = value, y = Data, fill = sig)) +
            #coord_flip() +
            geom_col() +
            scale_y_continuous(labels = scales::percent) +
            scale_fill_manual(values = sig_levels) +
            labs(x = "", y = "", title = "Age, years") +
            theme(panel.grid.minor.x = element_blank(),
                  #axis.text.x = element_text(angle = 90, hjust=1),
                  legend.position = "none")
    })
    
    code_yrs <- reactive({
        maketable(variable = "code_yrs") %>%
            rename(total = share) %>%
            add_column("Data" = maketable(variable = "code_yrs", sw_type = input$software, t_country = input$country)$share) %>%
                    mutate(value = replace(value, value == "I don't have any professional coding experience", "Not any")) %>%
                    mutate(value = factor(value, levels = c("Less than 1 year",
                                                            "1–2 years",
                                                            "3–5 years",
                                                            "6–10 years",
                                                            "11+ years",
                                                            "Not any",
                                                            "Base")))
    })
    
    code_yrs_sig <- reactive({
        if(input$sig == TRUE) {get_sig(code_yrs())} else {mutate(code_yrs(), sig = "N/A")}
        })
    
    output$code_yrs <- plotly::renderPlotly({
        code_yrs_sig() %>%
            filter(value != "Base") %>%
            ggplot(aes(x = value, y = Data, fill = sig)) +
            #coord_flip() +
            geom_col() +
            scale_y_continuous(labels = scales::percent) +
            scale_fill_manual(values = sig_levels) +
            labs(x = "", y = "", title = "Professional coding experience, years") +
            theme(panel.grid.minor.x = element_blank(),
                  #axis.text.x = element_text(angle = 90, hjust=1),
                  legend.position = "none")
    })
    
    # output$employment_status <- plotly::renderPlotly({
    #     maketable(variable = "employment_status") %>%
    #         rename(total = share) %>%
    #         add_column("Data" = maketable(variable = "employment_status", sw_type = input$software, t_country = input$country)$share) %>%
    #         mutate(value = replace(value,
    #                                which(value %in% c("Fully employed by a company / organization",
    #                                                   "Freelancer (a person pursuing a profession without a long-term commitment to any one employer)",
    #                                                   "Self-employed (a person earning income directly from their own business, trade, or profession)",
    #                                                   "Partially employed by a company / organization")),
    #                                c("Fully employed", "Freelancer", "Self-employed", "Partially employed"))) %>%
    #         {if(input$sig == TRUE) {get_sig(.)} else {mutate(., sig = "N/A")}} %>%
    #         filter(value != "Base") %>%
    #         ggplot(aes(x = reorder(value, -Data), y = Data, fill = sig)) +
    #         #coord_flip() +
    #         geom_col() +
    #         scale_y_continuous(labels = scales::percent) +
    #         scale_fill_manual(values = sig_levels) +
    #         labs(x = "", y = "", title = "What is your employment status?") +
    #         theme(panel.grid.minor.x = element_blank(),
    #               axis.text.x = element_text(angle = 75, hjust=1),
    #               legend.position = "none")
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
