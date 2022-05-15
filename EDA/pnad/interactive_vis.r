library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(sf)
library(rgeos)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(shinyWidgets)


pnad <- read_csv("pnad.csv")

brazil <- ne_states(country = "brazil", returnclass = "sf")

pnad_counties <- pnad %>% filter(year %in% c(2019,2020,2021)) %>% group_by(uf) %>% 
  summarise(mean_age =  weighted.mean(age, w = sample_weight, na.rm = T), mean_income = weighted.mean(income, w = sample_weight, na.rm = T), 
            mean_working_hours =  weighted.mean(weekly_working_hours, w = sample_weight, na.rm = T),
            percent_women = data.frame(table(sex)/sum(table(sex)))$Freq[2],
            precent_iliterate = data.frame(table(literate)/sum(table(literate)))$Freq[1])

brazil_dat <- brazil %>% left_join(pnad_counties, by=c("name" = "uf"))


ui <- fluidPage(
  theme = shinythemes::shinytheme('superhero'),
  titlePanel("PNAD-VIS"),
  tabsetPanel(
    tabPanel("Map", sidebarLayout(
                      sidebarPanel(sliderInput("year_map", "Select Years:", min = 2019, 
                           max = 2021, value = c(2019, 2021), ticks = T, sep = ""),
                        varSelectInput("selectedvar_map", "Variable:", pnad_counties[,-1], selected = "mean_age"),
                        checkboxInput("state_names_map", "Enable State Names", value = TRUE),
                        checkboxInput("text_values_map", "Show Values", value = TRUE),
                        h2("About:"),
                        h4("Continuous National Household Sample Survey"),
                        p("The Continuous National Household Sample Survey (PNADc) is a survey carried out by the Brazilian Institute of Geography and Statistics (IBGE) on a sample of Brazilian households that investigates various socioeconomic characteristics of society, such as population, education, work, income, housing, social security, migration, fertility, marriage, health, nutrition, etc., among others, that are included according to the Brazilian government needs. The research is carried out in all regions of Brazil, including the rural areas of Rondônia, Acre, Amazonas, Roraima, Pará and Amapá."),
                        p("More info and the dataset can be found ", a(href='https://www.ibge.gov.br/en/statistics/multi-domain/living-conditions-poverty-and-inequality/18083-annual-dissemination-pnadc3.html?=&t=o-que-e]', "here.")),
                        p("The full dataset contains over 300 variables. So, for the purpose of basic income related analysis, only 10 variables were croped from years 2019 - 2021 datasets.")
                        
                      ),
                      mainPanel(
                        plotOutput("map", width = "auto", height = "900px")
                        ))
                      ),
    tabPanel("Histogram", 
           sidebarLayout(
             sidebarPanel(
               sliderInput("year_hist", "Select Years:", min = 2019, 
                           max = 2021, value = c(2019, 2021), ticks = T, sep = ""),
               numericInput(inputId = "bin_hist", label = "Number of Bins", value = 10),
               checkboxInput("rm_outliers_hist", "Remove Outliers", value = F),
               checkboxInput("log_scale_hist", "Logarithmic Scale", value = F),
               checkboxInput("rm_top5_hist", "Remove Top 5%", value = F),
               varSelectInput("selectedvar_hist", "Variable:", pnad[,-c(1,2,3,5,6,7,10)], selected = "age"),
               checkboxGroupInput("states_hist", label = "Select States", choices = pnad_counties$uf, selected = pnad_counties$uf)
             ),
             mainPanel(plotOutput("hist"))
             )
           ),
    tabPanel("Barplot", 
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year_bar", "Select Years:", min = 2019, 
                             max = 2021, value = c(2019, 2021), ticks = T, sep = ""),
                 varSelectInput("selectedvar_bar", "Variable:", pnad[,c(3,5,6,7)], selected = "sex"),
                 checkboxGroupInput("states_bar", label = "Select States", choices = pnad_counties$uf, selected = pnad_counties$uf)
               ),
               mainPanel(plotOutput("bar"))
             )),
    tabPanel("Scatterplot", 
                      sidebarLayout(
                        sidebarPanel(
                          varSelectInput("selectedvar1_scatter", "Group Variable (y):", pnad[,-c(2,10)], selected = "age"),
                          varSelectInput("selectedvar2_scatter", "Mean Variable (x):", pnad[,-c(1,2,3,5,6,7,10)], selected = "income"),
                          checkboxGroupInput("states_bar", label = "Select States", choices = pnad_counties$uf, selected = pnad_counties$uf)
                        ),
                        mainPanel(plotOutput("scatter"))
                      )
    )
  )
  )


server <- function(input, output) {
  
  output$map <- renderPlot({
      theme_set(theme_void())
    
      pnad_counties <- pnad %>% filter(year >= input$year_map[1] & year <= input$year_map[2]) %>% group_by(uf) %>% 
        summarise(mean_age = weighted.mean(age, w = sample_weight, na.rm = T), 
        mean_income = weighted.mean(income, w = sample_weight, na.rm = T), 
        mean_working_hours = weighted.mean(weekly_working_hours, w = sample_weight, na.rm = T),
        percent_women = data.frame(table(sex)/sum(table(sex)))$Freq[2],
        precent_iliterate = data.frame(table(literate)/sum(table(literate)))$Freq[1])
      
      brazil_dat <- brazil %>% left_join(pnad_counties, by=c("name" = "uf"))
    
      ggplot(data = brazil_dat) + 
      theme(legend.text = element_text(color = "lightgrey"),
        legend.title = element_text(color = "lightgrey"),
        plot.title = element_text(color = "lightgrey")) +
      geom_sf(col = "white", aes(fill = !!input$selectedvar_map)) +
      geom_text(aes(x=longitude, y=latitude, label=name),
                color = "lightgrey", fontface = "bold", check_overlap = TRUE, alpha = input$state_names_map) +
      geom_text(aes(x=longitude, y=latitude + 1, label=round(!!input$selectedvar_map, digits = 2)),
                  color = "lightgrey", fontface = "bold", check_overlap = TRUE, alpha = input$text_values_map) +
      ggtitle(paste0("Map of Brazil - Variable: " , input$selectedvar_map))
      
    
  },  bg=NA, execOnResize=T)
  
  output$hist <- renderPlot({
    
    theme_set(theme_bw())
    
    pnad_data <- pnad %>% filter(year >= input$year_hist[1] & year <= input$year_hist[2])  %>% filter(uf %in% input$states_hist)
    
    if(input$rm_outliers_hist == T){
      
      var_median <- median(pnad_data[[input$selectedvar_hist]])
      var_sd <- sd(pnad_data[[input$selectedvar_hist]])
      
      pnad_data <- pnad_data %>% filter(!!input$selectedvar_hist <= (var_median + 2*var_sd) & !!input$selectedvar_hist >= (var_median - 2*var_sd))
      
    }
    
    if(input$rm_top5_hist == T){
      
      var_quant <- quantile(pnad_data[[input$selectedvar_hist]], probs = c(0.95), na.rm = T)
      
      pnad_data <- pnad_data %>% filter(!!input$selectedvar_hist <= var_quant)
      
    }
    
    if(input$log_scale_hist == T){
      
      ggplot(data = pnad_data) +
        geom_histogram(fill = "lightblue", aes(x = !!input$selectedvar_hist), bins = input$bin_hist ) +
        scale_y_continuous(trans = 'log2', labels = scales::comma) +
        scale_x_continuous(labels = scales::comma) +
        ggtitle(paste0("Histogram of Variable: " , input$selectedvar_hist)) 
        
      
    } else {
      
      ggplot(data = pnad_data) +
        geom_histogram(fill = "lightblue", aes(x = !!input$selectedvar_hist), bins = input$bin_hist ) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(labels = scales::comma) +
        ggtitle(paste0("Histogram of Variable: " , input$selectedvar_hist))

        
    }

  })
  
  output$bar <- renderPlot({
    
    theme_set(theme_bw())
    
    pnad_data <- pnad %>% filter(year >= input$year_bar[1] & year <= input$year_bar[2]) %>% filter(uf %in% input$states_bar)
    
    ggplot(data = pnad_data) +
      geom_bar(fill = "lightblue", aes(x = !!input$selectedvar_bar)) +
      scale_y_continuous(labels = scales::comma) +
      ggtitle(paste0("Barplot of Variable: " , input$selectedvar_bar)) +
      theme(axis.text.x = element_text(angle = 90))
    
    
  })
  
  output$scatter <- renderPlot({
    
    theme_set(theme_bw())
    
    pnad_data <- pnad %>% filter(uf %in% input$states_bar) %>% group_by(!!input$selectedvar1_scatter) %>% summarise(mean_var = mean(!!input$selectedvar2_scatter,  na.rm = T))
    
    ggplot(data = pnad_data) +
      geom_point(aes(y = mean_var, x = !!input$selectedvar1_scatter)) +
      theme(axis.text.x = element_text(angle = 90)) + 
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      ggtitle(paste0("Scatterplot of Variable: " , input$selectedvar1_scatter, " and ", input$selectedvar2_scatter, ". Color: ", input$selectedvar3_scatter))
    
    
  })
  
}


shinyApp(ui = ui, server = server)
 
 
  