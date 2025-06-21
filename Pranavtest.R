library(shiny)
library(tidyverse)

data <- read.csv("superstore_data.csv")

data$Dt_Customer <- as.Date(data$Dt_Customer, "%m/%d/%Y")
data$Year <- year(data$Dt_Customer)
data$Age <- 2025 - data$Year_Birth
data$Total_Spending <- rowSums(data[, c("MntWines", "MntFruits", "MntMeatProducts",
                                        "MntFishProducts", "MntSweetProducts", "MntGoldProds")])


ui <- fluidPage(
  titlePanel("Superstore Customer Purchase Analysis"),
  tabsetPanel(
    tabPanel("1. Year-wise Analysis",
             fluidRow(
               column(1,plotOutput("enrollment_plot")),
               column(1,plotOutput("year_spend_plot"))
             ),
             plotOutput("marital_spend_plot")
    ),
tabPanel(#2. Age-based Purchase Behavior",
             sliderInput("age_range", "Select Age Range:", min = min(data$Age),
                         max = max(data$Age), value = c(25, 60)),
             plotOutput("Age_purchase_plot")
  ),

tabPanel("3. Income vs Spending(Corellation Plot",
         selectInput("edu_filter", "Select Education Level:", choices = unique(data$Education),
                     selected = unique(data$Education), multiple = TRUE),
         selectInput("marital_filter", "Select Marital Status:", choices = unique(data$Marital_Status),
                     selected = unique(data$Marital_Status), multiple = TRUE),
         plotOutput("Income_vs_Spend")),

tabPanel("4. Best-selling Category",
         selectInput("year_input", "Select Year:", choices = c(unique(data$Year)), selected = "Year"),
         selectInput("marital_input", "Select Marital Status:", choices = c(unique(data$Marital_Status)), selected = "Marital_Status"),
         plotOutput("Best_category")),

tabPanel("5.In-store vs Online",
         plotOutput("purchase_channel_plot"),
         downloadButton("downloadPlot", "Download Plot"))
))

server <- function(input, output, session) {
  
  output$enrollment_plot <- renderPlot({
    input_data <- data %>%
      group_by(Year) %>%
      summarise(Count = n())
    
    ggplot(input_data, aes(x = Year, y = Count)) +
      geom_col(fill = "purple") +
      labs(title = "Year wise customer enrolment", x = "Year", y = "Number of Customers")
  })
  
  output$year_spend_plot <- renderPlot({
    spend_data <- data %>%
      group_by(Year) %>%
      summarise(TotalSpending = sum(Total_Spending))#Yearwise total spent on all category
    
    ggplot(spend_data, aes(x = Year, y = TotalSpending)) +
      geom_col(fill = "pink") +
      labs(title = "Total Spending by Year", x = "Year", y = "Total Spending")
  })
  
  output$marital_spend_plot <- renderPlot({
    marital_spend <- data %>%
      group_by(Marital_Status) %>%
      summarise(TotalSpending = sum(Total_Spending))
    
    ggplot(marital_spend, aes(x = Marital_Status, y = TotalSpending, fill = Marital_Status)) +
      geom_col() +
      labs(title = "Total Spending by Marital Status", x = "Marital Status", y = "Spending") 
  
  })
  
  #2. Age-based Purchase Behavior
  output$age_purchase_plot <- renderPlot({
    filtered <- data %>%
      filter(Age >= input$age_range[1], Age <= input$age_range[2]) %>%
      mutate(TotalPurchases = NumWebPurchases + NumCatalogPurchases + NumStorePurchases)
    
    ggplot(filtered, aes(x = Age, y = TotalPurchases)) +
      geom_point(color = "darkgreen") +
      labs(title = "Purchases by Age", x = "Age", y = "Total Purchases")
  })
  
  output$income_vs_spend <- renderPlot({
    filtered <- data %>%
      filter(Education %in% input$edu_filter, Marital_Status %in% input$marital_filter)
    
    ggplot(filtered, aes(x = Income, y = Total_Spending)) +
      geom_point(color = "purple", alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Income vs. Total Spending", x = "Income", y = "Total Spending")
  })
  
  output$best_category <- renderPlot({
    filtered <- data
    
    if (input$year_input != "All") {
      filtered <- filtered %>% filter(Year == input$year_input)
    }
    if (input$marital_input != "All") {
      filtered <- filtered %>% filter(Marital_Status == input$marital_input)
    }
    
    long_data <- filtered %>%
      select(MntWines, MntFruits, MntMeatProducts, MntFishProducts,
             MntSweetProducts, MntGoldProds) %>%
      summarise_all(sum) %>%
      pivot_longer(cols = everything(), names_to = "Category", values_to = "Total") %>%
      arrange(desc(Total))
    
    ggplot(long_data, aes(x = reorder(Category, -Total), y = Total, fill = Category)) +
      geom_col() +
      labs(title = "Best-Selling Categories", x = "Category", y = "Total Sales") 
  })
  
  output$purchase_channel_plot <- renderPlot({
    channel_data <- data %>%
      summarise(
        InStore_Purchases = sum(NumStorePurchases),
        Online_Purchases = sum(NumWebPurchases + NumCatalogPurchases),
        InStore_Spending = sum(Total_Spending * NumStorePurchases /
                                 (NumWebPurchases + NumCatalogPurchases + NumStorePurchases)),
        Online_Spending = sum(Total_Spending * (NumWebPurchases + NumCatalogPurchases) /
                                (NumWebPurchases + NumCatalogPurchases + NumStorePurchases))
      )
    
    channel_long <- data.frame(
      Channel = c("In-Store", "Online"),
      Purchases = c(channel_data$InStore_Purchases, channel_data$Online_Purchases),
      Spending = c(channel_data$InStore_Spending, channel_data$Online_Spending)
    )
    
    ggplot(channel_long, aes(x = Channel)) +
      geom_col(aes(y = Purchases, fill = "Purchases"), position = "dodge") +
      geom_col(aes(y = Spending, fill = "Spending"), position = "dodge") +
      labs(title = "In-store vs. Online Purchases and Spending", y = "Count/Amount") +
      scale_fill_manual(values = c("Purchases" = "yellow", "Spending" = "black"))
  })
  
  # Download handler
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("channel_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      channel_data <- data %>%
        summarise(
          InStore_Purchases = sum(NumStorePurchases),
          Online_Purchases = sum(NumWebPurchases + NumCatalogPurchases)
        )
      barplot(height = as.numeric(channel_data), names.arg = c("In-Store", "Online"),
              col = c("red", "green"), main = "In-store vs Online Purchases")
      dev.off()
    }
  )
}
shinyApp(ui, server)