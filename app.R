library(shiny) # створення інтерактивних веб-додатків
library(readxl) # для читання xlsx
library(dplyr) # маніпуляції з таблицями
library(ggplot2) # побудова графіків
library(DT)  # інтерактивні таблиці

# Завантаження та підготовка даних
df <- read_excel("Online_Retail.xlsx", sheet = "Online Retail") %>%
  filter(!is.na(CustomerID)) %>% # фільтр рядків Без кастомера
  mutate(
    InvoiceDate = as.Date(InvoiceDate), # перетворюємо в формат дати
    TotalPrice = Quantity * UnitPrice
  )

# Кольори для графіків 
main_color <- "#2C3E50" # основа і текст
accent_color <- "#18BC9C" # акцентований

# UI
ui <- fluidPage(   # UI + CSS-стилі для UI
  tags$head(
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', sans-serif; background-color: #FEF3E2; }
      h2, h4 { color: #2C3E50; }
      .shiny-output-error { color: red; }
    "))
  ),
  
  titlePanel("Online Retail Dashboard", windowTitle = "Online Retail Dashboard"), # заголовок додатку
  
  sidebarLayout( # вибираємо країни та дату
    sidebarPanel(
      selectInput("country", "Країна", choices = c("Усі", sort(unique(df$Country))), selected = "Усі"),
      dateRangeInput("date_range", "Діапазон дат:",
                     start = min(df$InvoiceDate), end = max(df$InvoiceDate)),
      width = 3
    ),
    
    mainPanel( # верхны статичні блоки для показників
      fluidRow(
        column(4, wellPanel(style = "background-color: #E8F8F5;", 
                            h4("Замовлень", style = "color: #2C3E50;"), 
                            textOutput("totalOrders"))),
        column(4, wellPanel(style = "background-color: #FEF9E7;", 
                            h4("Клієнтів", style = "color: #2C3E50;"), 
                            textOutput("totalCustomers"))),
        column(4, wellPanel(style = "background-color: #FDEDEC;", 
                            h4("Виручка (€)", style = "color: #2C3E50;"), 
                            textOutput("totalRevenue")))
      ),
      tabsetPanel(   # вкладки з графіками
        tabPanel("Виручка по країнах", plotOutput("countryPlot", height = 400)),
        tabPanel("Продажі по датах", plotOutput("datePlot", height = 400)),
        tabPanel("Таблиця товарів", DTOutput("productTable")),
        tabPanel("Середня ціна по країнах", plotOutput("avgPricePlot", height = 400)),
        tabPanel("Кругова діаграма", plotOutput("pieChart", height = 400))
      ),
      width = 9
    )
  )
)

# Серверна частинка
server <- function(input, output) {
  
  filtered <- reactive({ 
    data <- df
    if (input$country != "Усі") {
      data <- data[data$Country == input$country, ]
    }
    data <- data[data$InvoiceDate >= input$date_range[1] & data$InvoiceDate <= input$date_range[2], ]
    data
  })
  
  # статичні дані
  
  
  output$totalOrders <- renderText({
    format(n_distinct(filtered()$InvoiceNo), big.mark = " ")
  }) # к-сть унікальних замовлень
  
  output$totalCustomers <- renderText({
    format(n_distinct(filtered()$CustomerID), big.mark = " ")
  }) # к-сть унікальних клієнтів
  
  output$totalRevenue <- renderText({
    format(round(sum(filtered()$TotalPrice), 2), big.mark = " ")
  })   # загальна виручка
  
  # таблиці
  
  output$countryPlot <- renderPlot({
    data <- filtered() %>%
      group_by(Country) %>%
      summarise(Revenue = sum(TotalPrice)) %>%
      arrange(desc(Revenue)) %>%
      top_n(10, Revenue)
    
    ggplot(data, aes(x = reorder(Country, Revenue), y = Revenue), fill = Country) +
    geom_bar(stat = "identity", fill = accent_color) +
    coord_flip() +
      labs(
        x = "Країна",
        y = "Виручка",
        title = "Топ-10 країн за виручкою"
      ) +
      theme_minimal(base_family = "Helvetica") +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.minor = element_blank()
      )
  })
  # топ-10 краън за виручкою
  
  output$avgPricePlot <- renderPlot({
    data <- filtered() %>%
      group_by(Country) %>%
      summarise(СередняЦіна = mean(UnitPrice, na.rm = TRUE)) %>%
      arrange(desc(СередняЦіна)) %>%
      top_n(10, СередняЦіна)
    
    ggplot(data, aes(x = reorder(Country, СередняЦіна), y = СередняЦіна)) +
      geom_col(fill = "#3498DB") +
      coord_flip() +
      labs(
        title = "Топ-10 країн за середньою ціною товару",
        x = "Країна", y = "Середня ціна (€)"
      ) +
      theme_minimal(base_family = "Helvetica") +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = main_color),
        axis.title = element_text(size = 14, color = main_color),
        axis.text = element_text(size = 12),
        panel.grid.major = element_line(color = "#D0D0D0", linetype = "dotted"),
        panel.grid.minor = element_blank()
      )
  }) # cередня ціна товару по країнах (топ-10)
  
  output$datePlot <- renderPlot({
    data <- filtered() %>%
      group_by(InvoiceDate) %>%
      summarise(Revenue = sum(TotalPrice))
    
    ggplot(data, aes(x = InvoiceDate, y = Revenue)) +
      geom_line(color = main_color, size = 1) +
      labs(x = "Дата", y = "Виручка", title = "Продажі за датами") +
      theme_minimal(base_family = "Helvetica") +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = main_color),
        axis.title = element_text(size = 14, color = main_color),
        axis.text = element_text(size = 12),
        panel.grid.major = element_line(color = "#D0D0D0", linetype = "dotted"),
        panel.grid.minor = element_blank()
      )
  })  # виручка за датами
  
  output$productTable <- renderDT({
    filtered() %>%
      group_by(Description) %>%
      summarise(
        Кількість = sum(Quantity),
        Виручка = round(sum(TotalPrice), 2)
      ) %>%
      arrange(desc(Виручка)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  }) # таблиця товарів
  
  output$pieChart <- renderPlot({
    data <- filtered() %>%
      group_by(Description) %>%
      summarise(Revenue = sum(TotalPrice, na.rm = TRUE)) %>%
      arrange(desc(Revenue)) %>%
      slice_head(n = 5) %>%
      mutate(
        Percent = round(Revenue / sum(Revenue) * 100, 1),
        Label = paste0(Description, " (", Percent, "%)")
      )
    
    ggplot(data, aes(x = "", y = Revenue, fill = Label)) +
      geom_col(width = 1, color = "white") +
      coord_polar("y") +
      scale_fill_brewer(palette = "Pastel1") +
      labs(title = "Топ-5 товарів за виручкою") +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_blank()
      )
  })  # топ-5 товарів по виручці
}

# df %>%
#  filter(as.Date(InvoiceDate) == as.Date("2010-12-01")) %>%
#  summarise(
#   Revenue = sum(TotalPrice, na.rm = TRUE),
#    UniqueOrders = n_distinct(InvoiceNo),
#    UniqueCustomers = n_distinct(CustomerID)
#  ) # перевірка статичних в консоль



# Запуск додатку
shinyApp(ui = ui, server = server)
