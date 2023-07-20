library(shiny)
library(shinyWidgets)
library(randomForest)
library(shinyalert)
library(shinythemes)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #c65a5a;
      }
      h1 {
        margin-left:100px;
        font-size: 80px;
        color: #80bcde;
      }
      h3 {
        color: #f5ccda;
      }
      img {
        margin-left: 50px;
      }
      .btn-predict {
        background-color: #3498db;
        color: white;
        border: none;
        padding: 10px 20px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 16px;
        margin-left: 170px;
        transition-duration: 0.4s;
        cursor: pointer;
        box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);
      }
      .btn-predict:hover {
        box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 12px 40px 0 rgba(0,0,0,0.19);
      }
    "))
  ),
  theme = shinytheme("flatly"),
  fluidRow(
    column(width = 2),
    column(width = 1,
           img(src = "https://cdn-icons-png.flaticon.com/512/641/641825.png", height = 150, width = 150)
    ),
    column(width = 7,
           h1("Laptop Price Predict")
    ),
    column(width = 2)
  ),
  sidebarLayout(
    sidebarPanel(
      actionButton(class = "btn-predict", 
        inputId = 'predict',
        label = 'Predict'
      ),
      pickerInput(
        inputId = "brand",
        label = "Brand",
        choices = c('Acer', 'Apple', 'Asus', 'Chuwi','Dell','Fujitsu', 'Google','HP','Huawei','Lenovo','LG','Mediacom','Microsoft','MSI','Razer','Samsung','Toshiba','Vero','Xiaomi'),
        selected = 'Acer',
        options = list(`actions-box` = TRUE))
      ,
      pickerInput(
        inputId = "type",
        label = "Type",
        choices = c('2 in 1 Convertible', 'Gaming', 'Netbook', 'Notebook', 'Ultrabook', 'Workstation'),
        selected = 'Gaming',
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = "ram",
        label = "RAM",
        choices = c(2, 4, 6, 8, 12, 16, 24, 32, 64),
        selected = 8,
        options = list(`actions-box` = TRUE)
      ),
      textInput(
        inputId = "weight",
        label = "Weight"
      ),
      pickerInput(
        inputId = "gpu",
        label = "GPU(in GB)",
        choices = c('AMD','Intel','Nvidia'),
        selected = 'Nvidia',
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = "cpu",
        label = "CPU(in GB)",
        choices = c('AMD Processor','Intel Core i3','Intel Core i5','Intel Core i7','Other Intel Processor'),
        selected = 'Intel Core i7',
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = "os",
        label = "OS",
        choices = c('Mac','Others/No OS/Linux','Window'),
        selected = 'Window',
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = "hdd",
        label = "HDD",
        choices = c(0, 32, 128, 1000, 2000),
        selected = 128,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = "ssd",
        label = "SSD",
        choices = c(0, 1, 2, 8,16, 32, 64, 256, 512, 768, 1000, 1024),
        selected = 128,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = "touch",
        label = "Touch Screen",
        choices = c('Yes', 'No'),
        selected = 'No',
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = "ips",
        label = "IPS",
        choices = c('Yes', 'No'),
        selected = 'No',
        options = list(`actions-box` = TRUE)
      ),  
      pickerInput(
        inputId = "resolution",
        label = "Screen Resolution",
        choices = c('1920x1080', '1366x768', '1600x900', '3840x2160', '3200x1800', '2880x1800', '2560x1600', '2560x1440', '2304x1440'),
        selected = '1920x1080',
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = "inches",
        label = "Inches",
        choices = c(10.1, 11.3, 11.6, 12, 12.3, 12.5, 13.3, 13.5, 13.9, 14, 14.1, 15, 15.4, 15.6, 17, 17.3, 18.4),
        selected = 15.4,
        options = list(`actions-box` = TRUE)
      )
    ),
    mainPanel(
      h3("Selected Brand:"),
      verbatimTextOutput("selected_brand"),
      h3("Selected Type:"),
      verbatimTextOutput("selected_type"),
      h3("Selected RAM:"),
      verbatimTextOutput("selected_ram"),
      h3("Selected Weight:"),
      verbatimTextOutput("selected_weight"),
      h3("Selected GPU:"),
      verbatimTextOutput("selected_gpu"),
      h3("Selected CPU:"),
      verbatimTextOutput("selected_cpu"),
      h3("Selected OS:"),
      verbatimTextOutput("selected_os"),
      h3("Selected HDD:"),
      verbatimTextOutput("selected_hdd"),
      h3("Selected SSD:"),
      verbatimTextOutput("selected_ssd"),
      h3("Selected Touch Screen:"),
      verbatimTextOutput("selected_touch"),
      h3("Selected IPS:"),
      verbatimTextOutput("selected_ips"),
      h3("Calculated PPI:"),
      verbatimTextOutput("calculated_ppi"),
      textOutput("predict_o")
    )
  )
)

server <- function(input, output) {
  output$selected_brand <- renderText({
    paste("Brand:", input$brand)
  })

  output$selected_type <- renderText({
    paste("Type:", input$type)
  })

  output$selected_ram <- renderText({
    paste("RAM(in GB):", input$ram)
  })
  
  output$selected_weight <- renderText({
    paste("Weight of the Laptop:", input$weight)
  })  
  
  output$selected_gpu <- renderText({
    paste("GPU:", input$gpu)
  })  

  output$selected_cpu <- renderText({
    paste("CPU:", input$cpu)
  })  

  output$selected_os <- renderText({
    paste("Operating System:", input$os)
  })

  output$selected_hdd <- renderText({
    paste("HDD(in GB):", input$hdd)
  }) 

  output$selected_ssd <- renderText({
    paste("SSD(in GB):", input$ssd)
  }) 
  
  output$selected_touch <- renderText({
    paste("Touch Screen:", input$touch)
  })  

  output$selected_ips <- renderText({
    paste("In-Plane Switching(Ips):", input$ips)
  }) 
  
  
  calculate_ppi <- function(){
    X_res = as.numeric(strsplit(input$resolution, "x")[[1]][1])
    Y_res = as.numeric(strsplit(input$resolution, "x")[[1]][2])
    inches = as.numeric(input$inches)
    ppi = as.numeric(((X_res^2) + (Y_res^2))^0.5/inches)
    return(ppi)
  }
  
  output$calculated_ppi <- renderText({
    paste("Resolution:", calculate_ppi())
  })
  
  predict_price <- function(){
    lst_company = c('Acer', 'Apple', 'Asus', 'Chuwi','Dell','Fujitsu', 'Google','HP','Huawei','Lenovo','LG','Mediacom','Microsoft','MSI','Razer','Samsung','Toshiba','Vero','Xiaomi')
    company <- as.numeric(lst_company == input$brand)
    lst_type = c('2 in 1 Convertible', 'Gaming', 'Netbook', 'Notebook', 'Ultrabook', 'Workstation')
    type <- as.numeric(lst_type == input$type)
    lst_gpu = c('AMD','Intel','Nvidia')
    gpu <- as.numeric(lst_gpu == input$gpu)
    lst_os = c('Mac','Others/No OS/Linux','Window')
    os <- as.numeric(lst_os == input$os)
    lst_cpu = c('AMD Processor','Intel Core i3','Intel Core i5','Intel Core i7','Other Intel Processor')
    cpu <- as.numeric(lst_cpu == input$cpu)
    ram <- as.numeric(input$ram)
    weight <- as.numeric(input$weight)
    ssd <- as.numeric(input$ssd)
    hdd <- as.numeric(input$hdd)
    touch <- ifelse(input$touch == 'Yes', 1, 0)
    ips <- ifelse(input$ips == 'Yes', 1, 0)
    ppi <- calculate_ppi()
    df <- read.csv('data_clean.csv')
    column_names <- colnames(df[-44])
    inputt <- c(company, type, gpu, os, cpu, ram, weight, ssd, hdd, touch, ips, ppi)
    data_pre <- data.frame(matrix(inputt, ncol = length(inputt)))
    colnames(data_pre) <- column_names
    model <- readRDS("model.rds")
    prediction <- predict(model, newdata = data_pre)
    prediction <- format(round(prediction*26591.90,2), big.mark = ".", decimal.mark = ",")
    
    return(prediction)
  }
  
  observeEvent(input$predict,{
    predict_value <- predict_price()
    shinyalert::shinyalert(
      title = "Predict Price",
      text = paste("Price = ", predict_value, "VND"),
      type = 'success'
    )
  })

}

shinyApp(ui = ui, server = server)