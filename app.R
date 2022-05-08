library(ggplot2)
library(shiny)
library(dplyr)
library(rsconnect)
library(maps)
library(mapdata)
library(mapproj)
library(leaflet)
library(htmlwidgets)


ui <- fluidPage(
  h1("COVID-19 Skilled Nursing Facilities Cases and Deaths in California"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        
        h4("Instructions:"),
        verbatimTextOutput("text"),
        
        selectInput(
          inputId = "County",
          label = "Filter by Counties",
          choices = unique(df0$County),
          multiple = TRUE,
          selected = "alameda"
        ),
        selectInput(
          inputId = "xaxis",
          label = "Choose a Variable for the X-axis of the Graph",
          choices = colnames(df0),
          selected = "Total.Resident.Cases"
        ),
        selectInput(
          inputId = "yaxis", 
          label = "Choose a Variable for the Y-axis of the Graph", 
          choices = colnames(df0),
          selected = "Total.Resident.Deaths"
        )
        
      )
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Purpose and Data",
                           h4("As of May 2022, the World Health Organization has estimated", strong("14.9 million deaths"),"were associated with the COVID-19 pandemic in 2020 and 2021."),
                           h4("The purpose of this interactive dashboard is to discover trends and relationships between the available variables (Total Healthcare Worker Cases and Deaths, New Healthcare Worker and Resident Cases, and Total Resident Cases and Deaths) and learn more about COVID-19 cases and deaths in Skilled Nursing Facilities."),
                           h4("The goal is to bring awareness on counties that are most impacted by COVID-19 and how to allocate resources such as PPE, temporary workers, funding, etc. to those affected counties."),
                           h4(""),
                           h4("To view the data used in this dashboard, download the csv file below."),
                           list(uiOutput("tab")),
                           img(src="SNF Pic.png")
                  ),
                  
                  tabPanel("Scatter Plots",
                    fluidRow(
                      column(h4("Main Plot"),
                        width=6,
                           plotOutput(outputId = "plot2", width="200%",
                                      brush = brushOpts(id = "plot2_brush", resetOnNew = TRUE)
                                      ),
                        column(h4("Point-and-Click Plot"),
                          width=6,
                          plotOutput(outputId = "plot3", click="plot_click",width="440%"),
                          tableOutput("info"))
                        ))),
                  # tabPanel("Instructions",
                  #   verbatimTextOutput("text")),
                  # tabPanel("Reference Map with Counties",
                  #          img(src="california-county-map.png")),
                  # tabPanel("Median Cases and Deaths by Residents",
                  #          img(src="plot6.png"),
                  #          img(src="plot3.png"),
                  #          img(src="plot4.png")),
                  # tabPanel("Median Cases and Deaths by Healthcare Workers",
                  #          img(src="plot2.png"),
                  #          img(src="plot1.png"),
                  #          img(src="plot5.png"))
                  tabPanel("Map Plots",
                           fluidRow(
                             column(h4("Median Total Healthcare Worker Cases by County"),
                                    width=6, 
                                    verbatimTextOutput(
                                      "hover_infoa"),
                           plotOutput(outputId = "plota",
                                      hover = hoverOpts(id="plot_hover"))
                           ),
                           column(h4("Median Total Resident Cases by County"),
                                  width=6,
                                  verbatimTextOutput(
                                    "hover_infob"),
                           plotOutput(outputId = "plotb",
                                      hover = hoverOpts(id="plot_hover"))
                           ),
                           column(h4("Median Total Resident Deaths by County"),
                                  width=6,
                                  verbatimTextOutput(
                                    "hover_infoc"),
                           plotOutput(outputId = "plotc",
                                      hover = hoverOpts(id="plot_hover"))
)
                           ))
      )
    )
  )
)


server <- function(input, output, session) {
  
  csv_url <- a("SNF COVID Cases and Deaths Data (2020-present)", href="https://data.chhs.ca.gov/dataset/7759311f-1aa8-4ff6-bfbb-ba8f64290ae2/resource/d4d68f74-9176-4969-9f07-1546d81db5a7/download/covid19datanursinghome.csv")
  output$tab <- renderUI({
    tagList("CSV Download:", csv_url)
  })
  
  output$plot2 <- renderPlot({
    req(input$xaxis)
    req(input$yaxis)
    df0 %>% 
      filter(County == input$County) %>%
      # filter(as_of_date == input$dateRangeText)
      ggplot(aes_string(x = paste0("`", input$xaxis, "`"),
                        y = paste0("`", input$yaxis, "`"))) +
      geom_point(aes(color=County)) +
      labs(color='County')
  })
  
  ranges2 <- reactiveValues(x=NULL, y=NULL)
  
  output$plot3 <- renderPlot({
    df0 %>%
      filter(County == input$County) %>%
      # filter(as_of_date == input$dateRangeText) %>% 
      ggplot(aes_string(x = paste0("`", input$xaxis, "`"),
                        y = paste0("`", input$yaxis, "`"))) +
      geom_point(aes(color=County)) +
      coord_cartesian(xlim=ranges2$x, ylim=ranges2$y, expand = T) +
      labs(color='County') 
  })
  
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  output$info <- renderTable({
    df0 %>%
      filter(County == input$County) %>%
      nearPoints(input$plot_click, threshold = 10, maxpoints = 1, addDist = F)
  })
  
  output$text <- renderText({
    paste("Two tabs are available with interactive plots: 'Scatter Plots' and 'Map Plots'",
          "",
          "Scatter Plots Directions:",
          "First, located on the side panel, select the county or counties, x-variable and y-variable to observe.",
          "Second, use the main plot for the zoom interaction in the main panel.",
          "Below the main plot, the point-and-click interaction plot will display zoomed points.",
          "Finally, click on data point of interest on the point-and-click plot to generate results in the table output below.",
          "Note: To remove counties in the filter, use the delete or backspace key.",
          "",
          "Map Plots Directions:",
          "First, choose a plot out of the three plots to learn more details about the county and cases/deaths",
          "Second, hover over the county and the county name and cases/deaths data will populate in the output details table",
          "Interactive maps about Median Total Healthcare Worker and Resident Cases and Total Resident Deaths are displayed in the 'Map Plots' tab.",
          "Note: Alpine and Mono counties are not displayed due to unavailable/missing data.",
          "Median New Healthcare Worker and Resident Cases and Total Healthcare Worker Deaths are not displayed because data points equal to zero.",
          sep="\n")
  })
  
  # filtered_data <- reactive({
  #   filter(ca_counties2, name == input$subregion)
  # })
  
  output$plota <- renderPlot({ca_counties2 %>%
    ggplot(aes(x=long,y=lat,group=subregion, fill=totalhcwcases_med)) +
    geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
    scale_fill_continuous(type = "viridis")+
    theme(legend.position="bottom",
          axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank())
  })
  
  
  output$plotb <- renderPlot({ca_counties2 %>%
      ggplot(aes(x=long,y=lat,group=subregion, fill=totalrcases_med)) +
      geom_polygon(color = "gray90", size = 0.1) +
      coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
      scale_fill_continuous(type = "viridis")+
      theme(legend.position="bottom",
            axis.line=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid=element_blank())
  })
  
  output$plotc <- renderPlot({ca_counties2 %>%
      ggplot(aes(x=long,y=lat,group=subregion, fill=totalrdeaths_med)) +
      geom_polygon(color = "gray90", size = 0.1) +
      coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
      scale_fill_continuous(type = "viridis")+
      theme(legend.position="bottom",
            axis.line=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid=element_blank())
  })
  
  output$hover_infoa <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      dist=sqrt((hover$x-ca_counties2$long)^2+(hover$y-ca_counties2$lat)^2)
      cat("County Name and Median Total Healthcare Worker Cases\n")
      if(min(dist) < 1)
        c(ca_counties2$subregion[which.min(dist)],ca_counties2$totalhcwcases_med[which.min(dist)])
    }})
  
  output$hover_infob <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      dist=sqrt((hover$x-ca_counties2$long)^2+(hover$y-ca_counties2$lat)^2)
      cat("County Name and Median Total Resident Cases\n")
      if(min(dist) < 1)
        c(ca_counties2$subregion[which.min(dist)],ca_counties2$totalrcases_med[which.min(dist)])
    }})
  
  output$hover_infoc <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      dist=sqrt((hover$x-ca_counties2$long)^2+(hover$y-ca_counties2$lat)^2)
      cat("County Name and Median Total Resident Deaths\n")
      if(min(dist) < 1)
        c(ca_counties2$subregion[which.min(dist)],ca_counties2$totalrdeaths_med[which.min(dist)])
    }})
} 


shinyApp(ui, server)
