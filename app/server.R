library(shinydashboard)
library(shiny)
library(ggplot2) # depth percentage plot
library(plyr)  # rounding dollar amount
library(leaflet)
library(googleway)
library(magrittr)  # # for the pipe '%>%'
library(ggmap)

streetviewAPIkey <- "AIzaSyB-Mr1Dool2jrUUYB19SQN84d6VDcZNrEQ"

server <- function(input, output) {
  
  # # BBL
  output$choose_bbl <- renderUI({
    selectizeInput("bbl", label = "BBL of a Tax Lot", choices = allBBL)  # input$bbl
  })
  
  bblInput <- reactive({
    currentBBL <- input$bbl
  })
  
  output$choose_material <- renderUI({
    if(is.null(input$bbl))  
      return()
    lot <- mn[mn$BBL == input$bbl, ]
    # materialOptions: a vector contains all possible material options
    
    if(all(is.na(lot[matLogic])))
      return("No available material!")
    materialOptions <- materialList[which(!is.na(lot[matLogic]))]
    selectizeInput("material", "Choose a material:", 
                   choices = materialOptions, selected = materialOptions[1])
    # default option is the 1st element in materialOptions
    
  })
  
  output$choose_quality <- renderUI({
    if(is.null(input$bbl))       
      return()
    if (is.null(input$material)) 
      return()
    if (is.na(input$material))   
      return()
    
    qualityOptions <- get_QualityClasses(get_Typeid(input$bbl, input$material, mn))
    selectizeInput("quality", "Choose a construction quality:", 
                   choices = qualityOptions, selected = qualityOptions[1])
    # default option is the 1st element in materialOptions
  })
  
  output$choose_basement <- renderUI({
    radioButtons("basement", label = "Basement", inline = TRUE,
                 choices = list("Yes" = TRUE, "No" = FALSE), 
                 selected = TRUE)
  })
  
  output$choose_elevation <- renderUI({
    sliderInput("elevation", "First Floor Elevation:",
                min=0, max=25, value=0)
  })  
  
  output$choose_water <- renderUI({
    sliderInput("water", "Water Depth:",
                min=-4, max=24, value=0)
  })      
  
  inputValues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("BBL", 
               "Material",
               "Quality",
               "Basement",
               "In-structure Water Depth"),
      
      Value = as.character(c(input$bbl, 
                             input$material,
                             input$quality,
                             ifelse(input$basement, "With Basement", "Without Basement"),
                             paste(input$water - input$elevation, "ft")
      )), 
      stringsAsFactors=FALSE)
  }) 
  
  computeCost <- eventReactive(input$go, {
    quality <- which(get_QualityClasses(get_Typeid(input$bbl, input$material, mn)) == input$quality)
    costCompList <- get_TotalCost(BBL = input$bbl, Material = input$material, 
                                  QualityClassNum  = quality, 
                                  BsmtType = input$basement,
                                  PlutoTable = mn)
    return(costCompList)
    
  }) 
  
  
  computeDamage <- eventReactive(input$go, {
    pr <- mn[mn$BBL == input$bbl, ]
    bclass <- get_BldgClass(BBL = input$bbl, PlutoTable = mn)
    
    curve <- get_DamageProbs(BldgClass = bclass, BsmtType = input$basement,
                             FloodType = pr$flood_type)
    depth <- input$water - input$elevation
    # depth <- ifelse(depth < -4, -4, depth)
    depth <- ifelse(depth > 24, 24, depth)
    p.damage <- ifelse(depth < -4, 0, curve[depth+5])
    # p.damage <- curve[depth+5]
    damage <- p.damage * computeCost()$total.cost
    damage
  })
  
  #++++++++++++++++++++++++++++++++++ OUTPUT +++++++++++++++++++++++++++++++++++
  
  
  # # Show row information of BBL
  output$show_taxlot <- renderPrint({
    data.frame(mn[mn$BBL == input$bbl, displayColList])
  })
  
  # # Show the input values using an HTML table
  output$values <- renderTable({
    if(is.null(input$bbl))       
      return()
    if (is.null(input$material)) 
      return()
    if (is.null(input$quality))  
      return()
    if (is.null(input$basement))
      return()
    
    inputValues()
  })
  
  output$unitCosts <- renderTable({
    computeCost()$displayCosts
  })  
  
  output$underlying <- renderTable({
    computeCost()$displayUnderlying
  })     
  
  output$show_totalcost <- renderPrint({
    if(is.null(input$bbl))      
      return()
    if (is.null(input$material)) 
      return()
    if (is.null(input$quality)) 
      return()
    computeCost()$total.cost
  })
  
  output$show_damage <- renderPrint({
    if(is.null(input$bbl))       
      return()
    if (is.null(input$material)) 
      return()
    if (is.null(input$quality))   
      return()
    if (is.null(input$basement)) 
      return()
    computeDamage()
  })
  
  output$show_depthfunction <- renderPlot({
    bclass <- mn[mn$BBL == input$bbl, "BldgClass"]
    ftype <- mn[mn$BBL == input$bbl, "flood_type"]
    qplot(seq(-4,24), 100*get_DamageProbs(bclass, input$basement, "Fresh"), 
          ylab = "Percentage (%)", xlab ="Water Depth (ft)", 
          geom = c("point", "smooth")) + geom_line()      
  })  
  
  loc <- reactive({
    data.frame(lat = mn[mn$BBL == input$bbl, "Latitude"], 
               lon = mn[mn$BBL == input$bbl, "Longitude"], 
               info = mn[mn$BBL == input$bbl, "Address"])
  })
  
  # Satellite View Tab: Google Static Map
  output$satellite_view <- renderGoogle_map({
    google_map(key="AIzaSyBq2-Ly8zzGkNxmyvYHm1IYYt-sQsIXGmc", search_box=T,
               location=c(40.7589,-73.9851), zoom=12)
  })  
  
  # Update Marker on Map
  observeEvent(input$go,{
    google_map_update(map_id = "satellite_view") %>%
      clear_markers() %>%
      add_markers(data = loc(), info_window = "info")
  })
  
  # Choose columns to display
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(mn[, input$show_vars, drop = FALSE])
  })
  
  getStreetviewURL <- eventReactive(input$go, {
    addrString <- paste(mn[mn$BBL == input$bbl,"Address"], "New York",
                        mn[mn$BBL == input$bbl,"ZipCode"])
    google_streetview(location = addrString,
                      size = c(500,500),
                      panorama_id = NULL,
                      output = "html",
                      fov = 100,
                      pitch = 15,
                      response_check = FALSE,
                      key = streetviewAPIkey)
  })
  
  output$street_pic = renderUI({
    tags$img(src = getStreetviewURL())  
  })
  # output$street_pic <- renderPlot({
  #   getStreetview()
  # })
  
  
}
