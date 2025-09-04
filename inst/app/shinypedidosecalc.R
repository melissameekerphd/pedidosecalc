library(shiny)

ui <- fluidPage(
  titlePanel("Pediatric Dosing Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("weight", "Enter child's weight (kg):", value = 12, min = 1, max = 150, step = 1),
      selectInput("med", "Medication:", 
                  choices = c("Tylenol (Liquid)", 
                              "Infant Motrin (Liquid)",
                              "Children's Motrin (Liquid)",
                              "Tylenol (Suppository)",
                              "Motrin (Suppository)"))
    ),
    
    mainPanel(
      h3("Recommended Dose"),
      textOutput("display_weight"),
      
      textOutput("liquid"),
      textOutput("supp"),
      textOutput("warning")
    )
  )
)

server <- function(input, output) {
  
  # Display weight in both units
  output$display_weight <- renderText({
    paste("Weight entered:", input$weight, "kg")
  })
  
  # # Weight dose calculations
  # output$weight <- renderText({
  #   wt <- input$weight
  #   med <- input$med
  #   
  #   if (med == "Tylenol (Liquid)") {
  #     
  #     value_mg = wt*15
  #     
  #     paste("Weight dose:", format(round(value_mg, 1), nsmall = 1), "mg")
  #     
  #   }
  #   else if (med == "Infant Motrin (Liquid)") {
  #     
  #     value_mg = wt*10
  #     
  #     paste("Weight dose:", format(round(value_mg, 1), nsmall = 1), "mg")
  #     
  #   }
  #   else if (med == "Children's Motrin (Liquid)") {
  #     
  #     value_mg = wt*10
  #     
  #     paste("Weight dose:", format(round(value_mg, 1), nsmall = 1), "mg")
  #     
  #   }
  #   else {
  #     ""
  #   }
  # })
  
  # Liquid dose calculations
  output$liquid <- renderText({
    wt <- input$weight
    med <- input$med
    
    if (med == "Tylenol (Liquid)") {
      
      value_mg = wt*15
      
      value_ml = 5*((value_mg)/160)
      lower <- 5*((value_mg-(0.2*value_mg))/160)
      upper <- 5*((value_mg+(0.2*value_mg))/160)
      paste("Liquid dose: ", format(round(value_ml, 1), nsmall = 1), " (", format(round(lower,1), nsmall = 1), " to ", format(round(upper,1), nsmall = 1), ") mL")
    }
    else if (med == "Infant Motrin (Liquid)") {
      
      value_mg = wt*10
      
      value_ml = 1.25*((value_mg))/50
      lower <- 1.25*((value_mg-(0.2*value_mg))/50)
      upper <- 1.25*((value_mg+(0.2*value_mg))/50)
      paste("Liquid dose: ", format(round(value_ml, 1), nsmall = 1), " (", format(round(lower,1), nsmall = 1), " to ", format(round(upper,1), nsmall = 1), ") mL")
      
    }
    else if (med == "Children's Motrin (Liquid)") {
      
      value_mg = wt*10
      
      value_ml = 5*((value_mg)/100)
      lower <- 5*((value_mg-(0.2*value_mg))/100)
      upper <- 5*((value_mg+(0.2*value_mg))/100)
      paste("Liquid dose: ", format(round(value_ml, 1), nsmall = 1), " (", format(round(lower,1), nsmall = 1), " to ", format(round(upper,1), nsmall = 1), ") mL")
      
    }
    else {
      ""
    }
  })
  
  # Suppository dose calculations
  output$supp <- renderText({
    med <- input$med
    wt <- input$weight
    
    if (med == "Tylenol (Suppository)") {
      value_mg = wt*15
      lower <- value_mg-(0.2*value_mg)
      upper <- value_mg+(0.2*value_mg)
      paste("Suppository dose: ", format(round(value_mg, 1), nsmall = 1), " (", format(round(lower,1), nsmall = 1), " to ", format(round(upper,1), nsmall = 1), ") mg")
    }
    else if (med == "Motrin (Suppository)") {
      value_mg = wt*10
      lower <- value_mg-(0.2*value_mg)
      upper <- value_mg+(0.2*value_mg)
      paste("Suppository dose: ", format(round(value_mg, 1), nsmall = 1), " (", format(round(lower,1), nsmall = 1), " to ", format(round(upper,1), nsmall = 1), ") mg")
    }
    else {
      ""
    }
  })
  
}

shinyApp(ui = ui, server = server)
