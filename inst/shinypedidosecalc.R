################################################################################
# Pediatric Dosing Calculator
# Shiny App developed for use by CRCs in MEDS 3.5 study to aid in identifying
#     pediatric dosing errors (PI Margaret Samuels-Kalow).
# Developed by Melissa Meeker, PhD
################################################################################


# Loading Packages
library(shiny)

# Creating User Interface
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

      h2("Weight"),
      tags$em("Weight only accepted between 0kg and 150kg.",
              style = "font-size: 0.9em; color: gray;"),      br(),
      textOutput("display_weight"),
      br(),

      h2("Recommended Dose"),
      tags$em("Reported as exact dose (20% error range)",
              style = "font-size: 0.9em; color: gray;"),      br(),

      textOutput("liquid"),
      textOutput("supp")
    )
  )
)

# Code for calculating dose
server <- function(input, output) {

  # Only allows weight input in range (between 1kg and 65kg).
  wt_kg <- reactive({
    w <- as.numeric(input$weight)

    if(w<0 || w>=150){
      return(NA)  # invalid input → missing
    } else if(w>40){
      return(40)
    } else {
      return(w)   # valid input → use directly
    }
  })

  # Display weight in kilograms
  output$display_weight <- renderText({
    paste("Weight entered:", as.numeric(input$weight), "kg")
  })

  # Liquid dose calculations
  output$liquid <- renderText({
    wt <- wt_kg()
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
    wt <- wt_kg()

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

#Launch Shiny App
shinyApp(ui = ui, server = server)
