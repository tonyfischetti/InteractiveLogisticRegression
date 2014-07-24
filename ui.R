library(shiny)

# Define UI for miles per gallon application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Interactive visualization of non-linear logistic regression decision boundaries"),
  
  # Sidebar with controls to select the variable to plot against
  # mpg and to specify whether outliers should be included
  sidebarLayout(
    sidebarPanel(
      selectInput("pattern", "Pattern:",
                  c("Moon" = "moon",
                    "Circle" = "circle")),
      sliderInput("degree",
                  "Degree polynomial:",
                  min = 1,
                  max = 20,
                  value = 1),
      sliderInput("lambda",
                  "Lambda:",
                  min = 1,
                  max = 10,
                  value = 1),
      selectInput("opt", "Optimization algorithm:",
                  c("BFGS Quasi-Newton" = "BFGS",
                    "Nelder-Mead" = "Nelder-Mead",
                    "Conjugate Gradient" = "CG"))
    ),
    
    mainPanel(
      plotOutput("da.plot")
    )
  )
))