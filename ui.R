library(shiny)
library(car)
source("server.R")
shinyUI(pageWithSidebar(

  headerPanel("Models!"),

  sidebarPanel(

    wellPanel(
    	checkboxGroupInput("x_var", "Variables:",                    
    		c("Enrollment GCS" = "Enrollment_GCS_Add",                      
    		"Age" = "Age",                      
    		"Pre-Randomization ICH (per 10cc)" = "ICH_Pre_Rand_10",                      
    		"Surgery vs. Medical" = "Group_Assigned", 
    		"End of Treatment ICH (per 10cc)" = "ICH_EOT_10", 
    		"End of Treatment IVH (per 10cc)" = "IVH_EOT_10",
    		"Less or equal to 15cc at EOT" = "Under_15cc"
    		), selected=c("Enrollment GCS", "Age", "Pre-Randomization ICH (per 10cc)", "Surgery vs. Medical")),
    
      selectInput(inputId = "y_var",
                  label = "Y variable",
                  choices = c("Day 180" = "Bad_Outcome_Day_180",
							"Day 365" = "Bad_Outcome_Day_365"),
                  selected = "Day 180"
                 ), 
      selectInput(inputId = "fam",
                  label = "Family - all use default links",
                  choices = c("binomial" = "binomial()",
							"quasibinomial" = "quasibinomial()",
							"linear" = "gaussian()",
							"Poisson" = "poisson()",
							"quasipoisson"= "quasipoisson()"
							),
                  selected = "binomial"
                 )                 
    )

  ), 
  	mainPanel( htmlOutput(outputId = "mod_linear_text"),
	 	plotOutput(outputId = "main_plot")
    )

  
))
