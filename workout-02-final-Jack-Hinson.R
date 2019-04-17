#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
install.packages(tidyverse)
library(tidyverse)


ui <- fluidPage(
  
  
  titlePanel("Comparing Different Types of Savings Growth Patterns"),
  
  fluidRow(
    
    column(3,
           sliderInput("initial_amount",
                       "Initial Amount",
                       min = 0,
                       max = 100000,
                       value = 1000,
                       step=500)
    ),
    
    column(3,
           
           sliderInput("return_rate",
                       "Return Rate (in %)",
                       min=0,
                       max=20,
                       value=5,
                       step=.1)
    ),
    column(3,
           
           sliderInput("years",
                       "Years",
                       min=0,
                       max=50,
                       value=20,
                       step=1))
    
  ),
  fluidRow(
    
    column(3,
           
           sliderInput("contrib", 
                       "Annual Contribution",
                       min=0,
                       max=50000,
                       value=2000,
                       step=500)),
    column(3,
           sliderInput("growth_rate",
                       "Growth Rate (in %)",
                       min=0,
                       max=20,
                       value=2,
                       step=.1)
    ),
    
    column(3,
           selectInput("facet",
                       "Facet?",
                       choices=c("No","Yes")
                       
           ))
  ),
  
  fluidRow(
    plotOutput("modalitiesPlot")
  ),
  fluidRow("Balance",
   tableOutput("modalitiesTable")
  )
)



server <- function(input, output) {output$modalitiesPlot <- renderPlot({
  
  future_value<- function(amount, rate, years){
    investment<-amount*((1+rate)^years)
    return(investment)}
  
  annuity<-function(contrib, rate, years) {
    fva<- contrib*((((1+rate)^years)-1)/rate)
    return(fva)
  }
  
  growing_annuity<-function(contrib, rate, growth, years){
    fvga<-contrib*((((1+rate)^years)-((1+growth)^years))/(rate-growth))
    return(fvga)
  }
  
  mode_one<- matrix(NA, nrow=input$years+1)
  for (i in 0:input$years){mode_one[i+1,]<- print(future_value(amount=input$initial_amount, rate=input$return_rate/100, years=i))}
  
  mode_two<-matrix(NA, nrow=input$years+1)
  for (i in 0:input$years){mode_two[1+i,]<-print((future_value(amount=input$initial_amount, rate=input$return_rate/100, years=i))+annuity(contrib = input$contrib, rate=input$return_rate/100, years=i))}
  
  mode_three<-matrix(NA,nrow = input$years+1)
  for (i in 0:input$years){mode_three[1+i,]<-print(future_value(amount=input$initial_amount, rate=input$return_rate/100, years=i) +growing_annuity(contrib=input$contrib, rate=input$return_rate/100,growth=input$growth_rate/100, years=i))}
  
  year<- matrix(0:input$years,nrow=input$years+1)
  
  modalities<-data.frame(cbind(year,mode_one,mode_two,mode_three))
  
  colnames(modalities)<-c("year", "no_contrib","fixed_contrib","growing_contrib")
  
  
facet_modalities<-gather(modalities, no_contrib:growing_contrib, key=type, value=balance)
  
facet_modalities$type<-factor(facet_modalities$type, levels=c("no_contrib","fixed_contrib", "growing_contrib"))

  graph_modalities<- ggplot(facet_modalities, aes(x=year, y=balance))+
      geom_line(aes(color=type), alpha=.5)+
      geom_point(aes(color=type), alpha=.5,cex=.5)+
      labs(title="Timelines",subtitle="Three Modes of Investing", x="Year",y="Amount (in $)")+
      scale_x_continuous(limits =c(0,input$years))+
      scale_color_manual(name="Type of Savings Plan",values=c("red","green","blue"),labels=c("Future Value (No Contribution)","Annuity (Fixed Contribution)","Growing Annuity (Increasing Contribution)"))
      geom_area(aes(fill=type), alpha=.5)
  
  
if(input$facet=="No"){graph_modalities
     }
  else if (input$facet=="Yes"){graph_modalities+
      facet_wrap(.~type)+
      geom_area(aes(fill=type), alpha=.5)
    }
  
  })

output$modalitiesTable<-renderTable({
  
  future_value<- function(amount, rate, years){
    investment<-amount*((1+rate)^years)
    return(investment)}
  
  annuity<-function(contrib, rate, years) {
    fva<- contrib*((((1+rate)^years)-1)/rate)
    return(fva)
  }
  
  growing_annuity<-function(contrib, rate, growth, years){
    fvga<-contrib*((((1+rate)^years)-((1+growth)^years))/(rate-growth))
    return(fvga)
  }
  
  mode_one<- matrix(NA, nrow=input$years+1)
  for (i in 0:input$years){mode_one[i+1,]<- print(future_value(amount=input$initial_amount, rate=input$return_rate/100, years=i))}
  
  mode_two<-matrix(NA, nrow=input$years+1)
  for (i in 0:input$years){mode_two[1+i,]<-print((future_value(amount=input$initial_amount, rate=input$return_rate/100, years=i))+annuity(contrib = input$contrib, rate=input$return_rate/100, years=i))}
  
  mode_three<-matrix(NA,nrow = input$years+1)
  for (i in 0:input$years){mode_three[1+i,]<-print(future_value(amount=input$initial_amount, rate=input$return_rate/100, years=i) +growing_annuity(contrib=input$contrib, rate=input$return_rate/100,growth=input$growth_rate/100, years=i))}
  
  year<- matrix(0:input$years,nrow=input$years+1)
  
  
  modalities<-data.frame(cbind(year,mode_one,mode_two,mode_three))
  
  colnames(modalities)<-c("year", "no_contrib","fixed_contrib","growing_contrib")
  
  modalities$year<-as.integer(modalities$year)
  
  modalities
})

}

# Run the application 
shinyApp(ui = ui, server = server)
