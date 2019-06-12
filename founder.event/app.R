library(shiny)
library(learnPopGen)

ui<-fluidPage(
  h3("Population bottleneck / Founder event simulation",align="center"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="p0",label="p0 (initial value of p):",value=0.5,min=0,max=1,
        step=0.01,round=2),
      sliderInput(inputId="Ne",label="Ne (effective population size):",value=1000,min=0,
        max=1000),
      sliderInput(inputId="Nf",label="number of founders:",value=10,min=0,max=20),
      selectInput(inputId="show",label="show plot of:",choices=c("p(A)","genetic variance")),
      selectInput(inputId="lty",label="line type:",choices=c("stairstep","regular line")),
      sliderInput(inputId="etime",label="timing & duration of founding event:",min=0,max=100,value=c(50,60)),
      checkboxInput(inputId="react",label="reactive (plot changes instantly with new parameter values)",
        value=TRUE),
      textInput(inputId="seed",label="set seed (optional)",value=""),
      actionButton("newplot","new plot"),
      h4("Instructions:\n"),
      p("Explore founder-effect (aka. population bottleneck) changes in allele frequency in a population.\n\n"),
      h4("Details:\n"),
      p("Web application for founder.event of the learnPopGen R package (Revell, 2019).")
    ),
    mainPanel(
      plotOutput("plot",width="100%",height="500px")
    )
  )
)

server <- function(input, output) {
  output$plot<-renderPlot({
    if(input$seed!="") set.seed(as.numeric(input$seed))
    if(!input$react){
      input$newplot
      isolate(
        if(input$Ne>0&&input$Nf>0){
          if(input$etime[2]==input$etime[1]) e<-input$etime[1]
          else e<-input$etime[1]:input$etime[2]
          lty<-if(input$lty=="stairstep") "s" else "l"
          show<-if(input$show=="p(A)") "p" else "var"
          founder.event(input$p0,input$Ne,input$Nf,100,e,ltype=lty,show=show)
        }
      )
    } else {
      input$newplot
      if(input$Ne>0&&input$Nf>0){
        if(input$etime[2]==input$etime[1]) e<-input$etime[1]
        else e<-input$etime[1]:input$etime[2]
        lty<-if(input$lty=="stairstep") "s" else "l"
        show<-if(input$show=="p(A)") "p" else "var"
        founder.event(input$p0,input$Ne,input$Nf,100,e,ltype=lty,show=show)
      }
    }
  })
}

shinyApp(ui = ui, server = server)
