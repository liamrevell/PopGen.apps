library(shiny)
library(PopGen)

ui<-fluidPage(
  h3("Frequency dependent selection model",align="center"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="p0",label="p0 (initial value of p):",value=0.01,
        min=0,max=1,step=0.01),
      sliderInput(inputId="s",
        label="real number value of s (see Intructions, below)",value=0,min=-2,
        max=2,step=0.1),
      sliderInput(inputId="ngen",label="number of generations:",value=100,min=0,
        max=1000),
      selectInput(inputId="show",label="Show plot of:",
        choices=c("p","q","fitness","surface","deltap","cobweb")),
      checkboxInput(inputId="react",
        label=
        "reactive (plot changes instantly with new parameter values)",
        value=TRUE),
      actionButton("newplot","new plot"),
      h4("Instructions:\n"),
      p("Frequency dependent selection model. Value of s changes the degree of
        selection against (or in favor of) the heterozygous genotype when 
        common.\n\n"),
      h4("Details:\n"),
      p("This frequency dependent selection model based on Rice (2004). The 
        fitnesses of the three genotypes are as follows, where f() denotes 
        the frequency of each genotype: w(AA)=1-3*f(Aa)+3*f(aa); w(Aa)=1-s*f(Aa); 
        and w(aa)=1-3*f(Aa)+3*f(AA)."),
      p("Web application for freqdep of the PopGen R package (Revell, 2018).")
    ),
    mainPanel(
      plotOutput("plot",width="100%",height="500px")
    )
  )
)

server <- function(input, output) {
  output$plot<-renderPlot({
    if(!input$react){
      input$newplot
      isolate(
        if(input$ngen>0){
          freqdep(p0=input$p0,s=input$s,time=input$ngen,show=input$show,
            color=phytools::make.transparent("blue",0.5))
        }
      )
    } else {
      input$newplot
      if(input$ngen>0){
        freqdep(p0=input$p0,s=input$s,time=input$ngen,show=input$show,
          color=phytools::make.transparent("blue",0.5))
      }
    }
  })
}

shinyApp(ui = ui, server = server)
