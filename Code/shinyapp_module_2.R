library(shiny)
library(ggplot2)
#Gauge Chart function
gauge_chart <- function(pos,breaks=c(0,8,17,25,45)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/45)
    th.end   <- pi*(1-b/45)
    th       <- seq(th.start,th.end,length=45)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="yellow")+
    geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(pos-0.2,pos+0.2,0.1),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/45)),y=1.1*sin(pi*(1-breaks/45)),label=paste0(breaks,"%")))+
    annotate("text",x=0,y=-0.02,label=pos,vjust=0,size=6,fontface="bold")+
    annotate("text",x=1.13*cos(pi*(1-3/45)),y=1.13*sin(pi*(1-3/45)),label=paste0("Underweight"),vjust=0,size=4,fontface="bold") +
    annotate("text",x=1.11*cos(pi*(1-12/45)),y=1.11*sin(pi*(1-12/45)),label=paste0("Fit"),vjust=0,size=4,fontface="bold") +
    annotate("text",x=1.11*cos(pi*(1-21/45)),y=1.1*sin(pi*(1-21/45)),label=paste0("Normal"),vjust=0,size=4,fontface="bold") +
    annotate("text",x=1.15*cos(pi*(1-35/45)),y=1.15*sin(pi*(1-35/45)),label=paste0("Overweight/Obese"),vjust=0,size=4,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}

# UI part for Shiny App
ui <- fluidPage(
  titlePanel("Body Fat Estimator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("weight",
                  "Weight (lbs):",
                  min = 50,
                  max = 400,value=30),
      
      sliderInput("abdomen",
                  "Adomen Circumference (cm) or Waist Size",
                  min = 40,
                  max = 170,value=40),
      sliderInput("wrist",
                  "Wrist Circumference (cm)",
                  min = 12,
                  max = 25,value = 12),
      h6("Please contact to shravan.kaul@wisc.edu for any debugging or questions about the app.")
    ),
    mainPanel(h4("Predicted Bodyfat"),
              textOutput("result"),
              plotOutput("gauge"))#,
  )
)
#Server part for Shiny app
server <- function(input, output) {
  weight <-reactive({ input$weight})
  abdomen <-reactive({ input$abdomen})
  wrist <-reactive({ input$wrist})
  bodyfat<- reactive({(input$abdomen*0.88)+(input$weight*-0.08)+(input$wrist*-1.36)-22.94})
  output$result <- renderText({
    if (input$weight==50 | input$abdomen==40 | input$wrist ==12)
      paste0("Welcome to the Estimator App, kindly enter your measurements")
    else if (bodyfat()< 0)
      paste0("Your input may be incorrect.Bodyfat can't be negative!")
    else if (bodyfat()<4)
      paste0("Your body fat percentage is between 0 and 4! Recheck your inputs")
    else if (bodyfat()<8)
      paste0("Your body fat percentage is ",bodyfat(),". You are underweight! Please have a nutritious diet and exercise")
    else if (bodyfat()<17)
      paste0("Your body fat percentage is ",bodyfat(),". You are in the fit range! That's great! Maintain it well.")
    else if (bodyfat()<25)
      paste0("Your body fat percentage is ",bodyfat(),". You have normal body fat % ! Continue your diet and try to exercise more.")
    else if (bodyfat()<45)
      paste0("Your body fat percentage is ",bodyfat(),". You are either overweight or obese! Time to cut down carbs and start exercising")
    else
      paste0("Your input may be incorrect.")
  })
  output$gauge <- renderPlot(if (input$weight==50 | input$abdomen==40 | input$wrist ==12)
    gauge_chart(0,breaks = c(0,8,17,25,45))
    else if (bodyfat()< 0)
      gauge_chart(0,breaks = c(0,8,17,25,45))
    else if (bodyfat()<4)
      gauge_chart(bodyfat(),breaks = c(0,8,17,25,45))
    else if (bodyfat()<50)
      gauge_chart(bodyfat(),breaks = c(0,8,17,25,45))
    else
      paste0("Recheck inputs")
    )
}
shinyApp(ui = ui, server = server)