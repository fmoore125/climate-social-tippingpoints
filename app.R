library(shiny)
library(ggplot2)
library(reshape2)

ui <- fluidPage(
  # App title ----
  titlePanel("Tipping Points in the Climate-Social System"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      h2("Opinion Dynamics Parameters"),
      #opinion dynamics parameters
      sliderInput("homophily_param", "Network Homophily - min=0.3 (no separation), max=1 (fully separated)", 0.7, min = 0.3, max = 1, step = 0.05),
      sliderInput("frac_opp_0", "Initial Fraction Opposing - min=0 (no opposers), max=1 (all opposers)", 0.2, min = 0, max = 1, step = 0.05),
      sliderInput("frac_neut_0", "Initial Fraction Neutral - min=0 (no neutral), max=1-Initial Fraction Opposing", 0.6, min = 0, max = 1, step = 0.05),
      sliderInput("forcestrong", "Persuasive Force from Opinionated - min=0 (no effect), max=1 (instant persuasion)", 0.2, min = 0, max = 1, step = 0.05),
      sliderInput("forceweak", "Persuasive Force from Neutral - min=0 (no effect), max=Force from Opinionated", 0.1, min = 0, max = 1, step = 0.05),
      sliderInput("evidenceeffect","Effect of Perceived Weather on Opinion min=0 (no effect), max=0.5 (large effect)",0,min=0,max=0.5,step=0.1),
      
      
      h2("Policy Response Parameters"),
      sliderInput("pol_response", "Responseiveness of Policy to Opinion - min=1 (fully responsive), max=10 (very unresponsive)", 1.5, min = 1, max = 10, step = 0.5),
      
      h2("Adoption of Individual Mitigative Behavior Parameters"),
      h3("Perceived Behavioral Control"),
      sliderInput("pbc_mid", "PBC Value at 50% Adoption - Starting Value PBC=-1.5, Higher Values => Later Adoption", 0, min = -1, max = 5, step = 0.5),
      sliderInput("pbc_steep", "PBC Adoption Steepness - min=1 (gradual), max=5 (steep)", 3, min = 1, max = 5, step = 0.5),
      sliderInput("policy_pbcchange_max", "Max Effect of Policy on PBC Curve - min=0 (no effect), max=1 (large effect)", 0.5, min = 0, max = 1, step = 0.1),
      
      h3("Opinion and Norm Effect on Adoption"),
      sliderInput("oppose_adopt","Effect of Opposing Climate Policy on Adoption - min=0 (no effect), max=0.7",0.2,min=0,max=0.7,step=0.1),
      sliderInput("support_adopt","Effect of Supporting Climate Policy on Adoption - min=0 (no effect), max=0.7",0.2,min=0,max=0.7,step=0.1),
      sliderInput("normeffect","Sensitivity of Adoption to Social Norm - min=0 (no sensitivity), max=0.6 (large effect)",0.1,min=0,max=0.6,step=0.1),
      
      h3("Endogenous Technical Change Parameters"),
      sliderInput("etc_mid","Fraction of Adopters for 50% of ETC Potential - min=0.25, max=0.8",0.5,min=0.25,max=0.8,step=0.05),
      sliderInput("etc_steep","Steepness of ETC-PBC Curve - min=1, max=5",2,min=1,max=5,step=0.5),
      sliderInput("etc_total","Max Effect of ETC on PBC - min=0 (no effect), max=5",1,min=0,max=5,step=0.5),
      
      h2("Mitigation Parameters"),
      sliderInput("m_max","Max Contemporaneous Effect of Policy on Emissions (% Reduction) - min=0.1, max=5",2.5,min=0.5,max=5,step=0.5),
      sliderInput("r_max","Max Scaling Time of Mitigation Investments (years, Initial Value=2) - min=5, max=30",25,min=5,max=30,step=1),
      sliderInput("adopt_effect","Effectiveness of Adoption at Reducing Emissions (% Reduction) - min=0 (not effective), max=30",10,min=0,max=30,step=5),
      
      h2("Cognition Parameters"),
      radioButtons("shiftingbaseline","Shifting or Pre-Industrial Baselines?",choices = list("shifting"=1,"fixed"=0),selected=1),
      sliderInput("biassedassimilation","Weighting of Opinion on Evidence from Weather - min=0 (no bias), max=0.9 (very biased) ",0.2,min=0,max=0.9,step=0.1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput(outputId = "distributionsPlot"),
      splitLayout(
        plotOutput(outputId = "adoptionPlot"),
        plotOutput(outputId = "policyPlot"),
        plotOutput(outputId = "pbcPlot")
      ),
      splitLayout(
        plotOutput(outputId = "emissionsPlot"),
        plotOutput(outputId="temperaturePlot")
      )
    )
  )
)

server <- function(input, output, session) {
  source("src/model.R")
  
  m<-reactive(model(homophily_param=input$homophily_param,frac_opp_0 = input$frac_opp_0,frac_neut_0 = input$frac_neut_0,
                    forcestrong=input$forcestrong, forceweak=input$forceweak,pol_response = input$pol_response,
                    pbc_mid=input$pbc_mid,pbc_steep=input$pbc_steep,policy_pbcchange_max=input$policy_pbcchange_max,
                    pbc_opinionchange=c(input$oppose_adopt,0,-1*input$support_adopt),normeffect=input$normeffect, etc_mid=input$etc_mid,
                    etc_steep=input$etc_steep,etc_total=input$etc_total,r_max=input$r_max,m_max=input$m_max/100,adopt_effect=input$adopt_effect/100,
                    evidenceeffect = input$evidenceeffect,biassedassimilation = input$biassedassimilation,shiftingbaseline=input$shiftingbaseline ))

  output$distributionsPlot <- renderPlot({
    
    mod=m()
    temp=data.frame(Time=mod$year,Oppose_Adopt=mod$distributions[,1]*mod$adoptersfrac[,1],Oppose_NonAdopt=mod$distributions[,1]*(1-mod$adoptersfrac[,1]),
                    Neutral_Adopt=mod$distributions[,2]*mod$adoptersfrac[,2],Neutral_NonAdopt=mod$distributions[,2]*(1-mod$adoptersfrac[,2]),
                    Support_Adopt=mod$distributions[,3]*mod$adoptersfrac[,3],Support_NonAdopt=mod$distributions[,3]*(1-mod$adoptersfrac[,3]))
    temp=melt(as.data.frame(temp),id.vars=c("Time"),variable.name="Opinion",value.name="Fraction")
    ggplot(temp,aes(x=Time,y=Fraction,fill=Opinion))+geom_area()+theme_bw()+theme(text = element_text(size=20))+labs(title="Opinion and Adoption Dynamics",x="")+
      scale_fill_manual(values=c("#0f334a","#82b5ee","#1d4846","#add0b0","#361432","#985d93"))
    
  })
  
  output$adoptionPlot <- renderPlot({
    mod=m()
    temp=data.frame(Time=mod$year,Fraction_Adopters=mod$nadopters)
    ggplot(temp,aes(x=Time,y=Fraction_Adopters*100))+geom_line(lwd=2)+theme_bw()+theme(text = element_text(size=20))+labs(title="Percent of Adopters",x="",y="")
  })
  output$policyPlot <- renderPlot({
    mod=m()
    temp=data.frame(Time=mod$year,Policy=mod$policy)
    ggplot(temp,aes(x=Time,y=Policy))+geom_line(lwd=2)+theme_bw()+theme(text = element_text(size=20))+labs(title="Policy",x="",y="")
  })
  
  output$pbcPlot <- renderPlot({
    mod=m()
    temp=data.frame(Time=mod$year,PBC=mod$pbc)
    ggplot(temp,aes(x=Time,y=PBC))+geom_line(lwd=2)+theme_bw()+theme(text = element_text(size=20))+labs(title="Perceived Behavioral Control",x="",y="")
  })
  output$emissionsPlot <- renderPlot({
    mod=m()
    temp=melt(data.frame(Time=mod$year,Emissions=mod$emissions,BAU=mod$bau),id.vars="Time",variable.name="Scenario",value.name="Emissions")
    ggplot(temp,aes(x=Time,y=Emissions,group=Scenario,col=Scenario))+geom_line(lwd=2)+theme_bw()+theme(text = element_text(size=20),legend.position=c(0.2,0.8))+labs(title="Emissions (GtC per Year)",x="",y="")+
      scale_color_manual(values=c("#efbd13","#142c31"),labels=c("Emissions","BAU - RCP7.0"))
    
  })
  output$temperaturePlot <- renderPlot({
    mod=m()
    temp=melt(data.frame(Time=mod$year,Emissions=mod$temp[,1],BAU=mod$bau_temp[,1]),id.vars="Time",variable.name="Scenario",value.name="Temperature")
    ggplot(temp,aes(x=Time,y=Temperature,group=Scenario,col=Scenario))+geom_line(lwd=2)+theme_bw()+theme(text = element_text(size=20),legend.position=c(0.2,0.8))+labs(title="Temperature Change (Degrees Above Pre-Industrial)",x="",y="")+
      scale_color_manual(values=c("#efbd13","#142c31"),labels=c("Temp Change","BAU Temp Change"))
  })
}

shinyApp(ui = ui, server = server)
