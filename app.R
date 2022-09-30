library(shiny)
library(dplyr)
library(tidyverse)
library(colourpicker)
library(rhandsontable)
library(svglite)
library(ggrepel)
library(extrafont)
library(data.table)

loadfonts(device = "win")

#some default colours to start off the user when selecting colours

xrwmata <- as.data.frame(c("black","blue","red"))

#begin building ui

ui <- fluidPage(
  
  #title
  
  titlePanel("EveRplot"),
  
  #parameters section
  
  sidebarLayout(
    sidebarPanel(h2("Parameters"),
                 
                h3("Input Data"),
                
                #upload csv section
                
                fileInput("file1",
                          "Choose CSV File",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                
                h3("Aesthetics"),
                fluidRow(column(6,
                                
                                h4("Datapoints"),
                                
                                #button for selecting datashapes
                                
                                radioButtons("points",
                                             "Shape of Datapoints",
                                             choices = list("Dots" = "1",
                                                            "Lines" = "2"),
                                             selected = 1),
                                
                                #slider for changing datapoint size
                                
                                sliderInput("psz",
                                            "Size of Datapoints",
                                            min = 0,
                                            max = 30,
                                            value = 4),
                                
                                #slider for changing datapoint colour
                                
                                colourInput("p_col",
                                            "Select Datapoint Colour",
                                            "purple"),
                                
                                #checkbox for multiple pathway colouring
                                
                                checkboxInput("mpath",
                                              "Colour by Pathway",
                                              value = FALSE),
                                
                                #colourpicker conditional on multiple pathway
                                #colouring being selected
                                
                                conditionalPanel(condition = "input.mpath == 1",
                                                 selectizeInput(
                                                   "sel_col",
                                                   "Enter Colour Name or Hex Code",
                                                   choices = xrwmata,
                                                   selected = NULL,
                                                   multiple = TRUE,
                                                   width = "100%",
                                                   options = list(
                                                     'plugins' = list('remove_button'),
                                                     'create' = TRUE,
                                                     'persist' = TRUE
                                                   )
                                                 ))
                ),
                
                #new column beside datapoints
                
                column(6,
                       
                       h4("Curve"),
                       
                       #select Gaussian or linear plot
                       
                       radioButtons("fn",
                                    "Type of Plot",
                                    choices = list("Gaussian" = "1",
                                                   "Linear" = "2"),
                                    selected = 1),
                       
                       #if linear, show slider that allows to cycle through 
                       #types of lines
                       
                       conditionalPanel(condition = "input.fn == 2",
                                        sliderInput("linet",
                                                    "Type of Line",
                                                    min = 1,
                                                    max = 6,
                                                    value = 2),
                                        
                                        textInput("adjf",
                                                  "Adjust Lines",
                                                  value = "0.5")
                       ),
                       
                       #slider for curve size
                       
                       sliderInput("csz",
                                   "Size of Curve",
                                   min = 0,
                                   max = 10,
                                   value = 2),
                       
                       #slider for curve opacity
                       
                       sliderInput("alpha",
                                   "Opacity of Curve",
                                   min = 0,
                                   max = 1,
                                   value = 0.25),
                       
                       #slider for curve colour
                       
                       colourInput("c_col",
                                   "Select Curve Colour",
                                   "black")
                )
                ),
                
                fluidRow(
                  column(6,
                         
                         #new column for labels
                         
                         h4("Labels"),
                         
                         #which labels do you want
                         
                         radioButtons("labs",
                                      "Labels",
                                      choices = list("Energies" = "1",
                                                     "Names" = "2",
                                                     "Energies and Names" = "3",
                                                     "Dynamic" = "4",
                                                     "None" = "5"),
                                      selected = "5"),
                         
                         #slider for changing positioning of labels
                         
                         sliderInput("nud",
                                     "Position of Labels",
                                     min = 0,
                                     max = 30,
                                     value = 5),
                         
                         #slider for chainging label size
                         
                         sliderInput("lsz",
                                     "Size of Labels",
                                     min = 0,
                                     max = 30,
                                     value = 5),
                         
                         ),
                  
                  column(6,
                         
                         #section for axes
                         
                         h4("Axes"),
                         
                         #text input for changing axis font
                         
                         textInput("lab_fn",
                                   "Axis Font",
                                   value = "Verdana"),
                         
                         #slider for altering axis title and axis label size
                         
                         sliderInput("lab_f",
                                     "Axis Element Font Size",
                                     min = 0,
                                     max = 50,
                                     value = 20),
                         
                         #slider for altering axis line thickness
                         
                         sliderInput("lab_t",
                                     "Axis Thickness",
                                     min = 0,
                                     max = 20,
                                     value = 1),
                         
                         #text input for y axis title
                         
                         textInput("y_lab",
                                   "y-Axis Label",
                                   value = "ΔG (kcal/mol)"),
                         
                         #tickbox for whether the y axis is shown
                         
                         checkboxInput("y_check",
                                       "Show y-Axis",
                                       value = TRUE),
                         
                         #tickbox for whether the x axis is shown
                         
                         checkboxInput("x_check",
                                       "Show x-Axis",
                                       value = FALSE),
                         
                         #if the x axis is shown, a text input appears which 
                         #allows for 
                         
                         conditionalPanel(condition = "input.x_check == 1",
                                          textInput("x_lab",
                                                    "x-Axis Label",
                                                    value = "Reaction Coordinate")
                                          )
                  )
                  
                ),
                
                br(),
                
                #statement of developer and license under which it is published
                
                h6("Created by Michael Bogdos; GNU v 3 License. Please cite DOI
                   10.1021/XXXXXX when using figures created with EveRplot for
                   scientific publications.")
    ),
    
    mainPanel(
      
      #create graph output panel
      
      h2("Graph"),
      
      #output plot
        
      plotOutput("diagram"),
      
      #download buttons
      
      downloadButton("d_plot_s",
                     "Download Graph as .svg"),
      
      downloadButton("d_plot_p",
                     "Download Graph as .png"),
      
      downloadButton("downtab",
                     "Download Table as .csv"),
      
      #text input for defining dimensions of the plot to be downloaded using the
      #download buttons
      
      textInput("h_s",
                "Plot Height (cm)",
                value = "20"),
      
      textInput("w_s",
                "Plot Width (cm)",
                value = "30"),
      
      #section for editable datatable
      
      h2("Data"),
      
      #editable data table output
      
        rHandsontableOutput("dtab")
                    )
  )
)

server <- function(input,output,session){
  
  #create the default dataset that is plotted when no input has been provided
  
  rc <- c(0,5)
  
  dg <- c(0,10)
  
  cpd_num <- c("GS1","TS1")
  
  ex_dat <- data.frame(rc,dg,cpd_num) %>%
    mutate(pathway = 1)
  
  #importing the data from the upload button into the table
  
  df_up <- reactive({
    instuff <- input$file1
    if (is.null(instuff))
      return(ex_dat)
    read.csv(instuff$datapath)
  })
  
  #render the table

  output$dtab <- renderRHandsontable({
    rhandsontable(df_up())
  }) %>%
    bindCache(df_up())
  
  #define the themes
  
  themething <- reactive({theme(axis.line = element_line(size = input$lab_t),
                                axis.ticks.y = element_line(size = 1.5), 
                                axis.text.y = element_text(size = input$lab_f), 
                                axis.title = element_text(size = input$lab_f),
                                axis.text.x = element_blank(),
                                axis.ticks.x = element_blank(),
                                plot.title = element_text(size = input$lab_f,
                                                          face = "bold"),
                                plot.title.position = "panel",
                                legend.key = element_blank(),
                                legend.background = element_blank(),
                                legend.position = "none",
                                text = element_text(size = input$lab_f, 
                                                    family = input$lab_fn),
                                panel.grid.major = element_blank(), 
                                panel.grid.minor = element_blank(),
                                panel.background = element_rect(fill = "transparent",colour = NA),
                                plot.background = element_rect(fill = "transparent",colour = NA))
  })
  
  themething2 <- reactive({theme(axis.line = element_line(size = input$lab_t), 
                                 axis.ticks = element_line(size = 1.5), 
                                 axis.text = element_text(size = input$lab_f), 
                                 axis.title = element_text(size = input$lab_f),
                                 plot.title = element_text(size = input$lab_f,
                                                           face = "bold"),
                                 plot.title.position = "panel",
                                 legend.key = element_blank(),
                                 legend.background = element_blank(),
                                 legend.position = "none",
                                 axis.line.x = element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank(),
                                 text = element_text(size = input$lab_f, 
                                                     family = input$lab_fn),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_rect(fill = "transparent",colour = NA),
                                 plot.background = element_rect(fill = "transparent",colour = NA))
  })
  
  themething3 <- reactive({theme(axis.line = element_blank(), 
                                 plot.title.position = "panel",
                                 legend.key = element_blank(),
                                 legend.background = element_blank(),
                                 legend.position = "none",
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 axis.title = element_blank(),
                                 text = element_text(size = input$lab_f, 
                                                     family = input$lab_fn),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_rect(fill = "transparent",colour = NA),
                                 plot.background = element_rect(fill = "transparent",colour = NA))
  })
  
  #modify provided data to have the coefficients of the functions to be plotted
  #the arrange() are needed so that the input data needn't be organised in any
  #way THIS IS FOR THE GAUSSIAN
  
  df_up_e <- reactive({hot_to_r(input$dtab) %>%
      arrange(pathway) %>%
      group_by(pathway) %>%
      arrange(rc, .by_group = TRUE) %>%
      mutate(A = ifelse(lead(dg)>dg,
                        (lead(dg)-dg),
                        (dg-lead(dg)))) %>%
      mutate(B = ifelse(lead(dg)>dg,
                        lead(rc),
                        rc)) %>%
      mutate(C = ifelse(lead(dg)>dg,
                        (lead(rc)-rc),
                        (lead(rc)-rc))) %>%
      mutate(D = ifelse(lead(dg)>dg,
                        (dg),
                        lead(dg))) %>%
      mutate(nudge = ifelse(dg > lead(dg),
                            dg + input$nud,
                            dg - input$nud)) %>%
      mutate(nudge = ifelse(is.na(nudge),
                            dg -input$nud,
                            nudge))
  })
  
  #make dataframe for adjusting lines, as well as the factor which affects
  #their position
  
  adj_factor <- reactive({
    as.numeric(input$adjf)
  })
    
  shifted_ex <- reactive({
    data.table(x = head(hot_to_r(input$dtab)$rc,-1) + adj_factor(),
               xend = tail(hot_to_r(input$dtab)$rc,-1) - adj_factor(),
               y = head(hot_to_r(input$dtab)$dg,-1),
               yend = tail(hot_to_r(input$dtab)$dg,-1))
  })
  
  #define what the options for the different shapes mean
  
  sxhmata <- reactive({switch(input$points,
                    "1" = 16,
                    "2" = "\u2015"
  )})
  
  #pass the typed in colours into a list that updates
  
  epil_xrwm <- reactive({ as.list(input$sel_col) 
                })
  
  #make the height and width reactive
  
  upsos <- reactive({
    as.integer(input$h_s)
  })
  
  platos <- reactive({
    as.integer(input$w_s)
  })
  
  #plot that will be outputted
  
  uelw <- reactive({
    
    #create basic plot
    
    t_plot <- ggplot(data = NULL)+
      labs(x = input$x_lab,
           y = input$y_lab) 
    
    #conditional for what theme is used
    
    if (input$x_check == TRUE)
      t_plot <- t_plot +
        themething()
    
    if (input$x_check == FALSE)
      t_plot <- t_plot +
        themething2()
    
    if (input$y_check == FALSE)
      t_plot <- t_plot +
        themething3()
    
    #create layers that will be the lines connecting points
    
    #for Gaussian
    
    if (input$fn == "1")
    for (j in 1:(nrow(df_up_e())-1)) {
      t_plot <- t_plot + stat_function(fun = function(x,j)
      {
        df_up_e()$A[j]*exp(-((x-df_up_e()$B[j])^2)/(2*(df_up_e()$C[j]/3.5)^2))+df_up_e()$D[j]
      },
      xlim = c(df_up_e()$rc[j],
               df_up_e()$rc[(j+1)]),
      size = input$csz, 
      alpha = input$alpha,
      args = list(j=j),
      color = input$c_col)
    }
    
    #for linear plot
    
    if (input$fn == "2")
      t_plot <- t_plot +
        geom_segment(data = shifted_ex(), 
                     mapping = aes(x = x, 
                                   xend = xend, 
                                   y = y, 
                                   yend = yend),
                  linetype = input$linet,
                  size = input$csz,
                  color = input$c_col,
                  alpha = input$alpha)
    
    #create datapoints on layer above lines
    
    #for one pathway
    
    if (input$mpath == FALSE)
      t_plot <- t_plot +
        geom_point(data = df_up_e(),
                   size = input$psz,
                   shape = sxhmata(),
                   color = input$p_col,
                   aes(x = rc,
                       y = dg))
    
    #for multiple pathways
    
    if (input$mpath == TRUE)
      t_plot <- t_plot +
      geom_point(data = df_up_e(),
                 size = input$psz,
                 shape = sxhmata(),
                 aes(x = rc,
                     y = dg,
                     color = as.factor(pathway))) +
      scale_color_manual(values = epil_xrwm())
    
    #options for labels
    
    #energies only
    
    if (input$labs == "1"){
               t_plot <- t_plot +
                         geom_text(data = df_up_e(),
                                   mapping = aes(x = rc,
                                              y = nudge,
                                              label = paste0("(", 
                                                             dg, 
                                                             ")")),
                                    size = input$lsz)
    }
    
    #names only
    
    if (input$labs == "2"){
      t_plot <- t_plot +
        geom_label(data = df_up_e(),
                   mapping = aes(x = rc,
                                 y = nudge,
                                 label = paste0(cpd_num)),
                   size = input$lsz)
    }
    
    #names and energies
    
    if (input$labs == "3"){
      t_plot <- t_plot +
        geom_label(data = df_up_e(),
                   mapping = aes(x = rc,
                                 y = nudge,
                                 label = paste0(cpd_num, 
                                                "\n", 
                                                "(", 
                                                dg, 
                                                ")")),
                   size = input$lsz)
    }
    
    #dynamic names and energies
    
    if (input$labs == "4"){
      t_plot <- t_plot +
        geom_label_repel(data = df_up_e(),
                         mapping = aes(x = rc,
                                 y = nudge,
                                 label = paste0(cpd_num, 
                                                "\n", 
                                                "(", 
                                                dg, 
                                                ")")),
                   size = input$lsz)
    }
    
    #no labels
    
    if (input$labs == "5"){
      t_plot <- t_plot } 
    
    t_plot
    })
  
  #render the diagram of interest
  
  output$diagram <- renderPlot({
       uelw() }) %>%
    bindCache(uelw())
  
  #make the download svg button functional
  
  output$d_plot_s <- downloadHandler(
    filename = function(){"EveR_plot.svg"},
      content = function(file) {
        ggsave(file,
               plot = uelw(),
               height = as.integer(input$h_s),
               width = as.integer(input$w_s),
               units = "cm",
               bg = "transparent",
               device = svg)
      })
  
  #make the download png button functional
  
  output$d_plot_p <- downloadHandler(
    filename = function(){"EveR_plot.png"},
    content = function(file) {
      ggsave(file,
             plot = uelw(),
             height = as.integer(input$h_s),
             width = as.integer(input$w_s),
             units = "cm",
             bg = "transparent",
             device = png)
    })
  
  #make the download button for the reactive table functional
  
  trapezi <- reactive({
    subset(df_up_e(), select = c("rc","dg","cpd_num","pathway"))
  })
  
  output$downtab<- downloadHandler(
    filename = function(){"EveR_table.csv"},
    content = function(file) {
      write.csv(trapezi(), 
                file = file,
                sep = ",")
    })
} 

shinyApp(ui = ui, server = server)