library(shiny)
library(dplyr)
library(ggplot2)
library(purrr)
library(colourpicker)
library(rhandsontable)
library(svglite)
library(ggrepel)
library(extrafont)
library(data.table)

loadfonts(device = "win")

#Some default colours to start off the user when selecting colours
xrwmata <- as.data.frame(c("black","blue","red"))

#=================
#=================
#Building of the UI
#=================
#=================
ui <- fluidPage(
  
  #=====
  #Title
  #=====
  titlePanel("EveRplot"),
  
  #==================
  #Parameters section
  #==================
  sidebarLayout(
    sidebarPanel(h2("Parameters"), #Name the section
                 
                h3("Input Data"), #Subtitle for the subsection
                
                #Upload csv button
                fileInput("file1",
                          "Choose CSV File",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                #Change subsection
                h3("Aesthetics"),
                fluidRow(column(6,
                                
                                h4("Datapoints"), #Name the subsubsection
                                
                                #Button for selecting datashapes
                                radioButtons("points",
                                             "Shape of Datapoints",
                                             choices = list("Dots" = "1",
                                                            "Lines" = "2"),
                                             selected = 1),
                                
                                #Slider for datapoint size
                                textInput("psz",
                                            "Size of Datapoints",
                                            value = 4),
                                
                                #Colourpicker for datapoint colour, 
                                #a default is given
                                colourInput("p_col",
                                            "Select Datapoint Colour",
                                            "purple"),
                                
                                #Checkbox for datapoint multiple 
                                #pathway colouring
                                checkboxInput("mpath",
                                              "Colour Datapoints by Pathway",
                                              value = FALSE),
                                
                                #List of colours conditional on multiple pathway
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
                
                #New column beside datapoints
                column(6,
                       
                       h4("Curve"), #Name the subscetion for the curves
                       
                       #Select Gaussian or linear plot line types
                       radioButtons("fn",
                                    "Type of Plot",
                                    choices = list("Gaussian" = "1",
                                                   "Linear" = "2"),
                                    selected = 1),
                       
                       #If linear selected, show slider that allows to cycle 
                       #through the types of lines available
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
                       
                       #Slider for curve size
                       textInput("csz",
                                   "Size of Curve",
                                   value = 2),
                       
                       #Slider for curve opacity
                       sliderInput("alpha",
                                   "Opacity of Curve",
                                   min = 0,
                                   max = 1,
                                   value = 0.25),
                       
                       #Slider for curve colour
                       colourInput("c_col",
                                   "Select Curve Colour",
                                   "black"),
                       
                       #Checkbox for multiple pathway colouring by curve
                       checkboxInput("mpath_c",
                                     "Colour Lines by Pathway",
                                     value = FALSE),
                       
                       #Conditional list for colouring lines by pathway
                       conditionalPanel(condition = "input.mpath_c == 1",
                                        selectizeInput(
                                          "sel_col_c",
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
                )
                ),
                
                fluidRow(
                  column(6,
                         
                         #New column for labels
                         h4("Labels"),
                         
                         #Which labels do you want
                         radioButtons("labs",
                                      "Labels",
                                      choices = list("Energies" = "1",
                                                     "Names" = "2",
                                                     "Energies and Names" = "3",
                                                     "Dynamic" = "4",
                                                     "None" = "5"),
                                      selected = "5"),
                         
                         #Slider for changing positioning of labels
                         sliderInput("nud",
                                     "Position of Labels",
                                     min = 0,
                                     max = 30,
                                     value = 5),
                         
                         #Slider for changing label size
                         textInput("lsz",
                                     "Size of Labels",
                                     value = 5),
                         
                         ),
                  
                  column(6,
                         
                         #Section for axes
                         h4("Axes"), #Define the title of the section
                         
                         #Checkbox for manually editing the y axis
                         checkboxInput("y_auto",
                                       "Manually Format Axes",
                                       value = FALSE),
                         
                         #Conditional list for colouring lines by pathway
                         conditionalPanel(condition = "input.y_auto == 1",
                                          #Text input for y axis title
                                          textInput("y_lab",
                                                    "y-Axis Label",
                                                    value = "ΔG (kcal/mol)"),
                                          
                                          #Text input for altering y axis number of tick marks
                                          textInput("lab_tick",
                                                    "Y Axis Marker Spacing",
                                                    value = 5),
                                          
                                          #Text input for altering y axis max value
                                          textInput("y_max",
                                                    "Y Axis Max Value",
                                                    value = 15),
                                          
                                          #Text input for altering y axis min value
                                          textInput("y_min",
                                                    "Y Axis Min Value",
                                                    value = -5),
                                          
                                          #Text input for changing axis font
                                          textInput("lab_fn",
                                                    "Axis Font",
                                                    value = "Verdana"),
                                          
                                          #Slider for altering axis line thickness
                                          textInput("lab_t",
                                                      "Axis Thickness",
                                                      value = 1),
                                          
                                          #Slider for altering axis title and axis label size
                                          textInput("lab_f",
                                                      "Axis Element Font Size",
                                                      value = 20)
                         ),
                         
                         #Tickbox for whether the y axis is shown
                         checkboxInput("y_check",
                                       "Show y-Axis",
                                       value = TRUE),
                         
                         #Tickbox for whether the x axis is shown
                         checkboxInput("x_check",
                                       "Show x-Axis",
                                       value = FALSE),
                         
                         #If the x axis is shown, a text input appears which 
                         #allows for editing the text it displays
                         conditionalPanel(condition = "input.x_check == 1 && input.y_auto == 1",
                                          textInput("x_lab",
                                                    "x-Axis Label",
                                                    value = "Reaction Coordinate")
                                          )
                  )
                  
                ),
                
                br(),
                
                #Statement of developer and license under which it is published
                h6("©2016-2021 ETH Zurich, Michael K. Bogdos. Please cite the 
                corresponding publication or Zenodo DOI
                when using figures created with EveRplot."),
                
                # Add a text which is hyperlinked to a github repository
                HTML(paste0("The code, documentation and a 
                            tutorial can be found on 
                            <a href='https://github.com/mbogdos96/EveRplot'>Github</a>."))
    ),
    
    mainPanel(
      
      #Create graph output panel
      h2("Graph"),
      
      #Output plot here
      plotOutput("diagram"),
      
      #Show download buttons here
      downloadButton("d_plot_s",
                     "Download Graph as .svg"),
      
      downloadButton("d_plot_p",
                     "Download Graph as .png"),
      
      downloadButton("downtab",
                     "Download Table as .csv"),
      
      #Text input for defining dimensions of the plot to be downloaded using the
      #download buttons
      textInput("h_s",
                "Plot Height (cm)",
                value = "20"),
      
      textInput("w_s",
                "Plot Width (cm)",
                value = "30"),
      
      #Create and name section where datatable will appear
      h2("Data"),
      
      #Show editable datatable here
        rHandsontableOutput("dtab")
                    )
  )
)

#======================================
#======================================
#Section of code which runs the backend
#======================================
#======================================

server <- function(input,output,session){
  
  #Create a default dataset that is plotted when no input has been provided
  rc <- c(0,5)
  
  dg <- c(0,10)
  
  cpd_num <- c("GS1","TS1")
  
  ex_dat <- data.frame(rc,dg,cpd_num) %>%
    mutate(pathway = 1)
  
  #Import the data from the upload button into the table
  df_up <- reactive({
    instuff <- input$file1
    if (is.null(instuff))
      return(ex_dat)
    read.csv(instuff$datapath)
  })
  
  #Render the table
  output$dtab <- renderRHandsontable({
    rhandsontable(df_up())
  }) %>%
    bindCache(df_up())
  
  #=======================
  #Dataframe manipulations
  #=======================
  
  #Modify provided data to have the coefficients of the functions to be plotted
  #the arrange() are needed so that the input data needn't be organised in any
  #way and group it by pathway
  df_up_e <- reactive({hot_to_r(input$dtab) %>%
      arrange(pathway) %>%
      group_by(pathway) %>%
      arrange(rc, .by_group = TRUE) %>%
      mutate(A = ifelse(lead(dg) > dg, 
                        lead(dg) - dg, 
                        dg - lead(dg)),
             B = ifelse(lead(dg) > dg, 
                        lead(rc), 
                        rc),
             C = ifelse(lead(dg) > dg,
                        lead(rc) - rc, 
                        rc - lead(rc)),
             D = ifelse(lead(dg) > dg, 
                        dg, 
                        lead(dg)),
             nudge = ifelse(dg > lead(dg), 
                            dg + input$nud, 
                            dg - input$nud),
             nudge = ifelse(is.na(nudge), 
                            dg - input$nud, 
                            nudge))
  })
  
  #Here you split the modified dataframe into n ones where n is the no. of 
  #pathways. This is only needed for when multiple pathways are needed so this
  #is why it is a different dataframe
  s_df <- reactive({
    df_up_e() %>%
      group_split(pathway)
  })
  
  #Here you define in the data analysis part the adjustment factor that the user
  #has control over in the UI
  adj_factor <- reactive({
    as.numeric(input$adjf)
  })
  
  #Here you create n geometric objects to use for each pathway in the 
  #linear plot which is parsed in geom_segment later, where the line layers are
  #created
  shifted_ex_list <- reactive({
    lapply(s_df(), function(df) {
      data.frame(x = head(df$rc,-1) + adj_factor(),
                 xend = tail(df$rc,-1) - adj_factor(),
                 y = head(df$dg, -1),
                 yend = tail(df$dg, -1),
                 pathway = df$pathway[1])
    })
  })
  
  #===================================================================
  #Definitions of themes, shape, colour, height and width UI elements
  #===================================================================
  
  #Define themes
  themething <- reactive({theme(axis.line = element_line(size = as.numeric(input$lab_t)),
                                axis.ticks.y = element_line(size = as.numeric(input$lab_t)), 
                                axis.text.y = element_text(size = as.numeric(input$lab_f)), 
                                axis.title = element_text(size = as.numeric(input$lab_f)),
                                axis.text.x = element_blank(),
                                axis.ticks.x = element_blank(),
                                plot.title = element_text(size = as.numeric(input$lab_f),
                                                          face = "bold"),
                                plot.title.position = "panel",
                                legend.key = element_blank(),
                                legend.background = element_blank(),
                                legend.position = "none",
                                text = element_text(size = as.numeric(input$lab_f), 
                                                    family = as.numeric(input$lab_fn)),
                                panel.grid.major = element_blank(), 
                                panel.grid.minor = element_blank(),
                                panel.background = element_rect(fill = "transparent",colour = NA),
                                plot.background = element_rect(fill = "transparent",colour = NA))
  })
  
  themething2 <- reactive({theme(axis.line = element_line(size = as.numeric(input$lab_t)), 
                                 axis.ticks = element_line(size = as.numeric(input$lab_t)), 
                                 axis.text = element_text(size = as.numeric(input$lab_f)), 
                                 axis.title = element_text(size = as.numeric(input$lab_f)),
                                 plot.title = element_text(size = as.numeric(input$lab_f),
                                                           face = "bold"),
                                 plot.title.position = "panel",
                                 legend.key = element_blank(),
                                 legend.background = element_blank(),
                                 legend.position = "none",
                                 axis.line.x = element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank(),
                                 text = element_text(size = as.numeric(input$lab_f), 
                                                     family = as.numeric(input$lab_fn)),
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
                                 text = element_text(size = as.numeric(input$lab_f), 
                                                     family = as.numeric(input$lab_fn)),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_rect(fill = "transparent",colour = NA),
                                 plot.background = element_rect(fill = "transparent",colour = NA))
  })
  
  #Define what the options for the different shapes mean
  sxhmata <- reactive({switch(input$points,
                    "1" = 16,
                    "2" = "\u2015"
  )})
  
  #Pass the typed in colours into a list that updates
  #For datapoints
  epil_xrwm <- reactive({ as.list(input$sel_col) 
                })
  
  #For lines
  epil_xrwm_c <- reactive({ as.list(input$sel_col_c) 
  })
  
  #Make the height and width reactive
  upsos <- reactive({
    as.integer(input$h_s)
  })
  
  platos <- reactive({
    as.integer(input$w_s)
  })
  
  #================
  #Plot generation
  #================
  uelw <- reactive({
    
    #Create basic plot
    #Automatic axes
    if (input$y_auto == FALSE){
      t_plot <- ggplot(data = NULL)+
        labs(x = input$x_lab,
             y = input$y_lab)
    }
    
    #Manually formatted axes
    if (input$y_auto == TRUE){
    t_plot <- ggplot(data = NULL)+
      labs(x = input$x_lab,
           y = input$y_lab) +
      scale_y_continuous(breaks = (seq(input$y_min, 
                                       input$y_max,
                                       by = as.numeric(input$lab_tick))))
    }
    
    #Conditional for what theme is used
    if (input$x_check == TRUE){
      t_plot <- t_plot +
        themething()
    }
    
    if (input$x_check == FALSE){
      t_plot <- t_plot +
        themething2()
    }
    
    if (input$y_check == FALSE){
      t_plot <- t_plot +
        themething3()
    }
    
    #Layers corresponding to lines connecting datapoints
    #Gaussian, one pathway
    if (input$fn == "1" && input$mpath_c == FALSE) {
      t_plot <- t_plot + map(1:(nrow(df_up_e()) - 1), ~ {
        j <- .x
        stat_function(fun = function(x) {
          df_up_e()$A[j] * exp(-((x - df_up_e()$B[j])^2) / (2 * (df_up_e()$C[j] / 3.5)^2)) + df_up_e()$D[j]
        }, 
        xlim = c(df_up_e()$rc[j], df_up_e()$rc[j + 1]),
        size = as.numeric(input$csz), 
        alpha = input$alpha,
        color = input$c_col)
      })
    }
    
    #Gaussian, more than one pathway
    if (input$fn == "1" && input$mpath_c){
      t_plot_layers <- list() #Create a list to store each stat_function layer
      for (df in s_df()) {
        for (j in 1:(nrow(df)-1)) {
          fun_def <- function(j) { #Define a string which will be iterated
            sprintf("function(x) {
          A_j <- %f
          B_j <- %f
          C_j <- %f
          D_j <- %f
          A_j*exp(-((x-B_j)^2)/(2*(C_j/3.5)^2))+D_j
        }", df$A[j], df$B[j], df$C[j], df$D[j]) #These arguments are what %f are
          }
          layer <- stat_function(data = df, aes(x = rc), 
                                 fun = eval(parse(text = fun_def(j))),
                                 xlim = c(df$rc[j], df$rc[j+1]),
                                 #Ensure that you are comparing 
                                 #the nth dataframe to the nth colour in the list
                                 #and not the nth entry in some dataframe
                                 color = epil_xrwm_c()[[which(sapply(s_df(), identical, df))]],
                                 size = as.numeric(input$csz),
                                 alpha = input$alpha) #Create each layer
          t_plot_layers <- append(t_plot_layers, list(layer))
          #Append all the layers to list
        }
      }
      t_plot <- t_plot + t_plot_layers #Add the list of layers to the plot
    }
    
    
    #Linear, one pathway
    if (input$fn == "2")
      t_plot <- t_plot +
        lapply(shifted_ex_list(), function(df) {
          geom_segment(data = df, 
                       aes(x = x, xend = xend, y = y, yend = yend),
                       linetype = input$linet,
                       size = as.numeric(input$csz),
                       color = input$c_col,
                       alpha = input$alpha)
        })
    
    #Linear, more than one pathway
    if (input$fn == "2" && input$mpath) {
      t_plot <- t_plot +
        lapply(seq_along(shifted_ex_list()), function(i) {
          geom_segment(data = shifted_ex_list()[[i]], 
                       aes(x = x, xend = xend, y = y, yend = yend),
                       linetype = input$linet,
                       size = as.numeric(input$csz),
                       color = epil_xrwm_c()[[i]],
                       alpha = input$alpha)
        })
    }
    
    #Datapoints as a layer above lines
    #One pathway
    if (input$mpath == FALSE)
      t_plot <- t_plot +
        geom_point(data = df_up_e(),
                   size = as.numeric(input$psz),
                   shape = sxhmata(),
                   color = input$p_col,
                   aes(x = rc,
                       y = dg))
    
    #Multiple pathways
    if (input$mpath == TRUE)
      t_plot <- t_plot +
      geom_point(data = df_up_e(),
                 size = as.numeric(input$psz),
                 shape = sxhmata(),
                 aes(x = rc,
                     y = dg,
                     color = as.factor(pathway))) +
      scale_color_manual(values = epil_xrwm())
    
    #===================
    #Options for labels
    #===================
    #Energies only
    if (input$labs == "1"){
               t_plot <- t_plot +
                         geom_text(data = df_up_e(),
                                   mapping = aes(x = rc,
                                              y = nudge,
                                              label = paste0("(", 
                                                             dg, 
                                                             ")")),
                                    size = as.numeric(input$lsz))
    }
    
    #Names only
    if (input$labs == "2"){
      t_plot <- t_plot +
        geom_label(data = df_up_e(),
                   mapping = aes(x = rc,
                                 y = nudge,
                                 label = paste0(cpd_num)),
                   size = as.numeric(input$lsz))
    }
    
    #Names and Energies
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
                   size = as.numeric(input$lsz))
    }
    
    #Dynamic names and energies
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
                   size = as.numeric(input$lsz))
    }
    
    #No labels
    if (input$labs == "5"){
      t_plot <- t_plot } 
    
    t_plot
    })
  
  #==============================
  #Render the diagram of interest
  #==============================
  output$diagram <- renderPlot({
       uelw() }) %>%
    bindCache(uelw())
  
  #=================
  #Download Buttons
  #=================
  
  #Make the download svg button functional
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
  
  #Make the download png button functional
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
  
  #Make the download button for the reactive table functional
  #First create a dataframe which only contains a subset of the table
  #which is in the input
  trapezi <- reactive({
    subset(df_up_e(), 
           select = c("rc",
                      "dg",
                      "cpd_num",
                      "pathway"))
  })
  
  #Define the functionality of the download button
  output$downtab<- downloadHandler(
    filename = function(){"EveR_table.csv"},
    content = function(file) {
      write.csv(trapezi(), 
                file = file,
                sep = ",")
    })
} 

#===================
#End command for app
#=================== 
shinyApp(ui = ui, server = server)
