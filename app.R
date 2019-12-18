##############################################################################
##########           AGILE LDP Phenology App                        ##########
##############################################################################
# Libraries
##############################################################################
# setwd("C:/Google Drive/gitfolder/AGILE_LDP_Phenology")
library(shiny); library(shinythemes); library(plotly);
library(leaflet); library(leaflet.minicharts)
library(tidyverse)  # data wrangling
library(scales)     # rescale()
library(rworldmap)  # mapBubbles()
library(ggrepel)    # geom_text_repel() + geom_label_repel()
library(magick)     # image editing
library(GGally)     # ggpairs() + ggmatrix()
library(ggpubr)     # ggarrange()
library(ggbeeswarm) # geom_quasirandom()
library(agricolae)  # AMMI()
library(FactoMineR) # PCA() & HCPC()
library(caret)      # modelling
library(plot3D)     # 3D plots
##############################################################################
# Pre-App R work
##############################################################################
# General color palettes
colors <- c("darkred",   "darkorange3", "darkgoldenrod2", "deeppink3",
            "steelblue", "darkorchid4", "cornsilk4",      "darkgreen")
# Expts color palette
colors_Expt <- c("darkolivegreen3", "darkolivegreen4", "palegreen2", "palegreen3",
                 "palegreen4", "darkgreen", "orangered2", "orangered4",  "coral2",
                 "coral4", "orange2", "orange4", "aquamarine3","aquamarine4",
                 "slateblue1", "slateblue4", "deepskyblue3", "deepskyblue4" )
# Locations
names_Location <- c("Rosthern, Canada", "Sutherland, Canada",  "Central Ferry, USA",
                    "Bhopal, India",    "Jessore, Bangladesh", "Bardiya, Nepal",
                    "Cordoba, Spain",   "Marchouch, Morocco",  "Metaponto, Italy" )
# Experiments
names_Expt <- c("Rosthern, Canada 2016",    "Rosthern, Canada 2017",
                "Sutherland, Canada 2016",  "Sutherland, Canada 2017",
                "Sutherland, Canada 2018",  "Central Ferry, USA 2018",
                "Bhopal, India 2016",       "Bhopal, India 2017",
                "Jessore, Bangladesh 2016", "Jessore, Bangladesh 2017",
                "Bardiya, Nepal 2016",      "Bardiya, Nepal 2017",
                "Cordoba, Spain 2016",      "Cordoba, Spain 2017",
                "Marchouch, Morocco 2016",  "Marchouch, Morocco 2017",
                "Metaponto, Italy 2016",    "Metaponto, Italy 2017" )
# Experiment short names
names_ExptShort <- c("Ro16", "Ro17", "Su16", "Su17", "Su18", "Us18",
                     "In16", "In17", "Ba16", "Ba17", "Ne16", "Ne17",
                     "Sp16", "Sp17", "Mo16", "Mo17", "It16", "It17" )
# Shape
mypch <- c(11,14,2,6,17,8, 0,5,15,18,7,9, 3,4,1,16,10,13)
#
trts <- c("DTE","DTF","DTS","DTM","VEG","REP","RDTF","DTF2","DTF2_scaled", "Tb", "Tf", "Pc", "Pf", "PTT")
# Scaling function
traitScale <- function(x, trait) {
  xout <- rep(NA, nrow(x))
  for(i in unique(x$Expt)) {
    mn <- x %>% filter(Expt == i) %>% pull(trait) %>% min(na.rm = T)
    mx <- x %>% filter(Expt == i) %>% pull(trait) %>% max(na.rm = T)
    rg <- mx - mn
    xout <- ifelse(x$Expt == i, rescale(x %>% pull(trait), c(1,5), c(mn,mx)), xout)
  }
  xout
}
# Country info
ct <- read.csv("data/data_Countries.csv")
# Lentil Diversity Panel metadata
ldp <- read.csv("data/data_LDP.csv") %>%
  mutate(Lat2 = ifelse(duplicated(Lat), jitter(Lat, 1, 0.1), Lat),
         Lon2 = ifelse(duplicated(Lon), jitter(Lon, 1, 0.1), Lon),
         Lat3 = ifelse(is.na(Lat), ct$Lat[match(Origin, ct$Country)], Lat),
         Lat3 = ifelse(duplicated(Lat3), jitter(Lat3, 1, 0.1), Lat3),
         Lon3 = ifelse(is.na(Lon), ct$Lon[match(Origin, ct$Country)], Lon),
         Lon3 = ifelse(duplicated(Lon3), jitter(Lon3, 1, 0.1), Lon3)
         )
# Modeling
m1 <- read.csv("data/model_T+P_d.csv") %>%
  mutate(Expt = factor(Expt, levels = names_Expt))
m2 <- read.csv("data/model_T+P_coefs.csv")
# PCA results
pca <- read.csv("data/data_PCA_Results.csv") %>%
  mutate(Cluster = factor(Cluster)) %>%
  select(Entry, Name, Origin, Region, Cluster, everything())
ldp <- ldp %>% left_join(select(pca, Entry, Cluster), by = "Entry")
# Tf, Pf, PTT
ptt <- read.csv("data/data_Tf_Pf.csv") %>% select(Entry, Expt, Tb, Pc, Tf, Pf, PTT) %>%
  mutate(Expt = factor(Expt, levels = names_Expt))
# Prep raw data
# Note: DTF2 = non-flowering genotypes <- group_by(Expt) %>% max(DTF)
rr <- read.csv("data/data_Raw.csv") %>%
  mutate(Rep = factor(Rep), Year = factor(Year), PlantingDate = as.Date(PlantingDate),
         Location    = factor(Location, levels = names_Location),
         Expt        = factor(Expt,     levels = names_Expt),
         ExptShort   = plyr::mapvalues(Expt, names_Expt, names_ExptShort),
         DTF2_scaled = traitScale(., "DTF2"),
         RDTF = round(1 / DTF2, 6),
         VEG  = DTF - DTE,
         REP  = DTM - DTF) %>%
  left_join(ptt, by = c("Entry","Expt")) %>%
  left_join(select(ldp, Entry, Cluster, Origin), by = "Entry")
# Average raw data
dd <- rr %>%
  group_by(Entry, Name, Expt, Location, Year, Origin, Cluster) %>%
  summarise_at(vars(DTE, DTF, DTS, DTM, VEG, REP, RDTF, DTF2, DTF2_scaled, Tb, Tf, Pc, Pf, PTT),
               funs(mean), na.rm = T) %>% ungroup() %>%
  mutate(Expt        = factor(Expt, levels = names_Expt),
         ExptShort   = plyr::mapvalues(Expt, names_Expt, names_ExptShort),
         Location    = factor(Location, levels = names_Location),
         DTF2_scaled = traitScale(., "DTF2"))
# Prep environmental data
ee <- read.csv("data/data_Env.csv") %>%
  mutate(Date      = as.Date(Date),
         ExptShort = plyr::mapvalues(Expt, names_Expt, names_ExptShort),
         ExptShort = factor(ExptShort, levels = names_ExptShort),
         Expt      = factor(Expt,      levels = names_Expt),
         Location  = factor(Location,  levels = names_Location),
         DayLength_rescaled = rescale(DayLength, to = c(0, 40)) )
# Prep field trial info
xx <- dd %>%
  group_by(Expt) %>%
  summarise_at(vars(DTE, DTF, DTS, DTM), funs(min, mean, max), na.rm = T) %>%
  ungroup()
ff <- read.csv("data/data_Info.csv") %>%
  mutate(Start = as.Date(Start), Expt = factor(Expt, levels = names_Expt)) %>%
  left_join(xx, by = "Expt")
for(i in unique(ee$Expt)) {
  ee <- ee %>%
    filter(Expt != i | (Expt == i & DaysAfterPlanting <= ff$DTM_max[ff$Expt == i]))
}
e2 <- ee
for(i in unique(ee$Expt)) {
  e2 <- e2 %>%
    filter(Expt != i | (Expt == i & DaysAfterPlanting <= ff$DTF_max[ff$Expt==i]))
}
tt <- e2 %>% group_by(Location, Year) %>%
  summarise(T_mean = mean(Temp_mean, na.rm = T), T_sd = sd(Temp_mean, na.rm = T),
            P_mean = mean(DayLength, na.rm = T), P_sd = sd(DayLength, na.rm = T) ) %>%
  ungroup() %>%
  mutate(Expt = paste(Location, Year)) %>%
  select(-Location, -Year)
me <- c("Temperate", "South Asia", "Mediterranean")
ff <- ff %>% left_join(tt, by = "Expt") %>%
  mutate(ExptShort = plyr::mapvalues(Expt, names_Expt, names_ExptShort),
         ExptShort = factor(ExptShort, levels = names_ExptShort),
         MacroEnv  = factor(MacroEnv,  levels = me),
         Expt      = factor(Expt,      levels = names_Expt),
         T_mean = round(T_mean, 1),
         P_mean = round(P_mean, 1))
# ggplot theme
theme_AGL <- theme_bw() + theme(strip.background = element_rect(fill = "White"))
#
# R^2 function
modelR2 <- function(x, y) {
  1 - ( sum((x - y)^2, na.rm = T) / sum((x - mean(x, na.rm = T))^2, na.rm = T))
}
# RMSE function
modelRMSE <- function(x, y) {
  sqrt(sum((x-y)^2) / length(x))
}
##############################################################################
# User interface
##############################################################################
ui <- fluidPage(theme = shinytheme("yeti"), br(),
  sidebarLayout(
    sidebarPanel(numericInput("Entry", "Entry", 1, 1, 324, 1),
                 tableOutput("EntryTable"),
                 radioButtons("Plot_Entry", "Plot Entry", c(T, F), T, inline = T),
                 selectInput("Trait", "Trait", trts, "DTF"),
                 checkboxGroupInput("MyClusters", "Clusters", c("1","2","3","4","5","6","7","8"), c("1","2","3","4","5","6","7","8"), inline = T),
                 selectInput("Expt", "Expt", names_Expt, "Rosthern, Canada 2016"),
                 checkboxGroupInput("Expts", "Expts", names_Expt, selected = names_Expt),
                 width = 2),
    mainPanel(
      tabsetPanel(
        tabPanel("Home",
                 h1("AGILE LDP Phenology App"), hr(),
                 uiOutput("AgileLogo"), hr(), 
                 p("Author: Derek Wright"),
                 p("Contact: derek.wright@usask.ca"),
                 p("Associated Paper:"),
                 p(strong("ASSESSING ADAPTATION AGILITY OF LENTIL (", em("Lens culinaris"), " Medik.) DIVERSITY PANEL TO TEMPERATURE AND PHOTOPERIOD")),
                 p("Derek Wright, Sandesh Neupane, Taryn Heidecker, Teketel Haile, Clarice Coyne, Sripada Udupa, Fatima Henkrar, Eleonora Barilli, Diego Rubiales, Tania Gioia, Reena Mehra, Ashutosh Sarker, Rajeev Dhakal, Babul Anwar, Debashish Sarker, Albert Vandenberg, and Kirstin E. Bett"),
                 p(strong("Project Collaborators:")),
                 p("- Department of Plant Sciences and Crop Development Centre, University of Saskatchewan, Saskatoon, Saskatchewan, Canada"),
                 p("- United States Department of Agriculture Western Region Plant Introduction Station, Pullman, Washington, USA"),
                 p("- International Center for Agriculture Research in the Dry Areas, Rabat, Morocco"),
                 p("- Institute for Sustainable Agriculture, Spanish National Research Council, Cordoba, Spain"),
                 p("- School of Agriculture, Forestry, Food and Environmental Sciences, University of Basilicata, Potenza, Italy"),
                 p("- International Center for Agriculture Research in the Dry Areas, New Delhi, India"),
                 p("- Local Initiatives for Biodiversity, Research and Development, Pokhara, Nepal"),
                 p("- Bangladesh Agricultural Research Institute, Jessore, Bangladesh"), hr(),
                 p(strong("Project Sponsors:")),
                 p("- Saskatchewan Pulse Growers Association"),
                 p("- Western Grains Research Foundation"),
                 p("- GenomePrairie"),
                 p("- GenomeCanada"),
                 p("- Saskatchewan Ministry of Agriculture")
        ), #Home tab
        tabPanel("Tables", tabsetPanel(
          tabPanel("LDP", h1("Lentil Diversity Panel"), DT::dataTableOutput("Table_LDP")),
          tabPanel("Field Trials", h1("Field Trial Info"), DT::dataTableOutput("Table_FieldTrials")),
          tabPanel("PCA", h1("PCA and Hierarchical Clustering of DTF"), DT::dataTableOutput("Table_PCA")),
          tabPanel("Coefs", h1("Photothermal Model Coeficients"), DT::dataTableOutput("Table_Coefs")),
          tabPanel("DTF Predictions", h1("Field Trial Info"), DT::dataTableOutput("Table_Model")) 
          )), #Tables tab
        tabPanel("Phenology", tabsetPanel(
          tabPanel("Trait 1",  
                   radioButtons("GroupByCluster", "Group by Cluster", c(T,F), F, inline = T),
                   plotlyOutput("Violin",  height = 500)),
          tabPanel("Trait 2", plotlyOutput("Violins", height = 500)),
          tabPanel("Lines 1"),
          tabPanel("Lines 2"),
          tabPanel("EnvData 1",
                   fluidRow(column(radioButtons("Plot_DTF", "Plot DTF Window", c(T,F), F, inline = T), width = 2),
                            column(radioButtons("Plot_DTS", "Plot DTS Window", c(T,F), F, inline = T), width = 2),
                            column(radioButtons("Plot_DTM", "Plot DTM Window", c(T,F), F, inline = T), width = 2)),
                   plotOutput("EnvData", height = 500)),
          tabPanel("EnvData 2", plotOutput("Phenology")),
          tabPanel("ggridges", plotOutput("ggridges")),
          tabPanel("Map", leafletOutput("Data_Map", height = 500)),
          tabPanel("Corr", 
                   fluidRow(column(selectInput("Trait2", "Trait (x axis)", trts, "DTS"), width = 2),
                            column(selectInput("Expt2", "Expt (x axis)", names_Expt), width = 3),
                            column(radioButtons("AddTrendline", "Add Trendline", c(T,F), T, inline = T), width = 2)),
                   plotlyOutput("Corr", height = 500))
        )), #Phenology tab
        tabPanel("Clusters", tabsetPanel(
          tabPanel("PCA",
                   radioButtons("PC_Type","Plot", c("PC1 x PC2","PC1 x PC3","PC2 x PC3"), "PC1 x PC2", inline = T),
                   plotlyOutput("PCA_PC", height = 500)),
          tabPanel("Ribbon", plotlyOutput("PCA_Ribbon", height = 500)),
          tabPanel("Map", tabsetPanel(
            tabPanel("Pies", leafletOutput("PCA_Pies", height = 500)),
            tabPanel("Points", radioButtons("PCA_Map_Points", "Plot", c("GeoLocated", "All"), inline = T),
                     leafletOutput("PCA_Points", height = 500))
          ) ),
          tabPanel("Origins",
                   checkboxGroupInput("PCA_Countries", "Countries", unique(ldp$Origin), selected = c("Canada", "India"), inline = T),
                   tabsetPanel(
                     tabPanel("Expt",  plotlyOutput("PCA_Origins_Expt")),
                     tabPanel("Expts", plotOutput("PCA_Origins_Expts"))
  
          ) ) ) ), #HCPC tab
        tabPanel("PhotoThermal Model", tabsetPanel(
          tabPanel("PhotoThermal Plane", br(), plotOutput("Modeling_3D" , height = 500)),
          tabPanel("Obs x Pre", plotlyOutput("Modeling", height = 500)),
          tabPanel("Expt",  plotlyOutput("Modeling_Expt", height = 500)),
          tabPanel("Expts", plotOutput("Modeling_Expts", height = 500)),
          tabPanel("Coefficients", tabsetPanel(
            tabPanel("abc",   plotlyOutput("abc", height = 500)),
            tabPanel("TraitxCoef",
                     radioButtons("Select_abc", "Select Coefficient", c("a","b","c"), "a", inline = T),
                     plotlyOutput("TraitxCoef", height = 500)),
            tabPanel("cxb",   plotlyOutput("cxb", height = 500))
            ) ),
          tabPanel("Adaptation",
                   fluidRow(column(selectInput("Ad_Country", "Adaptation Country", unique(ldp$Origin)[order(unique(ldp$Origin))], selected = "Canada"), width = 3),
                            column(numericInput("sd_Mult","sd Multiplier",1,0.5,3,0.1), width = 2)),
                   checkboxGroupInput("Ad_Countries", "Countries", unique(ldp$Origin)[order(unique(ldp$Origin))], selected = "USA", inline = T),
                   radioButtons("Select_Coef_Ad", "Select Coefficient", c("a","b","c"), "c", inline = T),
                   tabsetPanel(
                     tabPanel("TraitxCoef",  plotlyOutput("Adapted1", height = 500)),
                     tabPanel("cxb", plotlyOutput("Adapted2", height = 500)),
                     tabPanel("Table", 
                              downloadButton("Adapted_Table.csv", "Download Selected Table"),
                              DT::dataTableOutput("Adapted_Table")) 
                     ) ),
          tabPanel("Temperature", numericInput("TempIncrease","Temperature Increase",1,0,5,0.1), tabsetPanel(
            tabPanel("Expts",    plotlyOutput("PCA_TempInc_Expts", height = 500)),
            tabPanel("Clusters", plotOutput("PCA_TempInc_Clusters")),
            tabPanel("Expt", plotlyOutput("PCA_TempInc_Cluster", height = 500)) 
            ) ),
          tabPanel("Predict DTF", 
                   fluidRow(column(numericInput("Mean_T", "Mean Temperature",13,0,25,0.1),
                                   numericInput("Mean_P", "Mean Photoperiod",12,0,18,0.1), width = 2),
                            column(numericInput("DTF_Min", "Min DTF", 30,30,200,1),
                                   numericInput("DTF_Max", "Max DTF", 80,30,200,1), width = 2)),
                   tabsetPanel(
                     tabPanel("Clusters", plotlyOutput("Predict_DTF_Clusters", height = 500)),
                     tabPanel("DTFxCoef", 
                              radioButtons("Select_Coef_DTF", "Select Coefficient", c("a","b","c"), "c", inline = T),
                              plotlyOutput("Predict_DTF_DTFxCoef", height = 500)),
                     tabPanel("cxb", plotlyOutput("Predict_DTF_cxb", height = 500)),
                     tabPanel("Table", 
                              downloadButton("Adapted_DTF_Table.csv", "Download Selected Table"),
                              DT::dataTableOutput("Adapted_DTF_Table"))
                   ) )
        ) )
    ) ) ) 
) #fluidpage
##############################################################################
# Server
##############################################################################
server <- function(input, output) {
  #
  # - Sidebar
  #
  output$EntryTable <- renderTable({
    ldp %>% filter(Entry == input$Entry) %>% select(Name, Origin)
  })
  #
  # - Home
  #
  output$AgileLogo     <- renderUI({img(src = "Logo_Agile.png", height = 150)})
  #
  # - Tables
  #
  output$Table_LDP <- DT::renderDataTable(ldp %>% select(-Lat2, -Lat3, -Lon2, -Lon3, -Cluster), options = list(pageLength = 324))
  #
  output$Table_FieldTrials <- DT::renderDataTable(ff %>% select(ExptShort, Expt, MacroEnv, Start, End, Days, T_mean, P_mean, Lat, Lon), options = list(pageLength = 18))
  #
  output$Table_PCA <- DT::renderDataTable(pca, options = list(pageLength = 324))
  #
  output$Table_Coefs <- DT::renderDataTable(m2, options = list(pageLength = 324))
  #
  output$Table_Model <- DT::renderDataTable(m1%>%filter(Expt%in% input$Expts), options = list(pageLength = 324))
  #
  #
  # - Data
  #
  # input <- list(Expts = names_Expt, Trait = "DTF", Plot_Entry = "TRUE", Entry = 1, MyClusters = c("1","2","3","4"))
  output$Violins <- renderPlotly({
    xx <- dd %>% select(Entry, Name, Expt, ExptShort, Origin, Cluster, input$Trait) %>%
      filter(Expt %in% input$Expts, Cluster %in% input$MyClusters) %>%
      left_join(select(ff, Expt, MacroEnv), by = "Expt") %>%
      gather(Trait, Value, input$Trait) %>%
      mutate(Trait = factor(Trait, levels = c(input$Trait)) )
    mp <- ggplot(xx, aes(x = ExptShort, y = Value)) +
      geom_violin(aes(fill = MacroEnv), alpha = 0.25, color = NA) +
      geom_quasirandom(aes(key1 = Entry, key2 = Name, key3 = Origin, key4 = Cluster, key5 = Expt, color = Cluster), alpha = 0.5) +
      theme_AGL +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "none") +
      scale_fill_manual(values = c("darkgreen","darkred","darkblue")) +
      scale_color_manual(values = colors[as.numeric(input$MyClusters)])
      labs(x = NULL, y = input$Trait)
    if(input$Plot_Entry == T) {
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry),
                            aes(key1 = Entry, key2 = Name, key3 = Origin, key4 = Cluster, key5 = Expt),
                            color = "Black", fill = "Red", pch = 23)
    }
    mp
  })
  # input <- list(Expt = "Rosthern, Canada 2017", Trait = "DTF", Plot_Entry = T, Entry = 1, GroupByCluster = F, MyClusters = c("1","2","3","4"))
  output$Violin <- renderPlotly({
    xx <- dd %>% select(Entry, Name, Expt, ExptShort, Origin, Cluster, input$Trait) %>%
      filter(Expt == input$Expt, Cluster %in% input$MyClusters) %>%
      left_join(select(ff, Expt, MacroEnv), by = "Expt") %>%
      gather(Trait, Value, input$Trait) %>%
      mutate(Trait = factor(Trait, levels = c(input$Trait)) )
    if(input$GroupByCluster == F) { xx <- xx %>% mutate(Cluster = "1") }
    mp <- ggplot(xx, aes(x = Cluster, y = Value)) +
      geom_violin(aes(fill = Cluster), alpha = 0.5, color = NA) +
      geom_quasirandom(aes(key1 = Entry, key2 = Name, key3 = Origin)) +
      scale_fill_manual(values = colors[as.numeric(input$MyClusters)]) +
      theme_AGL +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(title = input$Expt, y = input$Trait)
    if(input$Plot_Entry == T) {
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry),
                            aes(key1 = Entry, key2 = Name, key3 = Origin),
                            color = "Black", fill = "Red", size = 3, pch = 23)
    }
    mp
  })
  # input <- list(Expt = "Rosthern, Canada 2017", Trait = "DTF", Plot_Entry = T, Entry = 1, MyClusters = c("1","2","3","4"))
  output$Data_Map <- renderLeaflet({
    xx <- dd %>% filter(Expt == input$Expt) %>%
      gather(Trait, Value, DTE, DTF, DTF2, DTS, DTM, VEG, REP, RDTF, DTF2_scaled, Tb, Tf, Pc, Pf, PTT) %>%
      filter(Trait == input$Trait) %>%
      left_join(select(ldp, Entry, Lat=Lat3, Lon=Lon3), by = "Entry") %>%
      filter(!is.na(Lat), !is.na(Lon)) %>%
      select(Entry, Name, Origin, Cluster, Lat, Lon, Expt, Trait, Value)
    xE <- xx %>% filter(Entry == input$Entry)
    pal <- colorNumeric(c("darkgoldenrod2", "darkblue"), xx$Value)
    xx <- xx %>% filter(Cluster %in% input$MyClusters)
    #
    mp <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addMinicharts(
        lat = xx$Lat, lng = xx$Lon, width = 10, fillColor = pal(xx$Value),
        popup = popupArgs(supValues = xx, showValues = F)
      ) %>%
    addLegend("bottomleft", pal = pal, values = xx$Value,
             title = input$Trait, opacity = 1 )
    if(input$Plot_Entry == T) {
      mp <- mp %>% addMinicharts(lat = xE$Lat, lng = xE$Lon, width = 10, fillColor = "Red") 
    }
    mp
  })
  # input <- list(Expt = "Rosthern, Canada 2017", Trait = "DTF", Expt2 = "Rosthern, Canada 2017", Trait2 = "DTS", Plot_Entry = T, Entry = 1, MyClusters = c("1","2","3","4"))
  output$Corr <- renderPlotly({
    x1 <- dd %>% select(Entry, Name, Expt, Origin, Cluster, input$Trait) %>%
      filter(Expt %in% input$Expt, Cluster %in% input$MyClusters) 
    x2 <- dd %>% filter(Expt %in% input$Expt2) %>% select(Entry, input$Trait2)
    xx <- left_join(x1, x2, by = "Entry") %>% as.data.frame()
    r2 <- round(cor(xx[,input$Trait2], xx[,input$Trait], method = "pearson", use = "complete"), 3)
    rmse <- round(modelRMSE(xx[,input$Trait2], xx[,input$Trait]), 1)
    coefs <- round(lm(xx[,input$Trait2] ~ xx[,input$Trait])$coefficients, 3)
    mp <- ggplot(xx, aes(y = get(input$Trait2), x = get(input$Trait)))
    if(input$AddTrendline == T) { mp <- mp + geom_smooth(method = "lm") }
    mp <- mp +
      geom_point(aes(key1 = Entry, key2 = Name, key3 = Origin, color = Cluster)) +
      scale_color_manual(values = colors[as.numeric(input$MyClusters)]) +
      theme_AGL +
      theme(legend.position = "none") +
      labs(title = paste(input$Expt, "| RR = ", r2, " | RMSE = ", rmse, " | y = ", coefs[2],"x + ", coefs[1]), 
           x = paste(input$Expt2, input$Trait2, sep = " - "), 
           y = paste(input$Expt, input$Trait, sep = " - ") )
    if(input$Plot_Entry == T) {
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry),
                            aes(key1 = Entry, key2 = Name, key3 = Origin),
                            color = "Black", fill = "Red", size = 3, pch = 23)
    }
    mp
  })
  # input <- list(Expts = c("Rosthern, Canada 2016","Rosthern, Canada 2017")
  observe({
    output$ggridges <- renderPlot({ 
      xx <- dd %>% select(Expt, DTF, DTS, DTM) %>% 
        filter(Expt %in% input$Expts) %>%
        gather(Trait, Value, DTF, DTS, DTM) %>%
        mutate(Trait = factor(Trait, levels = c("DTF", "DTS", "DTM")))
      # Plot
      mp <- ggplot(xx, aes(x = Value, y = Expt, fill = Trait)) + 
        ggridges::geom_density_ridges() +
        scale_fill_manual(values = c("darkgreen", "darkred", "darkgoldenrod2")) +
        theme(legend.position = "top", legend.margin = unit(c(0,0,0,0), "cm")) +
        labs(y = NULL, x = "Days After Sowing")
      mp
    }, height = 50*length(input$Expts))
  })
  # input <- list(Expt = "Rosthern, Canada 2016", Plot_Entry = T, Entry = 1, Plot_DTF = T, Plot_DTS = F, Plot_DTM = F)
  output$EnvData <- renderPlot({
    xx <- ee %>% filter(Expt == input$Expt)
    yy <- ff %>% filter(Expt == input$Expt)
    y1 <- select(yy, Expt, Location, Year, min = DTF_min, max = DTF_max) %>%
      mutate(Trait = "DTF")
    y2 <- select(yy, Expt, Location, Year, min = DTS_min, max = DTS_max) %>%
      mutate(Trait = "DTS")
    y3 <- select(yy, Expt, Location, Year, min = DTM_min, max = DTM_max) %>%
      mutate(Trait = "DTM")
    yy <- bind_rows(y1, y2, y3)
    xE <- dd %>% filter(Entry == input$Entry, Expt == input$Expt)
    mp <- ggplot(xx)
    if(input$Plot_DTF == F) {
      yy <- yy %>% filter(Trait != "DTF")
    } else { if(input$Plot_Entry == T) { mp <- mp + geom_vline(xintercept = xE$DTF) } }
    if(input$Plot_DTS == F) {
      yy <- yy %>% filter(Trait != "DTS")
    } else { if(input$Plot_Entry == T) { mp <- mp + geom_vline(xintercept = xE$DTS) } }
    if(input$Plot_DTM == F) {
      yy <- yy %>% filter(Trait != "DTM")
    } else { if(input$Plot_Entry == T) { mp <- mp + geom_vline(xintercept = xE$DTM) } }
    myrects <- as.logical(c(input$Plot_DTF, input$Plot_DTS, input$Plot_DTM))
    yy <- yy %>% mutate(Trait = factor(Trait, levels = c("DTF","DTS","DTM")[myrects]))
    if(as.logical(input$Plot_DTF) | as.logical(input$Plot_DTS) | as.logical(input$Plot_DTM) == TRUE) {
      mp <- mp + geom_rect(data = yy, aes(xmin = min, xmax = max, fill = Trait),
                           ymin = -Inf, ymax = Inf, alpha = 0.4)
    }
    mp <- mp +
      geom_line(aes(x = DaysAfterPlanting, y = DayLength_rescaled, color = "Blue")) +
      geom_line(aes(x = DaysAfterPlanting, y = Temp_mean, color = "darkred") ) +
      geom_ribbon(aes(x = DaysAfterPlanting, ymin = Temp_min, ymax = Temp_max),
                  fill = alpha("darkred", 0.25),
                  color = alpha("darkred", 0.25)) +
      facet_grid(.~Expt, scales = "free_x", space = "free_x") +
      scale_color_manual(name = NULL, values = c("Blue", "darkred"),
                         labels = c("Day length", "Temperature") ) +
      scale_fill_manual(name = NULL,
                        values = c("darkgreen", "darkblue", "darkgoldenrod2")[myrects]) +
      coord_cartesian(ylim=c(0,40)) +
      theme_AGL +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_y_continuous(sec.axis = sec_axis(~ (16.62 - 9.11) * . / (40 - 0) + 9.11,
                                             breaks = c(10, 12, 14, 16), name = "Hours")) +
      
      labs(y = expression(paste(degree, "Celcius")), x = "Days after Sowing") +
      guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2))
    mp
  })
  # input <- list(Expts = c("Rosthern, Canada 2016", "Rosthern, Canada 2017")
  observe({
    output$Phenology <- renderPlot({
      # Prep data
      xx <- ee %>% filter(Expt %in% input$Expts)
      yy <- ff %>% filter(Expt %in% input$Expts) %>%
        mutate(DTF_min = Start + DTF_min, DTF_max = Start + DTF_max,
               DTM_min = Start + DTM_min, DTM_max = Start + DTM_max)
      y1 <- select(yy, Expt, Location, Year, min = DTF_min, max = DTF_max) %>%
        mutate(Trait = "DTF")
      y2 <- select(yy, Expt, Location, Year, min = DTM_min, max = DTM_max) %>%
        mutate(Trait = "DTM")
      yy <- bind_rows(y1, y2)
      # Plot
      ggplot(xx) +
        geom_rect(data = yy, aes(xmin = min, xmax = max, fill = Trait),
                  ymin = 0, ymax = 40, alpha = 0.4) +
        geom_line(aes(x = Date, y = DayLength_rescaled, color = "Blue")) +
        geom_line(aes(x = Date, y = Temp_mean, color = "darkred") ) +
        geom_ribbon(aes(x = Date, ymin = Temp_min, ymax = Temp_max),
                    fill = alpha("darkred", 0.25),
                    color = alpha("darkred", 0.25)) +
        facet_grid(Expt~., scales = "free_x", space = "free_x") +
        scale_color_manual(name = NULL, values = c("Blue", "darkred"),
                           labels = c("Day length", "Temperature") ) +
        scale_fill_manual(name = NULL, values = c("darkgreen", "darkgoldenrod2")) +
        coord_cartesian(ylim=c(0,40)) +
        theme_AGL +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        scale_x_date(breaks = "1 month", labels = date_format("%B")) +
        scale_y_continuous(sec.axis = sec_axis(~ (16.62 - 9.11) * . / (40 - 0) + 9.11,
                                               breaks = c(10, 12, 14, 16), name = "Hours")) +
        
        labs(y = expression(paste(degree, "Celcius"))) +
        guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2))
    }, height = 150*length(input$Expts))
  })
  #
  # - HCPC
  #
  # input <- list(PC_Type = "PC1 x PC2", Plot_Entry = T, Entry = 1)
  output$PCA_PC <- renderPlotly({
    hulltype <- paste(input$PCx, input$PCy)
    if(input$PC_Type == "PC1 x PC2") { myx <- "PC1"; myy <- "PC2" }
    if(input$PC_Type == "PC1 x PC3") { myx <- "PC1"; myy <- "PC3" }
    if(input$PC_Type == "PC2 x PC3") { myx <- "PC2"; myy <- "PC3" }
    find_hull <- function(df) df[chull(df[,myx], df[,myy]), ]
    polys <- plyr::ddply(pca, "Cluster", find_hull) %>% mutate(Cluster = factor(Cluster))
    mp <- ggplot(pca, aes(x = get(myx), y = get(myy), color = Cluster)) +
      geom_polygon(data = polys, alpha = 0.15, aes(fill = Cluster)) +
      geom_point(aes(key1 = Entry, key2 = Name, key3 = Origin)) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      theme_AGL +
      labs(title = "PCA", x = myx, y = myy)
    if(input$Plot_Entry == T) {
      mp <- mp + geom_point(data = pca %>% filter(Entry == input$Entry),
                            color = "Black", fill = "Red", size = 3, pch = 23)
    }
    mp
  })
  # input <- list(Expts = names_Expt, Trait = "DTF", Plot_Entry = T, Entry = 1)
  output$PCA_Ribbon <- renderPlotly({
    xx <- dd %>%
      filter(Expt %in% input$Expts) %>%
      group_by(Expt, ExptShort, Cluster) %>%
      summarise(mean = mean(DTF2_scaled, na.rm = T), sd = sd(DTF2_scaled, na.rm = T)) %>%
      ungroup() %>%
      mutate(ClusterNum = plyr::mapvalues(Cluster, as.character(1:8), summary(pca$Cluster)))
    xE <- dd %>% filter(Entry %in% input$Entry, Expt %in% input$Expts)
    mp <- ggplot(xx, aes(x = ExptShort)) +
      geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = Cluster, group = Cluster),
                  alpha = 0.1, color = NA) +
      geom_point(aes(y = mean, color = Cluster)) +
      geom_line(aes(y = mean, color = Cluster, group = Cluster), size = 1) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      coord_cartesian(ylim = c(0.95,5.05), expand = F) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            strip.placement = "outside",
            axis.line = element_line(), axis.ticks = element_line()) +
      labs(y = "DTF (scaled 1-5)", x = NULL)
    if(input$Plot_Entry == T) {
      mp <- mp +
        geom_line(data = xE, aes(y = DTF2_scaled, group = Entry), color = "Red")  +
        geom_point(data = xE, aes(key1 = Entry, key2 = Name, key3 = Origin, key4 = Cluster, key5 = Expt, y = DTF2_scaled),
                   color = "Black", fill = "Red", size = 2, pch = 23)
    }
    mp
  })
  # 
  output$PCA_Pies <- renderLeaflet({
    pal <- colorFactor(colors, domain = 1:8)
    xx <- ldp %>%
      mutate(test1 = factor(paste(Origin, Cluster))) %>%
      group_by(Origin, Cluster) %>% summarise(Count = n()) %>%
      spread(Cluster, Count) %>%
      left_join(select(ct, Origin=Country, Lat, Lon), by = "Origin") %>%
      ungroup() %>%
      filter(!is.na(Lat), !is.na(Lon))
    xx[is.na(xx)] <- 0
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addMinicharts(
          lat = xx$Lat, lng = xx$Lon, width = 20, height = 20, type = "pie",
          chartdata = select(xx, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`),
          colorPalette = pal(1:8) )
  })
  # input <- list(Plot_Entry = T, Entry = 1, PCA_Map_Points = "All", MyClusters = c("a","2","3","4"))
  output$PCA_Points <- renderLeaflet({
    pal <- colorFactor(colors, domain = 1:8)
    xx <- ldp %>% select(-Lat, -Lon)
    if(input$PCA_Map_Points == "All") {
      xx <- xx %>% rename(Lat=Lat3, Lon=Lon3) %>% select(-Lat2,-Lon2)
    } else { xx <- xx %>% rename(Lat=Lat2, Lon=Lon2) %>% select(-Lat3,-Lon3) }
    xx <- xx %>% filter(!is.na(Lat), !is.na(Lon))
    xE <- xx %>% filter(Entry == input$Entry)
    xx <- xx %>% filter(Cluster %in% input$MyClusters)
    #
    mp <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addMinicharts(lng = xx$Lon, lat = xx$Lat, width = 10,
                    fillColor = pal(xx$Cluster),
                    popup = popupArgs(supValues = xx, showValues = F) ) %>%
      addLegend("bottomleft", pal = pal, values = xx$Cluster,
                title = "Cluster", opacity = 1 )
    if(input$Plot_Entry == T) {
      mp <- mp %>% addMinicharts(lng = xE$Lon, lat = xE$Lat, width = 10, fillColor = "Red" )
    }
    mp
  })
  # input <- list(Expts = c("Rosthern, Canada 2017", "Bhopal, India 2017"), Trait = "DTF", Plot_Entry = T, Entry = 1, PCA_Countries = c("Canada","India"))
  observe({
    output$PCA_Origins_Expts <- renderPlot({
      yy <- input$PCA_Countries
      xx <- dd %>%
        filter(Expt %in% input$Expts) %>%
        mutate(Origin = factor(Origin, levels = unique(Origin)[rev(order(unique(Origin)))])) %>%
        filter(Origin %in% yy) %>%
        mutate(Origin = factor(Origin, levels = yy))
      mp <- ggplot(xx, aes(y = get(input$Trait), x = Origin)) +
        geom_quasirandom(aes(color = Cluster)) +
        facet_grid(Expt~., scales = "free_y") +
        scale_color_manual(values = colors) +
        theme_AGL +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        labs(title = input$Trait, y = NULL, x = NULL)
      if(input$Plot_Entry == T) {
        mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry),
                              color = "Black", fill = "Red", size = 3, pch = 23)
      }
      mp
    }, height = 250*length(input$Expts) )
  })
  # input <- list(Expt = "Rosthern, Canada 2017", Trait = "DTF", Plot_Entry = T, Entry = 1, PCA_Countries = c("Canada","India"))
  output$PCA_Origins_Expt <- renderPlotly({
    yy <- input$PCA_Countries
    xx <- dd %>%
      filter(Expt == input$Expt) %>%
      mutate(Origin = factor(Origin, levels = unique(Origin)[rev(order(unique(Origin)))])) %>%
      filter(Origin %in% yy) %>%
      mutate(Origin = factor(Origin, levels = yy))
    mp <- ggplot(xx, aes(y = get(input$Trait), x = Origin)) +
      geom_quasirandom(aes(key1 = Entry, key2 = Name, color = Cluster)) +
      facet_grid(.~Expt, scales = "free_y") +
      scale_color_manual(values = colors) +
      theme_AGL +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      labs(y = input$Trait, x = NULL)
    if(input$Plot_Entry == T) {
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry),
                            aes(key1 = Entry, key2 = Name, color = Cluster),
                            color = "Black", fill = "Red", size = 3, pch = 23)
    }
    mp
  })
  #
  # -PhotoThermal Model
  #
  # input <- list(Entry = 3)
  output$Modeling_3D <- renderPlot({
    xx <- rr %>% filter(!is.na(RDTF)) %>%
      left_join(select(ff, Expt, T_mean, P_mean, MacroEnv), by = "Expt")
    #
    x1 <- xx %>% filter(Entry == input$Entry) %>% arrange(Expt) %>% 
      mutate(myPal = as.character(plyr::mapvalues(MacroEnv, 
               c("Temperate", "South Asia", "Mediterranean"), c("darkgreen", "darkred", "darkblue"))))
    x <- x1$T_mean
    y <- x1$P_mean
    z <- x1$RDTF
    fit <- lm(z ~ x + y)
    # predict values on regular xy grid
    grid.lines = 12
    x.pred <- seq(min(x), max(x), length.out = grid.lines)
    y.pred <- seq(min(y), max(y), length.out = grid.lines)
    xy <- expand.grid( x = x.pred, y = y.pred)
    z.pred <- matrix(predict(fit, newdata = xy),
                     nrow = grid.lines, ncol = grid.lines)
    # fitted points for droplines to surface
    fitpoints <- predict(fit)
    # scatter plot with regression plane
    par(mar=c(1.5, 2.5, 1.5, 0.5))
    scatter3D(x, y, z, pch = 18, cex = 2, zlim = c(0.005,0.03), 
              col = x1$myPal, colvar = as.numeric(x1$MacroEnv), colkey = F,
              theta = 40, phi = 25, ticktype = "detailed", cex.lab = 1, cex.axis = 0.5,
              xlab = "Temperature", ylab = "Photoperiod", zlab = "1 / DTF",
              surf = list(x = x.pred, y = y.pred, z = z.pred, col = "black",
                          facets = NA, fit = fitpoints), main = unique(x1$Name))
  })
  # input <- list(Expts = names_Expt, Plot_Entry = T, Entry = 1, MyClusters = c("a","2","3","4"))
  output$Modeling <- renderPlotly({
    xx <- m1 %>% left_join(select(pca, Entry, Origin, Cluster), by = "Entry") %>%
      filter(!is.na(DTF), Expt %in% input$Expts, Cluster %in% input$MyClusters)
    mymean <- mean(xx$DTF)
    r2  <- 1 - (sum((xx$DTF - xx$Predicted_DTF)^2) / (sum((xx$DTF - mymean)^2)))
    r2 <- round(r2, 3)
    #
    mp <- ggplot(xx, aes(x = DTF, y = Predicted_DTF, color = Expt)) +
      geom_point(aes(key1 = Entry, key2 = Name, key3 = Origin, key4 = Cluster), alpha = 0.8) +
      geom_abline() +
      scale_x_continuous(limits = c(30,160)) +
      scale_y_continuous(limits = c(30,160)) +
      scale_color_manual(name = NULL, values = colors_Expt[levels(xx$Expt) %in% input$Expts]) +
      theme_AGL +
      labs(title = paste("1/DTF = a + bT + Pc | RR =", r2), y = "Predicted", x = "Observed")
    if(input$Plot_Entry == T) {
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry),
                            aes(key1 = Entry, key2 = Name, key3 = Origin, key4 = Cluster),
                            color = "black", fill = "Red", size = 2, pch = 23)
    }
    mp
  })
  # input <- list(Expt = "Rosthern, Canada 2016", Plot_Entry = T, Entry = 1, MyClusters = c("a","2","3","4"))
  output$Modeling_Expt <- renderPlotly({
    xx <- m1 %>% left_join(select(pca, Entry, Origin, Cluster), by = "Entry") %>%
      filter(!is.na(DTF), Expt == input$Expt, Cluster %in% input$MyClusters)
    mymean <- mean(xx$DTF)
    r2  <- 1 - (sum((xx$DTF - xx$Predicted_DTF)^2) / (sum((xx$DTF - mymean)^2)))
    r2 <- round(r2, 3)
    mymin <- min(xx$DTF, na.rm = T)
    mymax <- max(xx$DTF, na.rm = T)
    #
    mp <- ggplot(xx, aes(x = DTF, y = Predicted_DTF, color = Cluster)) +
      geom_point(aes(key1 = Entry, key2 = Name, key3 = Origin), alpha = 0.8) +
      geom_abline() +
      scale_x_continuous(limits = c(mymin,mymax)) +
      scale_y_continuous(limits = c(mymin,mymax)) +
      scale_color_manual(name = NULL, values = colors[as.numeric(input$MyClusters)]) +
      theme_AGL +
      labs(title = paste(input$Expt, "| RR =", r2), y = "Predicted", x = "Observed")
    if(input$Plot_Entry == T) {
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry),
                            aes(key1 = Entry, key2 = Name, key3 = Origin),
                            color = "Black", fill = "Red", size = 2, pch = 23)
    }
    mp
  })
  # input <- list(Plot_Entry = T, Entry = 1, MyClusters = c("a","2","3","4"))
  output$Modeling_Expts <- renderPlot({
    xx <- m1 %>% left_join(select(pca, Entry, Origin, Cluster), by = "Entry") %>%
      filter(!is.na(DTF), Cluster %in% input$MyClusters)
    mymean <- mean(xx$DTF)
    r2  <- 1 - (sum((xx$DTF - xx$Predicted_DTF)^2) / (sum((xx$DTF - mymean)^2)))
    r2 <- round(r2, 3)
    x1 <- xx %>% group_by(Expt) %>%
      summarise(Mean = mean(DTF)) %>% ungroup() %>%
      mutate(r2 = NA)
    #
    for(i in 1:nrow(x1)) {
      xi <- xx %>% filter(Expt == x1$Expt[i])
      x1[i,"r2"]<-round(1 - (sum((xi$DTF - xi$Predicted_DTF)^2) /
                               sum((xi$DTF - x1$Mean[i])^2)), 2)
    }
    #
    mp <- ggplot(xx, aes(x = DTF, y = Predicted_DTF)) +
      geom_point(aes(fill = Expt), color = "black", pch = 21, alpha = 0.8) + geom_abline() +
      geom_label(x = 26, y = 143, color = "black", hjust = 0, vjust = 0,
                aes(label = r2), size = 3, data = x1) +
      facet_wrap(Expt~., ncol = 6, labeller = label_wrap_gen(width = 17)) +
      scale_x_continuous(limits = c(30,160)) +
      scale_y_continuous(limits = c(30,160)) +
      scale_fill_manual(name = NULL, values = colors_Expt) +
      theme_AGL +
      theme(legend.position = "none") +
      labs(title = paste("1/DTF = a + bT + Pc | RR =", r2), y = "Predicted", x = "Observed")
    if(input$Plot_Entry == T) {
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry),
                            color = "Black", fill = "Red", size = 2, pch = 23)
    }
    mp
  })
  # input <- list(Plot_Entry = T, Entry = 1, MyClusters = c("a","2","3","4"))
  output$abc <- renderPlotly({
    xx <- m2 %>%
      left_join(select(pca, Entry, Origin, Cluster), by = "Entry") %>%
      select(Entry, Name, Origin, Cluster, a, b, c) %>%
      gather(Trait, Value, a, b, c) %>%
      filter(Cluster %in% input$MyClusters)
    xE <- xx %>% filter(Entry == input$Entry)
    mp <- ggplot(xx, aes(x = Cluster, y = Value * 10000) ) +
      geom_violin(aes(fill = Cluster), color = NA, alpha = 0.7) +
      geom_quasirandom(size = 0.5, aes(key1 = Entry, key2 = Name, key3 = Origin)) +
      facet_wrap(Trait~., nrow = 1, scales = "free") +
      theme_AGL +
      theme(legend.position = "none", strip.text = element_text(face = "italic")) +
      scale_fill_manual(name = NULL, values = colors) +
      labs(y = "x 10000", x = "Cluster")
    if(input$Plot_Entry == T) {
      mp <- mp + geom_point(data = xE, color = "Black", fill = "Red", size = 3, pch = 23,
                            aes(key1 = Entry, key2 = Name, key3 = Origin))
    }
    mp
  })
  # input <- list(Expt = "Rosthern, Canada 2016", Trait = "DTF", Select_abc = "c", Plot_Entry = T, Entry = 1)
  output$TraitxCoef <- renderPlotly({
    x1 <- dd %>% filter(Expt == input$Expt) %>% 
      select(Entry, Trait=input$Trait)
    xx <- m2 %>% rename(Coef=input$Select_abc) %>%
      left_join(select(pca, Entry, Origin, Cluster), by = "Entry") %>%
      left_join(x1, by = "Entry")
    x1 <- xx %>%
      group_by(Origin) %>%
      summarise_at(vars(Trait, Coef), funs(mean, sd)) %>%
      rename(Coef=Coef_mean, Trait=Trait_mean)
    x2 <- x1 %>% mutate(CO = 1) %>%
      filter(Origin %in% c("Syria", "Jordan", "Turkey", "Lebanon", "Israel")) %>%
      select(Trait, Coef, everything())
    # Plot
    find_hull <- function(df) df[chull(df[,2], df[,1]), ]
    polys <- plyr::ddply(x2, "CO", find_hull)
    mp <- ggplot(xx, aes(y = Coef * 10000, x = Trait)) +
      geom_polygon(data = polys, fill = NA, color = "black") +
      geom_point(aes(key1 = Entry, key2 = Name, key3 = Origin, color = Cluster)) +
      scale_color_manual(values = colors) +
      theme_AGL +
      labs(x = paste(input$Expt, input$Trait), y = paste(input$Select_abc, "* 10000"))
    if(input$Plot_Entry == T) {
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry),
                            aes(key1 = Entry, key2 = Name, key3 = Origin, color = Cluster),
                            color = "Black", fill = "Red", size = 3, pch = 23)
    }
    mp
  })
  # input <- list(Plot_Entry = T, Entry = 1)
  output$cxb <- renderPlotly({
    xx <- m2 %>% left_join(select(pca, Entry, Origin, Cluster), by = "Entry")
    x1 <- xx %>%
      group_by(Origin) %>%
      summarise_at(vars(a, b, c), funs(mean, sd)) %>%
      rename(c=c_mean, b=b_mean)
    x2 <- x1 %>% mutate(CO = 1) %>%
      filter(Origin %in% c("Syria", "Jordan", "Turkey", "Lebanon", "Israel"))
    # Plot
    find_hull <- function(df) df[chull(df[,"c"], df[,"b"]), ]
    polys <- plyr::ddply(x2, "CO", find_hull)
    mp <- ggplot(xx, aes(x = c * 10000, y = b * 10000)) +
      geom_polygon(data = polys, fill = NA, color = "black") +
      geom_point(aes(key1 = Entry, key2 = Name, key3 = Origin, color = Cluster)) +
      scale_color_manual(values = colors) +
      theme_AGL
    if(input$Plot_Entry == T) {
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry),
                            aes(key1 = Entry, key2 = Name, key3 = Origin, color = Cluster),
                            color = "Black", fill = "Red", size = 3, pch = 23)
    }
    mp
  })
  # input <- list(Ad_Country = "Canada", Ad_Countries = c("India"), sd_Mult = 1, Expt = "Rosthern, Canada 2016", Trait = "DTF", Select_Coef_Ad = "c", Plot_Entry = T, Entry = 1)
  output$Adapted1 <- renderPlotly({
    myOrigins <- unique(c(input$Ad_Country, input$Ad_Countries))
    x1 <- dd %>% filter(Expt == input$Expt) %>% select(Entry, Trait=input$Trait)
    xx <- m2 %>% rename(Coef=input$Select_Coef_Ad) %>%
      left_join(select(ldp, Entry, Cluster, Origin), by = "Entry") %>%
      left_join(x1, by = "Entry") %>%
      mutate(Origin = as.character(Origin),
             Origin2 = ifelse(Origin %in% myOrigins, Origin, "Other"),
             Trait = ifelse(is.nan(Trait), max(.$Trait,na.rm = T), Trait))
    yy <- xx %>%
      group_by(Origin) %>%
      summarise_at(vars(Trait, Coef), funs(mean, sd), na.rm = T) %>%
      rename(Coef=Coef_mean, Trait=Trait_mean) %>%
      filter(Origin == input$Ad_Country)
    #
    myYmin <- yy$Coef -  input$sd_Mult * yy$Coef_sd
    myYmax <- yy$Coef +  input$sd_Mult * yy$Coef_sd
    myXmin <- yy$Trait - input$sd_Mult * yy$Trait_sd
    myXmax <- yy$Trait + input$sd_Mult * yy$Trait_sd
    xx <- xx %>%
      mutate(Origin2 = ifelse(Trait > myXmin & Trait < myXmax &
                             !Origin %in% myOrigins, "\"Adapted\"", Origin2),
             Origin2 = factor(Origin2, levels = c("Other", "\"Adapted\"", myOrigins)))
    #
    mp <- ggplot(yy, aes(y = Coef * 10000, x = Trait)) +
      geom_point(data = xx, alpha = 0.7,
                 aes(key1 = Entry, key2 = Name, key3 = Cluster, key4 = Origin, color = Origin2)) +
      geom_point(pch = 18, size = 3) +
      geom_errorbar(aes(ymin = myYmin * 10000, ymax = myYmax * 10000)) +
      geom_errorbarh(aes(xmin = myXmin, xmax = myXmax)) +
      theme_AGL +
      scale_color_manual(values = colors[c(7,8,1,3,5,6,2)]) +
      labs(x = paste(input$Expt, input$Trait), y = paste(input$Select_Coef_Ad, "* 10000"))
    if(input$Plot_Entry == T) {
      mp <- mp +
        geom_point(data = xx %>% filter(Entry == input$Entry),
                   color = "Black", fill = "Red", size = 3, pch = 23,
                   aes(key1 = Entry, key2 = Name, key3 = Cluster, key4 = Origin))
    }
    mp
  })
  # input <- list(Ad_Country="Canada", Ad_Countries="India", sd_Mult = 2, Trait = "DTF", Select_Coef_Ad = "c", Expt = "Sutherland, Canada 2018", Plot_Entry = T, Entry = 1)
  output$Adapted2 <- renderPlotly({
    myOrigins <- unique(c(input$Ad_Country, input$Ad_Countries))
    x1 <- dd %>% filter(Expt == input$Expt) %>% select(Entry, Trait=input$Trait)
    xx <- m2 %>% 
      left_join(select(ldp, Entry, Cluster, Origin), by = "Entry") %>%
      left_join(x1, by = "Entry") %>%
      mutate(Origin = as.character(Origin),
             Origin2 = ifelse(Origin %in% myOrigins, Origin, "Other"))
    yy <- xx %>%
      group_by(Origin) %>%
      summarise_at(vars(c,b, Trait), funs(mean, sd), na.rm = T) %>%
      rename(b=b_mean, c=c_mean, Trait=Trait_mean) %>%
      filter(Origin == input$Ad_Country)
    #
    myYmin <- (yy$b - input$sd_Mult * yy$b_sd)
    myYmax <- (yy$b + input$sd_Mult * yy$b_sd)
    myXmin <- (yy$c - input$sd_Mult * yy$c_sd)
    myXmax <- (yy$c + input$sd_Mult * yy$c_sd)
    myDTFmin <- yy$Trait - input$sd_Mult * yy$Trait_sd
    myDTFmax <- yy$Trait + input$sd_Mult * yy$Trait_sd
    xx <- xx %>%
      mutate(Origin2 = ifelse(Trait > myDTFmin & Trait < myDTFmax &
                              !Origin2 %in% myOrigins, "\"Adapted\"", Origin2),
             Origin2 = factor(Origin2, levels = c("Other", "\"Adapted\"", myOrigins)))
    #
    mp <- ggplot(yy, aes(y = b * 10000, x = c * 10000)) +
      geom_point(data = xx, aes(key1 = Entry, key2 = Name, key3 = Cluster, key4 = Origin, color = Origin2), alpha = 0.7) +
      geom_point(aes(pch = Origin), size = 3) +
      geom_errorbar(aes(ymin = myYmin * 10000, ymax = myYmax * 10000)) +
      geom_errorbarh(aes(xmin = myXmin * 10000, xmax = myXmax * 10000)) +
      theme_AGL +
      scale_shape_manual(name = "Average", values = 18) +
      scale_color_manual(values = colors[c(7,8,1,3,5,6,2)]) +
      labs(title = input$Expt, x = "c * 10000", y = "b * 10000")
    if(input$Plot_Entry == T) {
      mp <- mp +
        geom_point(data = xx %>% filter(Entry == input$Entry),
                   color = "Black", fill = "Red", size = 3, pch = 23,
                   aes(key1 = Entry, key2 = Name, key3 = Cluster, key4 = Origin))
    }
    mp
  })
  # input <- list(Ad_Country="Canada", Ad_Countries="India", sd_Mult = 2, Expt = "Sutherland, Canada 2018", Trait = "DTF", Select_Coef_Ad = "c")
  output$Adapted_Table.csv <- downloadHandler(
    filename = function() {"Adapted_Table.csv"},
    content = function(file) {
      myOrigins <- unique(c(input$Ad_Country, input$Ad_Countries))
      x1 <- dd %>% filter(Expt == input$Expt) %>% select(Entry, Trait=input$Trait)
      xx <- m2 %>% rename(Coef=input$Select_Coef_Ad) %>%
        left_join(select(ldp, Entry, Cluster, Origin), by = "Entry") %>%
        left_join(x1, by = "Entry") %>%
        mutate(Origin = as.character(Origin),
               Origin2 = ifelse(Origin %in% myOrigins, Origin, "Other"),
               Trait = round(ifelse(is.nan(Trait), max(.$Trait,na.rm = T), Trait),1))
      yy <- xx %>%
        group_by(Origin) %>%
        summarise_at(vars(Trait, Coef), funs(mean, sd), na.rm = T) %>%
        rename(Coef=Coef_mean, Trait=Trait_mean) %>%
        filter(Origin == input$Ad_Country)
      #
      myXmin <- yy$Trait - input$sd_Mult * yy$Trait_sd
      myXmax <- yy$Trait + input$sd_Mult * yy$Trait_sd
      xx <- xx %>%
        mutate(Origin2 = ifelse(Trait > myXmin & Trait < myXmax &
                                  !Origin %in% myOrigins, "\"Adapted\"", Origin2)) %>%
        filter(Origin2 == "\"Adapted\"") %>% 
        select(Entry, Name, Origin, Cluster, Coef, Trait) %>%
        mutate(Coef = round(Coef * 10000, 1))
      #colnames(xx)[colnames(xx)=="Coef"] <- input$Select_Coef_Ad
      #colnames(xx)[colnames(xx)=="Trait"] <- input$Trait
      write.csv(xx, file, row.names = F)
      }
  )
  # input <- list(Ad_Country="Canada", Ad_Countries="India", sd_Mult = 2, Expt = "Sutherland, Canada 2018", Trait = "DTF", Select_Coef_Ad = "c")
  output$Adapted_Table <- DT::renderDataTable({
    myOrigins <- unique(c(input$Ad_Country, input$Ad_Countries))
    x1 <- dd %>% filter(Expt == input$Expt) %>% select(Entry, Trait=input$Trait)
    xx <- m2 %>% rename(Coef=input$Select_Coef_Ad) %>%
      left_join(select(ldp, Entry, Cluster, Origin), by = "Entry") %>%
      left_join(x1, by = "Entry") %>%
      mutate(Origin = as.character(Origin),
             Origin2 = ifelse(Origin %in% myOrigins, Origin, "Other"),
             Trait = round(ifelse(is.nan(Trait), max(.$Trait,na.rm = T), Trait),1))
    yy <- xx %>%
      group_by(Origin) %>%
      summarise_at(vars(Trait, Coef), funs(mean, sd), na.rm = T) %>%
      rename(Coef=Coef_mean, Trait=Trait_mean) %>%
      filter(Origin == input$Ad_Country)
    #
    myXmin <- yy$Trait - input$sd_Mult * yy$Trait_sd
    myXmax <- yy$Trait + input$sd_Mult * yy$Trait_sd
    xx <- xx %>%
      mutate(Origin2 = ifelse(Trait > myXmin & Trait < myXmax &
                                !Origin %in% myOrigins, "\"Adapted\"", Origin2)) %>%
      filter(Origin2 == "\"Adapted\"") %>% 
      select(Entry, Name, Origin, Cluster, Coef, Trait) %>%
      mutate(Coef = round(Coef * 10000, 1))
    #colnames(xx)[colnames(xx)=="Coef"] <- input$Select_Coef_Ad
    #colnames(xx)[colnames(xx)=="Trait"] <- input$Trait
  }, options = list(pageLength = 324))
  # input <- list(Mean_T = 13, Mean_P = 12, DTF_Max = 80, DTF_Min = 30, Select_Coef_DTF = "c", Plot_Entry = T, Entry = 1)
  output$Predict_DTF_Clusters <- renderPlotly({
    xx <- m2 %>% left_join(select(ldp, Entry, Origin, Cluster), by = "Entry") %>%
      mutate(DTF_Prediction = round(1 / ( a + b * input$Mean_T  + c * input$Mean_P ), 1),
             Adapted = ifelse(DTF_Prediction > input$DTF_Min & DTF_Prediction < input$DTF_Max, "Adapted", "Not Adapted") )
    mp <- ggplot(xx, aes(x = Cluster, y = DTF_Prediction)) +
      geom_rect(xmin = 0, xmax = 9, ymin = input$DTF_Min, ymax = input$DTF_Max, 
                fill = "grey", color = NA, alpha = 0.3) +
      geom_violin(aes(fill = Cluster), color = NA, alpha = 0.7) +
      geom_quasirandom(aes(key1 = Entry, key2 = Name, key3 = Origin, color = Adapted)) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = c("black", "grey")) +
      theme_AGL +
      theme(legend.position = "none") +
      labs(title = paste("Mean Temperature =", input$Mean_T, "| Mean Photoperiod =", input$Mean_P),
           y = "DTF Prediction")
    if(input$Plot_Entry == T) { 
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry), color = "Black", fill = "Red", size = 3, pch = 23)
    }
    mp
  })
  # input <- list(Mean_T = 13, Mean_P = 12, DTF_Max = 80, DTF_Min = 30, Select_Coef_DTF = "c", Plot_Entry = T, Entry = 1)
  output$Predict_DTF_DTFxCoef <- renderPlotly({
    xx <- m2 %>% left_join(select(ldp, Entry, Origin, Cluster), by = "Entry") %>%
      mutate(DTF_Prediction = round(1 / ( a + b * input$Mean_T  + c * input$Mean_P ), 1),
             Adapted = ifelse(DTF_Prediction > input$DTF_Min & DTF_Prediction < input$DTF_Max, "Adapted", "Not Adapted") )
    mp <- ggplot(xx, aes(x = DTF_Prediction, y = get(input$Select_Coef_DTF) * 10000)) +
      geom_rect(ymin = -Inf, ymax = Inf, xmin = input$DTF_Min, xmax = input$DTF_Max, 
                fill = "grey", color = NA, alpha = 0.3) +
      geom_point(aes(key1 = Entry, key2 = Name, key3 = Origin, color = Cluster, pch = Adapted)) +
      scale_color_manual(values = colors) +
      scale_shape_manual(values = c(16,1)) +
      theme_AGL +
      labs(title = paste("Mean Temperature =", input$Mean_T, "| Mean Photoperiod =", input$Mean_P),
           y = paste(input$Select_Coef_DTF," * 10000"), x = "Predicted DTF")
    if(input$Plot_Entry == T) { 
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry), color = "Black", fill = "Red", size = 3, pch = 23)
    }
    mp
  })
  # input <- list(Mean_T = 13, Mean_P = 12, DTF_Max = 80, DTF_Min = 30, Plot_Entry = T, Entry = 1)
  output$Predict_DTF_cxb <- renderPlotly({
    xx <- m2 %>% left_join(select(ldp, Entry, Origin, Cluster), by = "Entry") %>%
      mutate(DTF_Prediction = round(1 / ( a + b * input$Mean_T  + c * input$Mean_P ), 1),
             Adapted = ifelse(DTF_Prediction > input$DTF_Min & DTF_Prediction < input$DTF_Max, "Adapted", "Not Adapted") )
    mp <- ggplot(xx, aes(x = c * 10000, y = b * 10000)) +
      geom_point(aes(key1 = Entry, key2 = Name, key3 = Origin, color = Cluster, pch = Adapted)) +
      scale_color_manual(values = colors) +
      scale_shape_manual(values = c(16,1)) +
      theme_AGL +
      labs(title = paste("Mean Temperature =", input$Mean_T, "| Mean Photoperiod =", input$Mean_P))
    if(input$Plot_Entry == T) { 
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry), color = "Black", fill = "Red", size = 3, pch = 23)
    }
    mp
  })
  # input <- list(Mean_T = 13, Mean_P = 12, DTF_Max = 80, DTF_Min = 30)
  output$Adapted_DTF_Table <- DT::renderDataTable({
    xx <- m2 %>% left_join(select(ldp, Entry, Origin, Cluster), by = "Entry") %>%
      mutate(DTF_Prediction = round(1 / ( a + b * input$Mean_T  + c * input$Mean_P ), 1)) %>%
      filter(DTF_Prediction > input$DTF_Min, DTF_Prediction < input$DTF_Max) %>%
      select(Entry, Name, Origin, Cluster, DTF_Prediction)
  }, options = list(pageLength = 324))
  # input <- list(Mean_T = 13, Mean_P = 12, DTF_Max = 80, DTF_Min = 30)
  output$Adapted_DTF_Table.csv <- downloadHandler(
    filename = function() {"Adapted_DTF_Table.csv"},
    content = function(file) {
      xx <- m2 %>% left_join(select(ldp, Entry, Origin, Cluster), by = "Entry") %>%
        mutate(DTF_Prediction = round(1 / ( a + b * input$Mean_T  + c * input$Mean_P ), 1)) %>%
        filter(DTF_Prediction > input$DTF_Min, DTF_Prediction < input$DTF_Max) %>%
        select(Entry, Name, Origin, Cluster, DTF_Prediction)
      write.csv(xx, file, row.names = F)
      }
  )
  # input <- list(TempIncrease = 1, Expts = names_Expt, Plot_Entry = T, Entry = 1, MyClusters = c("a","2","3","4"))
  output$PCA_TempInc_Expts <- renderPlotly({
    xx <- dd %>%
      select(Entry, Name, Expt, ExptShort, Origin, Cluster, DTF) %>%
      left_join(select(m2, Entry, a, b, c), by = "Entry") %>%
      left_join(select(ff, Expt, MacroEnv, T_mean, P_mean), by = "Expt") %>%
      mutate(T_mean2 = T_mean + input$TempIncrease,
             DTF_GW = 1 / (a + b * T_mean2 + c * P_mean),
             DTF_P  = 1 / (a + b * T_mean + c * P_mean),
             Diff = DTF_P - DTF_GW,
             Expt = factor(Expt, levels = names_Expt)) %>%
      filter(Expt %in% input$Expts, Cluster %in% input$MyClusters)
    # Plot
    mp <- ggplot(xx, aes(x = ExptShort, y = Diff)) +
      geom_violin(aes(fill = MacroEnv), alpha = 0.3, color = NA) +
      geom_quasirandom(size = 0.7, aes(key1 = Entry, key2 = Name, key3 = Origin, color = Cluster)) +
      facet_grid(.~MacroEnv, scales = "free_x") + #
      theme_AGL +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            panel.grid.major.y = element_line(colour = "grey70", size = 0.5)) +
      scale_y_continuous(minor_breaks = seq(1,30,1),
                         breaks = seq(0,30,5)) +
      scale_fill_manual(values = c("darkgreen","darkred","darkblue")) +
      scale_color_manual(values = colors[as.numeric(input$MyClusters)]) +
      labs(title = paste("Temperature Increase of", input$TempIncrease),
           y = "Decrease in days to flower", x = NULL)
    if(input$Plot_Entry == T) {
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry),
                            color = "Black", fill = "Red", size = 2, pch = 23)
    }
    mp
  })
  # input <- list(TempIncrease = 1, Expt = names_Expt, Plot_Entry = T, Entry = 1, MyClusters = c("a","2","3","4"))
  observe({
    output$PCA_TempInc_Clusters <- renderPlot({
      xx <- dd %>%
        select(Entry, Name, Expt, ExptShort, Cluster, DTF) %>%
        left_join(select(m2, Entry, a, b, c), by = "Entry") %>%
        left_join(select(ff, Expt, MacroEnv, T_mean, P_mean), by = "Expt") %>%
        mutate(T_mean2 = T_mean + input$TempIncrease,
               DTF_GW = 1 / (a + b * T_mean2 + c * P_mean),
               DTF_P  = 1 / (a + b * T_mean + c * P_mean),
               Diff = DTF_P - DTF_GW,
               Expt = factor(Expt, levels = names_Expt)) %>%
        filter(Expt %in% input$Expts, Cluster %in% input$MyClusters)
      mp <- ggplot(xx, aes(x = Cluster, y = Diff, fill = Cluster)) +
        geom_violin(color = NA, alpha = 0.5) +
        geom_quasirandom() +
        facet_grid(Expt~., scales = "free") +
        theme_AGL +
        theme(legend.position = "none") +
        scale_fill_manual(name = NULL, values = colors) +
        labs(y = "Decrease in days to flower", x = NULL)
      if(input$Plot_Entry == T) {
        mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry),
                              color = "Black", fill = "Red", size = 3, pch = 23)
      }
      mp
    }, height = 250*length(input$Expts) )
  })
  # input <- list(TempIncrease = 1, Expt = "Rosthern, Canada 2017", Plot_Entry = T, Entry = 1, MyClusters = c("a","2","3","4"))
  output$PCA_TempInc_Cluster <- renderPlotly({
    xx <- dd %>%
      select(Entry, Name, Expt, ExptShort, Origin, Cluster, DTF) %>%
      left_join(select(m2, Entry, a, b, c), by = "Entry") %>%
      left_join(select(ff, Expt, MacroEnv, T_mean, P_mean), by = "Expt") %>%
      mutate(T_mean2 = T_mean + input$TempIncrease,
             DTF_GW = 1 / (a + b * T_mean2 + c * P_mean),
             DTF_P  = 1 / (a + b * T_mean + c * P_mean),
             Diff = DTF_P - DTF_GW,
             Expt = factor(Expt, levels = names_Expt)) %>%
      filter(Expt == input$Expt, Cluster %in% input$MyClusters)
    mp <- ggplot(xx, aes(x = Cluster, y = Diff)) +
      geom_violin(aes(fill = Cluster), color = NA, alpha = 0.5) +
      geom_quasirandom(aes(key1 = Entry, key2 = Name, key3 = Origin)) +
      theme_AGL +
      theme(legend.position = "none") +
      scale_fill_manual(name = NULL, values = colors) +
      labs(title = paste(input$Expt, "| Temperature Increase of", input$TempIncrease),
           y = "Decrease in days to flower", x = NULL)
    if(input$Plot_Entry == T) {
      mp <- mp + geom_point(data = xx %>% filter(Entry == input$Entry),
                            aes(key1 = Entry, key2 = Name, key3 = Origin),
                            color = "Black", fill = "Red", size = 3, pch = 23)
    }
    mp
  })
}
# Run the application
shinyApp(ui = ui, server = server)
