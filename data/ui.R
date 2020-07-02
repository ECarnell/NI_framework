svr_v<-T
if(svr_v == T){lib<-"/home/edcarn/R/x86_64-pc-linux-gnu-library/3.4"
}

if(svr_v == F){lib<-NULL
pth<-paste0("C:/Shiny_dev/")
setwd(pth)}

require(markdown,lib.loc = lib)
require(shinyjs,lib.loc = lib)
require(data.table,lib.loc = lib)
require(leaflet,lib.loc = lib)

# ag <- fread("./data/Agric_density_summary.csv")
# 
# ag[,VARIABLE:=gsub("NH3/ha/yr","NH\U2083 ha\U207B\U00B9 yr\U207B\U00B9",VARIABLE)]
# 
# sa <- fread("./data/Source_attr.csv")
# nh3 <- fread("./data/conc_summary.csv")
# nh3[,cle:=paste0(cle," \U03BC NH\U2083 m\U207B\U00B3")]
# cl_stat <-fread("./data/CL_status.csv")
# cl_stat[,VARIABLE:=gsub("kg N ha-1 yr-1","kg N ha\U207B\U00B9 yr\U207B\U00B9",VARIABLE)]

fluidPage(
  inlineCSS(list(".head" = c("color: black","font-size:18px","font-weight: bold"),
                 ".error" = c("color: red", "border: 1px solid red"),
                 ".ok" = c("color: seagreen"),
                 ".cl" = c("color: midnightblue"),
                 ".clhead" = c("color: midnightblue","font-size:18px","font-weight: bold"),
                 ".cle" = c("color: firebrick"),
                 ".clehead" = c("color: firebrick","font-size:18px","font-weight: bold"),
                 ".em" = c("color: seagreen"),
                 ".emhead" = c("color: seagreen","font-size:18px","font-weight: bold"),
                 ".sa" = c("color: chocolate"),
                 ".sahead" = c("color: chocolate","font-size:18px","font-weight: bold")
                 )),
  # App title ----
  titlePanel("Nitrogen threats at designated sites in NI"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      radioButtons("site_typ", p(class = "head","Site Type"),
                                     c("SAC","ASSI"),"SAC"),
      p(class = "emhead","NH\U2083 emissions"),
      checkboxInput("em_filt", label = p(class="em","Include indicators for agricultural NH\U2083 emissions"), value = FALSE),
      uiOutput("em_opt"),
      
      p(class = "clehead","Critical Levels"),
      checkboxInput("cle_filt", label = p(class="cle","Include indicators for NH\U2083 critical levels"), value = FALSE),
      uiOutput("cle_opt"),
      
      uiOutput("dep_sel"),
      
      p(class = "clhead","Critical Loads"),
      checkboxInput("cl_filt", label = p(class="cl","Include indicators for nitrogen critical loads"), value = FALSE),
      uiOutput("cl_opt"),
      
      p(class = "sahead","Source Attribution"),
      checkboxInput("sa_filt", label = p(class="sa","Include indicators for source attribution"), value = FALSE),
      uiOutput("sa_opt")
                        
    ),

    # Main panel for displaying outputs ----
    mainPanel(
#DT::dataTableOutput("mytable")
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Comparison table",
                          h4(textOutput("ind_pv")),
                          h4(textOutput("n_sites_pv")),
                          uiOutput("tab_desc_pv"),
                          DT::dataTableOutput("piv_tab")),
                  
                  tabPanel("Combined scores",
                           h4(textOutput("ind_agg")),
                           h4(textOutput("n_sites_agg")),
                           uiOutput("tab_desc_agg"),
                           DT::dataTableOutput("tab")),
                           
                 tabPanel("Results for selected site",
                          #downloadButton("downloadData", label = "Download"),
                          h3(textOutput("sel_site_tab")),
                          DT::dataTableOutput("sel_tab")),
                 
                 tabPanel("Location of selected site",
                        h3(textOutput("sel_site_map")),
                        absolutePanel(class = "panel panel-default", fixed = F,draggable = F,
                        bottom = 30, right = 20,
                        style="padding-left: 5px; padding-right: 5px; padding-top: 5px; padding-bottom: 5px",
                        width = "auto", height = "auto",
                        style = "opacity: 0.5",
                        h3("Map Controls"),
                
                        
                        radioButtons("basemap","Basemap:",c("Map" = "CartoDB.Positron", "Satellite" = "Esri.WorldImagery"), 
                                     selected =c("Map" = "CartoDB.Positron")),
                        
                        actionButton("re_cntr", label = "Zoom in to site"),
                        actionButton("ni_cntr", label = "Zoom out to NI"),
                        
                        radioButtons("rast", "Raster layer:",c("Basemap only" = "None","Ammonia concentration" = "NH3_conc",
                                                            "Total N Deposition" = "TotN_dep"),
                                     selected = c("Basemap only" = "None")),
                        uiOutput("map_dep_sel")),
                 
              leafletOutput("map",width="100%",height="100%")  
      )
)
    
  )
))

