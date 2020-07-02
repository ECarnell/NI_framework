  svr_v<-T
  if(svr_v == T){lib<-"/home/edcarn/R/x86_64-pc-linux-gnu-library/3.4"
  }
  
  if(svr_v == F){lib<-NULL
  pth<-paste0("C:/Shiny_dev/")
  setwd(pth)}
  
  
  require(shiny,lib.loc = lib)
  require(shinyjs,lib.loc = lib)
  require(markdown,lib.loc = lib)
  require(data.table,lib.loc = lib)
  require(DT,lib.loc = lib)
  require(leaflet,lib.loc = lib)
  require(raster,lib.loc = lib)
  require(sp,lib.loc = lib)
  require(rgdal,lib.loc = lib)
  
  #setwd("./ShinyApps/NI_frame")
  ag <- fread("./data/Agric_density_summary.csv")
  ag[,VARIABLE:=gsub("NH3/ha/yr","NH\U2083 ha\U207B\U00B9 yr\U207B\U00B9",VARIABLE)]

  sa <- fread("./data/Source_attr.csv")
  nh3 <- fread("./data/conc_summary.csv")
  nh3[,cle:=paste0(cle," \U03BC NH\U2083 m\U207B\U00B3")]
  
  nh3_r <-fread("./data/conc_r.csv")
  dep_r <-fread("./data/dep_r.csv")
  cl_stat <-fread("./data/CL_status.csv")
  cl_stat[,VARIABLE:=gsub("kg N ha-1 yr-1","kg N ha\U207B\U00B9 yr\U207B\U00B9",VARIABLE)]
#sac <- st_read("P:/NECxxxxx_NIEA_Ballynahone_NH3/framework/Designated_Sites/sac2019_ukwide.shp")
#sssi <- st_read("P:/NECxxxxx_NIEA_Ballynahone_NH3/framework/Designated_Sites/sssi2019_ukwide.shp")

#x <- rbind(sac[sac$SITECODE %in% ag$SITECODE,"SITECODE"],
           #sssi[sssi$SITECODE %in% ag$SITECODE,"SITECODE"])

#st_write(x,"P:/NECxxxxx_NIEA_Ballynahone_NH3/framework/Designated_Sites/NI_sites.shp")
sp<-readOGR("./data/NI_sites.shp")
sp_wgs84 <- spTransform(sp,crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"))
sp_wgs84 <- spTransform(sp_wgs84,crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


svr <- function(input,output,session){


####################################
##
##  Render UI
##
####################################
  
  output$tab_desc_agg <- renderUI({
  if(nrow(table_data())==0){return(tagList(h5("Please make a selection the list of indicators on the left")))}else{
  tagList(h4("Combined scores"),
                           h5("Each indicator has been assigned a score from 0 - 4"),
                           h5("Sites can be selected by clicking on the table"))
      }
    })
  
  output$tab_desc_pv <- renderUI({
  if(nrow(table_data())==0){
    return(tagList(h5("Please make a selection the list of indicators on the left")))}else{
                           tagList(h5("Each indicator has been assigned a score from 0 - 4"),
                           h5("Sites can be selected by clicking on the table"))
      }
    })
  
  output$cl_opt <- renderUI({
 
   if(!input$cl_filt){
    return()}else{
      
  id_all <- length(unique(ag[SITE_TYPE==input$site_typ,SITECODE]))
  id_cl <- length(unique(cl_stat[SITE_TYPE==input$site_typ,SITECODE]))
  id_sub <- length(unique(cl_stat[SITE_TYPE==input$site_typ&
                                    DepType==input$sa_dep,SITECODE]))
  id_miss <- id_all - id_cl
  cls <-ifelse(id_miss == 0,"ok","error")
  msg <- ifelse(id_miss == 0,"CL information available for all sites",
  paste0(" ",id_miss," sites without CL information removed"))
  msg2 <- paste0(" ",id_sub," sites with designated features")
  
   tagList(p(class = cls, msg),
           p(class = "cl", msg2),
           checkboxGroupInput("cl_var",p(class = "cl","Select indicators:"),
                                      unique(cl_stat$VARIABLE),NULL))}
  })
  
  output$dep_sel <- renderUI({if(!input$sa_filt&!input$cl_filt){return()}else{
    tagList(radioButtons("sa_dep", "Designated feature type",
                                     unique(sa$Dep_typ),NULL))}
    })
  
  output$cle_opt <- renderUI({
  if(!input$cle_filt){return()}else{
    tagList(radioButtons("nh3_cle", p(class = "cle","Critical level"),
                                      unique(nh3$cle),NULL),
            checkboxGroupInput("nh3_var",p(class = "cle","Select indicators:"),
                                      unique(nh3$VARIABLE),NULL))}
    
  })
  
  output$em_opt <- renderUI({
  if(!input$em_filt){return()}else{tagList(checkboxGroupInput("ag_var",
                                    p(class = "em","Select indicators:"),
                                      unique(ag$VARIABLE),NULL))}
  })
  
    output$sa_opt <- renderUI({
  if(!input$sa_filt){return()}else{tagList(checkboxGroupInput("sa_var", 
                                      p(class = "sa","Select indicators:"),
                                      unique(sa$VARIABLE),NULL))}
  })
    
  
# table creation
############################################################################
############################################################################
table_data <- reactive({
  
  if(!input$cl_filt){ids <- unique(ag[SITE_TYPE==input$site_typ,SITECODE])}
  if(input$cl_filt){ids <- unique(cl_stat[SITE_TYPE==input$site_typ&DepType==input$sa_dep,SITECODE])}
  
        x0 <- ag[0,.(SITECODE,NAME,DATA_SRCE,VARIABLE,VALUE,SCORE,LABEL)]
        vars <-c()  
        
        if(input$em_filt & !is.null(input$ag_var)){
          vars <-c(vars,input$ag_var) 
          x0 <- rbind(x0, ag[SITECODE %in% ids & VARIABLE %in% input$ag_var,
                                          .(SITECODE,NAME,DATA_SRCE,VARIABLE,VALUE=round(VALUE,1),SCORE,LABEL)])}
        
        if(input$sa_filt & !is.null(input$sa_dep) & !is.null(input$sa_var)){x1<-sa[ID %in% ids,] 
                                                   x1<-x1[Dep_typ %in% input$sa_dep,] 
                                                   x1<-x1[VARIABLE %in% input$sa_var,]
                                                   vars <-c(vars,input$sa_var) 
        x0 <- rbind(x0,x1[,.(SITECODE = ID,NAME,DATA_SRCE,VARIABLE,VALUE=round(VALUE,1),SCORE,LABEL)])}
        
        if(input$cl_filt & !is.null(input$sa_dep) & !is.null(input$cl_var)){x2<-cl_stat[SITECODE %in% ids,] 
                                                   x2<-x2[DepType %in% input$sa_dep,] 
                                                   x2<-x2[VARIABLE %in% input$cl_var,]
                                                   vars <-c(vars,input$cl_var) 
        x0 <- rbind(x0,x2[,.(SITECODE,NAME,DATA_SRCE,VARIABLE,VALUE=round(VALUE,1),SCORE,LABEL)])}
        
        
        if(input$cle_filt&!is.null(input$nh3_var)){vars <-c(vars,input$nh3_var) 
          x0 <- rbind(x0, nh3[SITECODE %in% ids &cle == input$nh3_cle & 
                                                        VARIABLE %in% input$nh3_var,
                          .(SITECODE,NAME,DATA_SRCE,VARIABLE,VALUE=round(VALUE,1),SCORE,LABEL)])}
        
        #x0[,VARIABLE:=factor(VARIABLE,levels=vars)]
        x0[,N_IND := length(c(input$ag_var,input$nh3_var,input$sa_var))]
        x0[order(-SCORE,NAME),]
        })

# aggregated table output
############################################################################
############################################################################  

output$tab <- DT::renderDataTable({
  if(nrow(table_data())==0){return()}else{
  x <- table_data()
  x[,rnk:=frank(VALUE,ties.method="dense")]
  x <- x[,.(`Rank` = mean(rnk),
            `Average Score`=round(sum(SCORE/N_IND),2),
            `Total Score`=sum(SCORE)), by=.(ID=SITECODE,`Site Name` = NAME)]
  x <- x[order(-`Total Score`,-Rank),]
 
  x[,Rank:=NULL]
  datatable(x,rownames = F,selection = 'single')}})

output$ind_agg <- renderText({if(nrow(table_data())==0){"No indicators selected"}else{
                  paste0("Indicators selected: ",table_data()[1,N_IND])}})

output$n_sites_agg <- renderText({if(nrow(table_data())==0){return()}else{
                  paste0("Number of sites considered: ",length(unique(table_data()[,SITECODE])))}})
output$ind_pv <- renderText({if(nrow(table_data())==0){"No indicators selected"}else{
                  paste0("Indicators selected: ",table_data()[1,N_IND])}})

output$n_sites_pv <- renderText({if(nrow(table_data())==0){return()}else{
                  paste0("Number of sites considered: ",length(unique(table_data()[,SITECODE])))}})

# table selection
############################################################################
############################################################################
output$sel_tab <- DT::renderDataTable({
  if(is.null(input$tab_rows_selected)){return()}else{
  
    y <- datatable(table_data()[SITECODE == site_sel()$ID,.(`Indicator Score`=SCORE,
                                                                    
                                                                    `Indicator`=VARIABLE,
                                                                    
                                                                    Value = VALUE,
                                                                    `Inicator source`=DATA_SRCE)],rownames=F)                                                                  
                      
                      y %>% formatStyle("Indicator Score",backgroundColor = styleEqual(c(0,1,2,3,4),
                                  c("#B0D6FF", "#FDFF99","#FFF757","#FFBE3B","#EB4034")))}
                       })

# Sitename
############################################################################
############################################################################
site_sel<-eventReactive(input$tab_rows_selected,{
   if(is.null(input$tab_rows_selected)){list(ID = NA, NAME = "Please select a site using the site selection tab")}
    if(!is.null(input$tab_rows_selected)){
    x <- table_data()
    x[,rnk:=frank(VALUE,ties.method="dense")]
    x <- x[,.(Rank=mean(rnk),`Average Score`=round(sum(SCORE/N_IND),2),`Total Score`=sum(SCORE)),by=.(ID=SITECODE,`Site Name`=NAME)]
    x <- x[order(-`Total Score`,-Rank),]
    ID <- x[input$tab_rows_selected,ID]
    NAME <- x[input$tab_rows_selected,`Site Name`]
    list(ID = ID, NAME = NAME)
    }
  })

output$sel_site_tab <- renderText(site_sel()$NAME)
output$sel_site_map <- renderText(site_sel()$NAME)

output$piv_tab <- DT::renderDataTable({
  if(nrow(table_data())==0){return()}else{
  x <- table_data()
  x[,rnk:=frank(VALUE,ties.method="dense")]

  x <- x[,.(`Rank` = mean(rnk),
            `Average Score`=round(sum(SCORE/N_IND),2),
            `Total Score`=sum(SCORE)), by=.(ID=SITECODE,`Site Name` = NAME)]
  x <- x[order(-`Total Score`,-Rank),ID]

  y <- copy(table_data())
  y[,LABEL:=paste0(SCORE," - ",LABEL)]
  
  labs <- dcast.data.table(y,SITECODE+NAME~VARIABLE,value.var = "LABEL")
  vals <- dcast.data.table(y,SITECODE+NAME~VARIABLE,value.var = "SCORE")
  
  labs <- labs[order(match(SITECODE, x)),]
  vals <- vals[order(match(SITECODE, x)),]
  labs<-labs[,c("SITECODE","NAME",unique(y$VARIABLE)),with=F]                                  
  
  vs <- names(labs)[unique(3:ncol(labs))]
  vs <- vs[!is.na(vs)]
  cls<-unique(3:ncol(labs))
  vals <- suppressWarnings(vals[,lapply(.SD,function(x){ifelse(x==0,"#B0D6FF",
                                       ifelse(x==1,"#FDFF99",
                                              ifelse(x==2,"#FFF757",
                                                     ifelse(x==3,"#FFBE3B",
                                                            ifelse(x==4,"#EB4034","#000000")))))}),.SDcols=cls])
  
  labs_XX <- datatable(labs)
  
  for(i in 1:length(vs)){
  
  #i<-1
  labs_XX <- labs_XX %>%
    formatStyle(vs[i],backgroundColor = styleEqual(
      as.vector(labs[,vs[i],with=F])[[1]],
      as.vector(vals[,vs[i],with=F])[[1]]))}
  
  labs_XX
  }
})

####################################
##
##   Leaflet map
##
####################################

# Raster
############################################################################
############################################################################
rast<-reactive({
  if(input$rast == "NH3_conc"){
    r <- nh3_r[,.(x,y,z=z)]
     cols <- c("#91e4ff","#fff200","#ffbb00","#ff0000","#cc0066")
      brks<-c(-1,1,2,3,4,500)
      lgnd <- "ug NH<SUB>3</SUB> m<SUP>-3</SUP>"
    labs<-c("0 - 1","> 1 - 2","> 2 - 3","> 3 - 4","> 4")
}
    
    
  if(input$rast == "TotN_dep"){
    r <- dep_r[Dep_typ==input$sa_dep,.(x,y,z)]
    cols <- c("#F1EEF6","#D7B5D8","#DF65B0","#DD1C77","#980043")
    brks<-c(-1,1,5,10,25,5000)
    lgnd <- "kg N ha<SUP>-1</SUP> yr<SUP>-1</SUP>"
    labs<-c("0 - 1","> 1 - 5","> 5 - 10","> 10 - 25","> 25 - 50")}
    
    r <- rasterFromXYZ(r[,.(x,y,z)],
                     crs=crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"))
                     
    r <- projectRaster(r,crs=c("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"),
                       res = c(1000),method = "bilinear")
    
    list(r = r,
         cols = cols,
         labs = labs,
         brks = brks,
         lgnd = lgnd,
         pal = colorBin(palette=cols, domain=values(r),bins = brks, 
                    na.color = "transparent",right=F))
  
    })

# Recentre
############################################################################
############################################################################
observeEvent(input$re_cntr, {

  sel_sf <- sp_wgs84[sp_wgs84$SITECODE== site_sel()$ID,]
                      bbx <- bbox(sel_sf)
                      s_lng <- mean(c(bbx["x","min"],bbx["x","max"]))
                      s_lat <- mean(c(bbx["y","min"],bbx["y","max"]))
                      mx_deg <- max(c(bbx["x","max"]-bbx["x","min"],bbx["y","max"]-bbx["y","min"]))
                      zm <- ifelse(mx_deg < 0.05,13,
                            ifelse(mx_deg <=0.1, 12,
                                    ifelse(mx_deg <=0.2, 12,
                                           ifelse(mx_deg <=0.5, 11,
                                                  ifelse(mx_deg < 1, 10,10)))))
leafletProxy("map")%>% setView(lng = s_lng, lat= s_lat,zoom = zm)})

# Zoom to NI
############################################################################
############################################################################
observeEvent(input$ni_cntr, {
leafletProxy("map")%>% setView(lng = -6, lat= 55,zoom = 6)})

# Raster
############################################################################
############################################################################
observeEvent(input$rast, {
  if(input$rast=="None"){leafletProxy("map")%>%clearImages()%>%
clearControls()}
   if(input$rast!="None"){leafletProxy("map")%>% removeTiles(layerId="ras")%>%
clearControls()%>%
    addLegend(colors=rast()$cols,values = values(rast()$r),labels = rast()$lab,
                    position = "bottomleft",
                    title = rast()$lgnd)%>%
    addRasterImage(rast()$r,colors = rast()$pal,layerId="ras",
                         opacity = 0.7,project = F)}
    })

# Basemap
############################################################################
############################################################################
observeEvent(input$basemap,{
prox <- leafletProxy("map")%>%
        addProviderTiles(input$basemap, layerId = "base")
if(input$rast == "None"){prox}
if(input$rast != "None"){prox %>% removeTiles(layerId="ras")%>%
clearControls()%>%
    addLegend(colors=rast()$cols,values = values(rast()$r),labels = rast()$lab,
                    position = "bottomleft",
                    title = rast()$lgnd)%>%
    addRasterImage(rast()$r,colors = rast()$pal,layerId="ras",
                         opacity = 0.7,project = F)}
})

# Map
############################################################################
############################################################################
output$map <- renderLeaflet({
  if(is.null(input$tab_rows_selected)){return()}else{

  sel_sf <- sp_wgs84[sp_wgs84$SITECODE== site_sel()$ID,]
                      bbx <- bbox(sel_sf)
                      s_lng <- mean(c(bbx["x","min"],bbx["x","max"]))
                      s_lat <- mean(c(bbx["y","min"],bbx["y","max"]))
                      mx_deg <- max(c(bbx["x","max"]-bbx["x","min"],bbx["y","max"]-bbx["y","min"]))
                      zm <- ifelse(mx_deg < 0.05,13,
                            ifelse(mx_deg <=0.1, 12,
                                    ifelse(mx_deg <=0.2, 12,
                                           ifelse(mx_deg <=0.5, 11,
                                                  ifelse(mx_deg < 1, 10,10)))))
  # raster

  leaflet() %>% leaflet(height="auto",width="auto") %>%
        addProviderTiles(input$basemap, layerId = "base")%>%
        setView(lng = s_lng, lat= s_lat,zoom = zm)%>%
    addPolygons(data = sp_wgs84[sp_wgs84$SITECODE== site_sel()$ID,], color = "black",fillColor="white",
               fillOpacity = 0.2)
    
}}
  )
############################################################################
############################################################################  
}
