#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(tidyr)
library(shiny)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(shinythemes)
library(shinydashboard)
library(leaflet)


colforest="#c0d7c2"
colriver="#7ec9dc"
colriver="#87cdde"
colland="ivory"
borderland = "ivory3"

characters = read_csv("data/characters.csv")
episodes = read_csv("data/episodes.csv")
scenes = read_csv("data/scenes.csv")
appearances = read_csv("data/appearances.csv")


locations=st_read("./data/GoTRelease/Locations.shp",crs=4326)
lakes=st_read("./data/GoTRelease/Lakes.shp",crs=4326)
conts=st_read("./data/GoTRelease/Continents.shp",crs=4326)
land=st_read("./data/GoTRelease/Land.shp",crs=4326)
wall=st_read("./data/GoTRelease/Wall.shp",crs=4326)
islands=st_read("./data/GoTRelease/Islands.shp",crs=4326)
kingdoms=st_read("./data/GoTRelease/Political.shp",crs=4326)
landscapes=st_read("./data/GoTRelease/Landscape.shp",crs=4326)
rivers=st_read("./data/GoTRelease/Rivers.shp",crs=4326)
scenes_locations=st_read("./data/GoTRelease/ScenesLocations.shp", crs=4326)

# Define UI for application that draws a histogram
ui <- navbarPage( theme = shinytheme("flatly"), "GAME OF THRONES",

    tabPanel("Accueil", 
               tags$img(height=550, width=1350, 
                       src="https://www.objeko.com/wp-content/uploads/2021/04/game-of-thrones-decouvrez-le-pactole-colossale-touche-par-les-acteurs-de-la-serie.jpg")
             ),
    tabPanel("Preliminaire",
             sidebarPanel(h3("Preliminaire"),
                          selectInput("inpID","selectionez une option",
                                      choices = c(inpID$option)),
                          tags$br(), h1( column(5, textOutput(outputId = "Graphe"))),
                          style='width: 1000px; height: 500px'
                )
            ),
    tabPanel("Generale",
             sidebarPanel(h4("Generale"),
                          selectInput("inpID1","selectionnez une option",
                                      choices = c(inpID1$option)),
                          plotOutput(outputId = "distPlot", width = "800px", height = "800px"),
                          style='width: 1000px; height: 1000px'
                )
            ),

    tabPanel("l'apparence d une personnage",
             sidebarPanel(
               selectInput ("characteR","veuillez choisir une personage",
                            choices = c(characters$name)),
               plotOutput(outputId = "visuaL", width = "800px", height = "400px"),
               style='width: 850px; height: 600px'
               
             )
          ),
    tabPanel("localisation induviduelle",
             sidebarPanel(
               selectInput("select","Veuillez choisir une personnage",
                           choices = c(characters$name)),
               plotOutput(outputId = "disk", width = "800px", height = "800px"),
               style='width: 900px; height: 1000px'
               
             )
         ),
    tabPanel("l'apparence d une personnage par season",
             sidebarPanel(
               selectInput ("character","veuillez choisir une personage",
                            choices = c(characters$name)),
               sliderInput("season",
                           "Numero de season:",
                           min = 1,
                           max = 8,
                           value = 1,step = 1)),
             mainPanel(
               plotOutput(outputId = "visual", width = "800px", height = "400px"), 
             )
        ),
              
    tabPanel("localisation induviduelle par season",
             sidebarPanel(
               selectInput("selecT","Veuillez choisir une personnage",
                           choices = c(characters$name)),
               sliderInput("seasoN",
                           "Numero de season:",
                           min = 1,
                           max = 8,
                           value = 1,step = 1)),
             mainPanel(
               plotOutput(outputId = "disK", width = "800px", height = "800px")
               
             )
        ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Graphe = renderText({
    c = input$inpID
    if (c == "Nombre de personnages morts dans l ensemble de la serie"){
      sum(scenes$nbdeath)
      
    }
    else if (c == "La duree de la scene la plus longue"){
      as.character(scenes[which.max(scenes$duration),][6][1])
    }
    else if (c == "Le nombre de personnages qui passent plus de 30 minutes a l ecran sur l ensemble des saisons"){
      appearances %>% left_join(scenes)  %>% 
        group_by(name) %>% 
        summarise(screenTime=sum(duration)) %>% 
        filter(screenTime>30*60) %>% 
        nrow()
    }
    else {
      sum(scenes$nbdeath[scenes$episodeId<=10])
    }
  })
  output$distPlot=renderPlot({
    b = input$inpID1
    if (b == "Map de GOT"){
      
      map = ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
        geom_sf(data=islands,fill=colland,col="ivory3")+
        geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest)+
        geom_sf(data=rivers,col=colriver)+
        geom_sf(data=lakes,col=colriver,fill=colriver)+
        geom_sf(data=wall,col="black",size=1)+
        geom_sf_text(data= locations %>% filter(size>4,name!='Tolos'),aes(label=name),size=2.5,family="Palatino", fontface="italic")+
        theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
        theme(panel.background = element_rect(fill = colriver,color=NA)) +
        labs(title = "GoT",caption = "El Am, 2021",x="",y="")
      map
      
    }
    
    else if (b == "Repartition des durees des scenes par episodes"){
      labels = scenes %>% filter(duration>400)
      map1 = ggplot(scenes %>% left_join(episodes))+
        geom_boxplot(aes(x=factor(episodeId),y=duration,fill=factor(seasonNum)))+
        geom_text(data=labels ,aes(x=factor(episodeId),y=duration,label=subLocation),hjust = "right",vjust="top")+
        scale_x_discrete("N episode",as.character(seq(1,73, by=5)))+
        scale_fill_brewer(palette="Spectral",guide="none")+
        ylab("Duree des scenes (min)")+
        ggtitle("Repartition des durees des scenes par episodes")+
        theme_bw()
      map1
      
    }
    else if (b == "Evolution du nombre de mort au cours du temps"){
      
      deaths = scenes %>% select(nbdeath,duration,location,episodeId) %>% 
        mutate(t=cumsum(duration),tdeath=cumsum(nbdeath))
      
      # instant de changement de saison
      # ? lag
      season_t = episodes %>% mutate(ld=lag(total_duration)) %>% 
        mutate(td = cumsum(ld)) %>% 
        filter(episodeNum==1) %>% pull(td)
      
      map2 = ggplot(deaths) + geom_line(aes(x=t/3600,y=tdeath)) +
        scale_x_continuous("",expand = c(0,0),breaks = season_t/3600,
                           labels =   paste("Saison",1:8),)+
        scale_y_continuous("Nombre de morts cumules", expand=c(0,0))+
        theme_bw()+
        theme(axis.text.x=element_text(angle=90))+
        ggtitle("Evolution du nombre de mort au cours du temps")
      map2
      
    }

    else if (b=="Clustering des personnages principaux suivant leur lieux de presence"){
      duration_location_character = scenes %>% left_join(appearances) %>% 
        group_by(name,location) %>% 
        summarize(duration=sum(duration))
      
      duration_large = duration_location_character %>% 
        pivot_wider(values_from = duration,names_from = location,values_fill = c("duration"=0))
      
      
      X=as.matrix(duration_large[,-1])
      Xs=X[rowSums(X)>60*60,]
      Xns=Xs/rowSums(Xs)
      rownames(Xns)=duration_large$name[rowSums(X)>60*60]
      
      hc=hclust(dist(Xns,method="manhattan"))
      plot(hc,main = "Clustering des personnages principaux suivant leur lieux de presences",sub ="@El Am, 2021",xlab = "")
    }
    else if (b=="Personnages qui apparaissent plus d une heure sur l ensemble des saisons"){
      screenTimePerSeasons = appearances %>% left_join(scenes) %>% 
        left_join(episodes) %>% 
        group_by(name,seasonNum) %>% 
        summarise(screenTime=sum(duration)) %>% 
        arrange(desc(screenTime)) 
      screenTimeTotal = screenTimePerSeasons %>% 
        group_by(name) %>% 
        summarise(screenTimeTotal=sum(screenTime))
      mainCharacters = screenTimeTotal %>% 
        filter(screenTimeTotal>60*60) %>% 
        arrange(screenTimeTotal) %>% 
        mutate(nameF=factor(name,levels = name))
      data = screenTimePerSeasons %>% left_join(mainCharacters) %>% filter(!is.na(nameF))
      ggplot(data)+
        geom_bar(aes(y=nameF,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
        scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
        geom_text(data=mainCharacters,aes(y=nameF,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
        scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
        ylab("")+ggtitle("Temps d'apparition cumule par personnage et saison")
    }
    
  })
  output$visuaL <- renderPlot({
    jstime = appearances %>% filter(name== input$characteR) %>% 
      left_join(scenes) %>% 
      group_by(episodeId) %>% 
      summarise(time=sum(duration))
    map3 = ggplot(jstime) + 
      geom_line(aes(x=episodeId,y=time))+
      theme_bw()+
      xlab("episode")+ylab("temps")+
      ggtitle("Duree de presence  par episode")
    map3
  })
  
  mydata <-reactive({
    time = appearances %>% 
      filter(name==input$characteR) %>%
      left_join(scenes) %>%
      group_by(episodeId) %>%
      left_join(episodes) %>% 
      filter(seasonNum == input$season) %>%
      summarise(time=sum(duration))
  })
  output$visual <- renderPlot({
    ggplot(mydata()) + 
      geom_line(aes(x=episodeId,
                    y=time))+
      theme_bw()+ggtitle("Duree de presence par episode")

      
    })
  output$disk <- renderPlot({
     main_char= input$select
     library(sf)
     library(tidyr)
     library(ggplot2)
     library(dplyr)
     library(readr)
     library(leaflet)
     characters = read_csv("data/characters.csv")
     episodes = read_csv("data/episodes.csv")
     scenes = read_csv("data/scenes.csv")
     appearances = read_csv("data/appearances.csv")
     
     locations=st_read("./data/GoTRelease/Locations.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
     locations=st_transform(locations,4326)
     locations=leaflet(locations)
     scenes_locations=st_read("./data/GoTRelease/ScenesLocations.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
     lakes=st_read("./data/GoTRelease/Lakes.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
     conts=st_read("./data/GoTRelease/Continents.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
     land=st_read("./data/GoTRelease/Land.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
     wall=st_read("./data/GoTRelease/Wall.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
     islands=st_read("./data/GoTRelease/Islands.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
     kingdoms=st_read("./data/GoTRelease/Political.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
     landscapes=st_read("./data/GoTRelease/Landscape.shp",crs=4326, stringsAsFactors = T, quiet=TRUE)
     landscapes = st_transform(landscapes,4326)
     roads=st_read("./data/GoTRelease/Roads.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
     rivers=st_read("./data/GoTRelease/Rivers.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
     
     colforest="#c0d7c2"
     colriver="#7ec9dc"
     colriver="#87cdde"
     colland="ivory"
     borderland = "ivory3"

     landpol = st_union(st_geometry(land)) 
     islandpol = st_union(st_geometry(islands))
     backpol=st_union(landpol,islandpol)
     background = st_as_sf(data.frame(name=main_char,geometry=rep(backpol,6)))
  
     loc_time=appearances %>% filter(name %in% main_char) %>% left_join(scenes) %>% group_by(location,name) %>% summarize(duration=sum(duration,na.rm=TRUE)) 
     loc_time_mc = scenes_locations %>% left_join(loc_time)
  
  
      mapp=ggplot()+geom_sf(data=background,color=borderland,fill=colland)+
        geom_sf(data=loc_time_mc%>% filter(!is.na(duration)),aes(size=duration/60,color=name))+
        geom_sf_text(data=loc_time_mc%>% filter(duration>60*60),aes(label=location),color="#000000",vjust="bottom",family="Palatino", fontface="italic")+
        coord_sf(expand = 0,ndiscr = 0)+
        scale_color_discrete(guide="none")+
        scale_size_area("Duree (min) :",max_size = 12,breaks=c(30,60,120,240))+
    
           theme(panel.background = element_rect(fill = colriver,color=NA),
           text = element_text(family="Palatino",face = "bold",size = 14),
           legend.key = element_rect(fill="#ffffff"),
            ) +
             labs(title = "Temps de presence par location",caption = "@El Am, 2021",x="",y="")
        mapp
  })
  output$disK <- renderPlot({
    main_char= input$selecT
    library(sf)
    library(tidyr)
    library(ggplot2)
    library(dplyr)
    library(readr)
    library(leaflet)
    characters = read_csv("data/characters.csv")
    episodes = read_csv("data/episodes.csv")
    scenes = read_csv("data/scenes.csv")
    appearances = read_csv("data/appearances.csv")
    
    locations=st_read("./data/GoTRelease/Locations.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    locations=st_transform(locations,4326)
    #locations=leaflet(locations)
    scenes_locations=st_read("./data/GoTRelease/ScenesLocations.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    lakes=st_read("./data/GoTRelease/Lakes.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    conts=st_read("./data/GoTRelease/Continents.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    land=st_read("./data/GoTRelease/Land.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    wall=st_read("./data/GoTRelease/Wall.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    islands=st_read("./data/GoTRelease/Islands.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    kingdoms=st_read("./data/GoTRelease/Political.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    landscapes=st_read("./data/GoTRelease/Landscape.shp",crs=4326, stringsAsFactors = T, quiet=TRUE)
    landscapes = st_transform(landscapes,4326)
    roads=st_read("./data/GoTRelease/Roads.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    rivers=st_read("./data/GoTRelease/Rivers.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    
    colforest="#c0d7c2"
    colriver="#7ec9dc"
    colriver="#87cdde"
    colland="ivory"
    borderland = "ivory3"
    
    landpol = st_union(st_geometry(land)) 
    islandpol = st_union(st_geometry(islands))
    backpol=st_union(landpol,islandpol)
    background = st_as_sf(data.frame(name=main_char,geometry=rep(backpol,6)))
    
    loc_time=appearances %>% filter(name %in% main_char) %>% left_join(scenes) %>% group_by(location,name) %>%  left_join(episodes) %>% 
      filter(seasonNum == input$seasoN) %>% summarize(duration=sum(duration,na.rm=TRUE)) 
    loc_time_mc = scenes_locations %>% left_join(loc_time)
    
    
    mapp=ggplot()+geom_sf(data=background,color=borderland,fill=colland)+
      geom_sf(data=loc_time_mc%>% filter(!is.na(duration)),aes(size=duration/60,color=name))+
      geom_sf_text(data=loc_time_mc%>% filter(duration>60*60),aes(label=location),color="#000000",vjust="bottom",family="Palatino", fontface="italic")+
      coord_sf(expand = 0,ndiscr = 0)+
      scale_color_discrete(guide="none")+
      scale_size_area("Duree (min) :",max_size = 12,breaks=c(30,60,120,240))+
      
      theme(panel.background = element_rect(fill = colriver,color=NA),
            text = element_text(family="Palatino",face = "bold",size = 14),
            legend.key = element_rect(fill="#ffffff"),
      ) +
      labs(title = "Temps de presence par location",caption = "@El Am, 2021",x="",y="")
    mapp
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
