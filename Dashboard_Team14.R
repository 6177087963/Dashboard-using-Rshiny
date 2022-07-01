library(shiny)
library(shinydashboard)
library(wordcloud2)
library(tidyverse)
library(plotly)
library(dplyr)
library(igraph)
library(survival)
library(tnet)

# Define UI for application that draws a histogram
d2<-read_csv("dat_2.csv")
d3<-read_csv("dat_3.csv")
d4<-read_csv("dat_4.csv")
d5<-read_csv("dat_5.csv")
d6<-read_csv("dat_6.csv")

df3<-d3%>%
  separate(term,c("term","topic_2"))%>%
  select(topic,term,beta,diff_ratio)

df3$topic <- paste0("X", df3$topic)

topic_table<-d4%>%
  gather("column","value",X1:X15)%>%
  group_by(id,word)%>%
  mutate(m_val = max(value))%>%
  ungroup()%>%
  mutate(is_max = ifelse(value == m_val,sample(column),NA))%>%
  drop_na()%>%
  group_by(id,word)%>%
  mutate(topic = sample(column,1))%>%
  ungroup()%>%
  select(id,topic)%>%
  distinct()

course_codes<-d4%>%
  separate(id,c("Code","Number"),sep="_")%>%
  select(Code)%>%
  distinct()

df4 <- d4%>%
  left_join(topic_table,by = c("id"))%>%
  tidyr::separate(id,c("Code","Number"),sep="_")

df5 <- d5%>%
  unite("word",gram1:gram2,sep = " ")%>%
  left_join(topic_table, by = c("id"))%>%
  tidyr::separate(id,c("Code","Number"),sep="_")

df6 <- d6%>%
  unite("word",gram1:gram3,sep = " ")%>%
  left_join(topic_table,by = c("id"))%>%
  tidyr::separate(id,c("Code","Number"),sep="_")

#use the list function to combine the d4,d5,d6
datafiles <- list(df4, df5, df6)

#Use the topic table to find the distinct topic
Stopic <- topic_table%>%
  select(topic)%>%
  distinct()

#Clustering
data_scaled1<-scale(d5%>%
                      select(anger:positive,ADJ:SYM,VERB_ratio,X_ratio))

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "College of Professional Studies Text Analysis",titleWidth = 1000),
  dashboardSidebar(sidebarMenu(
    menuItem("Theme Identification", tabName = "wordcount",icon=icon("th")),
    menuItem("Words by Courses", tabName = "wordcourse", icon = icon("th")),
    menuItem("Network Analysis", tabName = "network",icon=icon("book")),
    menuItem("Topic Identify--Keywords", tabName = "topics",icon=icon("tasks")),
    menuItem("Cluster Analysis", tabName = "clusters",icon=icon("list"))
  )),
  dashboardBody(
    tabItems(
      #Q1.Identify common themes across the entire dataset of course descriptions
      #Q1.1 Select the Top number of Course Code and count and select the Top number of words appearance
      tabItem(tabName = "wordcount",
              fluidRow(
                box(title = "Inputs", width=6,status = "warning", solidHeader = TRUE,
                    numericInput("top_c","Select Number of Top Keywords",value = 10)
                ),
                
                box(title = "Inputs",width=6,status = "warning", solidHeader = TRUE,
                    selectInput("dataset", "Chose Type of Combination",
                                choices=c("One Keyword"="1",
                                          "Two Keywords"="2"))
                )
              ),
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Top Number of Words",
                  plotlyOutput("Words", width = 1000, height = 350)
                )
                ),
              fluidRow(
                box(
                  width = 6, status = "info",
                  title = "Table for top words by count",
                  tableOutput("table1")
                )
              )
      ),
      
      tabItem(tabName="wordcourse",
      fluidRow( 
        box(
          title = "Top Number of Words to be displayed",status = "warning", width=12, 
          solidHeader = TRUE,
          selectInput("course_code","Course Code",course_codes$Code),
          numericInput("top_n_2","Top Number of Words",value = 10))
      ),
      fluidRow(
        box(
          title = "Top Words by Courses", status = "primary", solidHeader = TRUE,width=7,
          plotlyOutput("word_course",width=600, height = 400)),
        box(
          width = 5, status = "info",
          title = "Table for Keywords",
          tableOutput("table4")
        )
      )
      ),
      
      tabItem(tabName = "network",
              fluidRow(
                box(title = "Inputs", width=12,status = "warning", solidHeader = TRUE,
                    numericInput("top_wc","Select the Top Keywords",value = 15)
                )
               ),
              fluidRow(
                box(width = 12, status = "info", solidHeader = TRUE,
                    title = "Network Analysis for Bi-Gram",
                    plotlyOutput("network_analysis",width = 1000, height = 400)
                )
                ),
                fluidRow(
               box(
                  width = 4, status = "info",
                  title = "Table for Network Analysis",
                  tableOutput("table2")
                )
              )
      ),
      tabItem(tabName = "topics",
              fluidRow(
                box(title = "Inputs", width=6,status = "warning", solidHeader = TRUE,
                    selectInput("topic2", "Topic", Stopic$topic)
                ),
                box(title = "Inputs",width=6,status = "warning", solidHeader = TRUE,
                    numericInput("top_n2","Top Number of Keywords", value = 15)
                )
              ),
              fluidRow(
                box(
                  width = 7, status = "info", solidHeader = TRUE,
                  title = "Barplot per Topic - Keywords",
                  plotlyOutput("barPlot_keywords", width = "100%", height = 600)
                ),
                box(
                  width = 5, status = "info",
                  title = "Table for Keywords",
                  tableOutput("table_keywords")
                )
              )
      ),
      
      tabItem(tabName = "clusters",
              fluidRow(
                column(3, "1. Legal Policy for healthcare"),
                column(3, "2. Information technology and digital studies"),
                column(3, "3. Medical Studies"),
                column(3, "4. Leadership focused studies"),
                column(3, "5. Social Science"),
                column(3, "6. Enrollment courses"),
                column(3, "7. Analytics courses"),
                column(3, "8. Research-related studies"),
                column(3, "9. Course offerings and benefits"),
                column(3, "10. System and Process Management"),
                column(3, "11. Engineering and science"),
                column(3, "12. Product Development"),
                column(3, "13. Healthcare studies"),
                column(3, "14. Political"),
                column(3, "15. Marketing and Business")
              ),
              
              fluidRow(
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Topic profile within each class",
                  plotlyOutput("barplot1", width = "100%", height = 600)
                ),
                box(title = "Inputs",width=4,status = "warning", solidHeader = TRUE,
                    sliderInput("krange1",
                                "Select number of clusters to identify positive descriptivity",
                                min = 1, max = 25, value= 2)
                )
              )
      )
    )
  )
)
server <- function(input, output) {
  
  #### Word Count Analysis
  toplot_count <-reactive({
      if(input$dataset=='1'){
          wordcount<-df4%>%
          count(word)%>%
          arrange(-n)%>%
          slice_max(n,n=input$top_c)
          
      }else if(input$dataset=='2'){
        wordcount<-df5%>%
          count(word)%>%
          arrange(-n)%>%
          slice_max(n,n=input$top_c)
      }
      return(wordcount)
  })
  output$Words <- renderPlotly({
    ggplot(toplot_count(),aes(y=reorder(word,n,function(x) x),x=n))+
      geom_col(fill = "grey",col = "black")+labs(x="",y="")+
      theme_bw()
  })
  
  output$table1 <- renderTable({
    toplot_count()
  })
  
  # Top Words by Courses Analysis
  output$word_course <- renderPlotly({
    d4_filter<-d4%>%
      tidyr::separate(id,c("Code","Number"),sep="_")%>%
      dplyr::filter(Code == input$course_code)
    
    toplot_course<-d4_filter%>%
      count(word)%>%
      arrange(-n)%>%
      slice_max(n,n=input$top_n_2)
    ggplotly(ggplot(toplot_course,aes(y=reorder(word,n,function(x) x),x=n))+
      geom_col(fill = "grey",col = "black")+labs(x="",y=""))
  })
  
  output$table4 <- renderTable({
    d4_filter<-d4%>%
      tidyr::separate(id,c("Code","Number"),sep="_")%>%
      dplyr::filter(Code == input$course_code)
    
    toplot_course<-d4_filter%>%
      count(word)%>%
      arrange(-n)%>%
      slice_max(n,n=input$top_n_2)
    
    toplot_course
    })
  
  #### Network Analysis
  output$network_analysis <- renderPlotly({
      network_data <- d5%>%
        select(gram1,gram2)%>%
        count(gram1,gram2)
      
      network<-graph.data.frame(network_data)
      
      e_dat<-eigen_centrality(network)$vector
      
      dataset<-tibble(word = names(e_dat),eigen = e_dat)%>%
        arrange(-eigen)%>%
        slice_max(eigen,n=input$top_wc)
      
      ggplotly(ggplot(dataset,aes(y=reorder(word,eigen,function(x) x),x=eigen),height = 500, width = 500)+
                 geom_col(fill = "grey",col = "black")+labs(x="",y="")+
                 theme_bw())
  })
  
  output$table2 <- renderTable({
    network_data <- d5%>%
      select(gram1,gram2)%>%
      count(gram1,gram2)
    
    network<-graph.data.frame(network_data)
    
    e_dat<-eigen_centrality(network)$vector
    
    dataset<-tibble(word = names(e_dat),eigen = e_dat)%>%
      arrange(-eigen)%>%
      slice_max(eigen,n=input$top_wc)
  })
  
  #### Topic Analysis    
  to_plot2 <- reactive({
    temp2 <-df3%>%
      dplyr::filter(topic == input$topic2)%>%
      slice_max(beta,n=input$top_n2)
    
  })
  
  output$barPlot_keywords <- renderPlotly({
    ggplotly(ggplot(to_plot2(),aes(y=reorder(term,beta,function(x) x),x=beta))+
      geom_col(fill = "grey",col = "black")+labs(x="",y="")+
      theme_bw())
  })
  
  output$table_keywords <- renderTable({
    to_plot2()
  })
  
  #### Cluster Analysis
  output$barplot1<- renderPlotly({
    set.seed(30)
    k3 <- kmeans(data_scaled1, centers = input$krange1, nstart = 25)
    pos_cluster<-tibble(d5, cluster = k3$cluster)
    #Topic profile within each class based on POS
    pos_to_plot<-pos_cluster%>%
      group_by(cluster)%>%
      summarize(across(X1:X15,c(mean)))%>%
      gather(X1_1:X15_1,key = "Topic",value = "Probability")
    
    ggplotly(ggplot(pos_to_plot,aes(x=cluster, y=Probability, group = Topic))+
      geom_col(position = position_dodge2(preserve = "total"), color = 'grey', fill = "light green"))
    
  })
}

shinyApp(ui, server)