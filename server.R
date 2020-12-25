function(input,output){
  output$topskor <- renderPlotly({
    top_skor <- univ_rank_cat %>%
      select(University, Teaching, Research, Citations,
             Industry_Income, International_Outlook, Score_Result) %>% 
      arrange(desc(Score_Result)) %>% 
      slice(1:input$numbertopskor) %>% 
      mutate(text = paste0("Universitas: ", University, "<br>",
                           "Nilai: ", sprintf("%0.2f", round(Score_Result, digits = 2))))
    
    plot_topskor <- top_skor %>% 
      ggplot(aes(x = Score_Result,
                 y = reorder(University, Score_Result),
                 text = text
                 )) +
      geom_col(fill = "#212ca6") +
      labs(x = NULL,
           y = NULL,
           title = "Peringkat Universitas Terbaik Dunia 2020") +
      theme_CPDV
    
    ggplotly(plot_topskor, tooltip = "text")
  })
  
  output$topcountry <- renderPlotly({
    top_country <- univ_rank_cat %>% 
      slice(1:input$numbertopskor) %>% 
      group_by(Country) %>% 
      summarise(value = n()) %>% 
      arrange(desc(value)) %>% 
      mutate(text = paste0("Negara: ", Country, "<br>",
                           "Jumlah Universitas: ", value))
    
    plot_topcountry <- top_country %>% 
      ggplot(aes(x = value,
                 y = reorder(Country, value),
                 text = text)) + 
      geom_col(fill = "#e31445") +
      labs(x = NULL,
           y = NULL,
           title = "Negara berdasarkan Jumlah Universitas Terbaik Dunia") +
      theme_CPDV
    
    ggplotly(plot_topcountry, tooltip = "text")
  })
  
  output$corrskor <- renderPlotly({
    pilih <- input$skor %>% 
      str_replace_all(pattern = " ", replacement = "_")
    
    textskor <- function(x){
      if (x == "Teaching"){
        paste("Peringkat: ", univ_rank$Categoric, "<br>", 
              "Teaching: ", univ_rank$Teaching, "<br>",
              "Score Result: ", univ_rank$Score_Result, "<br>", 
              sep="")
      }else
        if (x == "Research"){
          paste("Peringkat: ", univ_rank$Categoric, "<br>", 
                "Research: ", univ_rank$Research, "<br>",
                "Score Result: ", univ_rank$Score_Result, "<br>", 
                sep="")
        }else
          if (x == "Citations"){
            paste("Peringkat: ", univ_rank$Categoric, "<br>", 
                  "Citations: ", univ_rank$Citations, "<br>",
                  "Score Result: ", univ_rank$Score_Result, "<br>", 
                  sep="")
          }else
            if (x == "Industry_Income"){
              paste("Peringkat: ", univ_rank$Categoric, "<br>", 
                    "Industry Income: ", univ_rank$Industry_Income, "<br>",
                    "Score Result: ", univ_rank$Score_Result, "<br>", 
                    sep="")
            }else {
              paste("Peringkat: ", univ_rank$Categoric, "<br>", 
                    "International Outlook: ", univ_rank$International_Outlook, "<br>",
                    "Score Result: ", univ_rank$Score_Result, "<br>", 
                    sep="")
            }
    }
    
    univ_rank_skor <- univ_rank %>% 
      mutate(text = textskor(pilih))
            
    plt_corrskor <- univ_rank_skor %>% 
      ggplot(aes_string(x = pilih, y = "Score_Result")) +
      geom_point(aes(color = Categoric, text = text)) +
      scale_y_continuous(limits = c(0, 100),
                         breaks = seq(0, 100, 25)) +
      scale_x_continuous(limits = c(0, 100),
                         breaks = seq(0, 100, 25)) +
      scale_color_manual(values = c("#fc0b03", "#c46104", "#ad9103", "#8ead03", "#429903", "#033801",
                                    "#98f5c9", "#036358", "#015a78", "#72a3f2", "#52038f", "#f200d2")) +
      labs(title = "Hubungan Kategori Penilaian terhadap Nilai Akhir",
           y = "Score Result",
           x = glue("{input$skor}")) +
      theme(plot.title = element_text(face = "bold", size = 14, hjust = 0),
            panel.background = element_rect(fill = "#ffffff"), 
            panel.grid.major.x = element_line(colour = "grey"),
            panel.grid.major.y = element_line(colour = "grey"),
            axis.line.x = element_line(color = "grey"),
            axis.line.y = element_line(color = "grey"),
            axis.text = element_text(size = 10, colour = "black"),
            legend.title = element_blank())
    
    ggplotly(plt_corrskor, tooltip = "text")
  })
  
  output$CorrBox <- renderValueBox({
    pilih <- input$skor %>% 
      str_replace_all(pattern = " ", replacement = "_")
    
    Corrvalue <- function(x){
      if (x == "Teaching"){
        round(cor(univ_rank_cat$Score_Result, univ_rank_cat$Teaching),2)
      }else
        if (x == "Research"){
          round(cor(univ_rank_cat$Score_Result, univ_rank_cat$Research),2)
        }else
          if (x == "Citations"){
            round(cor(univ_rank_cat$Score_Result, univ_rank_cat$Citations),2)
          }else
            if (x == "Industry_Income"){
              round(cor(univ_rank_cat$Score_Result, univ_rank_cat$Industry_Income),2)
            }else {
              round(cor(univ_rank_cat$Score_Result, univ_rank_cat$International_Outlook),2)
            }
    }
    
    nilai_cor <- Corrvalue(pilih)
    
    tags$head(tags$style(HTML(".small-box {height: 120px}")))
    valueBox(value = nilai_cor, subtitle = "Nilai Korelasi",
             color = "green", 
             icon = icon("shapes"))
  })
  
  output$numberstudent <- renderPlotly({
    top_numbstudent <- univ_rank_cat %>% 
      slice(1:input$numberuniversity) %>% 
      select(Score_Rank, University, Number_students) %>% 
      mutate(text = paste0("Universitas: ", University, "<br>",
                           "Peringkat: ", Score_Rank, "<br>",
                           "Jumlah Mahasiswa: ", Number_students))
    
    plot_top_numb <- top_numbstudent %>% 
      ggplot(aes(x = Number_students,
                 y = reorder(University, desc(Score_Rank)),
                 text = text)) +
      geom_col(fill = "#d4340d") +
      labs(x = NULL,
           y = NULL,
           title = "Jumlah Mahasiswa Universitas Terbaik Dunia") +
      theme_CPDV
    
    ggplotly(plot_top_numb, tooltip = "text")
  })
  
  output$gender <- renderPlotly({
    univ_rankplus <- univ_rank %>%
      select(Categoric, Percentage_Female, Percentage_Male) %>% 
      pivot_longer(cols = c(Percentage_Female, Percentage_Male), 
                   names_to = "var", 
                   values_to = "value") %>% 
      group_by(Categoric, var) %>% 
      summarise(rerata_student = mean(value)) %>% 
      mutate(gender = ifelse(var == "Percentage_Female", "Female", "Male")) %>%
      group_by(Categoric) %>% 
      mutate(proportion = round(rerata_student/sum(rerata_student),2)*100,
             text = paste0("Peringkat: ", Categoric, "<br>",
                           "Gender: ", gender, "<br>",
                           "Proporsi: ", proportion, "%"))
    
    plot_univ_rankplus <- univ_rankplus %>% 
      ggplot(aes(x = rerata_student, 
                 y = reorder(Categoric, desc(Categoric)),
                 text = text)) +
      geom_col(aes(fill = var), position = "fill") +
      geom_vline(xintercept = 0.5, col = "white", lty = 2, lwd = 1.3) + 
      labs(x = NULL,
           y = NULL,
           title = "Perbandingan Gender Mahasiswa Berdasarkan Peringkat Universitas") +
      scale_fill_manual(values = c("#9e065f", "#45069e")) +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      theme_CPDV +
      theme(legend.position = "none")
    
    ggplotly(plot_univ_rankplus, tooltip = "text")
  })
  
  output$studentstaf <- renderPlotly({
    plot_mahasiswa_staf <- univ_rank %>% 
      group_by(Categoric) %>% 
      summarise(rerata_students_per_Staff = median(Numb_students_per_Staff)) %>% 
      mutate(text = paste0("Peringkat: ", Categoric, "<br>",
                           "Rasio Mahasiswa per Staf: ", rerata_students_per_Staff)) %>% 
      ggplot(aes(x = Categoric,
                 y = rerata_students_per_Staff,
                 text = text)) +
      geom_col(fill = "#12a148") +
      scale_y_continuous(limits = c(0, 18),
                         breaks = seq(0, 18, 3)) +
      geom_hline(yintercept = median(univ_rank$Numb_students_per_Staff), linetype = 2, 
                 size = 1, col = "white") +
      labs(x = NULL,
           y = NULL,
           title = "Rasio Mahasiswa per Staf Berdasarkan Peringkat Universitas") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
      theme_CPDV
    
    ggplotly(plot_mahasiswa_staf, tooltip = "text")
  })
  
  output$univmap <- renderLeaflet({
    target <- c(as.character(input$mapinput))
    
    univ_rank_leaf <- univ_rank %>% 
      filter(Categoric %in% target)
    
    popup_univ <- paste("<h4><b>", univ_rank_leaf$University, "</b></h4>", 
                        "Negara: ", univ_rank_leaf$Country, "<br>", 
                        "Peringkat: ", univ_rank_leaf$Score_Rank, "<br>",
                        "Kategori Peringkat: ", univ_rank_leaf$Categoric, "<br>",
                        "Nilai: ", univ_rank_leaf$Score_Result, "<br>", 
                        sep="")
    
    m <- univ_rank_leaf %>%
      leaflet() %>% 
      addProviderTiles("Esri") %>% 
      setView(lat=10, lng=0 , zoom=2) %>% 
      addMarkers(label = ~University,
                 popup = popup_univ,
                 clusterOptions = markerClusterOptions())
    m
  })
  
  output$data_univ <- renderDataTable({
    oldName <- colnames(univ_rank_cat)
    newName <- univ_rank_cat %>% 
      colnames() %>% 
      str_replace_all(pattern = "_", replacement = " ") %>% 
      str_to_title()
    
    colnames(univ_rank_cat)[which(names(univ_rank_cat) == oldName)] <- newName
    
    datatable(data = univ_rank_cat,
              options = list(scrollX = T))
    
  })
}