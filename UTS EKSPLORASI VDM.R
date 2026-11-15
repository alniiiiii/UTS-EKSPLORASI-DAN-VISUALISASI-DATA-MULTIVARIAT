
# ============================================================
# DASHBOARD UTS EVDM
# Eksplorasi dan Visualisasi Data Multivariat
# Data: UTS_EVDM.xlsx
# ============================================================

# 1. PACKAGE
paket <- c("shiny", "readxl", "dplyr", "ggplot2")

baru <- paket[!paket %in% rownames(installed.packages())]

if (length(baru) > 0) {
  install.packages(baru)
}

invisible(lapply(paket, library, character.only = TRUE))


# ============================================================
# 2. MEMBACA DAN MEMBERSIHKAN DATA
# ============================================================

library(readxl)
UTS_EVDM <- read_excel("UTS_EVDM.xlsx")
View(UTS_EVDM)

# Menggunakan 498 observasi pertama berdasarkan pemeriksaan data
UTS_EVDM <- UTS_EVDM[1:498, ]

# Mengubah nama variabel
names(UTS_EVDM) <- c(
  "Weight", "Age", "FAF", "FCVC", "Gender"
)

# Mengubah tipe data
UTS_EVDM <- UTS_EVDM %>%
  mutate(
    Weight = as.numeric(Weight),
    Age = as.numeric(Age),
    FAF = as.numeric(FAF),
    FCVC = as.numeric(FCVC),
    Gender = as.factor(Gender)
  ) %>%
  filter(
    complete.cases(.),
    between(Weight, 30, 200),
    between(Age, 10, 100),
    between(FAF, 0, 3),
    between(FCVC, 1, 3)
  )


# ============================================================
# 3. MEMBANGUN MODEL REGRESI
# ============================================================

model <- lm(
  Weight ~ Age + FAF + FCVC + Gender,
  data = UTS_EVDM
)

hasil_model <- summary(model)

# Menghitung RMSE
RMSE <- sqrt(mean(residuals(model)^2))

# Menghitung p-value uji simultan
nilai_f <- hasil_model$fstatistic

p_value_f <- pf(
  nilai_f[1],
  nilai_f[2],
  nilai_f[3],
  lower.tail = FALSE
)


# ============================================================
# 4. TAMPILAN DASHBOARD
# ============================================================

ui <- fluidPage(
  
  titlePanel(
    "DASHBOARD EKSPLORASI DAN VISUALISASI DATA MULTIVARIAT"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Pengaturan Dashboard"),
      
      checkboxGroupInput(
        "gender",
        "Pilih Gender:",
        choices = levels(UTS_EVDM$Gender),
        selected = levels(UTS_EVDM$Gender)
      ),
      
      hr(),
      
      helpText(
        "Dashboard ini menggunakan data UTS_EVDM.xlsx."
      ),
      
      helpText(
        "Variabel: Weight, Age, FAF, FCVC, dan Gender."
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # ====================================================
        # TAB 1: RINGKASAN DATA
        # ====================================================
        
        tabPanel(
          "Ringkasan Data",
          
          br(),
          
          h3("Informasi Dataset"),
          
          textOutput("jumlah_data"),
          
          tableOutput("struktur_data"),
          
          h3("Statistik Deskriptif"),
          
          tableOutput("statistik"),
          
          h3("Distribusi Gender"),
          
          tableOutput("frekuensi_gender")
          
        ),
        
        
        # ====================================================
        # TAB 2: UNIVARIAT
        # ====================================================
        
        tabPanel(
          "Univariat",
          
          br(),
          
          h3("Visualisasi Variabel Numerik"),
          
          selectInput(
            "variabel_univ",
            "Pilih Variabel Numerik:",
            choices = c("Weight", "Age", "FAF", "FCVC")
          ),
          
          selectInput(
            "jenis_univ",
            "Pilih Jenis Grafik:",
            choices = c(
              "Histogram",
              "Boxplot"
            )
          ),
          
          plotOutput("grafik_univ", height = "400px"),
          
          hr(),
          
          h3("Visualisasi Variabel Kategorik: Gender"),
          
          h4("Bar Chart"),
          
          plotOutput("bar_gender", height = "350px"),
          
          h4("Pie Chart"),
          
          plotOutput("pie_gender", height = "350px"),
          
          h4("Pareto Chart"),
          
          plotOutput("pareto_gender", height = "400px")
          
        ),
        
        
        # ====================================================
        # TAB 3: BIVARIAT
        # ====================================================
        
        tabPanel(
          "Bivariat",
          
          br(),
          
          h3("Hubungan Age dengan Weight"),
          
          plotOutput("bivariat1", height = "400px"),
          
          h3("Hubungan FAF dengan Weight"),
          
          plotOutput("bivariat2", height = "400px"),
          
          h3("Perbedaan Weight Berdasarkan Gender"),
          
          plotOutput("bivariat3", height = "400px")
          
        ),
        
        
        # ====================================================
        # TAB 4: MULTIVARIAT
        # ====================================================
        
        tabPanel(
          "Multivariat",
          
          br(),
          
          h3("Hubungan Age, Weight, dan Gender"),
          
          plotOutput("multivariat1", height = "400px"),
          
          h3("Hubungan Weight, FAF, dan FCVC"),
          
          plotOutput("multivariat2", height = "400px")
          
        ),
        
        
        # ====================================================
        # TAB 5: REGRESI LINEAR BERGANDA
        # ====================================================
        
        tabPanel(
          "Regresi Berganda",
          
          br(),
          
          h3("Model Regresi Linear Berganda"),
          
          p(
            "Variabel respons: Weight"
          ),
          
          p(
            "Variabel prediktor: Age, FAF, FCVC, dan Gender"
          ),
          
          h4("Persamaan Model"),
          
          verbatimTextOutput("persamaan"),
          
          h4("Hasil Estimasi Koefisien"),
          
          tableOutput("koefisien"),
          
          h4("Evaluasi Model"),
          
          tableOutput("evaluasi"),
          
          h4("Grafik Aktual dan Prediksi"),
          
          plotOutput("aktual_prediksi", height = "400px"),
          
          h4("Grafik Residual"),
          
          plotOutput("grafik_residual", height = "400px")
          
        )
        
      )
      
    )
    
  )
  
)


# ============================================================
# 5. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # Filter data berdasarkan Gender
  
  data_filter <- reactive({
    
    if (is.null(input$gender) || length(input$gender) == 0) {
      return(UTS_EVDM)
    }
    
    UTS_EVDM %>%
      filter(Gender %in% input$gender)
    
  })
  
  
  # ==========================================================
  # RINGKASAN DATA
  # ==========================================================
  
  output$jumlah_data <- renderText({
    
    paste(
      "Jumlah observasi:",
      nrow(data_filter()),
      "| Jumlah variabel:",
      ncol(UTS_EVDM)
    )
    
  })
  
  output$struktur_data <- renderTable({
    
    data.frame(
      Variabel = names(UTS_EVDM),
      Tipe_Data = sapply(UTS_EVDM, function(x) class(x)[1])
    )
    
  })
  
  output$statistik <- renderTable({
    
    data_filter() %>%
      summarise(
        across(
          c(Weight, Age, FAF, FCVC),
          list(
            Mean = ~mean(.),
            Median = ~median(.),
            SD = ~sd(.),
            Min = ~min(.),
            Max = ~max(.)
          )
        )
      )
    
  })
  
  output$frekuensi_gender <- renderTable({
    
    data_filter() %>%
      count(Gender, name = "Frekuensi") %>%
      mutate(
        Persentase = round(
          Frekuensi / sum(Frekuensi) * 100, 2
        )
      )
    
  })
  
  
  # ==========================================================
  # VISUALISASI UNIVARIAT
  # ==========================================================
  
  output$grafik_univ <- renderPlot({
    
    d <- data_filter()
    
    if (input$jenis_univ == "Histogram") {
      
      ggplot(d, aes(x = .data[[input$variabel_univ]])) +
        geom_histogram(
          bins = 20,
          fill = "skyblue",
          color = "black"
        ) +
        labs(
          title = paste(
            "Histogram",
            input$variabel_univ
          ),
          x = input$variabel_univ,
          y = "Frekuensi"
        ) +
        theme_minimal()
      
    } else {
      
      ggplot(
        d,
        aes(y = .data[[input$variabel_univ]])
      ) +
        geom_boxplot(
          fill = "lightgreen"
        ) +
        labs(
          title = paste(
            "Boxplot",
            input$variabel_univ
          ),
          y = input$variabel_univ
        ) +
        theme_minimal()
      
    }
    
  })
  
  
  # BAR CHART
  
  output$bar_gender <- renderPlot({
    
    ggplot(
      data_filter(),
      aes(x = Gender, fill = Gender)
    ) +
      geom_bar(color = "black") +
      labs(
        title = "Distribusi Gender",
        x = "Gender",
        y = "Frekuensi"
      ) +
      theme_minimal()
    
  })
  
  
  # PIE CHART
  
  output$pie_gender <- renderPlot({
    
    data_gender <- data_filter() %>%
      count(Gender) %>%
      mutate(
        Persentase = n / sum(n) * 100,
        Label = paste0(
          Gender,
          "\n",
          round(Persentase, 1),
          "%"
        )
      )
    
    ggplot(
      data_gender,
      aes(x = "", y = n, fill = Gender)
    ) +
      geom_col(color = "white") +
      coord_polar("y") +
      geom_text(
        aes(label = Label),
        position = position_stack(vjust = 0.5)
      ) +
      labs(
        title = "Proporsi Gender",
        fill = "Gender"
      ) +
      theme_void()
    
  })
  
  
  # PARETO CHART
  
  output$pareto_gender <- renderPlot({
    
    data_pareto <- data_filter() %>%
      count(Gender) %>%
      arrange(desc(n)) %>%
      mutate(
        Persen = n / sum(n) * 100,
        Kumulatif = cumsum(Persen),
        Kumulatif_Skala =
          Kumulatif * max(n) / 100
      )
    
    ggplot(
      data_pareto,
      aes(x = reorder(Gender, -n))
    ) +
      geom_col(
        aes(y = n),
        fill = "skyblue",
        color = "black"
      ) +
      geom_line(
        aes(
          y = Kumulatif_Skala,
          group = 1
        ),
        color = "red",
        linewidth = 1
      ) +
      geom_point(
        aes(y = Kumulatif_Skala),
        color = "red",
        size = 3
      ) +
      scale_y_continuous(
        name = "Frekuensi",
        sec.axis = sec_axis(
          ~ . * 100 / max(data_pareto$n),
          name = "Persentase Kumulatif (%)"
        )
      ) +
      labs(
        title = "Pareto Chart Gender",
        x = "Gender"
      ) +
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # VISUALISASI BIVARIAT
  # ==========================================================
  
  # Grafik 1: Age dan Weight
  
  output$bivariat1 <- renderPlot({
    
    ggplot(
      data_filter(),
      aes(x = Age, y = Weight)
    ) +
      geom_point(
        color = "steelblue",
        alpha = 0.7
      ) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "red"
      ) +
      labs(
        title = "Hubungan Age dengan Weight",
        x = "Age",
        y = "Weight"
      ) +
      theme_minimal()
    
  })
  
  
  # Grafik 2: FAF dan Weight
  
  output$bivariat2 <- renderPlot({
    
    ggplot(
      data_filter(),
      aes(x = factor(FAF), y = Weight)
    ) +
      geom_boxplot(
        fill = "orange"
      ) +
      labs(
        title = "Weight Berdasarkan FAF",
        x = "FAF",
        y = "Weight"
      ) +
      theme_minimal()
    
  })
  
  
  # Grafik 3: Gender dan Weight
  
  output$bivariat3 <- renderPlot({
    
    ggplot(
      data_filter(),
      aes(x = Gender, y = Weight, fill = Gender)
    ) +
      geom_boxplot() +
      labs(
        title = "Weight Berdasarkan Gender",
        x = "Gender",
        y = "Weight"
      ) +
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # VISUALISASI MULTIVARIAT
  # ==========================================================
  
  # Grafik 1: Age, Weight, Gender
  
  output$multivariat1 <- renderPlot({
    
    ggplot(
      data_filter(),
      aes(
        x = Age,
        y = Weight,
        color = Gender
      )
    ) +
      geom_point(
        alpha = 0.7
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE
      ) +
      labs(
        title = "Age dan Weight Berdasarkan Gender",
        x = "Age",
        y = "Weight",
        color = "Gender"
      ) +
      theme_minimal()
    
  })
  
  
  # Grafik 2: Weight, FAF, FCVC
  
  output$multivariat2 <- renderPlot({
    
    ggplot(
      data_filter(),
      aes(
        x = factor(FAF),
        y = Weight,
        fill = factor(FCVC)
      )
    ) +
      geom_boxplot(
        position = position_dodge(width = 0.8)
      ) +
      labs(
        title = "Weight Berdasarkan FAF dan FCVC",
        x = "FAF",
        y = "Weight",
        fill = "FCVC"
      ) +
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # REGRESI LINEAR BERGANDA
  # ==========================================================
  
  output$persamaan <- renderPrint({
    
    print(model)
    
  })
  
  
  output$koefisien <- renderTable({
    
    koef <- as.data.frame(
      hasil_model$coefficients
    )
    
    koef$Variabel <- rownames(koef)
    
    rownames(koef) <- NULL
    
    names(koef) <- c(
      "Koefisien",
      "Standar_Error",
      "t_hitung",
      "P_value",
      "Variabel"
    )
    
    koef[, c(
      "Variabel",
      "Koefisien",
      "Standar_Error",
      "t_hitung",
      "P_value"
    )]
    
  }, digits = 4)
  
  
  output$evaluasi <- renderTable({
    
    data.frame(
      Ukuran = c(
        "R-Squared",
        "Adjusted R-Squared",
        "RMSE",
        "F-Statistic",
        "P-Value Uji Simultan"
      ),
      Nilai = c(
        hasil_model$r.squared,
        hasil_model$adj.r.squared,
        RMSE,
        unname(nilai_f[1]),
        p_value_f
      )
    )
    
  }, digits = 4)
  
  
  # Grafik aktual dan prediksi
  
  output$aktual_prediksi <- renderPlot({
    
    data_prediksi <- UTS_EVDM %>%
      mutate(
        Prediksi = predict(model, newdata = UTS_EVDM)
      )
    
    ggplot(
      data_prediksi,
      aes(x = Weight, y = Prediksi)
    ) +
      geom_point(
        color = "steelblue",
        alpha = 0.7
      ) +
      geom_abline(
        intercept = 0,
        slope = 1,
        color = "red",
        linetype = "dashed"
      ) +
      labs(
        title = "Weight Aktual dan Prediksi",
        x = "Weight Aktual",
        y = "Weight Prediksi"
      ) +
      theme_minimal()
    
  })
  
  
  # Grafik residual
  
  output$grafik_residual <- renderPlot({
    
    data_residual <- data.frame(
      Prediksi = fitted(model),
      Residual = residuals(model)
    )
    
    ggplot(
      data_residual,
      aes(x = Prediksi, y = Residual)
    ) +
      geom_point(
        color = "steelblue",
        alpha = 0.7
      ) +
      geom_hline(
        yintercept = 0,
        color = "red",
        linetype = "dashed"
      ) +
      labs(
        title = "Residual terhadap Nilai Prediksi",
        x = "Nilai Prediksi",
        y = "Residual"
      ) +
      theme_minimal()
    
  })
  
}


# ============================================================
# 6. MENJALANKAN DASHBOARD
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
