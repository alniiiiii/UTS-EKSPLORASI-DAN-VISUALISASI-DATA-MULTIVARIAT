# ============================================================
# DASHBOARD UTS EKSPLORASI DAN VISUALISASI DATA MULTIVARIAT
# ============================================================

# 1. PACKAGE
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)

# ============================================================
# 2. MEMBACA DAN MENYIAPKAN DATA
# ============================================================

UTS_EVDM <- read_excel("UTS_EVDM.xlsx")

# Menggunakan 498 baris pertama sesuai pengolahan sebelumnya
UTS_EVDM <- UTS_EVDM[1:498, ]

# Menyesuaikan nama variabel
names(UTS_EVDM) <- c("Weight", "Age", "FAF", "FCVC", "Gender")

# Mengubah variabel numerik menjadi numeric
UTS_EVDM <- UTS_EVDM %>%
  mutate(
    Weight = as.numeric(as.character(Weight)),
    Age = as.numeric(as.character(Age)),
    FAF = as.numeric(as.character(FAF)),
    FCVC = as.numeric(as.character(FCVC)),
    Gender = as.factor(Gender)
  ) %>%
  na.omit()

# Menentukan kelompok variabel
variabel_numerik <- c("Weight", "Age", "FAF", "FCVC")
variabel_kategorik <- c("Gender")

# ============================================================
# 3. TAMPILAN DASHBOARD
# ============================================================

ui <- fluidPage(
  
  titlePanel("Dashboard Eksplorasi dan Visualisasi Data Multivariat"),
  
  tabsetPanel(
    
    # --------------------------------------------------------
    # A. LATAR BELAKANG
    # --------------------------------------------------------
    
    tabPanel(
      "Latar Belakang",
      
      h3("Latar Belakang"),
      
      p("Obesitas merupakan salah satu permasalahan kesehatan
        yang dapat dipengaruhi oleh berbagai faktor, seperti
        karakteristik individu dan pola aktivitas fisik."),
      
      p("Eksplorasi data dilakukan untuk mengetahui karakteristik
        data serta hubungan antarvariabel yang terdapat dalam
        dataset UTS_EVDM."),
      
      h4("Tujuan Analisis"),
      
      tags$ul(
        tags$li("Mengetahui karakteristik setiap variabel."),
        tags$li("Melakukan analisis univariat, bivariat, dan multivariat."),
        tags$li("Mengetahui hubungan antara variabel numerik dan kategorik."),
        tags$li("Membangun model regresi linear berganda.")
      )
    ),
    
    # --------------------------------------------------------
    # B. DATA DAN VARIABEL
    # --------------------------------------------------------
    
    tabPanel(
      "Data dan Variabel",
      
      h3("Deskripsi Data"),
      
      p("Data yang digunakan adalah dataset UTS_EVDM.xlsx."),
      
      verbatimTextOutput("info_data"),
      
      h4("Keterangan Variabel"),
      
      tableOutput("tabel_variabel")
    ),
    
    # --------------------------------------------------------
    # C. RINGKASAN
    # --------------------------------------------------------
    
    tabPanel(
      "Ringkasan",
      
      h3("Ringkasan Data"),
      
      p("Ringkasan statistik deskriptif untuk variabel numerik."),
      
      tableOutput("ringkasan_numerik"),
      
      h4("Distribusi Variabel Gender"),
      
      tableOutput("ringkasan_gender")
    ),
    
    # --------------------------------------------------------
    # D. UNIVARIAT
    # --------------------------------------------------------
    
    tabPanel(
      "Univariat",
      
      h3("Analisis Univariat"),
      
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput(
            "var_univariat",
            "Pilih Variabel:",
            choices = c(variabel_numerik, variabel_kategorik)
          ),
          
          # Pilihan visualisasi (berubah sesuai jenis variabel)
          uiOutput("pilih_visualisasi")
          
        ),
        
        mainPanel(
          
          h4("Visualisasi Distribusi"),
          
          plotOutput("plot_univariat"),
          
          h4("Ringkasan Variabel"),
          
          tableOutput("hasil_univariat")
          
        )
      )
    ),
    
    # --------------------------------------------------------
    # E. BIVARIAT
    # --------------------------------------------------------
    
    tabPanel(
      "Bivariat",
      
      h3("Analisis Bivariat"),
      
      p("Pilih jenis hubungan variabel yang ingin dianalisis."),
      
      tabsetPanel(
        
        # KATEGORIK VS KATEGORIK
        tabPanel(
          "Kategorik vs Kategorik",
          
          h4("Analisis Kategorik vs Kategorik"),
          
          p("Analisis ini menggunakan tabel silang dan diagram
            batang untuk melihat hubungan dua variabel kategorik."),
          
          selectInput(
            "kat1",
            "Pilih Variabel Kategorik Pertama:",
            choices = variabel_kategorik
          ),
          
          selectInput(
            "kat2",
            "Pilih Variabel Kategorik Kedua:",
            choices = variabel_kategorik
          ),
          
          uiOutput("pesan_kategorik"),
          
          tableOutput("tabel_silang"),
          
          plotOutput("plot_kat_kat")
        ),
        
        # KATEGORIK VS NUMERIK
        tabPanel(
          "Kategorik vs Numerik",
          
          h4("Analisis Kategorik vs Numerik"),
          
          p("Analisis ini menggunakan boxplot untuk membandingkan
            distribusi variabel numerik berdasarkan kelompok kategorik."),
          
          selectInput(
            "var_kat",
            "Pilih Variabel Kategorik:",
            choices = variabel_kategorik
          ),
          
          selectInput(
            "var_num",
            "Pilih Variabel Numerik:",
            choices = variabel_numerik
          ),
          
          plotOutput("plot_kat_num")
        ),
        
        # NUMERIK VS NUMERIK
        tabPanel(
          "Numerik vs Numerik",
          
          h4("Analisis Numerik vs Numerik"),
          
          p("Analisis ini menggunakan scatterplot dan korelasi
            untuk melihat hubungan dua variabel numerik."),
          
          selectInput(
            "num1",
            "Pilih Variabel Numerik Pertama:",
            choices = variabel_numerik,
            selected = "Age"
          ),
          
          selectInput(
            "num2",
            "Pilih Variabel Numerik Kedua:",
            choices = variabel_numerik,
            selected = "Weight"
          ),
          
          plotOutput("plot_num_num"),
          
          h4("Nilai Korelasi"),
          
          verbatimTextOutput("hasil_korelasi")
        )
      )
    ),
    
    # --------------------------------------------------------
    # F. MULTIVARIAT
    # --------------------------------------------------------
    
    tabPanel(
      "Multivariat",
      
      h3("Analisis Multivariat"),
      
      p("Analisis multivariat digunakan untuk melihat pola
        hubungan beberapa variabel secara bersamaan."),
      
      h4("Scatterplot Matrix"),
      
      plotOutput("plot_matrix", height = "600px"),
      
      h4("Matriks Korelasi"),
      
      plotOutput("plot_heatmap")
    ),
    
    # --------------------------------------------------------
    # G. REGRESI
    # --------------------------------------------------------
    
    tabPanel(
      "Regresi",
      
      h3("Regresi Linear Berganda"),
      
      p("Model regresi digunakan untuk menganalisis hubungan
        variabel Age, FAF, FCVC, dan Gender terhadap Weight."),
      
      h4("Persamaan Model"),
      
      verbatimTextOutput("persamaan_model"),
      
      h4("Hasil Regresi"),
      
      verbatimTextOutput("hasil_regresi"),
      
      h4("Visualisasi Nilai Aktual dan Prediksi"),
      
      plotOutput("plot_prediksi"),
      
      h4("Plot Residual"),
      
      plotOutput("plot_residual")
    ),
    
    # --------------------------------------------------------
    # H. KESIMPULAN
    # --------------------------------------------------------
    
    tabPanel(
      "Kesimpulan",
      
      h3("Kesimpulan Analisis"),
      
      verbatimTextOutput("kesimpulan")
    ),
    
    # --------------------------------------------------------
    # I. DATA
    # --------------------------------------------------------
    
    tabPanel(
      "Data",
      
      h3("Tabel Data UTS_EVDM"),
      
      p("Tabel data dapat dicari, diurutkan, dan ditampilkan
        berdasarkan jumlah baris."),
      
      DTOutput("tabel_data")
    )
  )
)

# ============================================================
# 4. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # A. DATA DAN VARIABEL
  # ----------------------------------------------------------
  
  output$info_data <- renderPrint({
    
    cat("Jumlah observasi :", nrow(UTS_EVDM), "\n")
    cat("Jumlah variabel  :", ncol(UTS_EVDM), "\n")
    cat("Jumlah variabel numerik :", length(variabel_numerik), "\n")
    cat("Jumlah variabel kategorik :", length(variabel_kategorik), "\n")
    
  })
  
  output$tabel_variabel <- renderTable({
    
    data.frame(
      Variabel = c("Weight", "Age", "FAF", "FCVC", "Gender"),
      
      Jenis = c(
        "Numerik",
        "Numerik",
        "Numerik",
        "Numerik",
        "Kategorik"
      ),
      
      Keterangan = c(
        "Berat badan",
        "Usia",
        "Frekuensi aktivitas fisik",
        "Frekuensi konsumsi sayuran",
        "Jenis kelamin"
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # B. RINGKASAN
  # ----------------------------------------------------------
  
  output$ringkasan_numerik <- renderTable({
    
    data.frame(
      Variabel = variabel_numerik,
      
      Mean = sapply(
        UTS_EVDM[variabel_numerik],
        mean,
        na.rm = TRUE
      ),
      
      Median = sapply(
        UTS_EVDM[variabel_numerik],
        median,
        na.rm = TRUE
      ),
      
      Standar_Deviasi = sapply(
        UTS_EVDM[variabel_numerik],
        sd,
        na.rm = TRUE
      ),
      
      Minimum = sapply(
        UTS_EVDM[variabel_numerik],
        min,
        na.rm = TRUE
      ),
      
      Maksimum = sapply(
        UTS_EVDM[variabel_numerik],
        max,
        na.rm = TRUE
      )
    )
    
  }, digits = 3)
  
  output$ringkasan_gender <- renderTable({
    
    as.data.frame(table(UTS_EVDM$Gender))
    
  })
  
  # ----------------------------------------------------------
  # C. UNIVARIAT (DENGAN PILIHAN VISUALISASI)
  # ----------------------------------------------------------
  
  # Pilihan visualisasi dinamis berdasarkan jenis variabel
  pilihan_visualisasi <- reactive({
    
    req(input$var_univariat)
    
    if (input$var_univariat %in% variabel_numerik) {
      c("Histogram", "Boxplot", "Density Plot", "Histogram + Density")
    } else {
      c("Diagram Batang", "Diagram Lingkaran")
    }
    
  })
  
  output$pilih_visualisasi <- renderUI({
    
    selectInput(
      "jenis_plot",
      "Pilih Visualisasi:",
      choices = pilihan_visualisasi()
    )
    
  })
  
  output$plot_univariat <- renderPlot({
    
    variabel <- input$var_univariat
    req(variabel, input$jenis_plot)
    
    # Pastikan jenis plot sesuai dengan variabel yang dipilih
    req(input$jenis_plot %in% pilihan_visualisasi())
    
    jenis <- input$jenis_plot
    
    if (variabel %in% variabel_numerik) {
      
      if (jenis == "Histogram") {
        
        ggplot(UTS_EVDM, aes(x = .data[[variabel]])) +
          geom_histogram(bins = 20, fill = "skyblue", color = "black") +
          labs(title = paste("Histogram", variabel),
               x = variabel, y = "Frekuensi") +
          theme_minimal()
        
      } else if (jenis == "Boxplot") {
        
        ggplot(UTS_EVDM, aes(y = .data[[variabel]])) +
          geom_boxplot(fill = "skyblue", color = "black") +
          labs(title = paste("Boxplot", variabel), y = variabel) +
          theme_minimal() +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
        
      } else if (jenis == "Density Plot") {
        
        ggplot(UTS_EVDM, aes(x = .data[[variabel]])) +
          geom_density(fill = "skyblue", alpha = 0.6, color = "black") +
          labs(title = paste("Density Plot", variabel),
               x = variabel, y = "Kepadatan") +
          theme_minimal()
        
      } else if (jenis == "Histogram + Density") {
        
        ggplot(UTS_EVDM, aes(x = .data[[variabel]])) +
          geom_histogram(aes(y = after_stat(density)),
                         bins = 20, fill = "skyblue", color = "black") +
          geom_density(color = "red", linewidth = 1) +
          labs(title = paste("Histogram dan Density", variabel),
               x = variabel, y = "Kepadatan") +
          theme_minimal()
      }
      
    } else {
      
      if (jenis == "Diagram Batang") {
        
        ggplot(UTS_EVDM, aes(x = .data[[variabel]])) +
          geom_bar(fill = "steelblue", color = "black") +
          labs(title = paste("Diagram Batang", variabel),
               x = variabel, y = "Frekuensi") +
          theme_minimal()
        
      } else if (jenis == "Diagram Lingkaran") {
        
        data_pie <- as.data.frame(table(UTS_EVDM[[variabel]]))
        names(data_pie) <- c("Kategori", "Frekuensi")
        data_pie$Persen <- round(data_pie$Frekuensi / sum(data_pie$Frekuensi) * 100, 1)
        
        ggplot(data_pie, aes(x = "", y = Frekuensi, fill = Kategori)) +
          geom_col(width = 1, color = "white") +
          coord_polar(theta = "y") +
          geom_text(aes(label = paste0(Persen, "%")),
                    position = position_stack(vjust = 0.5),
                    size = 5) +
          labs(title = paste("Diagram Lingkaran", variabel),
               fill = variabel) +
          theme_void()
      }
    }
    
  })
  
  output$hasil_univariat <- renderTable({
    
    variabel <- input$var_univariat
    
    if (variabel %in% variabel_numerik) {
      
      data.frame(
        Mean = mean(UTS_EVDM[[variabel]], na.rm = TRUE),
        Median = median(UTS_EVDM[[variabel]], na.rm = TRUE),
        SD = sd(UTS_EVDM[[variabel]], na.rm = TRUE),
        Minimum = min(UTS_EVDM[[variabel]], na.rm = TRUE),
        Maksimum = max(UTS_EVDM[[variabel]], na.rm = TRUE)
      )
      
    } else {
      
      as.data.frame(table(UTS_EVDM[[variabel]]))
      
    }
    
  }, digits = 3)
  
  # ----------------------------------------------------------
  # D. BIVARIAT: KATEGORIK VS KATEGORIK
  # ----------------------------------------------------------
  
  output$pesan_kategorik <- renderUI({
    
    if (input$kat1 == input$kat2) {
      
      p(
        "Pilih dua variabel kategorik yang berbeda.",
        style = "color:red;"
      )
      
    } else if (length(variabel_kategorik) < 2) {
      
      p(
        "Belum dapat dilakukan karena dataset saat ini hanya
        memiliki satu variabel kategorik, yaitu Gender.
        Tambahkan variabel kategorik kedua agar analisis ini
        dapat dijalankan.",
        style = "color:red;"
      )
      
    }
    
  })
  
  output$tabel_silang <- renderTable({
    
    req(
      length(variabel_kategorik) >= 2,
      input$kat1 != input$kat2
    )
    
    table(
      UTS_EVDM[[input$kat1]],
      UTS_EVDM[[input$kat2]]
    )
    
  })
  
  output$plot_kat_kat <- renderPlot({
    
    req(
      length(variabel_kategorik) >= 2,
      input$kat1 != input$kat2
    )
    
    ggplot(
      UTS_EVDM,
      aes(
        x = .data[[input$kat1]],
        fill = .data[[input$kat2]]
      )
    ) +
      geom_bar(position = "dodge") +
      labs(
        title = "Diagram Batang Kategorik vs Kategorik",
        x = input$kat1,
        fill = input$kat2,
        y = "Frekuensi"
      ) +
      theme_minimal()
    
  })
  
  # ----------------------------------------------------------
  # E. BIVARIAT: KATEGORIK VS NUMERIK
  # ----------------------------------------------------------
  
  output$plot_kat_num <- renderPlot({
    
    ggplot(
      UTS_EVDM,
      aes(
        x = .data[[input$var_kat]],
        y = .data[[input$var_num]],
        fill = .data[[input$var_kat]]
      )
    ) +
      geom_boxplot() +
      labs(
        title = paste(
          input$var_kat,
          "vs",
          input$var_num
        ),
        x = input$var_kat,
        y = input$var_num
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
  })
  
  # ----------------------------------------------------------
  # F. BIVARIAT: NUMERIK VS NUMERIK
  # ----------------------------------------------------------
  
  output$plot_num_num <- renderPlot({
    
    ggplot(
      UTS_EVDM,
      aes(
        x = .data[[input$num1]],
        y = .data[[input$num2]]
      )
    ) +
      geom_point(
        color = "steelblue",
        size = 2,
        alpha = 0.7
      ) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "red"
      ) +
      labs(
        title = paste(input$num1, "vs", input$num2),
        x = input$num1,
        y = input$num2
      ) +
      theme_minimal()
    
  })
  
  output$hasil_korelasi <- renderPrint({
    
    req(input$num1 != input$num2)
    
    nilai <- cor(
      UTS_EVDM[[input$num1]],
      UTS_EVDM[[input$num2]],
      use = "complete.obs"
    )
    
    cat("Variabel pertama :", input$num1, "\n")
    cat("Variabel kedua   :", input$num2, "\n")
    cat("Koefisien korelasi :", round(nilai, 4), "\n")
    
    if (nilai > 0) {
      cat("Hubungan: Positif")
    } else if (nilai < 0) {
      cat("Hubungan: Negatif")
    } else {
      cat("Hubungan: Tidak ada korelasi linear")
    }
    
  })
  
  # ----------------------------------------------------------
  # G. MULTIVARIAT
  # ----------------------------------------------------------
  
  output$plot_matrix <- renderPlot({
    
    pairs(
      UTS_EVDM[variabel_numerik],
      main = "Scatterplot Matrix Variabel Numerik",
      pch = 19,
      col = "steelblue"
    )
    
  })
  
  output$plot_heatmap <- renderPlot({
    
    matriks <- cor(
      UTS_EVDM[variabel_numerik],
      use = "complete.obs"
    )
    
    data_korelasi <- as.data.frame(
      as.table(matriks)
    )
    
    ggplot(
      data_korelasi,
      aes(
        x = Var1,
        y = Var2,
        fill = Freq
      )
    ) +
      geom_tile(color = "white") +
      geom_text(
        aes(label = round(Freq, 2)),
        size = 5
      ) +
      scale_fill_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = 0
      ) +
      labs(
        title = "Heatmap Korelasi",
        x = "",
        y = "",
        fill = "Korelasi"
      ) +
      theme_minimal()
    
  })
  
  # ----------------------------------------------------------
  # H. REGRESI LINEAR BERGANDA
  # ----------------------------------------------------------
  
  model_regresi <- reactive({
    
    lm(
      Weight ~ Age + FAF + FCVC + Gender,
      data = UTS_EVDM
    )
    
  })
  
  output$persamaan_model <- renderPrint({
    
    cat("Weight = β0 + β1(Age) + β2(FAF) + β3(FCVC) + β4(Gender)")
    
  })
  
  output$hasil_regresi <- renderPrint({
    
    summary(model_regresi())
    
  })
  
  output$plot_prediksi <- renderPlot({
    
    data_prediksi <- data.frame(
      Aktual = UTS_EVDM$Weight,
      Prediksi = fitted(model_regresi())
    )
    
    ggplot(
      data_prediksi,
      aes(x = Aktual, y = Prediksi)
    ) +
      geom_point(
        color = "steelblue",
        size = 2
      ) +
      geom_abline(
        intercept = 0,
        slope = 1,
        color = "red"
      ) +
      labs(
        title = "Nilai Aktual vs Nilai Prediksi",
        x = "Weight Aktual",
        y = "Weight Prediksi"
      ) +
      theme_minimal()
    
  })
  
  output$plot_residual <- renderPlot({
    
    data_residual <- data.frame(
      Prediksi = fitted(model_regresi()),
      Residual = residuals(model_regresi())
    )
    
    ggplot(
      data_residual,
      aes(x = Prediksi, y = Residual)
    ) +
      geom_point(
        color = "steelblue"
      ) +
      geom_hline(
        yintercept = 0,
        color = "red"
      ) +
      labs(
        title = "Plot Residual",
        x = "Nilai Prediksi",
        y = "Residual"
      ) +
      theme_minimal()
    
  })
  
  # ----------------------------------------------------------
  # I. KESIMPULAN
  # ----------------------------------------------------------
  
  output$kesimpulan <- renderPrint({
    
    model <- model_regresi()
    hasil <- summary(model)
    
    cat("KESIMPULAN ANALISIS DATA\n\n")
    
    cat(
      "Jumlah observasi yang dianalisis:",
      nrow(UTS_EVDM),
      "\n\n"
    )
    
    cat(
      "Variabel respons: Weight\n",
      "Variabel prediktor: Age, FAF, FCVC, dan Gender\n\n"
    )
    
    cat(
      "Nilai R-squared:",
      round(hasil$r.squared, 4),
      "\n"
    )
    
    cat(
      "Nilai Adjusted R-squared:",
      round(hasil$adj.r.squared, 4),
      "\n\n"
    )
    
    cat(
      "Nilai R-squared menunjukkan proporsi variasi Weight
      yang dapat dijelaskan oleh variabel prediktor dalam model.\n\n"
    )
    
    cat(
      "Untuk mengetahui variabel yang berpengaruh secara
      statistik, perhatikan nilai p-value pada hasil regresi.
      Variabel dengan p-value < 0,05 dinyatakan signifikan
      pada taraf signifikansi 5%.\n"
    )
    
  })
  
  # ----------------------------------------------------------
  # J. DATA
  # ----------------------------------------------------------
  
  output$tabel_data <- renderDT({
    
    datatable(
      UTS_EVDM,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      filter = "top"
    )
    
  })
  
}

# ============================================================
# 5. MENJALANKAN DASHBOARD
# ============================================================

shinyApp(ui = ui, server = server)