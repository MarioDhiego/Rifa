
library(shiny)
library(shinydashboard)
library(qrencoder)
library(png)
library(DT)

# Definindo o UI
ui <- dashboardPage(
  dashboardHeader(title = "RIFA SOLIDÁRIA"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "Rifa Online",
        status = "primary",
        solidHeader = TRUE,
        width = 3,
        numericInput(
          "numero_escolhido",
          "Número:",
          value = 1,
          min = 1,
          max = 100
        ),
        actionButton("comprar", "Comprar")
      ),
      box(
        title = "QRCode",
        status = "primary",
        solidHeader = TRUE,
        width = 2,
        imageOutput("qrcode_pix", 
                    width = "50px", 
                    height = "50px")
      ),
      box(
        title = "Chave PIX",
        status = "info",
        solidHeader = TRUE,
        width = 3,
        h3("Author: Mário Diego"),
        h4("Pix: 91 980607471")
      )
    ),
    fluidRow(
      box(
        title = "Números Disponíveis",
        status = "info",
        solidHeader = TRUE,
        width = 8,
        DTOutput("numeros_disponiveis")
      )
    )
  )
)

# Definindo o Server
server <- function(input, output) {
  # Variável para armazenar os números vendidos
  numeros_vendidos <- reactiveVal(integer(0))
  
  # Criar a matriz de números com os grupos B, I, N, G, O
  numeros_bingo <- list(
    B = 1:20,
    I = 21:45,
    N = 46:65,
    G = 66:85,
    O = 86:100
  )
  
  # Função para criar a tabela com "X" para os números selecionados
  renderTableWithX <- function(numeros_selecionados) {
    numeros_table <- matrix("", nrow = 5, ncol = 15, 
                            dimnames = list(c("B", "I", "N", "G", "O"), NULL))
    for (grupo in names(numeros_bingo)) {
      for (i in 1:15) {
        num <- numeros_bingo[[grupo]][i]
        if (num %in% numeros_selecionados) {
          numeros_table[grupo, i] <- "X"
        } else {
          numeros_table[grupo, i] <- num
        }
      }
    }
    return(numeros_table)
  }
  
  # Renderizar a tabela de números disponíveis
  output$numeros_disponiveis <- renderDT({
    numeros_table <- renderTableWithX(numeros_vendidos())
    datatable(
      t(numeros_table),  # Transpose the matrix for proper display
      escape = FALSE,
      selection = "none",
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE
      )
    )
  })
  
  # Ao clicar no botão "Comprar"
  observeEvent(input$comprar, {
    numeros_vendidos(c(numeros_vendidos(), input$numero_escolhido))
  })
  
  # Renderizar o QR Code PIX
  output$qrcode_pix <- renderImage({
    qr_code <- qrencode("91 980607471")
    qr_code_path <- tempfile(fileext = ".png")
    writePNG(as.array(qr_code), qr_code_path)
    list(src = qr_code_path, contentType = "image/png")
  }, deleteFile = TRUE)
}

# Rodar o aplicativo Shiny
shinyApp(ui, server)
