library(shiny)
library(ggplot2)
library(dplyr)

ver <- read.table(
  "http://pbil.univ-lyon1.fr/R/donnees/VerhulstPF1847.txt",
  sep = "\t",
  header = TRUE
) |> mutate(obs = obs / 10^6, theo = theo / 10^6)

t0 <- ver$t[1]
N0 <- ver$obs[1]

# Model equations
logistic <- function(t, p) {
  with(as.list(p), {
    K / (1 + ((K - N0)/N0) * exp(-r*(t-t0)))
  })
}

# Minimize
sce <- function(p) {
  theo <- logistic(ver$t, p = setNames(p, c("r", "K")))
  sum((ver$obs - theo)^2)
}

tseq <- seq(from = 1800, to = 2000, by = 1)

lm_coef <- coef(lm(ver$obs ~ ver$t))

ui <- fluidPage(
  numericInput("r", "r param", 0.02, 0, 0.1, step = 0.001),
  numericInput("K", "K param", 5, 1, 10, step = 1),
  plotOutput("plot")
)

server <- function(input, output, session) {
  # Non ajusted part with parametrized guess
  res <- reactive({
    data.frame(
      t = tseq,
      y = logistic(tseq, p = c(r = input$r, K = input$K))
    )
  })

  fit <- reactive({
    # Fit: K = 5 means 5 millions people max (*10^6)
    nlmfit <- nlm(f = sce, p = c(input$r, input$K))
    data.frame(
      t = tseq,
      y = logistic(tseq, p = setNames(nlmfit$estimate, c("r", "K")))
    )
  })

  output$plot <- renderPlot({
    ggplot(data = res()) +
      geom_line(mapping = aes(x = t, y = y)) +
      geom_line(data = fit(), mapping = aes(x = t, y = y), color = "maroon") +
      geom_point(
        data = ver,
        mapping = aes(x = t, y = obs), color = "lightblue"
      ) +
      geom_abline(
        slope = lm_coef[2],
        intercept = lm_coef[1],
        color = "grey"
      ) +
      labs(
        x = "Year",
        y = "Population in million",
        title = "Population growth in belgium"
      )
  })
}

shinyApp(ui, server)
