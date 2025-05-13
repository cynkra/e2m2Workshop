library(shiny)
library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(bslib)
library(tidyr)

# TODO: change to page_sidebar
ui <- page_fluid(
  # TODO: put all inputs in a sidebar

  # TODO: radioButton might not be the best widget ...
  radioButtons(
    "x_var",
    "X Variable:",
    choices = c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    ),
    selected = "bill_length_mm"
  ),

  # TODO: radioButton might not be the best widget ...
  radioButtons(
    "y_var",
    "Y Variable:",
    choices = c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    ),
    selected = "bill_depth_mm"
  ),

  # TODO: numericInput might not be the best widget ...
  numericInput("clusters", "Number of Clusters:", min = 2, max = 6, value = 3),

  # TODO: checkboxInput might not be the best widget ...
  checkboxInput("show_species", "Show Actual Species", TRUE),

  # TODO: put plot inside a card and on the right side
  plotOutput("cluster_plot")
)

server <- function(input, output, session) {
  # Filter out rows with NA values
  penguins_clean <- reactive({
    penguins %>%
      drop_na(!!sym(input$x_var), !!sym(input$y_var))
  })

  # Perform k-means clustering
  kmeans_model <- reactive({
    # Select the two variables to use for clustering
    data_for_clustering <- penguins_clean() %>%
      select(!!sym(input$x_var), !!sym(input$y_var))

    # Run k-means
    kmeans(data_for_clustering, centers = input$clusters)
  })

  # Add clusters to dataset
  clustered_data <- reactive({
    penguins_clean() %>%
      mutate(cluster = as.factor(kmeans_model()$cluster))
  })

  # Scatter plot with clusters
  output$cluster_plot <- renderPlot({
    data <- clustered_data()

    p <- ggplot(data, aes(x = !!sym(input$x_var), y = !!sym(input$y_var))) +
      geom_point(
        aes(color = cluster, shape = if (input$show_species) species else NULL),
        size = 3,
        alpha = 0.8
      ) +
      geom_point(
        data = as.data.frame(kmeans_model()$centers),
        aes(x = !!sym(input$x_var), y = !!sym(input$y_var)),
        color = "black",
        size = 5,
        shape = 8
      ) +
      theme_minimal(base_size = 14) +
      labs(
        title = "K-means Clustering Results",
        x = gsub("_", " ", toupper(input$x_var)),
        y = gsub("_", " ", toupper(input$y_var)),
        color = "Cluster"
      ) +
      theme(legend.position = "right")

    if (input$show_species) {
      p <- p + labs(shape = "Species")
    }

    p
  })
}

shinyApp(ui, server)
