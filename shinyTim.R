library(shiny)

p0 = 0.5
q95init = 0.95

ui = fluidPage(
  titlePanel("Beta Distribution Optimization"),
  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      radioButtons("selection", "Choose whether you want to set the:",
                   choiceNames = c("mean (p) and the 0.95 quantile (q95)",
                                   "parameters (\\( \\alpha \\) and \\( \\beta \\)) of the Beta distribution"),
                   choiceValues = c("mean", "params"),
                   selected = "mean"),
      conditionalPanel(condition = "input.selection == 'mean'",
        sliderInput("p_value", "p:", min = 0, max = 1, value = 0.5, step = 0.01),
        sliderInput("q95_value", "q95:", min = 0, max = 1, value = 0.95, step = 0.01),
      ),
      conditionalPanel(condition = "input.selection == 'params'", 
        numericInput("alpha_value", label = "\\( \\alpha \\):", min = 0, max = 100, value = 1, step = 0.01),
        numericInput("beta_value", label = "\\( \\beta \\):", min = 0, max = 100, value = 1, step = 0.01)
      ),
      br(),
      numericInput("x_value", "x:", min = 0, max = 100, value = 0, step = 1),
      numericInput("n_value", "n:", min = 1, max = 500, value = 4, step = 1),
      uiOutput(("James"))
    ),
    mainPanel(
      tableOutput("summaryTable"),
      br(),
      plotOutput("beta_dist_plot"),
      br(),
      conditionalPanel(condition = "input.selection == 'mean'",
        textOutput("optimizeHelp"),
        br(),
        actionButton("optimize_button", "Find \\( \\alpha \\) and \\( \\beta \\)")
      )
    )
  )
)

server = function(input, output, session) {
  optimized_values = reactiveValues(m = NULL)
  
  observeEvent(input$p_value,{
    p = input$p_value
    q95 = input$q95_value
    
    # insist that q95 is always greater than p
    if(q95 <= p){
      updateSliderInput(session = session, inputId = "q95_value", value = p + 0.05)
    }
  })
  
  observeEvent(input$q95_value,{
    p = input$p_value
    q95 = input$q95_value
    m = input$m_value
    alpha = p
    beta = (1 - p)
    
    # insist that q95 is always greater than p
    if(q95 <= p){
      updateSliderInput(session = session, inputId = "q95_value", value = p + 0.05)
    }
  })
  
  observeEvent(input$x_value,{
    x = input$x_value
    n = input$n_value
    
    # make sure x isn't greater than n
    if(x > n){
      updateNumericInput(session = session, inputId = "x_value", value = n)
    }
  })
  
  
  observeEvent(input$n_value,{
    x = input$x_value
    n = input$n_value
    
    # make sure n isn't less than x
    if(n < x){
      updateNumericInput(session = session, inputId = "n_value", value = x + 1)
    }
  })
  
  observeEvent(input$alpha_value, {
    alpha = input$alpha_value
    beta = input$beta_value
    m = input$m_value
    p = alpha / (alpha + beta)
    q95 = qbeta(0.95, alpha, beta)
    
    isolate({
      updateSliderInput(session = session, inputId = "p_value", value = p)
      updateSliderInput(session = session, inputId = "q95_value", value = q95)
    })
  })
  
  observeEvent(input$beta_value, {
    alpha = input$alpha_value
    beta = input$beta_value
    m = input$m_value
    p = alpha / (alpha + beta)
    q95 = qbeta(0.95, alpha, beta)
    
    
    isolate({
      updateSliderInput(session = session, inputId = "p_value", value = p)
      updateSliderInput(session = session, inputId = "q95_value", value = q95)
    })
  })
  
  observeEvent(input$optimize_button, {
    p = input$p_value
    q95 = input$q95_value
    m = ifelse(is.null(optimized_values$m), 1, optimized_values$m)
    
    # Constrain values between 0 and 1 (exclusive) for p and q95
    p = max(.Machine$double.eps, min(p, 1 - .Machine$double.eps))
    q95 = max(.Machine$double.eps, min(q95, 1 - .Machine$double.eps))
    m = max(.Machine$double.eps, min(m, 10))
    
    alpha = p
    beta =  (1 - p)
    
    objFun = function(m){
      rval = (qbeta(0.95, m * alpha, m * beta) - q95)^2
    }
    
    
    # Optimize with objective function and scaling factor
    optimized_params = optim(par = m, fn = objFun, method = "L-BFGS-B", lower = 0)
    
    optimized_values$m = optimized_params$par
 
    alpha = optimized_params$par * p
    beta = optimized_params$par * (1 - p)
    
    isolate({
      updateNumericInput(session = session, inputId = "alpha_value", value = alpha)
      updateNumericInput(session = session, inputId = "beta_value", value = beta)
    })
  })
  

  output$beta_dist_plot = renderPlot({
    alpha = input$alpha_value
    beta = input$beta_value
    
    x = seq(from = 0.01, to = .99, length = 100)
    y = dbeta(x, shape1 = alpha, shape2 = beta)
    
    n = input$n_value
    x_value = input$x_value
    alpha_post = alpha + x_value
    beta_post = beta + (n - x_value)
    y1 = dbeta(x, alpha_post, beta_post)
    
    ymax = max(y, y1) * 1.05
    
    plot(x, y, type = "l", xlab = "x", ylab = "Density", main = "Beta Distribution",
         xaxs = "i", yaxs = "i", ylim = c(0, ymax), xlim = c(0,1))
   
    lines(x, y1, col = "red")
    legend("topright", lty = 1, col = c("black", "red"), legend = c("Prior", "Posterior"), bty  = "n")
    
  })
  
  output$summaryTable = renderTable({
    tableData = data.frame(Parameter = c("alpha", "beta", 
                                         "Prior Mean", "Prior Mode", "Prior 0.95 Quantile",
                                          "Posterior Mean", "Posterior Mode", "Posterior 0.95 Quantile",
                                         "Posterior 95% Credible Interval (Lower)",
                                         "Posterior 95% Credible Interval (Upper)"))
    
    alpha = input$alpha_value
    beta = input$beta_value
    prior.mean = alpha / (alpha + beta)
    prior.mode = ifelse(alpha > 1 & beta > 1, (alpha - 1) / (alpha + beta - 2), NA)
    prior.q95 = qbeta(0.95, alpha, beta)
    n = input$n_value
    x = input$x_value
    post.mean = (alpha + x) / (n + alpha + beta)
    post.mode = ifelse((alpha + x) > 1 & (beta + (n - x)) > 1, (alpha + x - 1) / (n + alpha + beta - 2), NA)
    post.q95 = qbeta(0.95, alpha + x, beta + n - x)
    post.lb = qbeta(0.025, alpha + x, beta + n - x)
    post.ub = qbeta(0.975, alpha + x, beta + n - x)
    
    
    tableData$value = formatC(
      c(
      alpha, beta,
      prior.mean, prior.mode, prior.q95,
      post.mean, post.mode, post.q95,
      post.lb,
      post.ub
      ),
      format = "g"
    )
    
    return(tableData)
  })
  
  output$optimizeHelp <- renderText({
    # Generate text content based on user input or calculations
    message = paste0("Click this button to try and find the values of \\( \\alpha \\) and \\( \\beta) \\)\n",
    " so that your prior has a mean of ", input$p_value, " and a 0.95 quantile of ", input$q95_value)
    return(message)
  })
}

shinyApp(ui = ui, server = server)

