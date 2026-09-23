library(shiny)
library(markdown)

## ---- Helpers ------------------------------------------------------------------

# Mode of a Beta(a, b). NA when there is no unique mode
# (uniform when a = b = 1, U-shaped when both are below 1).
betaMode = function(a, b) {
  if (a > 1 && b > 1) (a - 1) / (a + b - 2)
  else if (a <= 1 && b > 1) 0
  else if (a > 1 && b <= 1) 1
  else NA_real_
}

# Summary statistics for a Beta distribution given as list(alpha, beta)
betaSummary = function(d) {
  a = d$alpha
  b = d$beta
  list(
    alpha = a,
    beta  = b,
    mean  = a / (a + b),
    mode  = betaMode(a, b),
    q95   = qbeta(0.95, a, b),
    lower = qbeta(0.025, a, b),
    upper = qbeta(0.975, a, b)
  )
}

# Find alpha = m * p, beta = m * (1 - p) so that the 0.95 quantile equals q95.
# The concentration m is searched on the log scale over a wide range: first a
# grid scan to bracket a root, then uniroot() to pin it down. If the target
# can't be hit exactly (possible when p is small), the closest grid point is
# returned and `exact` is FALSE.
solveBeta = function(p, q95) {
  logm = seq(log(1e-2), log(1e6), length.out = 300)
  q95At = function(lm) suppressWarnings(qbeta(0.95, exp(lm) * p, exp(lm) * (1 - p)))
  
  f = q95At(logm) - q95
  ok = is.finite(f)
  k = length(f)
  crossings = which(ok[-1] & ok[-k] & sign(f[-1]) != sign(f[-k]))
  
  if (length(crossings) > 0) {
    # If there is more than one solution, prefer the most concentrated prior
    i = max(crossings)
    lm = uniroot(function(lm) q95At(lm) - q95,
                 lower = logm[i], upper = logm[i + 1], tol = 1e-10)$root
  } else {
    lm = logm[which.min(ifelse(ok, abs(f), Inf))]
  }
  
  m = exp(lm)
  alpha = m * p
  beta = m * (1 - p)
  achieved = qbeta(0.95, alpha, beta)
  list(alpha = alpha, beta = beta, q95 = achieved, exact = abs(achieved - q95) < 1e-4)
}

fmt = function(v) ifelse(is.na(v), "\u2013", formatC(v, digits = 4, format = "g"))

aboutContent = if (file.exists("README.md")) {
  includeMarkdown("README.md")
} else {
  p("No README.md was found next to the app, so there's nothing to show here yet.")
}

## ---- UI -----------------------------------------------------------------------

ui = fluidPage(
  withMathJax(),
  titlePanel("Beta Distribution Optimization"),
  tabsetPanel(id = "mytabs", selected = "Main",
              tabPanel("About",
                       fluidRow(column(10, br(), aboutContent))
              ),
              tabPanel("Main",
                       br(),
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons("selection", "Choose whether you want to set the:",
                                        choiceNames = list("mean (p) and the 0.95 quantile (q95)",
                                                           "parameters (\\( \\alpha \\) and \\( \\beta \\)) of the Beta distribution"),
                                        choiceValues = c("mean", "params"),
                                        selected = "mean"),
                           conditionalPanel(condition = "input.selection == 'mean'",
                                            sliderInput("p_value", "Prior mean, p:", min = 0.01, max = 0.99, value = 0.5, step = 0.01),
                                            sliderInput("q95_value", "Prior 0.95 quantile, q95:", min = 0.01, max = 0.99, value = 0.95, step = 0.01),
                                            uiOutput("solverStatus")
                           ),
                           conditionalPanel(condition = "input.selection == 'params'",
                                            numericInput("alpha_value", label = "\\( \\alpha \\):", min = 0.001, value = 1, step = 0.1),
                                            numericInput("beta_value", label = "\\( \\beta \\):", min = 0.001, value = 1, step = 0.1)
                           ),
                           hr(),
                           h4("Data"),
                           numericInput("n_value", "Number of trials, n:", min = 0, value = 4, step = 1),
                           numericInput("x_value", "Number of successes, x:", min = 0, max = 4, value = 0, step = 1)
                         ),
                         mainPanel(
                           plotOutput("beta_dist_plot"),
                           br(),
                           tableOutput("summaryTable")
                         )
                       )
              )
  )
)

## ---- Server -------------------------------------------------------------------

server = function(input, output, session) {
  
  # Prior specified through the mean and 0.95 quantile (solved automatically)
  meanPrior = reactive({
    p = input$p_value
    q95 = input$q95_value
    validate(need(q95 > p, "The 0.95 quantile (q95) must be greater than the mean (p)."))
    solveBeta(p, q95)
  })
  
  # Prior specified directly through alpha and beta
  paramPrior = reactive({
    a = input$alpha_value
    b = input$beta_value
    validate(
      need(is.numeric(a) && !is.na(a) && a > 0, "alpha must be a positive number."),
      need(is.numeric(b) && !is.na(b) && b > 0, "beta must be a positive number.")
    )
    list(alpha = a, beta = b)
  })
  
  prior = reactive({
    if (input$selection == "mean") meanPrior() else paramPrior()
  })
  
  observed = reactive({
    n = input$n_value
    x = input$x_value
    validate(
      need(is.numeric(n) && !is.na(n) && n >= 0 && n == round(n), "n must be a whole number, 0 or more."),
      need(is.numeric(x) && !is.na(x) && x >= 0 && x == round(x), "x must be a whole number, 0 or more."),
      need(x <= n, "x can't be larger than n.")
    )
    list(x = x, n = n)
  })
  
  posterior = reactive({
    pr = prior()
    d = observed()
    list(alpha = pr$alpha + d$x, beta = pr$beta + d$n - d$x)
  })
  
  # Keep x within [0, n] when n changes
  observeEvent(input$n_value, {
    n = input$n_value
    req(is.numeric(n), !is.na(n), n >= 0)
    x = input$x_value
    if (!is.na(x) && x > n) {
      updateNumericInput(session, "x_value", value = n, max = n)
    } else {
      updateNumericInput(session, "x_value", max = n)
    }
  })
  
  # When switching modes, carry the current prior across so the plot doesn't jump
  observeEvent(input$selection, {
    if (input$selection == "params") {
      fit = tryCatch(meanPrior(), error = function(e) NULL)
      if (!is.null(fit)) {
        updateNumericInput(session, "alpha_value", value = signif(fit$alpha, 4))
        updateNumericInput(session, "beta_value", value = signif(fit$beta, 4))
      }
    } else {
      pr = tryCatch(paramPrior(), error = function(e) NULL)
      if (!is.null(pr)) {
        s = betaSummary(pr)
        updateSliderInput(session, "p_value", value = round(s$mean, 2))
        updateSliderInput(session, "q95_value", value = round(s$q95, 2))
      }
    }
  }, ignoreInit = TRUE)
  
  output$solverStatus = renderUI({
    fit = meanPrior()
    if (fit$exact) {
      msg = sprintf("Matching prior: \\( \\alpha = %s \\), \\( \\beta = %s \\).",
                    fmt(fit$alpha), fmt(fit$beta))
      cls = "text-muted"
    } else {
      msg = sprintf(paste("No Beta prior with mean %s has a 0.95 quantile of %s.",
                          "The closest is \\( \\alpha = %s \\), \\( \\beta = %s \\), with q95 = %s."),
                    input$p_value, input$q95_value, fmt(fit$alpha), fmt(fit$beta), fmt(fit$q95))
      cls = "text-danger"
    }
    withMathJax(div(class = cls, HTML(msg)))
  })
  
  output$beta_dist_plot = renderPlot({
    pr = prior()
    po = posterior()
    d = observed()
    
    # Evenly spaced grid plus points placed by quantile, so very narrow
    # distributions are still drawn smoothly
    probs = seq(0.001, 0.999, length.out = 300)
    xs = c(seq(0.001, 0.999, length.out = 1000),
           qbeta(probs, pr$alpha, pr$beta),
           qbeta(probs, po$alpha, po$beta))
    xs = sort(unique(xs[xs > 0 & xs < 1]))
    
    yPrior = dbeta(xs, pr$alpha, pr$beta)
    yPost = dbeta(xs, po$alpha, po$beta)
    
    # Set the y-axis from the interior only so a density that shoots off to
    # infinity at 0 or 1 (alpha or beta < 1) doesn't flatten everything else
    interior = xs >= 0.005 & xs <= 0.995
    yAll = c(yPrior[interior], yPost[interior])
    ymax = max(yAll[is.finite(yAll)]) * 1.05
    
    plot(xs, yPrior, type = "n", xlab = expression(theta), ylab = "Density",
         main = "Prior and posterior", xaxs = "i", yaxs = "i",
         xlim = c(0, 1), ylim = c(0, ymax), las = 1)
    
    ci = qbeta(c(0.025, 0.975), po$alpha, po$beta)
    inCI = xs >= ci[1] & xs <= ci[2]
    shade = adjustcolor("firebrick", alpha.f = 0.2)
    polygon(c(ci[1], xs[inCI], ci[2]), c(0, pmin(yPost[inCI], ymax), 0),
            col = shade, border = NA)
    
    lines(xs, yPrior, lwd = 2, col = "grey30")
    lines(xs, yPost, lwd = 2, col = "firebrick")
    if (d$n > 0) abline(v = d$x / d$n, lty = 2, col = "steelblue")
    
    legend("topright", bty = "n",
           legend = c("Prior", "Posterior", "95% credible interval", "Observed x / n"),
           col = c("grey30", "firebrick", shade, "steelblue"),
           lty = c(1, 1, NA, 2), lwd = c(2, 2, NA, 1),
           pch = c(NA, NA, 15, NA), pt.cex = 2)
  })
  
  output$summaryTable = renderTable({
    column = function(s) {
      c(fmt(s$alpha), fmt(s$beta), fmt(s$mean), fmt(s$mode), fmt(s$q95),
        paste0("(", fmt(s$lower), ", ", fmt(s$upper), ")"))
    }
    data.frame(
      Quantity = c("\u03b1", "\u03b2", "Mean", "Mode", "0.95 quantile", "95% credible interval"),
      Prior = column(betaSummary(prior())),
      Posterior = column(betaSummary(posterior())),
      check.names = FALSE
    )
  }, striped = TRUE, align = "lrr")
}

shinyApp(ui = ui, server = server)