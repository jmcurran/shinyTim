library(shiny)
library(markdown)

## ---- Formatting -----------------------------------------------------------------

fmt = function(v) ifelse(is.na(v), "\u2013", formatC(v, digits = 4, format = "g"))

# Express a probability as "1 in N" (for p < 0.5)
oneIn1 = function(p) {
  if (is.na(p)) return("\u2013")
  if (p <= 0) return("0")
  if (p >= 0.5) return(fmt(p))
  N = signif(1 / p, 3)
  if (N >= 1e9) return(paste0("1 in ", formatC(N, digits = 3, format = "g")))
  paste0("1 in ", format(N, big.mark = ",", scientific = FALSE, trim = TRUE))
}
oneIn = function(p) vapply(p, oneIn1, character(1))

# Probability followed by its "1 in N" form, e.g. "0.00165 (1 in 606)"
both = function(p) {
  ifelse(is.na(p) | p >= 0.5 | p <= 0, fmt(p), paste0(fmt(p), " (", oneIn(p), ")"))
}

## ---- Beta prior from a median and an upper quantile --------------------------------

# The b for which Beta(a, b) has the given median (NA if there is none)
betaForMedian = function(a, med) {
  f = function(lb) qbeta(0.5, a, exp(lb)) - med
  lo = -20
  hi = 50
  flo = suppressWarnings(f(lo))
  fhi = suppressWarnings(f(hi))
  if (!is.finite(flo) || !is.finite(fhi) || sign(flo) == sign(fhi)) return(NA_real_)
  lb = tryCatch(uniroot(f, c(lo, hi), f.lower = flo, f.upper = fhi, tol = 1e-10)$root,
                error = function(e) NA_real_)
  exp(lb)
}

# Beta(a, b) with the given median whose `level` quantile equals `upper`.
# With the median held fixed, the upper quantile falls steadily as a grows,
# so a grid scan over log(a) brackets the answer and uniroot() refines it.
fitBetaMedian = function(med, upper, level) {
  qAt = function(la) {
    a = exp(la)
    b = betaForMedian(a, med)
    if (is.na(b)) return(NA_real_)
    suppressWarnings(qbeta(level, a, b))
  }
  las = seq(log(1e-3), log(1e6), length.out = 40)
  f = vapply(las, qAt, numeric(1)) - upper
  ok = is.finite(f)
  if (!any(ok)) return(NULL)
  k = length(f)
  crossings = which(ok[-1] & ok[-k] & sign(f[-1]) != sign(f[-k]))

  la = NA_real_
  if (length(crossings) > 0) {
    i = crossings[1]
    la = tryCatch(uniroot(function(la) qAt(la) - upper, c(las[i], las[i + 1]), tol = 1e-8)$root,
                  error = function(e) NA_real_)
  }
  if (is.na(la)) la = las[which.min(ifelse(ok, abs(f), Inf))]

  a = exp(la)
  b = betaForMedian(a, med)
  achieved = qbeta(level, a, b)
  list(family = "beta", alpha = a, beta = b, upper = achieved,
       exact = abs(achieved - upper) <= 1e-4 * upper)
}

## ---- Logit-normal prior -------------------------------------------------------------

# logit(p) ~ Normal(mu, sigma). Quantiles map exactly through the logit, so the
# median and upper quantile determine mu and sigma directly.
fitLogitNormal = function(med, upper, level) {
  mu = qlogis(med)
  sigma = (qlogis(upper) - mu) / qnorm(level)
  list(family = "logitnormal", mu = mu, sigma = sigma, grid = lnGrid(mu, sigma))
}

# Posterior for a logit-normal prior after x successes in n trials, evaluated
# on an evenly spaced grid on the logit scale. A coarse pass finds where the
# posterior lives, then a fine grid covers that region.
lnGrid = function(mu, sigma, x = 0, n = 0, npts = 4001) {
  logPost = function(t) {
    dnorm(t, mu, sigma, log = TRUE) +
      x * plogis(t, log.p = TRUE) + (n - x) * plogis(-t, log.p = TRUE)
  }
  lo = mu - 12 * sigma
  hi = mu + 12 * sigma
  if (x > 0) {
    centre = qlogis((x + 0.5) / (n + 1))
    lo = min(lo, centre - 5)
    hi = max(hi, centre + 5)
  }
  t0 = seq(lo, hi, length.out = npts)
  lp0 = logPost(t0)
  keep = which(lp0 > max(lp0) - 50)
  t = seq(t0[max(min(keep) - 1, 1)], t0[min(max(keep) + 1, npts)], length.out = npts)
  lp = logPost(t)
  w = exp(lp - max(lp))
  list(family = "grid", t = t, p = plogis(t), w = w / sum(w))
}

## ---- Working with any of the distributions ------------------------------------------
# A distribution is list(family = "beta", alpha, beta),
# list(family = "logitnormal", mu, sigma, grid), or a posterior grid from lnGrid().

updateDist = function(d, x, n) {
  switch(d$family,
    beta = list(family = "beta", alpha = d$alpha + x, beta = d$beta + n - x),
    logitnormal = lnGrid(d$mu, d$sigma, x, n))
}

dQuantile = function(d, prob) {
  switch(d$family,
    beta = qbeta(prob, d$alpha, d$beta),
    logitnormal = plogis(d$mu + d$sigma * qnorm(prob)),
    grid = {
      cw = cumsum(d$w)
      keep = !duplicated(cw)
      approx(cw[keep], d$p[keep], xout = prob, rule = 2)$y
    })
}

# The mean, which is also the probability of a success on the next trial
dMean = function(d) {
  switch(d$family,
    beta = d$alpha / (d$alpha + d$beta),
    logitnormal = sum(d$grid$p * d$grid$w),
    grid = sum(d$p * d$w))
}

dAbove = function(d, thr) {
  switch(d$family,
    beta = pbeta(thr, d$alpha, d$beta, lower.tail = FALSE),
    logitnormal = pnorm(qlogis(thr), d$mu, d$sigma, lower.tail = FALSE),
    grid = sum(d$w[d$p > thr]))
}

# Density of log10(p), for plotting on a log scale
dLog10Density = function(d, u) {
  p = 10^u
  switch(d$family,
    beta = dbeta(p, d$alpha, d$beta) * p * log(10),
    logitnormal = dnorm(qlogis(p), d$mu, d$sigma) * log(10) / (1 - p),
    grid = {
      dt = d$t[2] - d$t[1]
      dens = approx(d$t, d$w / dt, xout = qlogis(p), rule = 1)$y
      dens[is.na(dens)] = 0
      dens * log(10) / (1 - p)
    })
}

describe = function(d) {
  switch(d$family,
    beta = sprintf("Beta(%s, %s)", fmt(d$alpha), fmt(d$beta)),
    logitnormal = sprintf("Logit-normal(\u03bc = %s, \u03c3 = %s)", fmt(d$mu), fmt(d$sigma)))
}

## ---- Predictive distribution for m future trials -------------------------------------

predSummary = function(d, m) {
  k = 0:m
  if (d$family == "beta") {
    a = d$alpha
    b = d$beta
    pmf = exp(lchoose(m, k) + lbeta(k + a, m - k + b) - lbeta(a, b))
    pAny = -expm1(lbeta(a, b + m) - lbeta(a, b))
  } else {
    g = if (d$family == "logitnormal") d$grid else d
    use = g$w > 1e-15 * max(g$w)
    p = g$p[use]
    w = g$w[use] / sum(g$w[use])
    pmf = vapply(k, function(kk) sum(w * dbinom(kk, m, p)), numeric(1))
    pAny = -sum(w * expm1(m * log1p(-p)))
  }
  pmf = pmf / sum(pmf)
  cdf = cumsum(pmf)
  mean = sum(k * pmf)
  list(k = k, pmf = pmf, mean = mean,
       sd = sqrt(max(sum(k^2 * pmf) - mean^2, 0)),
       p0 = 1 - pAny, pAny = pAny,
       lower = k[which(cdf >= 0.025)[1]],
       upper = k[which(cdf >= 0.975)[1]])
}

# A "1 in N" input with its decimal equivalent shown to the right of the box.
# The label sits above the row so it can use the full width.
oneInInput = function(id, label, value) {
  div(
    tags$label(`for` = id, label),
    fluidRow(
      column(7, numericInput(id, NULL, value = value, min = 1.001, width = "100%")),
      column(5, div(style = "padding-top: 7px;",
                    textOutput(paste0(id, "_decimal"), inline = TRUE)))
    )
  )
}

# Decimal form of 1/N: plain decimals down to 1e-6, scientific below that
decimalOf = function(N) {
  p = 1 / N
  if (p >= 1e-6) format(signif(p, 4), scientific = FALSE) else formatC(p, digits = 4, format = "g")
}

familyCols = c("Beta" = "firebrick", "Logit-normal" = "steelblue")

aboutContent = if (file.exists("README.md")) {
  includeMarkdown("README.md")
} else {
  p("No README.md was found next to the app, so there's nothing to show here yet.")
}

## ---- UI ----------------------------------------------------------------------------

ui = fluidPage(
  withMathJax(),
  titlePanel("Priors for a binomial probability"),
  tabsetPanel(id = "mytabs", selected = "Main",
    tabPanel("About",
      fluidRow(column(10, br(), aboutContent))
    ),
    tabPanel("Main",
      br(),
      sidebarLayout(
        sidebarPanel(
          radioButtons("priorMode", "How do you want to specify the prior?",
                       choiceNames = list("From expert judgement: a typical value and an upper value",
                                          "As a Beta distribution with given \\( \\alpha \\) and \\( \\beta \\)"),
                       choiceValues = c("expert", "params"),
                       selected = "expert"),
          conditionalPanel(condition = "input.priorMode == 'expert'",
            oneInInput("medianN", "Typical value (the median): 1 in", value = 10000),
            oneInInput("upperN", "Upper value: 1 in", value = 100),
            numericInput("level", "Probability that the true value is below the upper value:",
                         value = 0.95, min = 0.5, max = 0.999, step = 0.01),
            uiOutput("fitStatus")
          ),
          conditionalPanel(condition = "input.priorMode == 'params'",
            numericInput("alpha_value", label = "\\( \\alpha \\):", min = 0.001, value = 1, step = 0.1),
            numericInput("beta_value", label = "\\( \\beta \\):", min = 0.001, value = 1, step = 0.1)
          ),
          oneInInput("thresholdN", "Also report the chance that the true value exceeds: 1 in", value = 20),
          hr(),
          h4("Data"),
          numericInput("n_value", "Number of trials, n:", min = 0, value = 10, step = 1),
          numericInput("x_value", "Number of successes, x:", min = 0, max = 10, value = 0, step = 1)
        ),
        mainPanel(
          tabsetPanel(id = "results",
            tabPanel("Prior and posterior",
              plotOutput("distPlot"),
              br(),
              tableOutput("summaryTable")
            ),
            tabPanel("Predictive",
              br(),
              uiOutput("nextTrial"),
              h4("How much does the prior matter?"),
              helpText("The same data combined with your prior and with two common",
                       "default priors. With few trials, and especially when x = 0,",
                       "the choice of prior can change the answer considerably."),
              tableOutput("priorSensitivity"),
              uiOutput("skewNote"),
              hr(),
              checkboxInput("showMulti", "Show predictions for more than one future trial", value = FALSE),
              conditionalPanel(condition = "input.showMulti",
                numericInput("m_value", "Number of future trials, m:",
                             min = 1, max = 1000, value = 10, step = 1),
                plotOutput("predPlot"),
                helpText("Probabilities for the number of successes in m new trials,",
                         "averaged over the uncertainty in p rather than plugging in",
                         "a single estimate of it."),
                tableOutput("predTable")
              )
            ),
            tabPanel("Sensitivity",
              br(),
              helpText("How the probability of a success on the next trial (after the data)",
                       "changes with the expert's upper value and the probability attached",
                       "to it, keeping the typical value fixed. Points mark the current choice."),
              plotOutput("sensPlot", height = "500px")
            )
          )
        )
      )
    )
  )
)

## ---- Server ------------------------------------------------------------------------

server = function(input, output, session) {

  # Show each "1 in N" input as a decimal alongside the box
  for (id in c("medianN", "upperN", "thresholdN")) {
    local({
      inputId = id
      output[[paste0(inputId, "_decimal")]] = renderText({
        N = input[[inputId]]
        if (is.numeric(N) && !is.na(N) && N >= 1) paste("=", decimalOf(N)) else ""
      })
    })
  }

  # Expert judgement: typical value (median), upper value and its probability
  elicited = reactive({
    medN = input$medianN
    upN = input$upperN
    level = input$level
    validate(
      need(is.numeric(medN) && !is.na(medN) && medN > 1, "The typical value must be 1 in N, with N greater than 1."),
      need(is.numeric(upN) && !is.na(upN) && upN > 1, "The upper value must be 1 in N, with N greater than 1."),
      need(upN < medN, "The upper value must be more likely than the typical value (a smaller N)."),
      need(is.numeric(level) && !is.na(level) && level > 0.5 && level < 1,
           "The probability for the upper value must be between 0.5 and 1.")
    )
    med = 1 / medN
    upper = 1 / upN
    fitB = fitBetaMedian(med, upper, level)
    validate(need(!is.null(fitB), "No Beta distribution could be fitted to these values."))
    list(median = med, upper = upper, level = level,
         beta = fitB, logitnormal = fitLogitNormal(med, upper, level))
  })

  paramPrior = reactive({
    a = input$alpha_value
    b = input$beta_value
    validate(
      need(is.numeric(a) && !is.na(a) && a > 0, "alpha must be a positive number."),
      need(is.numeric(b) && !is.na(b) && b > 0, "beta must be a positive number.")
    )
    list(family = "beta", alpha = a, beta = b)
  })

  # Named list of prior distributions: two families for expert judgement,
  # a single Beta otherwise
  priors = reactive({
    if (input$priorMode == "expert") {
      e = elicited()
      list("Beta" = e$beta, "Logit-normal" = e$logitnormal)
    } else {
      list("Beta" = paramPrior())
    }
  })

  # The probability level to report alongside the median
  reportLevel = reactive({
    if (input$priorMode == "expert") elicited()$level else 0.95
  })

  threshold = reactive({
    N = input$thresholdN
    validate(need(is.numeric(N) && !is.na(N) && N > 1, "The reporting threshold must be 1 in N, with N greater than 1."))
    1 / N
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

  posteriors = reactive({
    d = observed()
    lapply(priors(), updateDist, x = d$x, n = d$n)
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

  # Switching to alpha/beta mode starts from the Beta fitted to the expert's values
  observeEvent(input$priorMode, {
    if (input$priorMode == "params") {
      fit = tryCatch(elicited()$beta, error = function(e) NULL)
      if (!is.null(fit)) {
        updateNumericInput(session, "alpha_value", value = signif(fit$alpha, 4))
        updateNumericInput(session, "beta_value", value = signif(fit$beta, 4))
      }
    }
  }, ignoreInit = TRUE)

  output$fitStatus = renderUI({
    e = elicited()
    tagList(
      tags$table(class = "table table-condensed",
        tags$tr(tags$th("Family"), tags$th("Median"), tags$th(sprintf("%s quantile", fmt(e$level)))),
        tags$tr(tags$td("Requested"), tags$td(oneIn(e$median)), tags$td(oneIn(e$upper))),
        tags$tr(tags$td("Beta"), tags$td(oneIn(dQuantile(e$beta, 0.5))),
                tags$td(class = if (!e$beta$exact) "text-danger", oneIn(e$beta$upper))),
        tags$tr(tags$td("Logit-normal"), tags$td(oneIn(dQuantile(e$logitnormal, 0.5))),
                tags$td(oneIn(dQuantile(e$logitnormal, e$level))))
      ),
      helpText(describe(e$beta), br(), describe(e$logitnormal)),
      if (!e$beta$exact) {
        helpText(class = "text-danger", "The Beta distribution can't match these values exactly;",
                 "the closest one is shown and used.")
      }
    )
  })

  ## -- Prior and posterior tab ----------------------------------------------------------

  output$distPlot = renderPlot({
    pr = priors()
    po = posteriors()
    dists = c(pr, po)

    # Log-scale axis: from well below the smallest median up to where the
    # upper tails have run out
    meds = vapply(dists, dQuantile, numeric(1), prob = 0.5)
    tops = vapply(dists, dQuantile, numeric(1), prob = 0.999)
    lows = vapply(dists, dQuantile, numeric(1), prob = 0.01)
    lo = max(floor(log10(max(min(lows), 1e-300))), floor(log10(min(meds))) - 3)
    hi = min(0, ceiling(log10(max(tops))))
    lo = max(lo, hi - 15)
    if (hi <= lo) hi = lo + 1
    u = seq(lo, hi, length.out = 1000)

    yPr = lapply(pr, dLog10Density, u = u)
    yPo = lapply(po, dLog10Density, u = u)
    yAll = unlist(c(yPr, yPo))
    ymax = max(yAll[is.finite(yAll)]) * 1.05

    plot(NA, xlim = c(lo, hi), ylim = c(0, ymax), xaxt = "n", xaxs = "i", yaxs = "i", las = 1,
         xlab = "p (log scale)", ylab = "Density of log10(p)", main = "Prior and posterior")
    ticks = seq(ceiling(lo), floor(hi))
    axis(1, at = ticks, labels = parse(text = paste0("10^", ticks)))

    for (nm in names(pr)) {
      lines(u, yPr[[nm]], lwd = 2, lty = 2, col = familyCols[nm])
      lines(u, yPo[[nm]], lwd = 2, lty = 1, col = familyCols[nm])
    }
    d = observed()
    if (d$n > 0 && d$x > 0) abline(v = log10(d$x / d$n), lty = 3, col = "grey40")

    legend("topleft", bty = "n",
           legend = c(paste(names(pr), "prior"), paste(names(pr), "posterior"),
                      if (d$n > 0 && d$x > 0) "Observed x / n"),
           col = c(familyCols[names(pr)], familyCols[names(pr)], if (d$n > 0 && d$x > 0) "grey40"),
           lty = c(rep(2, length(pr)), rep(1, length(pr)), if (d$n > 0 && d$x > 0) 3),
           lwd = c(rep(2, 2 * length(pr)), if (d$n > 0 && d$x > 0) 1))
  })

  output$summaryTable = renderTable({
    pr = priors()
    po = posteriors()
    lev = reportLevel()
    thr = threshold()
    column = function(d) {
      c(both(dQuantile(d, 0.5)), both(dQuantile(d, lev)), both(dQuantile(d, 0.99)),
        fmt(dAbove(d, thr)), both(dMean(d)))
    }
    out = data.frame(Quantity = c("Median", sprintf("%s quantile", fmt(lev)), "99th percentile",
                                  sprintf("Chance p exceeds %s", oneIn(thr)),
                                  "Mean (probability for the next trial)"),
                     check.names = FALSE)
    for (nm in names(pr)) {
      out[[paste(nm, "prior")]] = column(pr[[nm]])
      out[[paste(nm, "posterior")]] = column(po[[nm]])
    }
    out
  }, striped = TRUE)

  ## -- Predictive tab --------------------------------------------------------------------

  output$nextTrial = renderUI({
    po = posteriors()
    rows = lapply(names(po), function(nm) {
      tags$tr(tags$td(paste(nm, "prior")), tags$td(both(dMean(po[[nm]]))))
    })
    withMathJax(tagList(
      p(class = "lead", "Probability of a success on the next trial"),
      tags$table(class = "table table-condensed", style = "width: auto;", rows),
      helpText(HTML(paste(
        "This is the posterior predictive probability for a single future trial, the",
        "quantity needed, for example, as a node probability in a Bayesian network.",
        "It happens to equal the posterior mean, because averaging P(success | p) = p",
        "over the posterior gives E[p]. For a Beta prior this is",
        "\\( \\alpha^\\prime / (\\alpha^\\prime + \\beta^\\prime) \\); for the logit-normal",
        "prior it is found by numerical integration. The equality is special to a single",
        "trial: substituting the posterior mean into other functions of p, such as the",
        "probability of at least one success in several trials, gives the wrong answer.")))
    ))
  })

  output$priorSensitivity = renderTable({
    d = observed()
    pr = priors()
    tag = if (input$priorMode == "expert") " (your judgement)" else " (your \u03b1, \u03b2)"
    rowFor = function(label, prior) {
      post = updateDist(prior, d$x, d$n)
      c(label, describe(prior), both(dMean(post)), both(dQuantile(post, 0.95)))
    }
    rows = c(
      lapply(names(pr), function(nm) rowFor(paste0(nm, tag), pr[[nm]])),
      list(rowFor("Uniform", list(family = "beta", alpha = 1, beta = 1)),
           rowFor("Jeffreys", list(family = "beta", alpha = 1/2, beta = 1/2)))
    )
    out = as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    names(out) = c("Prior", "Distribution", "P(success on next trial)", "95% upper credible bound for p")
    out
  }, striped = TRUE)

  # Warn when a Beta posterior is so skewed that its mean lies above its own
  # 95% upper bound (typically when alpha' is well below 1)
  output$skewNote = renderUI({
    po = posteriors()
    b = po[["Beta"]]
    if (!is.null(b) && b$family == "beta" && dMean(b) > dQuantile(b, 0.95)) {
      pAboveMean = pbeta(dMean(b), b$alpha, b$beta, lower.tail = FALSE)
      helpText(class = "text-warning", sprintf(paste(
        "With the Beta prior, the probability for the next trial lies above the posterior's",
        "own 95%% upper bound. That is not an error. Because \u03b1\u2032 = %s is well",
        "below 1, the posterior piles almost all of its probability extremely close to 0,",
        "and its mean is carried by a thin tail: only %s%% of the posterior lies above the",
        "mean."), fmt(b$alpha), fmt(100 * pAboveMean)))
    }
  })

  futureTrials = reactive({
    m = input$m_value
    validate(need(is.numeric(m) && !is.na(m) && m >= 1 && m <= 1000 && m == round(m),
                  "m must be a whole number between 1 and 1,000."))
    m
  })

  predictions = reactive({
    m = futureTrials()
    lapply(posteriors(), predSummary, m = m)
  })

  output$predPlot = renderPlot({
    m = futureTrials()
    preds = predictions()
    lastK = function(s) s$k[which(cumsum(s$pmf) >= 0.999)[1]]
    top = max(1, vapply(preds, lastK, numeric(1)))
    k = 0:top
    ymax = max(vapply(preds, function(s) max(s$pmf[k + 1]), numeric(1))) * 1.05

    plot(NA, xlim = c(-0.5, top + 0.5), ylim = c(0, ymax), xaxt = "n",
         xaxs = "i", yaxs = "i", las = 1,
         xlab = sprintf("Number of successes in %d future trials", m),
         ylab = "Probability", main = "Posterior predictive distribution")
    ticks = pretty(k)
    axis(1, at = ticks[ticks >= 0 & ticks <= top & ticks == round(ticks)])

    barLwd = max(1, min(12, 300 / (top + 1)))
    nf = length(preds)
    offsets = if (nf == 2) c(-0.15, 0.15) else 0
    for (i in seq_len(nf)) {
      segments(k + offsets[i], 0, k + offsets[i], preds[[i]]$pmf[k + 1],
               lwd = barLwd, col = familyCols[names(preds)[i]], lend = "butt")
    }
    legend("topright", bty = "n", legend = paste(names(preds), "prior"),
           col = familyCols[names(preds)], pch = 15, pt.cex = 2)
  })

  output$predTable = renderTable({
    preds = predictions()
    out = data.frame(Quantity = c("Expected number of successes", "Standard deviation",
                                  "P(no successes)", "P(at least one success)",
                                  "95% prediction interval"),
                     check.names = FALSE)
    for (nm in names(preds)) {
      s = preds[[nm]]
      out[[paste(nm, "prior")]] = c(fmt(s$mean), fmt(s$sd), fmt(s$p0), both(s$pAny),
                                    paste0(s$lower, " to ", s$upper))
    }
    out
  }, striped = TRUE)

  ## -- Sensitivity tab -------------------------------------------------------------------

  output$sensPlot = renderPlot({
    validate(need(input$priorMode == "expert",
                  "This plot is available when the prior comes from expert judgement."))
    e = elicited()
    d = observed()
    med = e$median
    levels = sort(unique(c(0.9, 0.95, 0.99, e$level)))
    levelCols = setNames(hcl.colors(length(levels), "Dark 3"), fmt(levels))
    xmax = 0.5
    validate(need(med * 1.5 < xmax, "The typical value is too large for this plot."))

    # Beta: trace the curve through the shape parameter a (with the median
    # fixed), which avoids refitting for every point
    as = exp(seq(log(1e-3), log(1e6), length.out = 150))
    bs = vapply(as, betaForMedian, numeric(1), med = med)
    okB = is.finite(bs)
    as = as[okB]
    bs = bs[okB]
    predB = (as + d$x) / (as + bs + d$n)

    # Logit-normal: a grid of upper values, each giving sigma directly
    uppers = 10^seq(log10(med * 1.5), log10(xmax), length.out = 40)

    curves = lapply(levels, function(lev) {
      upB = suppressWarnings(qbeta(lev, as, bs))
      predL = vapply(uppers, function(up) {
        sigma = (qlogis(up) - qlogis(med)) / qnorm(lev)
        g = lnGrid(qlogis(med), sigma, d$x, d$n, npts = 2001)
        sum(g$p * g$w)
      }, numeric(1))
      list(upB = upB, predB = predB, upL = uppers, predL = predL)
    })

    inRange = function(v) v >= med * 1.5 & v <= xmax
    yAll = unlist(lapply(curves, function(cv) c(cv$predB[inRange(cv$upB)], cv$predL)))
    yAll = yAll[is.finite(yAll) & yAll > 0]

    plot(NA, xlim = c(med * 1.5, xmax), ylim = range(yAll), log = "xy", las = 1,
         xlab = "Upper value", ylab = "P(success on next trial), after the data",
         main = sprintf("Typical value %s; data: %d successes in %d trials", oneIn(med), d$x, d$n))
    for (i in seq_along(levels)) {
      cv = curves[[i]]
      col = levelCols[i]
      keep = inRange(cv$upB)
      o = order(cv$upB[keep])
      lines(cv$upB[keep][o], cv$predB[keep][o], lwd = 2, lty = 1, col = col)
      lines(cv$upL, cv$predL, lwd = 2, lty = 2, col = col)
    }

    # The current choice, for each family
    po = posteriors()
    curCol = levelCols[fmt(e$level)]
    points(e$beta$upper, dMean(po[["Beta"]]), pch = 19, cex = 1.5, col = curCol)
    points(e$upper, dMean(po[["Logit-normal"]]), pch = 1, cex = 1.5, lwd = 2, col = curCol)

    legend("topleft", bty = "n",
           legend = c(paste("Probability", fmt(levels)), "Beta", "Logit-normal"),
           col = c(levelCols, "grey30", "grey30"),
           lty = c(rep(1, length(levels)), 1, 2), lwd = 2)
  })
}

shinyApp(ui = ui, server = server)
