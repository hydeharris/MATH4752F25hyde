# app.R
# Shiny MLE Lab: Normal, Exponential, Poisson, Bernoulli, Gamma
# -----------------------------------------
# - Simulate data from "true" params
# - Compute MLEs (closed-form when available, otherwise 'optim')
# - Visualize histogram/pmf overlay, likelihood curves/contours
# -----------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .param-row { margin-top: 8px; }
    .small-note { color:#666; font-size:0.9em; }
    .codebox { background:#f7f7f7;padding:8px;border-radius:6px;font-family:monospace;}
  "))),
  titlePanel("Maximum Likelihood Estimation — Univariate Distributions"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput("dist", "Distribution",
                  c("Normal (μ, σ)" = "normal",
                    "Exponential (λ)" = "exponential",
                    "Poisson (λ)" = "poisson",
                    "Bernoulli (p)" = "bernoulli",
                    "Gamma (k, θ)" = "gamma")
      ),
      div(class = "param-row",
          numericInput("n", "Sample size n", value = 100, min = 5, step = 1)
      ),
      div(class = "param-row",
          numericInput("seed", "Random seed", value = 123, step = 1)
      ),
      uiOutput("trueParamUI"),
      actionButton("resim", "Simulate sample", class = "btn btn-primary"),
      hr(),
      h4("Likelihood Plot Range"),
      uiOutput("likeRangeUI"),
      div(class="small-note",
          "For 2-parameter models (Normal, Gamma), a 2D log-likelihood contour is shown.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data & Fit",
                 br(),
                 plotOutput("dataPlot", height = 380),
                 br(),
                 tableOutput("estTable")
        ),
        tabPanel("Likelihood",
                 br(),
                 uiOutput("likeHint"),
                 plotOutput("likePlot", height = 420)
        ),
        tabPanel("Formulas",
                 br(),
                 h4("Log-likelihoods & Closed-form MLEs"),
                 div(HTML("
            <div class='codebox'>
            <b>Bernoulli(p)</b>: x ∈ {0,1}<br/>
            ℓ(p) = ∑[x_i log p + (1 − x_i) log(1 − p)],  0<p<1.<br/>
            MLE: <b>p̂ = x̄</b>.
            </div>
            <div class='codebox'>
            <b>Poisson(λ)</b>: x ∈ {0,1,2,…}<br/>
            ℓ(λ) = ∑[x_i log λ − λ − log(x_i!)], λ>0.<br/>
            MLE: <b>λ̂ = x̄</b>.
            </div>
            <div class='codebox'>
            <b>Exponential(λ)</b>: x>0<br/>
            ℓ(λ) = n log λ − λ ∑x_i, λ>0.<br/>
            MLE: <b>λ̂ = 1 / x̄</b>.
            </div>
            <div class='codebox'>
            <b>Normal(μ, σ)</b>: x∈ℝ, σ>0<br/>
            ℓ(μ,σ) = −n log σ − (1/(2σ²))∑(x_i−μ)² + const.<br/>
            MLEs: <b>μ̂ = x̄</b>, <b>σ̂² = (1/n)∑(x_i−x̄)²</b>.
            </div>
            <div class='codebox'>
            <b>Gamma(k, θ)</b> (shape k>0, scale θ>0): x>0<br/>
            ℓ(k,θ) = −n k log θ − n log Γ(k) + (k−1)∑log x_i − (1/θ)∑x_i.<br/>
            MLE: no closed form for k; solve numerically. Then θ̂ = x̄/k̂.
            </div>
          "))
        )
      )
    )
  )
)

server <- function(input, output, session){
  
  # ----- True-parameter UI by distribution -----
  output$trueParamUI <- renderUI({
    switch(input$dist,
           "normal" = tagList(
             div(class="param-row", numericInput("mu_true", HTML("True &mu;"), 0)),
             div(class="param-row", numericInput("sigma_true", HTML("True &sigma; (>0)"), 1, min = 0.001, step = 0.1))
           ),
           "exponential" = tagList(
             div(class="param-row", numericInput("lambda_exp_true", HTML("True &lambda; (>0)"), 1, min = 1e-6))
           ),
           "poisson" = tagList(
             div(class="param-row", numericInput("lambda_pois_true", HTML("True &lambda; (>0)"), 3, min = 1e-6, step = 0.5))
           ),
           "bernoulli" = tagList(
             div(class="param-row", sliderInput("p_true", HTML("True p (0,1)"), min=0.01, max=0.99, value = 0.4))
           ),
           "gamma" = tagList(
             div(class="param-row", numericInput("k_true", HTML("True shape k (>0)"), 2, min = 1e-3, step = 0.1)),
             div(class="param-row", numericInput("theta_true", HTML("True scale &theta; (>0)"), 1, min = 1e-6, step = 0.1))
           )
    )
  })
  
  # ----- Likelihood range UI -----
  output$likeRangeUI <- renderUI({
    switch(input$dist,
           "normal" = tagList(
             sliderInput("mu_range", HTML("μ range"),
                         min = -10, max = 10, value = c(-3, 3), step = 0.1),
             sliderInput("sigma_range", HTML("σ range"),
                         min = 0.1, max = 5, value = c(0.2, 3), step = 0.1)
           ),
           "exponential" = sliderInput("lambda_exp_range", HTML("λ range"),
                                       min = 0.05, max = 5, value = c(0.2, 3), step = 0.05),
           "poisson" = sliderInput("lambda_pois_range", HTML("λ range"),
                                   min = 0.1, max = 15, value = c(0.5, 10), step = 0.1),
           "bernoulli" = sliderInput("p_range", HTML("p range"),
                                     min = 0.01, max = 0.99, value = c(0.05, 0.95), step = 0.01),
           "gamma" = tagList(
             sliderInput("k_range", HTML("k (shape) range"),
                         min = 0.2, max = 8, value = c(0.5, 5), step = 0.1),
             sliderInput("theta_range", HTML("θ (scale) range"),
                         min = 0.05, max = 5, value = c(0.2, 3), step = 0.05)
           )
    )
  })
  
  # ----- Simulate sample -----
  dat <- eventReactive(input$resim, {
    set.seed(input$seed)
    n <- input$n
    switch(input$dist,
           "normal" = rnorm(n, mean = input$mu_true, sd = input$sigma_true),
           "exponential" = rexp(n, rate = input$lambda_exp_true),
           "poisson" = rpois(n, lambda = input$lambda_pois_true),
           "bernoulli" = rbinom(n, size = 1, prob = input$p_true),
           "gamma" = rgamma(n, shape = input$k_true, scale = input$theta_true)
    )
  }, ignoreInit = TRUE)
  
  # Run once at startup for a default dataset
  observeEvent(TRUE, {
    isolate({
      updateActionButton(session, "resim", label = "Simulate sample")
    })
    session$sendCustomMessage(type='dummy', message=list())
  }, once = TRUE)
  # initialize with click
  observeEvent(session, { if (is.null(isolate(dat()))) { isolate({set.seed(123)}); }})
  # auto simulate first time UI loads
  observeEvent(input$dist, { if (is.null(isolate(dat()))) { set.seed(input$seed); }}, ignoreInit = TRUE)
  observeEvent(input$seed, {}, ignoreInit = TRUE)
  # simulate on initial load
  observe({
    if (is.null(dat())) {
      set.seed(input$seed)
      n <- input$n
      d0 <- switch(input$dist,
                   "normal" = rnorm(n, 0, 1),
                   "exponential" = rexp(n, 1),
                   "poisson" = rpois(n, 3),
                   "bernoulli" = rbinom(n, 1, 0.4),
                   "gamma" = rgamma(n, 2, scale = 1))
      assign("._init_data", d0, envir = .GlobalEnv)
    }
  })
  
  get_data <- reactive({
    if (!is.null(dat())) return(dat())
    if (exists("._init_data", envir = .GlobalEnv)) get("._init_data", envir = .GlobalEnv) else numeric(0)
  })
  
  # ----- Log-likelihoods -----
  ll_normal <- function(par, x){
    mu <- par[1]; sigma <- par[2]
    if (sigma <= 0) return(-Inf)
    sum(dnorm(x, mean = mu, sd = sigma, log = TRUE))
  }
  ll_exponential <- function(lambda, x){
    if (lambda <= 0 || any(x < 0)) return(-Inf)
    sum(dexp(x, rate = lambda, log = TRUE))
  }
  ll_poisson <- function(lambda, x){
    if (lambda <= 0 || any(x < 0)) return(-Inf)
    sum(dpois(x, lambda = lambda, log = TRUE))
  }
  ll_bernoulli <- function(p, x){
    if (p <= 0 || p >= 1 || any(!(x %in% c(0,1)))) return(-Inf)
    sum(dbinom(x, 1, p, log = TRUE))
  }
  ll_gamma <- function(par, x){
    k <- par[1]; theta <- par[2]
    if (k <= 0 || theta <= 0 || any(x <= 0)) return(-Inf)
    sum(dgamma(x, shape = k, scale = theta, log = TRUE))
  }
  
  # ----- Compute MLEs -----
  fit <- reactive({
    x <- get_data()
    n <- length(x)
    if (n == 0) return(NULL)
    
    out <- list()
    if (input$dist == "normal"){
      mu_hat <- mean(x)
      sigma_hat <- sqrt(mean((x - mu_hat)^2)) # MLE uses 1/n
      out$par <- c(mu = mu_hat, sigma = sigma_hat)
      out$logLik <- ll_normal(c(mu_hat, sigma_hat), x)
      out$method <- "Closed-form"
    } else if (input$dist == "exponential"){
      lambda_hat <- 1 / mean(x)
      out$par <- c(lambda = lambda_hat)
      out$logLik <- ll_exponential(lambda_hat, x)
      out$method <- "Closed-form"
    } else if (input$dist == "poisson"){
      lambda_hat <- mean(x)
      out$par <- c(lambda = lambda_hat)
      out$logLik <- ll_poisson(lambda_hat, x)
      out$method <- "Closed-form"
    } else if (input$dist == "bernoulli"){
      p_hat <- mean(x)
      # clamp to (0,1) open interval for numerical stability in plots
      p_hat <- min(max(p_hat, 1e-6), 1-1e-6)
      out$par <- c(p = p_hat)
      out$logLik <- ll_bernoulli(p_hat, x)
      out$method <- "Closed-form"
    } else if (input$dist == "gamma"){
      xbar <- mean(x); lxbar <- mean(log(x))
      # Good starting values: method of moments
      # Var = k*θ^2 => k0 = (xbar/SD)^2, θ0 = Var/xbar
      v <- var(x)
      k0 <- if (v > 0) xbar^2 / v else 1
      theta0 <- if (xbar > 0 && k0 > 0) v / xbar else 1
      k0 <- max(k0, 0.5); theta0 <- max(theta0, 0.2)
      
      o <- optim(c(k0, theta0),
                 fn = function(par) -ll_gamma(par, x),
                 method = "L-BFGS-B",
                 lower = c(1e-4, 1e-4), upper = c(200, 200))
      k_hat <- o$par[1]; theta_hat <- o$par[2]
      out$par <- c(k = k_hat, theta = theta_hat)
      out$logLik <- -o$value
      out$method <- "Numerical (optim)"
    }
    out
  })
  
  # ----- Data + overlay plot -----
  output$dataPlot <- renderPlot({
    req(get_data())
    x <- get_data()
    dist <- input$dist
    f <- fit()
    
    if (dist %in% c("bernoulli","poisson")){
      # discrete bar plot with fitted pmf overlay (points/segments)
      df <- data.frame(x = x)
      counts <- df %>% count(x) %>% mutate(freq = n / nrow(df))
      xs <- 0:max(max(x), ifelse(dist == "poisson", ceiling(max(f$par["lambda"] * 3, 5)), 1))
      
      if (dist == "bernoulli"){
        p <- f$par["p"]
        pmf <- dbinom(xs, 1, p)
        gg <- ggplot(counts, aes(x = factor(x), y = freq)) +
          geom_col(fill = "#a6bddb") +
          geom_point(aes(x = factor(xs), y = pmf), size = 3) +
          geom_segment(aes(x = as.numeric(factor(xs)) - 0.25,
                           xend = as.numeric(factor(xs)) + 0.25,
                           y = pmf, yend = pmf)) +
          labs(x = "x", y = "Relative frequency",
               title = sprintf("Bernoulli(p) — data vs fitted (p̂ = %.3f)", p)) +
          theme_minimal(base_size = 13)
      } else {
        lambda <- f$par["lambda"]
        pmf <- dpois(xs, lambda)
        gg <- ggplot(counts, aes(x = as.factor(x), y = freq)) +
          geom_col(fill = "#a6bddb") +
          geom_point(aes(x = as.factor(xs), y = pmf), size = 2.2) +
          geom_segment(aes(x = as.numeric(as.factor(xs)) - 0.25,
                           xend = as.numeric(as.factor(xs)) + 0.25,
                           y = pmf, yend = pmf)) +
          labs(x = "x", y = "Relative frequency",
               title = sprintf("Poisson(λ) — data vs fitted (λ̂ = %.3f)", lambda)) +
          theme_minimal(base_size = 13)
      }
      gg
      
    } else {
      # continuous histogram + fitted density
      df <- data.frame(x = x)
      gg <- ggplot(df, aes(x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#a6bddb") +
        theme_minimal(base_size = 13)
      
      if (dist == "normal"){
        mu <- f$par["mu"]; sigma <- f$par["sigma"]
        gg + stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), size = 1.1) +
          labs(title = sprintf("Normal(μ,σ) — fitted μ̂=%.3f, σ̂=%.3f", mu, sigma),
               x = "x", y = "Density")
      } else if (dist == "exponential"){
        lam <- f$par["lambda"]
        gg + stat_function(fun = dexp, args = list(rate = lam), size = 1.1) +
          labs(title = sprintf("Exponential(λ) — fitted λ̂=%.3f", lam),
               x = "x", y = "Density")
      } else if (dist == "gamma"){
        k <- f$par["k"]; th <- f$par["theta"]
        gg + stat_function(fun = dgamma, args = list(shape = k, scale = th), size = 1.1) +
          labs(title = sprintf("Gamma(k,θ) — fitted k̂=%.3f, θ̂=%.3f", k, th),
               x = "x", y = "Density")
      }
    }
  })
  
  # ----- Estimates table -----
  output$estTable <- renderTable({
    req(get_data(), fit())
    x <- get_data(); f <- fit()
    n <- length(x)
    
    tbl <- switch(input$dist,
                  "normal" = {
                    data.frame(Parameter = c("μ (true)","σ (true)","μ̂ (MLE)","σ̂ (MLE)","Log-likelihood","n"),
                               Value = c(input$mu_true, input$sigma_true, unname(f$par["mu"]), unname(f$par["sigma"]), round(f$logLik, 3), n))
                  },
                  "exponential" = {
                    data.frame(Parameter = c("λ (true)","λ̂ (MLE)","Log-likelihood","n"),
                               Value = c(input$lambda_exp_true, unname(f$par["lambda"]), round(f$logLik, 3), n))
                  },
                  "poisson" = {
                    data.frame(Parameter = c("λ (true)","λ̂ (MLE)","Log-likelihood","n"),
                               Value = c(input$lambda_pois_true, unname(f$par["lambda"]), round(f$logLik, 3), n))
                  },
                  "bernoulli" = {
                    data.frame(Parameter = c("p (true)","p̂ (MLE)","Log-likelihood","n"),
                               Value = c(input$p_true, unname(f$par["p"]), round(f$logLik, 3), n))
                  },
                  "gamma" = {
                    data.frame(Parameter = c("k (true)","θ (true)","k̂ (MLE)","θ̂ (MLE)","Log-likelihood","n"),
                               Value = c(input$k_true, input$theta_true, unname(f$par["k"]), unname(f$par["theta"]), round(f$logLik, 3), n))
                  }
    )
    tbl
  }, striped = TRUE, bordered = TRUE, digits = 4)
  
  # ----- Likelihood visualization -----
  output$likeHint <- renderUI({
    if (input$dist %in% c("normal","gamma")){
      HTML("<span class='small-note'>Showing a 2D <b>log-likelihood</b> contour. The red dot marks the MLE.</span>")
    } else {
      HTML("<span class='small-note'>Showing a 1D <b>log-likelihood</b> curve with the MLE marked.</span>")
    }
  })
  
  output$likePlot <- renderPlot({
    req(get_data(), fit())
    x <- get_data(); f <- fit()
    
    if (input$dist == "normal"){
      mus <- seq(input$mu_range[1], input$mu_range[2], length.out = 80)
      sigs <- seq(input$sigma_range[1], input$sigma_range[2], length.out = 80)
      grid <- expand.grid(mu = mus, sigma = sigs)
      grid$ll <- apply(grid, 1, function(r) ll_normal(c(r[1], r[2]), x))
      ggplot(grid, aes(mu, sigma, z = ll)) +
        geom_contour_filled(bins = 12) +
        geom_point(aes(x = f$par["mu"], y = f$par["sigma"]), color = "red", size = 3) +
        labs(x = "μ", y = "σ", title = "Normal(μ,σ) log-likelihood") +
        theme_minimal(base_size = 13)
      
    } else if (input$dist == "gamma"){
      ks <- seq(input$k_range[1], input$k_range[2], length.out = 80)
      thetas <- seq(input$theta_range[1], input$theta_range[2], length.out = 80)
      grid <- expand.grid(k = ks, theta = thetas)
      grid$ll <- apply(grid, 1, function(r) ll_gamma(c(r[1], r[2]), x))
      ggplot(grid, aes(k, theta, z = ll)) +
        geom_contour_filled(bins = 12) +
        geom_point(aes(x = f$par["k"], y = f$par["theta"]), color = "red", size = 3) +
        labs(x = "k (shape)", y = "θ (scale)", title = "Gamma(k,θ) log-likelihood") +
        theme_minimal(base_size = 13)
      
    } else if (input$dist == "exponential"){
      lam <- seq(input$lambda_exp_range[1], input$lambda_exp_range[2], length.out = 400)
      ll <- sapply(lam, ll_exponential, x = x)
      df <- data.frame(lambda = lam, ll = ll)
      ggplot(df, aes(lambda, ll)) + geom_line(size = 1.1) +
        geom_vline(xintercept = f$par["lambda"], color = "red") +
        labs(x = "λ", y = "log-likelihood", title = "Exponential(λ) log-likelihood") +
        theme_minimal(base_size = 13)
      
    } else if (input$dist == "poisson"){
      lam <- seq(input$lambda_pois_range[1], input$lambda_pois_range[2], length.out = 400)
      ll <- sapply(lam, ll_poisson, x = x)
      df <- data.frame(lambda = lam, ll = ll)
      ggplot(df, aes(lambda, ll)) + geom_line(size = 1.1) +
        geom_vline(xintercept = f$par["lambda"], color = "red") +
        labs(x = "λ", y = "log-likelihood", title = "Poisson(λ) log-likelihood") +
        theme_minimal(base_size = 13)
      
    } else if (input$dist == "bernoulli"){
      pgrid <- seq(input$p_range[1], input$p_range[2], length.out = 400)
      ll <- sapply(pgrid, ll_bernoulli, x = x)
      df <- data.frame(p = pgrid, ll = ll)
      ggplot(df, aes(p, ll)) + geom_line(size = 1.1) +
        geom_vline(xintercept = f$par["p"], color = "red") +
        labs(x = "p", y = "log-likelihood", title = "Bernoulli(p) log-likelihood") +
        theme_minimal(base_size = 13)
    }
  })
  
}

shinyApp(ui, server)