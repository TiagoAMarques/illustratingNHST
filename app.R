library(shiny)

fmt_p <- function(p) format.pval(p, digits = 3, eps = 0.001)

metric <- function(label, value) {
  div(class = "metric", div(class = "metric-label", label), div(class = "metric-value", value))
}

decision_text <- function(p, alpha) {
  if (p < alpha) "Reject H0" else "Do not reject H0"
}

pvalue_hist <- function(p, alpha, main = "Distribution of p-values") {
  breaks <- sort(unique(c(seq(0, 1, by = 0.05), alpha)))
  h <- hist(p, breaks = breaks, plot = FALSE)
  ymax <- max(h$density, 1) * 1.12
  plot(h, freq = FALSE, col = "#b9ddeb", border = "white", ylim = c(0, ymax),
       xlab = "p-value", ylab = "Density", main = main, bty = "l")
  idx <- h$breaks[-1] <= alpha
  if (any(idx)) {
    rect(h$breaks[which(idx)], 0, h$breaks[which(idx) + 1], h$density[idx],
         col = "#dd5b45", border = "white")
  }
  abline(h = 1, col = "#62788a", lty = 3, lwd = 1.5)
  abline(v = alpha, col = "#a63423", lty = 2, lwd = 2)
  legend("topright", bty = "n", fill = c("#dd5b45", "#b9ddeb"),
         legend = c(sprintf("p < %.2f", alpha), sprintf("p >= %.2f", alpha)))
}

stat_hist <- function(x, null_value = 0, truth = NULL, xlab = "Test statistic",
                      main = "Sampling distribution of the statistic") {
  hist(x, breaks = "FD", probability = TRUE, col = "#b9ddeb", border = "white",
       xlab = xlab, ylab = "Density", main = main, bty = "l")
  abline(v = null_value, col = "#dd5b45", lty = 3, lwd = 2)
  if (!is.null(truth)) abline(v = truth, col = "#17324d", lty = 2, lwd = 2)
}

tab_controls <- function(...) {
  column(3, div(class = "test-controls", ...))
}

single_many_ui <- function(prefix) {
  tagList(
    conditionalPanel(
      condition = "input.mode == 'single'",
      uiOutput(paste0(prefix, "_single_metrics")),
      div(class = "plot-card", plotOutput(paste0(prefix, "_single_plot"), height = "470px")),
      div(class = "explain-card", uiOutput(paste0(prefix, "_single_text")))
    ),
    conditionalPanel(
      condition = "input.mode == 'many'",
      uiOutput(paste0(prefix, "_many_metrics")),
      div(class = "plot-card", plotOutput(paste0(prefix, "_many_plot"), height = "470px")),
      div(class = "explain-card", uiOutput(paste0(prefix, "_many_text")))
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$title("What does a significance test do?"),
    tags$style(HTML("
      :root{--ink:#17324d;--blue:#1479a8;--coral:#dd5b45;--paper:#f4f7f9;--muted:#62788a}
      body{background:var(--paper);color:var(--ink);font-family:system-ui,-apple-system,'Segoe UI',sans-serif}
      .container-fluid{max-width:1480px;padding:0 26px 28px}.hero{margin:0 -26px 20px;padding:22px 30px 18px;color:white;background:linear-gradient(120deg,#102f49,#1479a8)}
      .hero h1{margin:0 0 4px;font-size:31px;font-weight:780;letter-spacing:-.025em}.hero p{margin:0;opacity:.91;font-size:16px}
      .global-card,.test-controls,.plot-card,.explain-card,.metric{background:white;border:1px solid #dfe7ed;border-radius:14px;box-shadow:0 3px 14px rgba(23,50,77,.07)}
      .global-card{padding:16px 18px;margin-bottom:18px}.global-grid{display:grid;grid-template-columns:1.25fr 1fr 1fr 1fr;gap:18px;align-items:end}.global-grid .form-group{margin-bottom:0}
      .test-controls{padding:18px;position:sticky;top:10px}.test-controls h3{margin-top:0}.plot-card{padding:14px 18px;margin-bottom:14px}.explain-card{padding:14px 18px;border-left:5px solid var(--blue)}
      .metrics{display:grid;grid-template-columns:repeat(4,1fr);gap:10px;margin-bottom:14px}.metric{padding:11px 14px}.metric-label{color:var(--muted);font-size:11px;text-transform:uppercase;letter-spacing:.065em}.metric-value{font-size:21px;font-weight:760;margin-top:2px}
      .nav-tabs{border:0;margin-bottom:15px}.nav-tabs>li>a{border:0!important;border-radius:9px;color:var(--ink);font-weight:650}.nav-tabs>li.active>a{background:#dceff7!important;color:#075a80!important}
      .btn-primary{background:var(--blue);border-color:var(--blue);border-radius:9px;font-weight:700}.definition{background:#eef7fb;border-left:4px solid var(--blue);padding:11px 13px;border-radius:0 8px 8px 0;margin-bottom:14px}.caution{background:#fff2ef;border-left-color:var(--coral)}
      .small-note{color:var(--muted);font-size:12px}.shiny-input-container{width:100%}
      @media(max-width:767px){.container-fluid{padding:0 12px}.hero{margin:0 -12px}.global-grid,.metrics{grid-template-columns:1fr}.test-controls{position:static;margin-bottom:12px}}
    "))
  ),
  div(class = "hero",
      h1("What does a significance test do?"),
      p("Generate evidence once, then repeat the same experiment to see what p-values and decisions mean in the long run.")),
  div(class = "global-card",
      div(class = "global-grid",
          radioButtons("mode", "View", choices = c("One dataset" = "single", "Many experiments" = "many"), inline = TRUE),
          conditionalPanel("input.mode == 'many'", sliderInput("B", "Experiments (B)", 100, 5000, 1000, 100)),
          sliderInput("alpha", "Significance level (alpha)", 0.01, 0.10, 0.05, 0.01),
          div(numericInput("seed", "Random seed", 2026, min = 1, step = 1),
              actionButton("generate", "Generate / repeat", class = "btn-primary", style = "width:100%"))
      )),
  tabsetPanel(id = "test_tab",
    tabPanel("One-sample t-test", value = "one_t",
      fluidRow(
        tab_controls(
          h3("Data-generating model"),
          div(class="definition", HTML("Test <strong>H<sub>0</sub>: &mu; = &mu;<sub>0</sub></strong> against a two-sided alternative.")),
          numericInput("ot_mu0", "Null mean (mu0)", 0, step = 0.5),
          numericInput("ot_mu", "True mean (mu)", 0, step = 0.5),
          numericInput("ot_sd", "Population SD", 1, min = 0.1, step = 0.25),
          sliderInput("ot_n", "Sample size", 5, 250, 25, 1),
          p(class="small-note", "Data are generated from a Normal population. The population SD is unknown to the test.")
        ),
        column(9, single_many_ui("ot"))
      )
    ),
    tabPanel("One-sample Wilcoxon", value = "wilcox",
      fluidRow(
        tab_controls(
          h3("Data-generating model"),
          div(class="definition", HTML("Test <strong>H<sub>0</sub>: location = 0</strong> using signed ranks.")),
          numericInput("wx_shift", "True location shift", 0, step = 0.25),
          numericInput("wx_scale", "Scale", 1, min = 0.1, step = 0.25),
          sliderInput("wx_n", "Sample size", 5, 150, 25, 1),
          selectInput("wx_shape", "Population shape", c("Normal (symmetric)"="normal", "Laplace (symmetric, heavy tails)"="laplace", "Shifted exponential (skewed)"="exponential")),
          div(class="definition caution", strong("Terminology: "), "the one-sample procedure is the Wilcoxon signed-rank test. Mann–Whitney/Wilcoxon rank-sum compares two independent samples.")
        ),
        column(9, single_many_ui("wx"))
      )
    ),
    tabPanel("Two-sample t-test", value = "two_t",
      fluidRow(
        tab_controls(
          h3("Data-generating model"),
          div(class="definition", HTML("Test <strong>H<sub>0</sub>: &mu;<sub>1</sub> - &mu;<sub>2</sub> = 0</strong> with Welch's t-test.")),
          numericInput("tt_mu1", "True mean, group 1", 0, step = 0.5),
          numericInput("tt_mu2", "True mean, group 2", 0, step = 0.5),
          numericInput("tt_sd1", "SD, group 1", 1, min = 0.1, step = 0.25),
          numericInput("tt_sd2", "SD, group 2", 1, min = 0.1, step = 0.25),
          sliderInput("tt_n1", "Sample size, group 1", 3, 200, 25, 1),
          sliderInput("tt_n2", "Sample size, group 2", 3, 200, 25, 1)
        ),
        column(9, single_many_ui("tt"))
      )
    ),
    tabPanel("Correlation test", value = "cor",
      fluidRow(
        tab_controls(
          h3("Data-generating model"),
          div(class="definition", HTML("Test <strong>H<sub>0</sub>: &rho; = 0</strong> using Pearson's correlation.")),
          sliderInput("cr_rho", "True correlation (rho)", -0.95, 0.95, 0, 0.05),
          sliderInput("cr_n", "Sample size", 5, 300, 30, 1),
          p(class="small-note", "The two variables are generated from a bivariate Normal distribution, each with mean 0 and SD 1.")
        ),
        column(9, single_many_ui("cr"))
      )
    ),
    tabPanel("How to use", value = "guide",
      div(class="plot-card",
          h2("A guided investigation"),
          tags$ol(
            tags$li("Set the true parameter equal to its null value. Generate one dataset several times: the p-value changes because the sample changes."),
            tags$li("Switch to Many experiments. Under a true null, about alpha of the tests reject and the p-values are approximately Uniform(0,1)."),
            tags$li("Move the true parameter away from the null. The p-values accumulate near zero and the rejection rate becomes the power."),
            tags$li("Increase sample size or reduce variability. Notice that power rises even when the true effect stays unchanged."),
            tags$li("For the Wilcoxon test, compare symmetric and skewed populations. Signed-rank inference is about a symmetric location shift, not automatically the median for every population shape.")
          ),
          div(class="definition", strong("The central lesson: "), "a significance test is a rule applied to random data. A p-value is not the probability that H0 is true; it measures how incompatible the observed statistic is with the null model."))
    )
  )
)

server <- function(input, output, session) {
  draw_wilcox <- function(n, shift, scale, shape) {
    z <- switch(shape,
      normal = rnorm(n),
      laplace = { u <- runif(n, -0.5, 0.5); -sign(u) * log(1 - 2 * abs(u)) / sqrt(2) },
      exponential = rexp(n) - log(2)
    )
    shift + scale * z
  }

  one_t <- eventReactive(input$generate, {
    req(input$ot_sd > 0, input$ot_n >= 3)
    set.seed(as.integer(input$seed) + input$generate + 11L)
    one <- rnorm(input$ot_n, input$ot_mu, input$ot_sd)
    fit <- t.test(one, mu = input$ot_mu0)
    many <- replicate(input$B, {
      z <- rnorm(input$ot_n, input$ot_mu, input$ot_sd)
      ans <- t.test(z, mu = input$ot_mu0)
      c(estimate = mean(z), statistic = unname(ans$statistic), p = ans$p.value)
    })
    list(x=one, fit=fit, sim=t(many), truth=input$ot_mu, null=input$ot_mu0)
  }, ignoreNULL = FALSE)

  wilcox_dat <- eventReactive(input$generate, {
    req(input$wx_scale > 0, input$wx_n >= 3)
    set.seed(as.integer(input$seed) + input$generate + 22L)
    one <- draw_wilcox(input$wx_n, input$wx_shift, input$wx_scale, input$wx_shape)
    fit <- suppressWarnings(wilcox.test(one, mu=0, exact=FALSE, conf.int=TRUE))
    many <- replicate(input$B, {
      z <- draw_wilcox(input$wx_n, input$wx_shift, input$wx_scale, input$wx_shape)
      ans <- suppressWarnings(wilcox.test(z, mu=0, exact=FALSE))
      c(estimate=median(z), statistic=unname(ans$statistic), p=ans$p.value)
    })
    list(x=one, fit=fit, sim=t(many), truth=input$wx_shift, shape=input$wx_shape)
  }, ignoreNULL = FALSE)

  two_t <- eventReactive(input$generate, {
    req(input$tt_sd1 > 0, input$tt_sd2 > 0, input$tt_n1 >= 3, input$tt_n2 >= 3)
    set.seed(as.integer(input$seed) + input$generate + 33L)
    x <- rnorm(input$tt_n1, input$tt_mu1, input$tt_sd1)
    y <- rnorm(input$tt_n2, input$tt_mu2, input$tt_sd2)
    fit <- t.test(x, y)
    many <- replicate(input$B, {
      a <- rnorm(input$tt_n1, input$tt_mu1, input$tt_sd1)
      b <- rnorm(input$tt_n2, input$tt_mu2, input$tt_sd2)
      ans <- t.test(a,b)
      c(estimate=mean(a)-mean(b), statistic=unname(ans$statistic), p=ans$p.value)
    })
    list(x=x,y=y,fit=fit,sim=t(many),truth=input$tt_mu1-input$tt_mu2)
  }, ignoreNULL = FALSE)

  cor_dat <- eventReactive(input$generate, {
    req(abs(input$cr_rho) < 1, input$cr_n >= 4)
    set.seed(as.integer(input$seed) + input$generate + 44L)
    make_pair <- function(n, rho) {
      x <- rnorm(n); y <- rho*x + sqrt(1-rho^2)*rnorm(n); list(x=x,y=y)
    }
    one <- make_pair(input$cr_n, input$cr_rho)
    fit <- cor.test(one$x, one$y)
    many <- replicate(input$B, {
      z <- make_pair(input$cr_n, input$cr_rho); ans <- cor.test(z$x,z$y)
      c(estimate=unname(ans$estimate), statistic=unname(ans$statistic), p=ans$p.value)
    })
    list(x=one$x,y=one$y,fit=fit,sim=t(many),truth=input$cr_rho)
  }, ignoreNULL = FALSE)

  output$ot_single_plot <- renderPlot({
    z <- one_t(); par(mfrow=c(1,2),mar=c(4,4,3,1))
    stripchart(z$x, method="jitter", vertical=FALSE, pch=19, col="#1479a8", cex=1.15,
               xlab="Observed values", main="The generated sample", ylab="")
    abline(v=z$null,col="#dd5b45",lty=3,lwd=2); abline(v=mean(z$x),col="#17324d",lwd=3)
    df <- length(z$x)-1; xx <- seq(-4.5,4.5,length.out=500); yy <- dt(xx,df)
    plot(xx,yy,type="l",lwd=2,col="#17324d",xlab="t statistic under H0",ylab="Density",main="Reference distribution",bty="l")
    obs <- unname(z$fit$statistic); tails <- abs(xx)>=abs(obs); polygon(c(xx[tails & xx<0],rev(xx[tails & xx<0])),c(yy[tails & xx<0],rep(0,sum(tails & xx<0))),col="#dd5b45",border=NA)
    polygon(c(xx[tails & xx>0],rev(xx[tails & xx>0])),c(yy[tails & xx>0],rep(0,sum(tails & xx>0))),col="#dd5b45",border=NA); lines(xx,yy,lwd=2); abline(v=obs,lwd=2,lty=2,col="#a63423")
  }, res=110)
  output$ot_single_metrics <- renderUI({z<-one_t(); div(class="metrics",metric("Sample mean",sprintf("%.3f",mean(z$x))),metric("t statistic",sprintf("%.3f",z$fit$statistic)),metric("p-value",fmt_p(z$fit$p.value)),metric("Decision",decision_text(z$fit$p.value,input$alpha)))})
  output$ot_single_text <- renderUI({z<-one_t(); HTML(sprintf("If H<sub>0</sub> were true, the two red tails contain test statistics at least as extreme as the observed t = <strong>%.3f</strong>. Their total probability is p = <strong>%s</strong>.",z$fit$statistic,fmt_p(z$fit$p.value)))})
  output$ot_many_plot <- renderPlot({z<-one_t();par(mfrow=c(1,2),mar=c(4,4,3,1));stat_hist(z$sim[,"statistic"],0,xlab="t statistic");pvalue_hist(z$sim[,"p"],input$alpha)},res=110)
  output$ot_many_metrics <- renderUI({z<-one_t();r<-mean(z$sim[,"p"]<input$alpha);div(class="metrics",metric(if(z$truth==z$null)"Type I error" else "Power",sprintf("%.1f%%",100*r)),metric("True mean",z$truth),metric("Mean estimate",sprintf("%.3f",mean(z$sim[,"estimate"]))),metric("Experiments",nrow(z$sim)))})
  output$ot_many_text <- renderUI({z<-one_t();r<-mean(z$sim[,"p"]<input$alpha);HTML(sprintf("The null is <strong>%s</strong>. The test rejected in <strong>%.1f%%</strong> of repeated experiments. When the null is true this is the Type I error rate; otherwise it estimates power.",if(z$truth==z$null)"true" else "false",100*r))})

  output$wx_single_plot <- renderPlot({z<-wilcox_dat();par(mfrow=c(1,2),mar=c(4,4,3,1));hist(z$x,breaks="FD",col="#b9ddeb",border="white",xlab="Observed values",main="The generated sample",bty="l");abline(v=0,col="#dd5b45",lty=3,lwd=2);abline(v=median(z$x),col="#17324d",lwd=3);absr<-rank(abs(z$x));cols<-ifelse(z$x>=0,"#1479a8","#dd5b45");barplot(absr,names.arg=ifelse(z$x>=0,"+","−"),col=cols,border=NA,xlab="Sign of each observation",ylab="Rank of |X|",main="Signed ranks")},res=110)
  output$wx_single_metrics <- renderUI({z<-wilcox_dat();div(class="metrics",metric("Sample median",sprintf("%.3f",median(z$x))),metric("V statistic",sprintf("%.1f",z$fit$statistic)),metric("p-value",fmt_p(z$fit$p.value)),metric("Decision",decision_text(z$fit$p.value,input$alpha)))})
  output$wx_single_text <- renderUI({z<-wilcox_dat();HTML(sprintf("The test ranks the absolute distances from zero, then compares positive and negative signed ranks. Here V = <strong>%.1f</strong> and p = <strong>%s</strong>.",z$fit$statistic,fmt_p(z$fit$p.value)))})
  output$wx_many_plot <- renderPlot({z<-wilcox_dat();par(mfrow=c(1,2),mar=c(4,4,3,1));stat_hist(z$sim[,"statistic"],xlab="V statistic");pvalue_hist(z$sim[,"p"],input$alpha)},res=110)
  output$wx_many_metrics <- renderUI({z<-wilcox_dat();r<-mean(z$sim[,"p"]<input$alpha);div(class="metrics",metric(if(z$truth==0)"Type I error" else "Power",sprintf("%.1f%%",100*r)),metric("True shift",z$truth),metric("Mean sample median",sprintf("%.3f",mean(z$sim[,"estimate"]))),metric("Experiments",nrow(z$sim)))})
  output$wx_many_text <- renderUI({z<-wilcox_dat();r<-mean(z$sim[,"p"]<input$alpha);tagList(HTML(sprintf("The test rejected in <strong>%.1f%%</strong> of experiments.",100*r)),if(z$shape=="exponential")div(class="definition caution","Caution: the signed-rank test assumes symmetry for its usual location interpretation. Skewness can change its null behavior.") else div(class="definition","With a symmetric population and zero shift, p-values should be approximately uniform."))})

  output$tt_single_plot <- renderPlot({z<-two_t();par(mfrow=c(1,2),mar=c(4,4,3,1));boxplot(z$x,z$y,names=c("Group 1","Group 2"),col=c("#b9ddeb","#f4c2b9"),border="#62788a",ylab="Observed values",main="The two samples");stripchart(list(z$x,z$y),vertical=TRUE,method="jitter",add=TRUE,pch=19,col=c("#1479a8","#dd5b45"));df<-unname(z$fit$parameter);xx<-seq(-4.5,4.5,length.out=500);plot(xx,dt(xx,df),type="l",lwd=2,xlab="t statistic under H0",ylab="Density",main="Reference distribution",bty="l");abline(v=z$fit$statistic,lwd=2,lty=2,col="#dd5b45")},res=110)
  output$tt_single_metrics <- renderUI({z<-two_t();div(class="metrics",metric("Mean difference",sprintf("%.3f",mean(z$x)-mean(z$y))),metric("t statistic",sprintf("%.3f",z$fit$statistic)),metric("p-value",fmt_p(z$fit$p.value)),metric("Decision",decision_text(z$fit$p.value,input$alpha)))})
  output$tt_single_text <- renderUI({z<-two_t();HTML(sprintf("Welch's test compares the observed mean difference with its estimated standard error. The observed t = <strong>%.3f</strong>, giving p = <strong>%s</strong>.",z$fit$statistic,fmt_p(z$fit$p.value)))})
  output$tt_many_plot <- renderPlot({z<-two_t();par(mfrow=c(1,2),mar=c(4,4,3,1));stat_hist(z$sim[,"estimate"],0,z$truth,xlab="Estimated mean difference");pvalue_hist(z$sim[,"p"],input$alpha)},res=110)
  output$tt_many_metrics <- renderUI({z<-two_t();r<-mean(z$sim[,"p"]<input$alpha);div(class="metrics",metric(if(z$truth==0)"Type I error" else "Power",sprintf("%.1f%%",100*r)),metric("True difference",z$truth),metric("Mean estimate",sprintf("%.3f",mean(z$sim[,"estimate"]))),metric("Experiments",nrow(z$sim)))})
  output$tt_many_text <- renderUI({z<-two_t();r<-mean(z$sim[,"p"]<input$alpha);HTML(sprintf("The true mean difference is <strong>%.3f</strong>. Across repeated samples, <strong>%.1f%%</strong> of tests rejected H<sub>0</sub>.",z$truth,100*r))})

  output$cr_single_plot <- renderPlot({z<-cor_dat();par(mfrow=c(1,2),mar=c(4,4,3,1));plot(z$x,z$y,pch=19,col=adjustcolor("#1479a8",.7),xlab="X",ylab="Y",main="The generated pairs",bty="l");grid(col="#dfe7ed");points(z$x,z$y,pch=19,col=adjustcolor("#1479a8",.7));abline(lm(z$y~z$x),col="#dd5b45",lwd=3);df<-length(z$x)-2;xx<-seq(-4.5,4.5,length.out=500);plot(xx,dt(xx,df),type="l",lwd=2,xlab="t statistic under H0",ylab="Density",main="Reference distribution",bty="l");abline(v=z$fit$statistic,lwd=2,lty=2,col="#dd5b45")},res=110)
  output$cr_single_metrics <- renderUI({z<-cor_dat();div(class="metrics",metric("Sample correlation",sprintf("%.3f",z$fit$estimate)),metric("t statistic",sprintf("%.3f",z$fit$statistic)),metric("p-value",fmt_p(z$fit$p.value)),metric("Decision",decision_text(z$fit$p.value,input$alpha)))})
  output$cr_single_text <- renderUI({z<-cor_dat();HTML(sprintf("The sample correlation is <strong>r = %.3f</strong>. If the population correlation were zero, a correlation at least this extreme would occur with probability p = <strong>%s</strong>.",z$fit$estimate,fmt_p(z$fit$p.value)))})
  output$cr_many_plot <- renderPlot({z<-cor_dat();par(mfrow=c(1,2),mar=c(4,4,3,1));stat_hist(z$sim[,"estimate"],0,z$truth,xlab="Sample correlation, r");pvalue_hist(z$sim[,"p"],input$alpha)},res=110)
  output$cr_many_metrics <- renderUI({z<-cor_dat();r<-mean(z$sim[,"p"]<input$alpha);div(class="metrics",metric(if(z$truth==0)"Type I error" else "Power",sprintf("%.1f%%",100*r)),metric("True correlation",sprintf("%.2f",z$truth)),metric("Mean sample r",sprintf("%.3f",mean(z$sim[,"estimate"]))),metric("Experiments",nrow(z$sim)))})
  output$cr_many_text <- renderUI({z<-cor_dat();r<-mean(z$sim[,"p"]<input$alpha);HTML(sprintf("The true correlation is <strong>%.2f</strong>. The test rejected zero correlation in <strong>%.1f%%</strong> of experiments.",z$truth,100*r))})
}

shinyApp(ui, server)
