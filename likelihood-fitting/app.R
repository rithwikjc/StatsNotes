# The interactive app. All the statistics live in likelihood.R; this file only
# lays out the page and connects sliders and buttons to those functions.
#
# Run locally:  shiny::runApp("likelihood-fitting")

library(shiny)
source("likelihood.R")


# ── Page layout ──────────────────────────────────────────────────────────────
# Top: the true model that generates the data, across the full width.
# Below: the score of the candidate line and the two plots, with the
# sliders in a column on the right.

# Panels are white cards with a coloured stripe on the left. The stripe matches
# what the panel controls in the plots: black = true model, blue = candidate
# model, orange = likelihood.
panel_css <- "
  /* Page: if the window is taller than the app, centre the app vertically */
  body { min-height: 100vh; display: flex; flex-direction: column;
         justify-content: center; }

  /* Panels */
  .well { background: #fff; border: 1px solid #e4e4e4; border-left: 4px solid #bbb;
          border-radius: 8px; box-shadow: 0 1px 3px rgba(0, 0, 0, 0.06); }
  .stripe-true { border-left-color: #333; }
  .stripe-cand { border-left-color: #0072B2; background: #f5f9fd; }
  .stripe-lik  { border-left-color: #F0A848; background: #fff8ec; }

  /* True-model row: no up/down arrows in the number boxes (in narrow windows
     they cover the number); typing and the keyboard arrows still work */
  .stripe-true input[type=number] { appearance: textfield; -moz-appearance: textfield; }
  .stripe-true input[type=number]::-webkit-inner-spin-button { -webkit-appearance: none; margin: 0; }

  /* Buttons, tinted to match their panel */
  .btn-true { background: #eeeeee; border-color: #cccccc; color: #333333; }
  .btn-lik  { background: #fde4cc; border-color: #f2c08f; color: #7a4210; }
  .btn-true:hover, .btn-true:focus { background: #e2e2e2; }
  .btn-lik:hover,  .btn-lik:focus  { background: #fbd5b0; color: #7a4210; }

  /* Candidate panel: heading and the Raw | Centered switch on one row, drawn
     as a pair of buttons (radio buttons with their dots hidden) */
  .panel-head { display: flex; flex-wrap: wrap; align-items: center; }
  .panel-head .form-group { margin: 0 0 0 auto; }   /* pushed to the right */
  #centered .radio-inline { padding: 0; margin: 0; }
  #centered input { display: none; }
  #centered span { display: inline-block; padding: 2px 10px; font-size: 12px;
                   border: 1px solid #a9cbe8; background: #fff; color: #1f4e79;
                   cursor: pointer; }
  #centered .radio-inline:first-child span { border-radius: 4px 0 0 4px; }
  #centered .radio-inline:last-child span  { border-radius: 0 4px 4px 0; border-left: 0; }
  #centered input:checked + span { background: #0072B2; color: #fff; }
  .model-line { font-size: 12px; color: #555; margin: 4px 0 8px; }

  /* Readout: numbers side by side, at the right, next to the button */
  .readout-row   { display: flex; justify-content: flex-end; align-items: center;
                   gap: 40px; }
  .readout       { display: flex; gap: 40px; align-items: flex-end; text-align: right; }
  .readout-title { font-size: 14px; font-weight: 600; color: #6b5a3e; }
  .readout-value { font-size: 22px; font-weight: 600; line-height: 1.2; }
  .readout-label { font-size: 12px; color: #6b5a3e; }
  .readout-text  { font-size: 14px; }

  /* Help */
  .help-text   { font-size: 13px; color: #555; }
  .help-text p { margin: 6px 0 0; }

  /* Plots */
  #data_plot, #likelihood_plot { min-height: 300px; max-height: 400px; }
"

# The plots take the window's height minus room for everything above and
# below them, so the whole app fits on one screen; between 300 and 400px
# (taller, and the 3D plot zooms in so far that its edges get cut off).
plot_height <- "calc(100vh - 400px)"

# When R is slower than the sliders (as in the browser version), slider values
# pile up and R would redraw every one of them in turn. This small piece of
# JavaScript holds them back while R is busy, keeps only the latest value of
# each slider, and sends it as soon as R is free.
send_when_idle_js <- "
$(function() {
  var busy = false, waiting = {};
  $(document).on('shiny:busy', function() { busy = true; });
  $(document).on('shiny:idle', function() {
    busy = false;
    for (var name in waiting) Shiny.setInputValue(name, waiting[name]);
    waiting = {};
  });
  $(document).on('shiny:inputchanged', function(e) {
    if (busy && ['b0', 'b1', 'sigma'].includes(e.name)) {
      waiting[e.name] = e.value;
      e.preventDefault();
    }
  });
});
"

# Slider tick labels at round numbers. The ticks start at the slider's minimum
# and split its range into n equal parts (Shiny picks n itself otherwise).
round_ticks <- function(slider, n) {
  htmltools::tagQuery(slider)$find("input")$removeAttrs("data-grid-num")$
    addAttrs(`data-grid-num` = n)$allTags()
}

# Plotly's JavaScript library, loaded with the page. Without this, in the
# browser version the first plot can try to draw before the library has
# arrived, and stays empty. (A built plotly plot lists the files it needs.)
plotly_library <- plotly_build(plot_ly(type = "scatter", mode = "markers"))$dependencies

# On the StatsNotes page the app runs in the browser (webR, whose R reports
# its operating system as "emscripten"), and the page already shows the title.
on_web <- R.version$os == "emscripten"

ui <- fluidPage(
  tags$head(tags$style(HTML(panel_css)), tags$script(HTML(send_when_idle_js)),
            plotly_library),
  if (!on_web) titlePanel("Linear regression by likelihood maximization"),

  wellPanel(class = "stripe-true",
    fluidRow(
      column(2, strong("True model"), br(), "(generates the data)"),
      column(1, numericInput("true_b0", "β₀", 1)),
      column(1, numericInput("true_b1", "β₁", 0.3)),
      column(1, numericInput("true_sigma", "σ", 0.3)),
      column(3, textInput("sleep_levels", "Sleep levels (h)", "4, 5, 6, 7, 8")),
      column(1, numericInput("blocks", "Blocks", 4)),
      column(1, numericInput("seed", "Seed", 42)),
      column(2, br(), actionButton("new_data", "New data", class = "btn-true"))
    )
  ),

  fluidRow(
    column(9,
      wellPanel(class = "stripe-lik",
        div(class = "readout-row",
          uiOutput("readout", class = "readout"),
          actionButton("jump_to_mle", "Jump to MLE fit", class = "btn-lik")
        )
      ),
      fluidRow(
        column(6,
          plotlyOutput("data_plot", height = plot_height),
          checkboxGroupInput("show_2d", NULL, inline = TRUE,
            choices = c("True line" = "true", "MLE fit" = "mle",
                        "Residuals" = "residuals", "±1σ and ±2σ bands" = "bands"),
            selected = "bands")
        ),
        column(6,
          plotlyOutput("likelihood_plot", height = plot_height),
          checkboxGroupInput("show_3d", NULL, inline = TRUE,
            choices = c("True line" = "true", "MLE fit" = "mle",
                        "Residuals" = "residuals", "Likelihoods" = "likelihoods",
                        "Log scale" = "log"),
            selected = "likelihoods")
        )
      )
    ),
    column(3,
      wellPanel(class = "stripe-cand",
        div(class = "panel-head",
          strong("Candidate model"),
          radioButtons("centered", NULL, inline = TRUE, selected = "centered",
                       choices = c("Raw" = "raw", "Centered" = "centered"))
        ),
        div(class = "model-line", textOutput("model_line")),
        round_ticks(sliderInput("b0", "β₀ (d′ at average sleep)", 1, 5, 2,
                                step = 0.05), 8),
        round_ticks(sliderInput("b1", "β₁", -0.5, 1, 0, step = 0.05), 6),
        round_ticks(sliderInput("sigma", "σ", 0.1, 1.5, 0.5, step = 0.05), 7)
      ),
      wellPanel(class = "help-text",
        strong("How to use"),
        p("Drag the sliders to find the candidate model that makes the data most likely."),
        p("In the 3D plot, the likelihood of a point is the height of the probability",
          "density curve at that point, shown by the orange dot. With", strong("Log scale"),
          "on, those heights are log-likelihoods, and the candidate model log-likelihood",
          "is their sum."),
        p("The", strong("MLE fit"), "(maximum likelihood estimate) is the candidate",
          "with the highest log-likelihood.")
      )
    )
  )
)


# ── Formatting numbers for the readout ───────────────────────────────────────
# A proper minus sign (−) instead of a hyphen, rounded to `digits` decimals
with_minus <- function(x, digits = 2) sub("-", "−", sprintf("%.*f", digits, x))

# The likelihood itself is far too small to compute directly (it rounds to 0),
# so we write it as a power of ten using its log:
#   likelihood = e^loglik = 10^(loglik / log(10))
as_power_of_ten <- function(loglik) {
  p <- loglik / log(10)
  HTML(sprintf("%.1f × 10<sup>%d</sup>", 10^(p - floor(p)), floor(p)))
}


# ── Connecting inputs to the math ────────────────────────────────────────────
server <- function(input, output, session) {

  # The data: regenerated whenever the true model or the design changes
  sleep_levels <- reactive({
    levels <- suppressWarnings(as.numeric(strsplit(input$sleep_levels, ",")[[1]]))
    levels <- levels[!is.na(levels)]
    validate(need(length(unique(levels)) >= 2,
                  "Enter at least two different sleep levels, separated by commas."))
    levels
  })

  dataset <- reactive({
    req(input$true_b0, input$true_b1, input$true_sigma > 0, input$blocks >= 1, input$seed)
    validate(need(length(sleep_levels()) * input$blocks >= 3,
                  "Need at least 3 observations (with 2, the line fits them exactly)."))
    simulate_data(input$true_b0, input$true_b1, input$true_sigma,
                  sleep_levels(), input$blocks, input$seed)
  })

  # The MLE fit (maximum likelihood estimate) for the current data
  mle <- reactive(mle_fit(dataset()))

  # The true line and the MLE fit as c(intercept, slope, sigma), for the plots
  true_params <- reactive(c(input$true_b0, input$true_b1, input$true_sigma))
  mle_params  <- reactive(c(mle()$b0_hat, mle()$b1_hat, mle()$sigma_hat))

  # Centering. Sleep is measured from 0 hours (raw) or from its average
  # (centered); `center` is that starting point. The β₀ slider and the MLE fit's
  # β̂₀ are shown in the chosen form; the math and the plots use the raw line.
  # b0_form records which form the β₀ slider's value is in right now.
  b0_form       <- reactiveVal("centered")
  average_sleep <- reactive(mean(sleep_levels()))
  center  <- reactive(if (b0_form() == "centered") average_sleep() else 0)
  cand_b0 <- reactive(raw_intercept(input$b0, input$b1, center()))
  mle_b0  <- reactive(centered_intercept(mle()$b0_hat, mle()$b1_hat, center()))

  output$model_line <- renderText({
    if (b0_form() == "centered") {
      sprintf("d′ = β₀ + β₁ · (sleep − %g)", round(average_sleep(), 2))
    } else {
      "d′ = β₀ + β₁ · sleep"
    }
  })

  # Flipping the switch keeps the same line: β₀ is converted to the other form,
  # and its slider gets the matching label and range. The slider's new value
  # takes a moment to come back from the browser, so b0_form only changes once
  # it has arrived; until then the old value is still read the old way.
  pending_form <- reactiveVal(NULL)
  observeEvent(input$centered, ignoreInit = TRUE, {
    if (input$centered == "centered") {
      new_b0 <- centered_intercept(input$b0, input$b1, average_sleep())
      updateSliderInput(session, "b0", label = "β₀ (d′ at average sleep)",
                        min = 1, max = 5, value = new_b0)
    } else {
      new_b0 <- raw_intercept(input$b0, input$b1, average_sleep())
      updateSliderInput(session, "b0", label = "β₀ (d′ at 0 h sleep)",
                        min = -1, max = 3, value = new_b0)
    }
    if (abs(round(new_b0, 2) - input$b0) < 1e-9) {
      b0_form(input$centered)        # same number in both forms: nothing to wait for
    } else {
      pending_form(input$centered)
    }
  })
  observeEvent(input$b0, {
    if (!is.null(pending_form())) {
      b0_form(pending_form())
      pending_form(NULL)
    }
  })

  # New data = same true model, new random seed (shown in the seed box)
  observeEvent(input$new_data, {
    updateNumericInput(session, "seed", value = sample(1:9999, 1))
  })

  # Move the sliders to the MLE fit
  observeEvent(input$jump_to_mle, {
    updateSliderInput(session, "b0", value = mle_b0())
    updateSliderInput(session, "b1", value = mle()$b1_hat)
    updateSliderInput(session, "sigma", value = mle()$sigma_hat)
  })

  # The data with the candidate line
  output$data_plot <- renderPlotly({
    plot_data_fit(dataset(), cand_b0(), input$b1, input$sigma,
      true_line      = if ("true" %in% input$show_2d) true_params(),
      mle_line       = if ("mle" %in% input$show_2d) mle_params(),
      show_residuals = "residuals" %in% input$show_2d,
      show_bands     = "bands" %in% input$show_2d)
  })

  # Each observation lifted to its likelihood (3D). Every slider move redraws
  # the plot, which would reset the view, so we remember the view whenever the
  # user changes it, and put it back on each redraw. Rotating changes the
  # camera; zooming (with no perspective) changes the box's aspect ratio.
  camera <- reactiveVal(NULL)
  aspect <- reactiveVal(NULL)
  observeEvent(event_data("plotly_relayout", source = "likelihood_3d"), {
    changed <- event_data("plotly_relayout", source = "likelihood_3d")
    if (!is.null(changed[["scene.camera"]]))      camera(changed[["scene.camera"]])
    if (!is.null(changed[["scene.aspectratio"]])) aspect(changed[["scene.aspectratio"]])
  })

  output$likelihood_plot <- renderPlotly({
    p <- plot_likelihood_3d(dataset(), cand_b0(), input$b1, input$sigma,
      true_line        = if ("true" %in% input$show_3d) true_params(),
      mle_line         = if ("mle" %in% input$show_3d) mle_params(),
      show_residuals   = "residuals" %in% input$show_3d,
      show_likelihoods = "likelihoods" %in% input$show_3d,
      log_scale        = "log" %in% input$show_3d)   # (density surface: always on)
    if (!is.null(isolate(camera()))) {
      p <- layout(p, scene = list(camera = isolate(camera())))
    }
    if (!is.null(isolate(aspect()))) {
      p <- layout(p, scene = list(aspectratio = isolate(aspect())))
    }
    event_register(p, "plotly_relayout")
  })

  # How good is the candidate? The MLE fit's values in smaller print and the
  # maximum log-likelihood, then the candidate's log-likelihood (nearest the
  # sliders).
  output$readout <- renderUI({
    candidate <- data_log_likelihood(dataset(), cand_b0(), input$b1, input$sigma)
    tagList(
      div(div(class = "readout-title", "MLE fit"),
          div(class = "readout-text",
              sprintf("β̂₀ = %s,  β̂₁ = %s,  σ̂ = %s",
                      with_minus(mle_b0(), 3), with_minus(mle()$b1_hat, 3),
                      with_minus(mle()$sigma_hat, 3))),
          div(class = "readout-label", sprintf("n = %d observations", nrow(dataset())))),
      div(div(class = "readout-title", "Maximum log-likelihood"),
          div(class = "readout-value", with_minus(mle()$max_log_likelihood)),
          div(class = "readout-label", "likelihood = ",
              as_power_of_ten(mle()$max_log_likelihood))),
      div(div(class = "readout-title", "Candidate model log-likelihood"),
          div(class = "readout-value", with_minus(candidate)),
          div(class = "readout-label", "likelihood = ", as_power_of_ten(candidate)))
    )
  })
}

shinyApp(ui, server)
