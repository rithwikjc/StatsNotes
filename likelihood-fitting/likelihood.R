# Linear regression by likelihood maximization
# ─────────────────────────────────────────────────────────────────────────────
# This script contains the implementation of likelihood maximization
# of a simple linear model.
# app.R connects these functions to an interface of sliders and buttons.

library(plotly)


# ── 1. The model ─────────────────────────────────────────────────────────────
#   dprime = beta_0 + beta_1 * hours_sleep + epsilon
# where, epsilon ~ Normal(0, sigma)
#
# Said another way, at each sleep level, dprime is drawn from a bell curve
# centered on the line, with spread sigma:
#   dprime ~ Normal(beta_0 + beta_1 * hours_sleep, sigma)

predict_mean <- function(hours_sleep, b0, b1) {
  b0 + b1 * hours_sleep
}

# Centering. beta_0 is the predicted dprime at 0 hours of sleep, far from the
# data. Measuring sleep from its average instead,
#   dprime = beta_0c + beta_1 * (hours_sleep - average_sleep)
# describes the same line: only the intercept changes, to the predicted
# dprime at average sleep. Converting between the two:
#   beta_0c = beta_0 + beta_1 * average_sleep

centered_intercept <- function(b0, b1, average_sleep)  b0  + b1 * average_sleep
raw_intercept      <- function(b0c, b1, average_sleep) b0c - b1 * average_sleep


# ── 2. Simulating data from the true model ───────────────────────────────────
# One session per sleep level, several blocks per session, one dprime per block.

simulate_data <- function(b0, b1, sigma, sleep_levels = 4:8, blocks = 4, seed = 42) {
  set.seed(seed)
  d <- data.frame(hours_sleep = rep(sleep_levels, each = blocks),
                  block       = rep(seq_len(blocks), times = length(sleep_levels)))
  d$dprime <- rnorm(nrow(d), mean = predict_mean(d$hours_sleep, b0, b1), sd = sigma)
  d
}


# ── 3. How likely is one observation? ────────────────────────────────────────
# A candidate (beta_0, beta_1, sigma) puts a bell curve at each sleep level.
# The height of that bell at the observed dprime is the observation's
# likelihood: high when the point sits close to the line (relative to sigma),
# low when it is far away.
# (log = TRUE returns log probability density of the point.)

point_likelihood <- function(d, b0, b1, sigma, log = FALSE) {
  dnorm(d$dprime, mean = predict_mean(d$hours_sleep, b0, b1), sd = sigma, log = log)
}


# ── 4. How likely is the whole dataset? ──────────────────────────────────────
# If the observations are independent, the likelihood of the whole dataset is
# the product of the individual likelihoods. Multiplying many small numbers
# quickly gives something absurdly tiny, so we work with logs instead: the log
# of a product is the sum of the logs. Maximizing the likelihood and maximizing
# the log likelihood are equivalent.
#
# (log = TRUE asks dnorm for log(height) directly. Same number, but it stays
# accurate for points so far from the line that their height rounds to 0.)

data_log_likelihood <- function(d, b0, b1, sigma) {
  sum(point_likelihood(d, b0, b1, sigma, log = TRUE))
}


# ── 5. The MLE fit: maximum likelihood ───────────────────────────────────────
# The MLE fit (maximum likelihood estimate) is the candidate with the highest
# log-likelihood. It is found in two steps: first the best line, then the best
# width for the bells around it.
#   - Best line: with bell-shaped errors, the most likely line is the one
#     closest to the points (smallest sum of squared residuals). lm() finds it.
#   - Best width: sigma should match how far the points typically sit from
#     that line, i.e. the root of the average squared residual.
#
# Careful: R's sigma(fit) divides by n - 2 instead of n. It is slightly larger
# and does not give the maximum likelihood.

mle_fit <- function(d) {
  fit       <- lm(dprime ~ hours_sleep, data = d)
  b0_hat    <- unname(coef(fit)[1])
  b1_hat    <- unname(coef(fit)[2])
  sigma_hat <- sqrt(mean(resid(fit)^2))
  list(b0_hat = b0_hat, b1_hat = b1_hat, sigma_hat = sigma_hat,
       max_log_likelihood = data_log_likelihood(d, b0_hat, b1_hat, sigma_hat))
}


# ── 6. Plots ─────────────────────────────────────────────────────────────────
# Colours used in both plots, chosen to be easy to tell apart (also for
# colourblind readers).
col_true <- "black"
col_cand <- "#0072B2"   # blue: the candidate model
col_mle  <- "#D55E00"   # red-orange: the MLE fit
col_lik  <- "#F0A848"   # soft orange: likelihood

# Axis limits shared by both plots. They depend only on the data, so they
# stay put while sliders move. The dprime axis has the data in the middle,
# with room above and below for candidate lines that miss them.
sleep_limits  <- function(d) range(d$hours_sleep) + c(-1, 1)
dprime_limits <- function(d) range(d$dprime) + c(-1.5, 1.5)


# 6a. The data with a candidate line and its residuals. true_line and mle_line
# are optional lines to compare against, given as c(intercept, slope, sigma);
# NULL leaves them out. The bands show +-1 and +-2 sigma around each line
# drawn, using that line's sigma.

plot_data_fit <- function(d, b0, b1, sigma, true_line = NULL, mle_line = NULL,
                          show_residuals = TRUE, show_bands = TRUE) {
  xlim <- sleep_limits(d)
  ylim <- dprime_limits(d)

  # Spread the blocks over a narrow strip so points at one sleep level
  # (and their residuals) don't sit on top of each other
  d$x  <- d$hours_sleep + 0.25 * (d$block - mean(d$block)) / max(d$block)
  d$mu <- predict_mean(d$hours_sleep, b0, b1)

  # The lines to draw, one row each: how it looks, and c(intercept, slope,
  # sigma). The candidate goes last so it is drawn on top; rank puts it first
  # in the legend.
  lines <- rbind(
    if (!is.null(true_line)) data.frame(label = "True line", colour = col_true,
                                        dash = "dash", rank = 2, b0 = true_line[1],
                                        b1 = true_line[2], sigma = true_line[3]),
    if (!is.null(mle_line))  data.frame(label = "MLE fit", colour = col_mle,
                                        dash = "solid", rank = 3, b0 = mle_line[1],
                                        b1 = mle_line[2], sigma = mle_line[3]),
    data.frame(label = "Candidate model", colour = col_cand, dash = "solid",
               rank = 1, b0 = b0, b1 = b1, sigma = sigma)
  )

  p <- plot_ly()

  if (show_bands) {
    # A +-2 sigma and a +-1 sigma band around every line drawn, each with that
    # line's own sigma. A band is a shape: along the top edge, back along the
    # bottom edge, filled in.
    for (i in seq_len(nrow(lines))) {
      line <- lines[i, ]
      mu   <- predict_mean(xlim, line$b0, line$b1)
      for (k in c(2, 1)) {
        p <- add_trace(p, type = "scatter", mode = "lines",
                       x = c(xlim, rev(xlim)),
                       y = c(mu + k * line$sigma, rev(mu - k * line$sigma)),
                       fill = "toself", fillcolor = line$colour,
                       opacity = if (k == 2) 0.08 else 0.12, line = list(width = 0),
                       hoverinfo = "skip", showlegend = FALSE)
      }
    }
    # Labels just inside the top edge of the candidate's bands, at the right
    p <- add_trace(p, type = "scatter", mode = "text",
                   x = rep(xlim[2] - 0.1, 2),
                   y = predict_mean(xlim[2], b0, b1) + c(1, 2) * sigma,
                   text = c("±1σ", "±2σ"), textposition = "bottom left",
                   textfont = list(color = col_cand), hoverinfo = "skip",
                   showlegend = FALSE)
  }
  if (show_residuals) {
    # From the line to each point, all in one trace with NA breaks between them
    p <- add_trace(p, type = "scatter", mode = "lines",
                   x = rep(d$x, each = 3), y = as.vector(rbind(d$mu, d$dprime, NA)),
                   line = list(color = col_cand, width = 1.5), opacity = 0.5,
                   hoverinfo = "skip", showlegend = FALSE)
  }
  for (i in seq_len(nrow(lines))) {
    line <- lines[i, ]
    p <- add_trace(p, type = "scatter", mode = "lines",
                   x = xlim, y = predict_mean(xlim, line$b0, line$b1),
                   line = list(color = line$colour, width = 3, dash = line$dash),
                   name = line$label, legendrank = line$rank, hoverinfo = "skip")
  }

  p |>
    add_trace(type = "scatter", mode = "markers", x = d$x, y = d$dprime,
              marker = list(color = "#333333", size = 8),
              text = sprintf("sleep = %g h<br>d′ = %.2f", d$hours_sleep, d$dprime),
              hoverinfo = "text", showlegend = FALSE) |>
    layout(
      xaxis  = list(title = "Hours of sleep", range = xlim, zeroline = FALSE),
      yaxis  = list(title = "d′", range = ylim, zeroline = FALSE),
      showlegend = TRUE,   # even when the candidate is the only line
      legend = list(orientation = "h", x = 0.5, xanchor = "center",
                    y = -0.2, yanchor = "top"),   # below the axis title
      margin = list(l = 50, r = 10, t = 10, b = 10)
    ) |>
    config(displayModeBar = FALSE)   # no toolbar: the view stays fixed
}


# 6b. Each observation lifted to its likelihood. At every sleep level the
# candidate puts a bell curve over dprime. Each data point (black, on the
# floor) is lifted straight up to the height of its bell (orange). Those
# heights are the likelihoods from section 3; section 4 adds up their logs.
# With log_scale = TRUE the heights are the logs themselves, so the
# log-likelihood of the data is the sum of the orange heights.

plot_likelihood_3d <- function(d, b0, b1, sigma, true_line = NULL, mle_line = NULL,
                               show_residuals = TRUE, show_likelihoods = TRUE,
                               log_scale = FALSE, show_surface = TRUE) {
  xlim <- sleep_limits(d)
  ylim <- dprime_limits(d)
  d$mu <- predict_mean(d$hours_sleep, b0, b1)

  # Height of the candidate's bell at y: the likelihood, or its log
  height <- function(y, mean) dnorm(y, mean, sigma, log = log_scale)

  # The height axis is set by the MLE fit's bells, so it stays put while
  # sliders move and you can see bells grow taller as sigma shrinks. The top
  # only moves if a bell outgrows it.
  peak_mle  <- dnorm(0, 0, mle_fit(d)$sigma_hat, log = log_scale)
  peak_cand <- dnorm(0, 0, sigma, log = log_scale)
  if (log_scale) {
    # Logs have no bottom, so the floor is put 8 units below the MLE fit's
    # peak. Points less likely than that sit on the floor.
    z_floor <- peak_mle - 8
    z_top   <- max(peak_mle + 0.5, peak_cand + 0.25)
  } else {
    z_floor <- 0
    z_top   <- max(1.3 * peak_mle, 1.05 * peak_cand)
  }

  # Bells (and the surface) span u = -u_max..u_max sigmas around the line:
  # +-4 sigma normally; on the log scale, out to where the bell meets the floor.
  u_max <- if (log_scale) sqrt(2 * max(peak_cand - z_floor, 0)) else 4
  u     <- seq(-u_max, u_max, length.out = 81)

  # One bell per sleep level. All bells go in one table, with an NA row between
  # them so plotly lifts the pen.
  bell_at <- function(x) {
    mu <- predict_mean(x, b0, b1)
    y  <- mu + sigma * u
    y  <- y[y >= ylim[1] & y <= ylim[2]]
    data.frame(x = c(rep(x, length(y)), NA), y = c(y, NA), z = c(height(y, mu), NA))
  }
  bells <- do.call(rbind, lapply(unique(d$hours_sleep), bell_at))

  # Each point lifted to its bell (points below the floor sit on it), with a
  # vertical stem from the floor
  d$z   <- pmax(height(d$dprime, d$mu), z_floor)
  stems <- data.frame(x = rep(d$hours_sleep, each = 3),
                      y = rep(d$dprime, each = 3),
                      z = as.vector(rbind(z_floor, d$z, NA)))

  likelihood <- point_likelihood(d, b0, b1, sigma)
  hover <- sprintf(
    "sleep = %g h<br>d′ = %.2f<br>likelihood = %.3f<br>log-likelihood = %.2f",
    d$hours_sleep, d$dprime, likelihood, log(likelihood))

  # (source = names this plot so app.R can listen to it; harmless elsewhere)
  p <- plot_ly(source = "likelihood_3d")

  if (show_surface) {
    # The bells at every sleep value, not just the observed ones: a faint ridge
    # along the line. Built on a grid that follows the line, so the ridge is
    # smooth even when sigma is small.
    us   <- seq(-u_max, u_max, length.out = 41)    # coarser than the bells: faster
    xs   <- seq(xlim[1], xlim[2], length.out = 25)
    xmat <- matrix(xs, nrow = length(us), ncol = length(xs), byrow = TRUE)
    ymat <- predict_mean(xmat, b0, b1) + sigma * us
    ymat <- pmin(pmax(ymat, ylim[1]), ylim[2])   # stop at the edge of the axis
    zmat <- height(ymat, predict_mean(xmat, b0, b1))
    # (rounded: 3 decimals is plenty for drawing, and keeps the data sent small)
    p <- add_surface(p, x = round(xmat, 3), y = round(ymat, 3), z = round(zmat, 3),
                     opacity = 0.25,
                     colorscale = list(c(0, "#e8f1f8"), c(1, col_cand)),
                     showscale = FALSE, hoverinfo = "skip")
  }
  if (show_residuals) {
    # Same residuals as the 2D plot, drawn on the floor
    resid <- data.frame(x = rep(d$hours_sleep, each = 3),
                        y = as.vector(rbind(d$mu, d$dprime, NA)))
    p <- add_trace(p, type = "scatter3d", mode = "lines", x = resid$x, y = resid$y,
                   z = z_floor, line = list(color = col_cand, width = 4),
                   opacity = 0.5, hoverinfo = "skip")
  }
  if (show_likelihoods) {
    p <- p |>
      add_trace(type = "scatter3d", mode = "lines",
                x = stems$x, y = stems$y, z = stems$z,
                line = list(color = "gray", width = 4), hoverinfo = "skip") |>
      add_trace(type = "scatter3d", mode = "markers",
                x = d$hours_sleep, y = d$dprime, z = d$z,
                marker = list(color = col_lik, size = 4),
                text = hover, hoverinfo = "text")
  }
  if (!is.null(true_line)) {
    p <- add_trace(p, type = "scatter3d", mode = "lines", x = xlim,
                   y = predict_mean(xlim, true_line[1], true_line[2]), z = z_floor,
                   line = list(color = col_true, width = 7, dash = "dash"),
                   hoverinfo = "skip")
  }
  if (!is.null(mle_line)) {
    p <- add_trace(p, type = "scatter3d", mode = "lines", x = xlim,
                   y = predict_mean(xlim, mle_line[1], mle_line[2]), z = z_floor,
                   line = list(color = col_mle, width = 6), hoverinfo = "skip")
  }

  p |>
    add_trace(type = "scatter3d", mode = "lines",
              x = bells$x, y = bells$y, z = bells$z,
              line = list(color = col_cand, width = 4), hoverinfo = "skip") |>
    add_trace(type = "scatter3d", mode = "lines",
              x = xlim, y = predict_mean(xlim, b0, b1), z = z_floor,
              line = list(color = col_cand, width = 6), hoverinfo = "skip") |>
    add_trace(type = "scatter3d", mode = "markers",
              x = d$hours_sleep, y = d$dprime, z = z_floor,
              marker = list(color = "black", size = 4), hoverinfo = "skip") |>
    layout(
      showlegend = FALSE,
      scene = list(
        xaxis = list(title = "Hours of sleep", range = xlim),
        yaxis = list(title = "d′", range = ylim),
        zaxis = list(title = if (log_scale) "Log-likelihood" else "Likelihood",
                     range = c(z_floor, z_top)),
        camera = list(eye = list(x = -1.61, y = -1.34, z = 0.86),
                      center = list(x = 0, y = 0, z = -0.15),   # box sits a bit higher
                      projection = list(type = "orthographic")),   # no perspective
        aspectmode = "manual", aspectratio = list(x = 1.3, y = 1.7, z = 0.9)
      ),
      margin = list(l = 0, r = 0, t = 0, b = 0)
    )
}
