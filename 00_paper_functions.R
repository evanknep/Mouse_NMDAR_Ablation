############################################################
# Manuscript functions: behavioral metrics and additive RLCK
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})


# Small helpers ------------------------------------------------------------

require_columns <- function(data, cols) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

check_choice_outcome <- function(choice, outcome = NULL) {
  if (!all(stats::na.omit(unique(choice)) %in% c(1L, 2L))) {
    stop("choice must be coded 1/2.", call. = FALSE)
  }
  if (!is.null(outcome) && !all(stats::na.omit(unique(outcome)) %in% c(0, 1))) {
    stop("outcome/reward must be coded 0/1.", call. = FALSE)
  }
  invisible(TRUE)
}

normalize_choice_12 <- function(choice, zero_one = FALSE) {
  case_when(
    choice %in% c(1, "1", "left", "Left", "LEFT", "L", "l") ~ 1L,
    choice %in% c(2, "2", "right", "Right", "RIGHT", "R", "r") ~ 2L,
    zero_one & choice %in% c(0, "0") ~ 2L,
    TRUE ~ suppressWarnings(as.integer(choice))
  )
}

softmax_vec <- function(x) {
  e <- exp(x - max(x))
  e / sum(e)
}

clip_prob <- function(p, eps = 1e-12) {
  pmax(pmin(p, 1 - eps), eps)
}

mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

finite_or_big <- function(x, big = 1e12) {
  if (!is.finite(x) || is.na(x)) big else x
}


# Behavioral metrics -------------------------------------------------------

calc_richness <- function(left_prob, right_prob) {
  (as.numeric(left_prob) + as.numeric(right_prob)) / 2
}

calc_delta_prob <- function(left_prob, right_prob, round_digits = NULL) {
  out <- abs(as.numeric(left_prob) - as.numeric(right_prob))
  if (!is.null(round_digits)) out <- round(out, round_digits)
  out
}

# Tied reward probabilities are ignored.
calc_p_best <- function(choice, left_prob, right_prob) {
  check_choice_outcome(choice)
  best_side <- case_when(
    left_prob > right_prob ~ 1L,
    right_prob > left_prob ~ 2L,
    TRUE ~ NA_integer_
  )
  mean_or_na(as.numeric(choice == best_side))
}

add_bandit_trial_vars <- function(data,
                                  choice_col = "choice",
                                  outcome_col = "outcome",
                                  left_prob_col = "left_prob",
                                  right_prob_col = "right_prob",
                                  group_cols = c("animal", "schedule")) {
  require_columns(data, c(choice_col, outcome_col, left_prob_col, right_prob_col))
  group_cols <- intersect(group_cols, names(data))

  data %>%
    mutate(
      choice = as.integer(.data[[choice_col]]),
      outcome = as.numeric(.data[[outcome_col]]),
      left_prob = as.numeric(.data[[left_prob_col]]),
      right_prob = as.numeric(.data[[right_prob_col]])
    ) %>%
    { check_choice_outcome(.$choice, .$outcome); . } %>%
    group_by(across(all_of(group_cols))) %>%
    arrange(across(any_of("trial")), .by_group = TRUE) %>%
    mutate(
      best_side = case_when(
        left_prob > right_prob ~ 1L,
        right_prob > left_prob ~ 2L,
        TRUE ~ NA_integer_
      ),
      best_choice = as.numeric(choice == best_side),
      chance = calc_richness(left_prob, right_prob),
      richness = chance,
      reward_minus_chance = outcome - chance,
      delta_prob = calc_delta_prob(left_prob, right_prob),
      prev_choice = lag(choice),
      prev_outcome = lag(outcome),
      switched = if_else(!is.na(prev_choice) & choice != prev_choice, 1, 0, missing = NA_real_),
      stayed = if_else(!is.na(prev_choice) & choice == prev_choice, 1, 0, missing = NA_real_),
      win_stay = if_else(prev_outcome == 1, stayed, NA_real_),
      lose_shift = if_else(prev_outcome == 0, switched, NA_real_)
    ) %>%
    ungroup()
}

mutual_information_categorical <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  joint <- table(x[keep], y[keep])
  if (sum(joint) == 0) return(NA_real_)

  joint <- joint / sum(joint)
  px <- rowSums(joint)
  py <- colSums(joint)
  mi <- 0

  for (i in seq_len(nrow(joint))) {
    for (j in seq_len(ncol(joint))) {
      if (joint[i, j] > 0) {
        mi <- mi + joint[i, j] * log2(joint[i, j] / (px[i] * py[j]))
      }
    }
  }
  mi
}

mutual_information_choice_history <- function(choice,
                                              outcome,
                                              history = c("choice_outcome", "choice_only")) {
  history <- match.arg(history)
  check_choice_outcome(choice, outcome)

  prev_choice <- lag(choice)
  prev_outcome <- lag(outcome)
  history_state <- if (history == "choice_outcome") {
    if_else(
      !is.na(prev_choice) & !is.na(prev_outcome),
      paste0(prev_choice, "_", prev_outcome),
      NA_character_
    )
  } else {
    as.character(prev_choice)
  }

  mutual_information_categorical(as.character(choice), history_state)
}

summarize_bandit_metrics <- function(data, group_cols = c("animal", "schedule")) {
  group_cols <- intersect(group_cols, names(data))
  needed <- c("best_choice", "switched", "stayed", "prev_outcome", "reward_minus_chance")
  if (!all(needed %in% names(data))) {
    data <- add_bandit_trial_vars(data, group_cols = group_cols)
  }

  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      reward_minus_chance = mean_or_na(reward_minus_chance),
      p_best = mean_or_na(best_choice),
      p_switch = mean_or_na(switched),
      win_stay = mean_or_na(stayed[prev_outcome == 1]),
      lose_shift = mean_or_na(switched[prev_outcome == 0]),
      p_switch_after_reward = mean_or_na(switched[prev_outcome == 1]),
      p_switch_after_loss = mean_or_na(switched[prev_outcome == 0]),
      negative_outcome_weight = (p_switch_after_loss - p_switch_after_reward) / p_switch,
      MI = mutual_information_choice_history(choice, outcome, "choice_outcome"),
      MI_choice_prev = mutual_information_choice_history(choice, outcome, "choice_only"),
      richness = mean_or_na(richness),
      delta_prob = mean_or_na(delta_prob),
      n_trials = n(),
      .groups = "drop"
    )
}

summarize_completed_trials <- function(data, group_cols = c("animal", "schedule")) {
  group_cols <- intersect(group_cols, names(data))
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(completed_trials = n(), .groups = "drop")
}

summarize_response_latency <- function(data,
                                       latency_col = "response_latency",
                                       group_cols = c("animal", "schedule")) {
  require_columns(data, latency_col)
  group_cols <- intersect(group_cols, names(data))
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      response_latency = mean_or_na(as.numeric(.data[[latency_col]])),
      n_trials = n(),
      .groups = "drop"
    )
}


# Additive-gains RLCK ------------------------------------------------------

rlck_spec <- function() {
  list(
    k = 4,
    nll_fun = nll_block_rlck_additive,
    init = c(0.4, 2.0, 0.3, 0.5),
    lower = c(0.001, 0.01, 0.001, -5.0),
    upper = c(1.0, 10.0, 0.999, 5.0),
    param_names = c("alpha", "beta", "alpha_ck", "beta_ck")
  )
}

check_rlck_par <- function(par) {
  if (length(par) != 4) {
    stop("RLCK parameters must be c(alpha, beta, alpha_ck, beta_ck).", call. = FALSE)
  }
  names(par) <- c("alpha", "beta", "alpha_ck", "beta_ck")
  par
}

clean_rlck_data <- function(data,
                            subject_col = "subject",
                            schedule_col = "schedule",
                            trial_col = "trial",
                            choice_col = "choice",
                            reward_col = "reward") {
  require_columns(data, c(subject_col, schedule_col, trial_col, choice_col, reward_col))

  data %>%
    transmute(
      subject = as.character(.data[[subject_col]]),
      schedule = as.character(.data[[schedule_col]]),
      trial = suppressWarnings(as.integer(.data[[trial_col]])),
      choice = suppressWarnings(as.integer(.data[[choice_col]])),
      reward = suppressWarnings(as.numeric(.data[[reward_col]])),
      left_prob = if ("left_prob" %in% names(data)) as.numeric(.data[["left_prob"]]) else NA_real_,
      right_prob = if ("right_prob" %in% names(data)) as.numeric(.data[["right_prob"]]) else NA_real_
    ) %>%
    filter(choice %in% c(1L, 2L), reward %in% c(0, 1)) %>%
    arrange(subject, schedule, trial) %>%
    { check_choice_outcome(.$choice, .$reward); . }
}

# One schedule only. Use nll_rlck() to reset at schedule boundaries.
nll_block_rlck_additive <- function(dat, par) {
  par <- check_rlck_par(par)
  require_columns(dat, c("choice", "reward"))
  check_choice_outcome(dat$choice, dat$reward)

  if ("schedule" %in% names(dat) && n_distinct(dat$schedule) > 1) {
    stop("nll_block_rlck_additive() accepts one schedule only. Use nll_rlck() for schedule-level resets.", call. = FALSE)
  }

  Q <- c(0.5, 0.5)
  CK <- c(0, 0)
  nll <- 0
  dat <- arrange(dat, across(any_of("trial")))

  for (i in seq_len(nrow(dat))) {
    a <- dat$choice[i]
    r <- dat$reward[i]
    p <- softmax_vec(par["beta"] * Q + par["beta_ck"] * CK)
    nll <- nll - log(clip_prob(p[a]))
    Q[a] <- Q[a] + par["alpha"] * (r - Q[a])
    CK[a] <- CK[a] + par["alpha_ck"] * (1 - CK[a])
    CK[3L - a] <- CK[3L - a] + par["alpha_ck"] * (0 - CK[3L - a])
  }

  finite_or_big(nll)
}

nll_block_rlck <- nll_block_rlck_additive
nll_block_rlck_recovery <- nll_block_rlck_additive

nll_rlck <- function(data, par) {
  require_columns(data, c("choice", "reward"))
  if (!"schedule" %in% names(data)) {
    warning("No schedule column found; treating data as one schedule.", call. = FALSE)
    return(nll_block_rlck_additive(data, par))
  }

  split(data, data$schedule, drop = TRUE) %>%
    map_dbl(~ nll_block_rlck_additive(.x, par)) %>%
    sum()
}

fit_rlck <- function(data, n_starts = 5, maxit = 300, seed = 42L) {
  require_columns(data, c("choice", "reward"))
  check_choice_outcome(data$choice, data$reward)

  spec <- rlck_spec()
  set.seed(seed)
  starts <- c(
    list(spec$init),
    replicate(
      max(0, n_starts - 1),
      stats::runif(4, spec$lower, spec$upper),
      simplify = FALSE
    )
  )
  best <- list(value = Inf, par = spec$init, convergence = NA_integer_)

  for (start in starts) {
    fit <- tryCatch(
      stats::optim(
        par = start,
        fn = function(p) nll_rlck(data, p),
        method = "L-BFGS-B",
        lower = spec$lower,
        upper = spec$upper,
        control = list(maxit = maxit, trace = 0)
      ),
      error = function(e) NULL
    )
    if (!is.null(fit) && finite_or_big(fit$value) < best$value) best <- fit
  }

  tibble(
    alpha = unname(best$par[1]),
    beta = unname(best$par[2]),
    alpha_ck = unname(best$par[3]),
    beta_ck = unname(best$par[4]),
    nll = finite_or_big(best$value),
    converged = isTRUE(best$convergence == 0),
    convergence = best$convergence
  )
}

fit_rlck_recovery <- function(data, n_starts = 5, maxit = 200, seed = 42L) {
  spec <- rlck_spec()
  fit_rlck(data, n_starts = n_starts, maxit = maxit, seed = seed) %>%
    transmute(
      alpha_hat = alpha,
      beta_hat = beta,
      alpha_ck_hat = alpha_ck,
      beta_ck_hat = beta_ck,
      nll = nll,
      converged = converged
    )
}

fit_rlck_by_subject <- function(data,
                                subject_col = "subject",
                                schedule_col = "schedule",
                                trial_col = "trial",
                                choice_col = "choice",
                                reward_col = "reward",
                                n_starts = 5,
                                maxit = 300) {
  dat <- clean_rlck_data(data, subject_col, schedule_col, trial_col, choice_col, reward_col)

  dat %>%
    group_by(subject) %>%
    group_split() %>%
    map_dfr(function(x) {
      fit_rlck(x, n_starts = n_starts, maxit = maxit) %>%
        mutate(subject = x$subject[1], n_trials = nrow(x), .before = 1)
    })
}

# One-step predictions reset Q and CK at schedule boundaries.
predict_rlck <- function(data, alpha, beta, alpha_ck, beta_ck) {
  par <- check_rlck_par(c(alpha, beta, alpha_ck, beta_ck))
  require_columns(data, c("choice", "reward"))
  check_choice_outcome(data$choice, data$reward)

  if (!"schedule" %in% names(data)) {
    warning("No schedule column found; treating data as one schedule.", call. = FALSE)
    data$schedule <- "schedule_1"
  }

  data %>%
    group_by(across(any_of(c("subject", "animal", "schedule")))) %>%
    group_modify(function(.x, .y) {
      .x <- arrange(.x, across(any_of("trial")))
      Q <- c(0.5, 0.5)
      CK <- c(0, 0)
      n <- nrow(.x)
      p_left <- p_right <- p_chosen <- numeric(n)
      pred_choice <- integer(n)

      for (i in seq_len(n)) {
        a <- .x$choice[i]
        r <- .x$reward[i]
        p <- softmax_vec(par["beta"] * Q + par["beta_ck"] * CK)
        p_left[i] <- p[1]
        p_right[i] <- p[2]
        p_chosen[i] <- p[a]
        pred_choice[i] <- if (p[1] >= p[2]) 1L else 2L
        Q[a] <- Q[a] + par["alpha"] * (r - Q[a])
        CK[a] <- CK[a] + par["alpha_ck"] * (1 - CK[a])
        CK[3L - a] <- CK[3L - a] + par["alpha_ck"] * (0 - CK[3L - a])
      }

      mutate(
        .x,
        p_left = p_left,
        p_right = p_right,
        p_chosen = p_chosen,
        pred_choice = pred_choice,
        correct_class = as.integer(pred_choice == choice)
      )
    }) %>%
    ungroup()
}

summarize_prediction_accuracy <- function(pred_data, group_cols = character()) {
  require_columns(pred_data, c("choice", "p_left", "p_chosen", "correct_class"))
  group_cols <- intersect(group_cols, names(pred_data))

  pred_data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n_trials = n(),
      accuracy = mean_or_na(correct_class),
      loglik = sum(log(clip_prob(p_chosen)), na.rm = TRUE),
      brier = mean_or_na((as.integer(choice == 1L) - p_left)^2),
      .groups = "drop"
    )
}


# Simulation, recovery, and PPC -------------------------------------------

simulate_rlck_schedule <- function(prob_df, alpha, beta, alpha_ck, beta_ck) {
  require_columns(prob_df, c("left_prob", "right_prob"))
  par <- check_rlck_par(c(alpha, beta, alpha_ck, beta_ck))

  Q <- c(0.5, 0.5)
  CK <- c(0, 0)
  n <- nrow(prob_df)
  choice <- integer(n)
  reward <- numeric(n)
  prob_left <- numeric(n)
  prob_right <- numeric(n)

  for (i in seq_len(n)) {
    p <- softmax_vec(par["beta"] * Q + par["beta_ck"] * CK)
    a <- sample(1:2, 1, prob = p)
    r <- stats::rbinom(1, 1, c(prob_df$left_prob[i], prob_df$right_prob[i])[a])
    choice[i] <- a
    reward[i] <- r
    prob_left[i] <- p[1]
    prob_right[i] <- p[2]
    Q[a] <- Q[a] + par["alpha"] * (r - Q[a])
    CK[a] <- CK[a] + par["alpha_ck"] * (1 - CK[a])
    CK[3L - a] <- CK[3L - a] + par["alpha_ck"] * (0 - CK[3L - a])
  }

  tibble(
    trial = seq_len(n),
    left_prob = prob_df$left_prob,
    right_prob = prob_df$right_prob,
    choice = choice,
    reward = reward,
    outcome = reward,
    prob_left = prob_left,
    prob_right = prob_right
  )
}

simulate_rlck_multiple_runs <- function(prob_df, alpha, beta, alpha_ck, beta_ck, n_runs = 100) {
  map_dfr(seq_len(n_runs), function(run_id) {
    simulate_rlck_schedule(prob_df, alpha, beta, alpha_ck, beta_ck) %>%
      mutate(run_id = run_id)
  })
}

simulate_rlck_subject <- function(prob_bundle, alpha, beta, alpha_ck, beta_ck) {
  map2_dfr(prob_bundle, seq_along(prob_bundle), function(prob_df, schedule_id) {
    simulate_rlck_schedule(prob_df, alpha, beta, alpha_ck, beta_ck) %>%
      mutate(schedule = as.character(schedule_id))
  })
}

simulate_p_best <- function(prob_df, alpha, beta, alpha_ck, beta_ck, n_runs = 1000) {
  simulate_rlck_multiple_runs(prob_df, alpha, beta, alpha_ck, beta_ck, n_runs) %>%
    summarize_bandit_metrics(group_cols = "run_id") %>%
    summarise(p_best = mean_or_na(p_best), .groups = "drop") %>%
    pull(p_best)
}

run_one_recovery <- function(sim_id,
                             alpha,
                             beta,
                             alpha_ck,
                             beta_ck,
                             prob_bundle,
                             n_starts = 5,
                             maxit = 200) {
  sim_dat <- simulate_rlck_subject(prob_bundle, alpha, beta, alpha_ck, beta_ck)
  fit <- fit_rlck_recovery(sim_dat, n_starts = n_starts, maxit = maxit)
  tibble(
    sim_id = sim_id,
    alpha_true = alpha,
    beta_true = beta,
    alpha_ck_true = alpha_ck,
    beta_ck_true = beta_ck
  ) %>%
    bind_cols(fit)
}

summarize_recovery <- function(recovery_results) {
  recovery_results %>%
    pivot_longer(
      cols = matches("_(true|hat)$"),
      names_to = c("parameter", ".value"),
      names_pattern = "(.*)_(true|hat)"
    ) %>%
    group_by(parameter) %>%
    summarise(
      n = sum(!is.na(true) & !is.na(hat)),
      pearson_r = suppressWarnings(stats::cor(true, hat, method = "pearson", use = "complete.obs")),
      spearman_rho = suppressWarnings(stats::cor(true, hat, method = "spearman", use = "complete.obs")),
      bias = mean_or_na(hat - true),
      MAE = mean_or_na(abs(hat - true)),
      RMSE = sqrt(mean_or_na((hat - true)^2)),
      .groups = "drop"
    )
}

simulate_one_animal_ppc <- function(animal_id,
                                    alpha,
                                    beta,
                                    alpha_ck,
                                    beta_ck,
                                    sim_id,
                                    observed_blocks) {
  require_columns(observed_blocks, c("animal", "schedule", "prob_df"))

  observed_blocks %>%
    filter(animal == animal_id) %>%
    mutate(sim_df = map(prob_df, ~ simulate_rlck_schedule(.x, alpha, beta, alpha_ck, beta_ck))) %>%
    select(animal, schedule, sim_df) %>%
    unnest(sim_df) %>%
    mutate(
      sim_id = sim_id,
      animal = animal_id,
      alpha = alpha,
      beta = beta,
      alpha_ck = alpha_ck,
      beta_ck = beta_ck
    )
}

compute_ppc_metrics <- function(data) {
  if (!"reward" %in% names(data) && "outcome" %in% names(data)) data <- mutate(data, reward = outcome)
  if (!"source" %in% names(data)) data$source <- "unknown"
  if (!"sim_id" %in% names(data)) data$sim_id <- 0L

  data %>%
    add_bandit_trial_vars(
      choice_col = "choice",
      outcome_col = "reward",
      group_cols = c("animal", "schedule", "sim_id", "source")
    ) %>%
    summarize_bandit_metrics(group_cols = c("animal", "sim_id", "source"))
}

compare_ppc_metrics <- function(obs_metrics,
                                sim_metrics,
                                metric_cols = c("p_best", "reward_minus_chance", "p_switch",
                                                "win_stay", "lose_shift", "negative_outcome_weight")) {
  obs <- obs_metrics %>%
    filter(source == "observed" | sim_id == 0L) %>%
    select(animal, all_of(metric_cols)) %>%
    pivot_longer(all_of(metric_cols), names_to = "metric", values_to = "obs")

  sim <- sim_metrics %>%
    group_by(animal) %>%
    summarise(across(all_of(metric_cols), mean_or_na), .groups = "drop") %>%
    pivot_longer(all_of(metric_cols), names_to = "metric", values_to = "sim")

  left_join(obs, sim, by = c("animal", "metric"))
}


# Example ------------------------------------------------------------------

# source("R/00_paper_functions.R")
# dat <- clean_rlck_data(trial_df,
#                        subject_col = "animal",
#                        schedule_col = "schedule",
#                        trial_col = "trial",
#                        choice_col = "choice",
#                        reward_col = "outcome")
# one_id <- unique(dat$subject)[1]
# one_animal <- dplyr::filter(dat, subject == one_id)
# fit <- fit_rlck(one_animal, n_starts = 5)
# pred <- predict_rlck(one_animal, fit$alpha, fit$beta, fit$alpha_ck, fit$beta_ck)
# summarize_prediction_accuracy(pred, group_cols = "schedule")
#
# sim <- simulate_rlck_schedule(prob_df, alpha = 0.4, beta = 2.0, alpha_ck = 0.3, beta_ck = 0.5)
