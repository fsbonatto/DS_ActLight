
#--------------------------.
#--------------------------.
# Quantity and Timing -----
#--------------------------.
#--------------------------.


# db: dataset used in the statistical analyses
# quantity_median: median melanopic EDI during the evaluated timing window
db

# Btw and wth
db <- db |>
  dplyr::group_by(id) |> 
  dplyr::mutate(
    between = mean(quantity_median),
    within = quantity_median - between,
    
    log_between = mean(log_quantity_median),
    log_within = log_quantity_median - log_between
  ) 

## Step 1: Selecting the random effects structure -----
## Test random vs fixed intercept 

# fit random intercept model
b0 <- lme4::lmer(
  outcome ~ between + within + "{adjustment variables}" + (1|id), 
  data = db, 
  control = lme4::lmerControl(optimizer = "bobyqa"),
  REML = FALSE
)

# fit fixed intercept model
b1 <- lme4::lmer(
  outcome ~ between + within + "{adjustment variables}" + (within|id),
  data = db, 
  control = lme4::lmerControl(optimizer = "bobyqa"),
  REML = FALSE
)

# compare models
performance::compare_performance(b0, b1) 



## Step 2: Evaluating the func;onal form of the independent terms -----
## Testing functional form


# Adjust linear and log models 
mod_lin <- lme4::lmer(
  outcome ~ within + between + "{adjustment variables}" + (1|id), 
  data = db, 
  control = lme4::lmerControl(optimizer = "bobyqa"),
  REML = FALSE
)

mod_log <- lme4::lmer(
  outcome ~ log_within + log_between + "{adjustment variables}" + (1|id), 
  data = db, 
  control = lme4::lmerControl(optimizer = "bobyqa"),
  REML = FALSE
)

# compare models
performance::compare_performance(mod_lin, mod_log)




## Step 3: Fit rlmer -----

mod_robust <- robustlmm::rlmer(
  outcome ~ log_within + log_between + "{adjustment variables}" + (1|id), 
  data = db, 
  control = lme4::lmerControl(optimizer = "bobyqa")
)
# Extract parameter
coefs_robust <- parameters::model_parameters(mod_robust, effects = "fixed")

coefs_robust |> 
  dplyr::mutate(
    # In the lin-log model, the expected absolute change in the outcome
    # associated with a x% increase in light exposure is given by:
    # beta * log(1 + x/100).
    # Setting percent_increase = 100 represents a doubling of light exposure.
    percent_increase = 100,
    dplyr::across(
      c(Coefficient, CI_low, CI_high), \(x) x*log(1 + percent_increase/100)
    )
  )






#----------------------------.
#----------------------------.
# Duration and Frequency ----
#----------------------------.
#----------------------------.

library(dplyr)

# db: dataset used in the statistical analyses
db


#' Weekly mean with minimum valid days
#' Computes the mean of a numeric vector only if the number
#' of non-missing values is at least `min_valid_days`.
#' @param x Numeric vector.
#' @param min_valid_days Minimum number of valid days required.
#' @return Numeric value or `NA`.
weekly_mean <- function(x, min_valid_days = 4) {
  n_valid_days <- length(na.omit(x))
  if (n_valid_days < min_valid_days) {
    return(NA)
  } else {
    return(mean(x, na.rm = TRUE))
  }
}



db <- db |> 
  group_by(id) |> 
  arrange(date) |>
  mutate(
    # 7-day average of outcomes and adjust variables 
    across(
      .cols = c(outcome, wakeup_hour, ms, rdle),
      .fns  = \(x) {
        zoo::rollapply(
          x, 
          width = 7, 
          FUN = weekly_mean, 
          align = "right", 
          fill = NA, 
          partial = FALSE
        )
      },
      .names = "{col}_7d"
    ),
    
    # frequency: number of days in the last 7 days above a given duration
    frequency = zoo::rollapply(
      day_above_duration, 
      width = 7, 
      FUN = sum, 
      align = "right", 
      fill = NA, 
      partial = FALSE
    )
    
  )


# The analysis follows the same approach as above, 
# but uses frequency instead of quantity_median,
# and 7-day average of outcomes

