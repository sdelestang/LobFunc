#' Pulls out the marginal means from a GAMM built using the gamm4 package
#'
#' Returns the marginal means and confidence intervals
#' @param mod the gamm4 model
#' @param lev is the level for which the means are wanted
#' @param smooth_avg sets the smooth term level, TRUE = average for all, FALSE = lowest observed value for all and a number (e.g. 2) can set the level. A vector of numbers can be used if there are multiple smooth terms (e.g. c(2,2))
#' @return Returns a data frame
#' @examples
#' library(gamm4)
#' library(dplyr)
#' library(magrittr)
#' set.seed(88)
#' dat <- expand.grid(year=factor(2000:2020), month=1:12)
#' dat %<>% mutate(temp=runif(nrow(dat), 50,100))
#' gm4 <- gamm4(log(temp)~year+s(month), data=dat)
#' out <- gammMeans(gm4,'year')
#' out
#' @import dplyr
#' @import magrittr
#' @import ggplot2
#' @import gamm4
#' @export
gammMeans <- function(mod, lev, smooth_avg = TRUE) {
  if (!inherits(mod, "gamm4")) stop("mod must be a gamm4 object")

  # Parametric terms
  param_terms <- attr(mod$gam$terms, "term.labels")
  if (!lev %in% param_terms) stop("lev must be a parametric term in the model")
  other_covs <- setdiff(param_terms, lev)

  # model frame
  mf <- mod$gam$model

  # smooth variable names
  smooth_vars <- if (length(mod$gam$smooth) > 0) {
    unlist(lapply(mod$gam$smooth, function(x) x$term))
  } else character(0)

  lev_levels <- levels(mf[[lev]])
  results_list <- vector("list", length(lev_levels))

  # ---- choose reference values ----

  # Factor covariates → global modal level
  factor_covs <- other_covs[sapply(other_covs, function(v) is.factor(mf[[v]]))]
  factor_vals <- lapply(factor_covs, function(v) {
    tab <- table(mf[[v]])
    chosen <- if (length(tab) == 0) NA_character_ else names(tab)[which.max(tab)]
    factor(chosen, levels = levels(mf[[v]]))
  })
  names(factor_vals) <- factor_covs

  # Numeric covariates → global mean
  numeric_covs <- other_covs[!other_covs %in% factor_covs]
  numeric_vals <- lapply(numeric_covs, function(v) mean(mf[[v]], na.rm = TRUE))
  names(numeric_vals) <- numeric_covs

  # Smooth covariates → mean or supplied value
  smooth_vals <- lapply(smooth_vars, function(sv) {
    if (isTRUE(smooth_avg)) {
      mean(mf[[sv]], na.rm = TRUE)
    } else if (is.numeric(smooth_avg)) {
      if (length(smooth_avg) == 1) smooth_avg else {
        idx <- match(sv, smooth_vars)
        if (!is.na(idx) && idx <= length(smooth_avg)) smooth_avg[idx] else mean(mf[[sv]], na.rm = TRUE)
      }
    } else {
      mean(mf[[sv]], na.rm = TRUE)
    }
  })
  names(smooth_vals) <- smooth_vars

  # ---- loop over levels of lev ----
  for (i in seq_along(lev_levels)) {
    lv <- lev_levels[i]

    # build newdat with ALL variables
    newdat <- data.frame(
      tmp_lev = lv,
      stringsAsFactors = FALSE
    )
    names(newdat)[1] <- lev

    # add all other covariates
    for (v in factor_covs) newdat[[v]] <- factor(as.character(factor_vals[[v]]), levels = levels(mf[[v]]))
    for (v in numeric_covs) newdat[[v]] <- numeric_vals[[v]]
    for (v in smooth_vars) newdat[[v]] <- smooth_vals[[v]]

    # predict
    pred <- stats::predict(mod$gam, newdata = newdat, se.fit = TRUE, exclude = NULL)
    fit_link <- as.numeric(pred$fit)
    se_link  <- as.numeric(pred$se.fit)

    out_row <- data.frame(
      newdat,
      fit_link = fit_link,
      se_link = se_link,
      lower_link = fit_link - 1.96 * se_link,
      upper_link = fit_link + 1.96 * se_link,
      fit = exp(fit_link + 0.5 * se_link^2),
      lower = exp((fit_link - 1.96 * se_link) + 0.5 * se_link^2),
      upper = exp((fit_link + 1.96 * se_link) + 0.5 * se_link^2),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    results_list[[i]] <- out_row
  }

  results <- do.call(rbind, results_list)
  rownames(results) <- NULL
  return(results)
}
