#' Compute a weighted variance or standard deviation of a vector.
#'
#' @details
#' Note that unlike the base R \code{\link{var}} function, these functions only
#' work with individual vectors not matrices or data frames.  Borrowed from hadley/bigvis
#'
#' @family weighted statistics
#' @seealso \code{\link[stats]{weighted.mean}}
#' @param x numeric vector of observations
#' @param w integer vector of weights, representing the number of
#'  time each \code{x} was observed
#' @param na.rm if \code{TRUE}, missing values in both \code{w} and \code{x}
#'   will be removed prior computation. Otherwise if there are missing values
#'   the result will always be missing.
#' @export
weighted.var <- function(x, w = NULL, na.rm = FALSE) {
  if (na.rm) {
    na <- is.na(x) | is.na(w)
    x <- x[!na]
    w <- w[!na]
  }
  
  sum(w * (x - weighted.mean(x, w)) ^ 2) / (sum(w) - 1)
}

weighted.sd = function(x, w = NULL, na.rm = FALSE) {
  sqrt(weighted.var(x,w,na.rm=na.rm))
}

weighted.scale <- function(x, w = NULL, na.rm = FALSE) {
  m <- weighted.mean(x, w, na.rm=na.rm)
  sd <- weighted.sd(x, w , na.rm = na.rm)
  as.numeric((x - m)/sd)
}
#' Standard error of weighted means
#' 
#' @details 
#' When means are estimated from surveys with panel weights,
#' we would like standard errors to also take the weights into account.
#' The formula used here is according to Cochran (1977) (which is not 
#' very specific), but it was examined in Donald F. Gatz, Luther Smith,
#' The standard error of a weighted mean concentration—I. Bootstrapping vs other methods,
#' Atmospheric Environment, Volume 29, Issue 11, 1995, Pages 1185-1193, 
#' https://doi.org/10.1016/1352-2310(94)00210-C
#' 
#' Code taken from https://stats.stackexchange.com/questions/25895/computing-standard-error-in-weighted-mean-estimation 
#'
#'
#' @family weighted statistics
#' @param x numeric vector of observations
#' @param w Vector of weights
#' @param na.rm if \code{TRUE}, missing values in both \code{w} and \code{x}
#'   will be removed prior computation. Otherwise if there are missing values
#'   the result will always be missing.
#' @export
weighted.se <- function(x, w, na.rm=FALSE)
  #  Computes the variance of a weighted mean following Cochran 1977 definition
{
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  n = length(w)
  xWbar = weighted.mean(x,w,na.rm=na.rm)
  wbar = mean(w)
  out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
  sqrt(out)
}


#' Minimal ggplot2 theme using the Roboto Condensed and Roboto Bold fonts
#'
#' @param base_size base font size
#' @param strip_text_size,strip_text_margin plot strip text size and margin
#' @param subtitle_size,subtitle_margin plot subtitle size and margin
#' @param plot_title_size,plot_title_margin plot title size and margin
#' @param ... Other arguments passed to \code{theme_minimal}
#'
#' @details The Roboto Condensed and Roboto Bold fonts are both Google fonts;
#' they can be found at \url{https://fonts.google.com/specimen/Roboto+Condensed}
#' and \url{https://fonts.google.com/specimen/Roboto}. These fonts must be
#' installed locally on your computer for this theme to work.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'     geom_point() +
#'     labs(title = "A Lovely Plot",
#'          subtitle = "What can the subtitle tell us?") +
#'     theme_roboto()
#'}
#'
#' @export
theme_roboto <- function(base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 13,
                         subtitle_margin = 10,
                         plot_title_size = 16,
                         plot_title_margin = 10,
                         ...) {
  ret <- ggplot2::theme_minimal(base_family = "RobotoCondensed-Regular",
                                base_size = base_size, ...)
  ret$strip.text <- ggplot2::element_text(hjust = 0, size=strip_text_size,
                                          margin=margin(b=strip_text_margin),
                                          family="Roboto-Bold")
  ret$plot.subtitle <- ggplot2::element_text(hjust = 0, size=subtitle_size,
                                             margin=margin(b=subtitle_margin),
                                             family="RobotoCondensed-Regular")
  ret$plot.title <- ggplot2::element_text(hjust = 0, size = plot_title_size,
                                          margin=margin(b=plot_title_margin),
                                          family="Roboto-Bold")
  ret
}


weighted.bars <- function(y, weight, values=NULL, name=NULL, na.rm=FALSE) {
  df <- dplyr::tibble(x=y, w=weight) 
  df <- df %>% fastDummies::dummy_columns(select_columns="x", ignore_na=TRUE,
                                   remove_selected_columns = TRUE)
  means <- df %>% summarize_at(vars(starts_with("x_")), 
                      ~weighted.mean(., w, na.rm=TRUE)) 
  out <- means %>% pivot_longer(starts_with("x_"), values_to ="proportion", names_to = "v", 
                         names_prefix="x_") %>% 
    mutate(v=as.numeric(v))
  if (!is.null(values)) {
    dfv <- tibble(v=values, proportion=0)
    out <- out %>% bind_rows(dfv)
    out <- out %>% group_by(v) %>% 
      summarize(proportion=sum(proportion))
  }
  if (!is.null(name)) {
    out <- out %>% rename(v=name)
  }
  out
}

fatwo_rcfigure <- function(df, outcome, se, group, 
                           title=NULL, caption=NULL, ytitle=NULL,
                           ylimits = NULL, hline=NULL,
                           theme = ggplot2::theme_minimal,
                           path = NULL, w = 16, h = 10, u="cm") {
  if (is.null(ytitle)) ytitle  = "Mean \u00B1 s.e."
  if (is.null(title)) title = ggplot2::element_blank()
  if (is.null(caption)) caption = ggplot2::element_blank()
  fig <- ggplot2::ggplot(df, aes(x=reorder({{group}}, {{outcome}}), y={{outcome}})) +
    ggplot2::geom_point() + 
    ggplot2::geom_errorbar(aes(ymin = {{outcome}} - {{se}}, ymax = {{outcome}} + {{se}})) +
    labs(y = ytitle, 
         x = ggplot2::element_blank(),
         title = title,
         caption = caption) +
    theme() + 
    ggplot2::coord_cartesian(ylim = ylimits) +
    ggplot2::theme( axis.text.x=element_text(size=7,
                                             angle=270,
                                             vjust=0.5,
                                             hjust=0))
  if (!is.null(hline)) {
    fig <- fig + ggplot2::geom_hline(yintercept = hline)
  }
  if (!is.null(path)) {
    ggplot2::ggsave(path, plot=fig, width=w, height = h, units = u)
  }
  fig
}

fatwo_rcfigure_bar <- function(df, outcome, se, group, 
                               title=NULL, caption=NULL, ytitle=NULL,
                               ylimits = NULL, hline=NULL,
                               color = NULL, colorname=NULL,
                               themew = ggplot2::theme_minimal,
                               path = NULL, w = 16, h = 10, u="cm") {
  if (is.null(ytitle)) ytitle  = "Mean \u00B1 s.e."
  if (is.null(title)) title = ggplot2::element_blank()
  if (is.null(caption)) caption = ggplot2::element_blank()
  if (is.null(colorname)) color = ggplot2::element_blank()
  fig <- ggplot2::ggplot(df, aes(x=reorder({{group}}, {{outcome}}), y={{outcome}}, fill={{color}})) +
    ggplot2::geom_bar(stat='identity') + 
    ggplot2::geom_errorbar(aes(ymin = {{outcome}} - {{se}}, ymax = {{outcome}} + {{se}},), 
                           color="black", width=0.5) +
    labs(y = ytitle, 
         x = ggplot2::element_blank(),
         title = title,
         caption = caption,
         fill = colorname) +    
    themew() +
    theme(plot.title.position = "plot", legend.position = "none") +
    ggplot2::coord_cartesian(ylim = ylimits) +
    ggplot2::theme( axis.text=element_text(size=9,
                                           angle=270,
                                           vjust=0.5,
                                           hjust=0))
  if (!is.null(hline)) {
    fig <- fig + ggplot2::geom_hline(yintercept = hline)
  }
  if (!is.null(path)) {
    ggplot2::ggsave(path, plot=fig, width=w, height = h, units = u)
  }
  fig
}

fatwo_rcfigure_bar_1to5 <- function(df, outcome, se, group, 
                               title=NULL, caption=NULL, ytitle=NULL,
                               ylimits = NULL, 
                               color = NULL, colorname=NULL,
                               themew = ggplot2::theme_minimal,
                               path = NULL, w = 16, h = 10, u="cm") {
  if (is.null(ytitle)) ytitle  = "Mean \u00B1 s.e."
  if (is.null(title)) title = ggplot2::element_blank()
  if (is.null(caption)) caption = ggplot2::element_blank()
  if (is.null(colorname)) color = ggplot2::element_blank()
  fig <- ggplot2::ggplot(df, aes(x=reorder({{group}}, {{outcome}}), y={{outcome}}-3, fill={{color}})) +
    ggplot2::geom_bar(stat='identity') + 
    ggplot2::geom_errorbar(aes(ymin = {{outcome}} - 3 - {{se}}, ymax = {{outcome}} - 3 + {{se}},), 
                           color="black", width=0.5) +
    labs(y = ytitle, 
         x = ggplot2::element_blank(),
         title = title,
         caption = caption,
         fill = colorname) +    
    themew() +
    theme(plot.title.position = "plot", legend.position = "none") +
    scale_y_continuous(breaks=c(-2,-1,0,1,2),
                       labels=c("1: Strongly\ndisagree","2","3: Neutral","4","5: Strongly\nagree"), 
                       limits = c(-2.2,2.2)) +
    theme(axis.text.y = element_text(angle = 0)) +
    ggplot2::theme( axis.text=element_text(size=9,
                                           angle=270,
                                           vjust=0.5,
                                           hjust=0))
  fig <- fig + ggplot2::geom_hline(yintercept =0)
  if (!is.null(path)) {
    ggplot2::ggsave(path, plot=fig, width=w, height = h, units = u)
  }
  fig
}

#---- Mode ----
# From https://raw.githubusercontent.com/marberts/smart/master/R/computations.R
weighted_mode <- function(x, w = rep(1L, length(x)), na.rm = FALSE) {
  if (length(x) != length(w)) {
    stop("'x' and 'w' must be the same length")
  }
  if (na.rm) {
    if (anyNA(x) || anyNA(w)) { # nested if to prevent anyNA(w) getting called twice
      keep <- !(is.na(x) | is.na(w))
      x <- x[keep]
      w <- w[keep]
    }
  } else if (anyNA(w)) {
    return(x[0][NA]) # impossible to know mode if any weights are missing
  }
  ux <- unique(x)
  if (!length(ux)) return(ux) # prevents max for returning -Inf
  f <- as.factor(match(x, ux))
  tab <- vapply(split(w, f), sum, numeric(1), USE.NAMES = FALSE)
  is_mode <- tab == max(tab) # lines up with ux
  if (anyNA(x)) {
    na <- which(is.na(ux)) # single integer
    modes <- which(is_mode)
    cond <- na %in% modes || # mode is NA if any mode is NA
      # or if the weight for any mode does not exceed the weight for the NA
      # and the weight for the next largest weight
      (length(ux) > 2 && tab[na] + max(tab[-c(modes[1], na)]) >= tab[modes[1]])
    if (cond) return(x[0][NA])
  }
  if (sum(is_mode) > 1) warning("mode is not unique")
  ux[is_mode]
}


