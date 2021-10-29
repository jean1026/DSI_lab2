

#' function 2
#' This function calculates statistics over all of the DRG codes for average Medicare payments.
#'
#' @param df a dataframe, which should be the dataframe read from "DRG_data.csv" file specifically
#' @param statistics indicates the statistics caculated and must be one of 'mean', 'median' or 'standard deviation'
#'
#' @return a dataframe with DRG code and its statistic
#' @export
#'
#' @examples
#'DRG <- read.csv("DRG_data.csv")
#'func2(df = DRG, statistics = 'mean')

func2 <- function(df, statistics = c('mean', 'median', 'standard deviation')) {
  DRG_byCode2 <- df %>%
    mutate(DRG_code = substr(DRG.Definition, 1, 3)) %>%
    group_by(DRG_code) %>%
    summarise(
      mean = mean(Average.Medicare.Payments),
      median = median(Average.Medicare.Payments),
      `standard deviation` = sd(Average.Medicare.Payments)
    )


  DRG_byCode2 %>% select(DRG_code, statistics)
}



