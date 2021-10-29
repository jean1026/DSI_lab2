



#' function 1
#'
#'This function makes a boxplot of payments by DRG code.
#' @param df a dataframe, which should be the dataframe read from "DRG_data.csv" file specifically
#' @param payments indicates the type of payments and must be one of ' total payment', ' total payment'
#'                  or ' covered charges'.
#'
#' @return A bocplot of one of the payments by DRG code
#' @export
#'
#' @examples
#' DRG <- read.csv("DRG_data.csv")
#' func1(DRG, 'covered charges')
#'
func1 <- function(df, payments = c(' medicare payment', ' total payment', ' covered charges')) {
  DRG_byCode <- df %>%
    mutate(DRG_code = substr(DRG.Definition, 1, 3)) %>%
    group_by(DRG_code)

  if (payments == 'medicare payment') {
    y = DRG_byCode$Average.Medicare.Payments
    }

  else if (payments == 'total payment') {
    y = DRG_byCode$Average.Total.Payments
  }

  else {
    y = DRG_byCode$Average.Covered.Charges
 }


  ggplot(data = DRG_byCode) +
    geom_boxplot(aes(x = DRG_code, y)) +
    labs(x = "DRG code", y = paste("average", payments),
         title = (paste("Average",str_to_title(payments), "by DRG Code"))) +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1))
}

