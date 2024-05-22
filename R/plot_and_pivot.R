#' @title Pivot Withdrawal Reports to Long Format
#' @description Pivotes withdrawal reports to long format with new columns Date and MGM.
#' @param reports Filtered withdrawal reports (default is entire WU_monit table)
#' @return A long format table of withdrawal reports
#' @export
pivot_withdrawal_reports <- function(reports = wu$WU_monit) {
  tidyr::pivot_longer(
    data=reports,
    cols = c(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec),
    names_to="month", values_to="MGM",
    names_transform = stringr::str_to_title) %>%
    dplyr::mutate(Date = paste0(month, ' ', year) %>%
                    zoo::as.yearmon(format="%b %Y") %>%
                    zoo::as.Date(frac=1))
}

# test <- pivot_withdrawal_reports()
# system.time({pivot_withdrawal_reports()})

#' @title Plot withdrawals at a single intake
#' @description Plots monthly withdrawals at a single intake.
#' @param id a sourceid character string
#' @param startyear a numeric year to start the plot
#' @param endyear a numeric year to end the plot
#' @param permit_limit a logical value to include the permit limit as a horizontal line
#' @param facility_title a logical value to include the permitted facility in the title
#' @return A ggplot object
#' @export
plot_intake <- function(id, startyear=NA, endyear=NA,
                        permit_limit=T, facility_title=T) {
  reports <- wu$WU_monit %>%
    dplyr::filter(sourceid == id &
                    (is.na(startyear) | year >= startyear) &
                    (is.na(endyear) | year <= endyear)) %>%
    pivot_withdrawal_reports()

  intake <- dplyr::filter(intakes, sourceid==id)

  no_intake <- nrow(intake) == 0
  no_ownersid <- no_intake | is.na(intake$ownersid)
  no_volmon <- no_intake | is.na(intake$VOL_MON)

  facility <- wu$Wat_use %>%
    semi_join(parse_sourceid(reports), 'userid')

  intake_title <- if(no_ownersid) id else paste0(id, ' - ', intake$ownersid)

  if(facility_title) {
    title <- facility$facility
    subtitle <- intake_title
  } else {
    title <- intake_title
    subtitle <- waiver()
  }

  g <- ggplot2::ggplot(
    data=reports, ggplot2::aes(x=Date, y=MGM)) +
    ggplot2::theme_bw() +
    ggplot2::ylab('Million Gallons per Month') +
    ggplot2::ggtitle(label=title, subtitle=subtitle)

  if(!no_volmon & permit_limit) {
    g <- g + ggplot2::geom_hline(yintercept = intake$VOL_MON, color='red', alpha=0.5)
  }

  g + ggplot2::geom_line()
  }

# plot_intake(id='09IN001S01', facility_title=F)

## Plot Withdrawal

# plot_withdrawals <- function(sourceids = '',
#                             userids = '',
#                             startyear = NA,
#                             endyear = NA) {
#   reports <- wu$WU_monit %>%
#     parse_sourceid() %>%
#     dplyr::filter(
#       (sourceid %in% sourceids | userid %in% userids) &
#         (is.na(startyear) | year >= startyear) &
#         (is.na(endyear) | year <= endyear)) %>%
#     pivot_withdrawal_reports()
# }







