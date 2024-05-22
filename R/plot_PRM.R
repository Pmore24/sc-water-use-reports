#functions copied from scwaterwithdrawaldata
#' @title Basin map
#' @description function for plotting a basin map
#' @param major_river_basin_shp shapefile for major river basins
#' @param req_basin_shp shapefile for basin that will be highlighted
#' @param counties_import county shapefile
#' @param maj_lakes_import major lakes in SC shapefile
#' @param maj_rivers_import major rivers in SC shapefile
#' @importFrom ggplot2 ggplot
#' @return map
#' @export
#'
basin_map <- function(major_river_basin_shp, req_basin_shp) {
  ggplot2::ggplot()+
    geom_sf(data=major_river_basin_shp,aes(fill=Basin), color = "black")+
    scale_fill_manual(values=c("Broad" = "#DAA520AA","Catawba" = "#CD853FAA","Edisto" = "#ADD8E6AA","Pee Dee"= "#98FB98AA","Lower Savannah-Salkehatchie" = "#9999FFAA", "Saluda" = "#FFDAB9AA","Santee" = "#FF8C00","Upper Savannah" = "#FFB6C1"))+
    geom_sf(data=counties_import, color = "grey35", fill = NA)+
    geom_sf(data=maj_lakes_import, color="#1E90FFAA", fill="#1E90FFAA")+
    geom_sf(data=maj_rivers_import, color="#1E90FFAA", fill="#1E90FFAA")+
    geom_sf(data=major_river_basin_shp, color = "black", fill=NA)+
    geom_sf(data=req_basin_shp, color = "red", fill=NA, size = 1.0)+
    scale_alpha_manual(values = (0.6))+
    theme(
      panel.background = element_blank(),
      plot.background = element_rect(fill = "white", color = "black"),
      panel.grid.major = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = unit(c(0.01, 0.01, 0.01, 0.01),"null"))

}

#' @title Descending Water Users
#' @description function for calculating top water withdrawers by basin
#' @param water_use_data water withdrawal dataset saves after cleaning DHEC's original dataset
#' that have the listed columns within the select function (here "wu_monit_2000_2020" was used)
#' @param req_year select the required year for data analysis
#' @param basin select required spatial basin for data analysis
#' @param category_to_exclude water use category to exclude
#' @param source select ground or surface water source
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr coalesce
#' @importFrom dplyr summarise
#' @return dataframe for selected water withdrawals in descending order
#' @export
top_water_withdrawers <- function(water_use_data, req_year, which_basin, category_to_exclude, source) {
  water_use_data %>%
    dplyr::filter(year== req_year,basin == which_basin, category %nin% category_to_exclude, water_source_code %in% source) %>%
    dplyr::select(sourceid, UserID, categoryCode, UserCounty, water_source_code, total_mgd, facility_name,
                  UserID_operator, ownersid, streamname, Spatial_county, UserID_facility) %>%
    dplyr::group_by(UserID_operator) %>%
    dplyr::arrange(desc(total_mgd), .by_group = TRUE) %>%
    dplyr::mutate(UserID_operator = dplyr::coalesce(UserID_operator, facility_name)) %>%
    dplyr::mutate(UserID_operator = dplyr::coalesce(UserID_operator, UserID_facility)) %>%
    dplyr::group_by(UserID_operator, categoryCode, UserID) %>%
    dplyr::summarise(total_mgd = sum(total_mgd))
}

#' @title Summarize data by source for pie charts
#' @description function for summarizing water withdrawalse by source
#' @param water_use_data water withdrawal dataset saves after cleaning DHEC's original dataset
#' that have the listed columns within the select function (here "wu_monit_2000_2020" was used)
#' @param req_year select the required year for data analysis
#' @param basin select required spatial basin for data analysis
#' @param category_to_exclude water use category to exclude
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr recode
#' @importFrom dplyr summarise
#' @importFrom scales percent
#' @return dataframe for source wise summary of water withdrawals
#' @export
pie_source_summary <- function(water_use_data, req_year, which_basin, category_to_exclude){
  water_use_data %>%
    dplyr::filter(year == req_year, basin==which_basin, category %nin% category_to_exclude) %>%
    dplyr::group_by(water_source_code) %>%
    dplyr::rename("Total_water_distribution" = water_source_code) %>%
    dplyr::mutate(`Total_water_distribution` = dplyr::recode(`Total_water_distribution`,
                                                             "G" = "Groundwater",
                                                             "S" = "Surface Water")) %>%
    dplyr::summarise(total_mgd = sum(total_mgd, na.rm = TRUE)) %>%
    dplyr::mutate(percentage = `total_mgd`/sum(`total_mgd`)) %>%
    dplyr::mutate(labels = scales::percent(percentage, accuracy = 0.1))
}

#' @export
pie_source_summary_subbasin <- function(water_use_data, req_year, which_basin, subbasin,category_to_exclude){
  water_use_data %>%
    dplyr::filter(year == req_year, basin == which_basin, Subbasin == subbasin,category %nin% category_to_exclude) %>%
    dplyr::group_by(water_source_code) %>%
    dplyr::rename("Total_water_distribution" = water_source_code) %>%
    dplyr::mutate(`Total_water_distribution` = dplyr::recode(`Total_water_distribution`,
                                                             "G" = "Groundwater",
                                                             "S" = "Surface Water")) %>%
    dplyr::summarise(total_mgd = sum(total_mgd, na.rm = TRUE)) %>%
    dplyr::mutate(percentage = `total_mgd`/sum(`total_mgd`)) %>%
    dplyr::mutate(labels = scales::percent(percentage, accuracy = 0.1))
}


#' @title Summarize data by category and source
#' @description function for summarizing water withdrawalse by source and categories
#' @param water_use_data water withdrawal dataset saved after cleaning DHEC's original dataset
#' that have the listed columns within the select function (here "wu_monit_2000_2020" was used)
#' @param req_year select the required year for data analysis
#' @param basin select required spatial basin for data analysis
#' @param category_to_exclude water use category to exclude
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr recode
#' @importFrom dplyr summarise
#' @importFrom scales percent
#' @return dataframe for source wise summary of water withdrawals
#' @export
pie_cat_source_summary <- function(water_use_data, req_year, which_basin, category_to_exclude){
  water_use_data %>%
    dplyr::filter(year == req_year, basin == which_basin, category %nin% category_to_exclude) %>%
    dplyr::mutate(category = dplyr::recode(category, 'Nuclear Power' = "Thermoelectric"),
                  category = dplyr::recode(category, 'Thermal Power' = "Thermoelectric"),
                  category = dplyr::recode(category, 'Golf' = "Golf Course")) %>%
    dplyr::group_by(category,water_source_code) %>%
    dplyr::summarise(total_mgd = sum(total_mgd)) %>%
    tidyr::spread(water_source_code, total_mgd) %>%
    dplyr::rename(Groundwater = G,
                  Surface_water =S) %>% rowwise() %>%
    dplyr::mutate(total_mgd = sum(Groundwater,Surface_water, na.rm = TRUE)) %>%
    dplyr::mutate(.,Percentage_GW = ((Groundwater/sum(.$Groundwater, na.rm = TRUE))*100)) %>%
    dplyr::mutate(.,Percentage_SW = ((Surface_water/sum(.$Surface_water, na.rm = TRUE))*100)) %>%
    dplyr::mutate(.,Percentage_Total = ((total_mgd/sum(.$total_mgd, na.rm = TRUE))*100)) %>%
    dplyr::relocate(Percentage_GW, .after = Groundwater) %>%
    dplyr::relocate(Percentage_SW, .after = Surface_water) %>%
    janitor::adorn_totals("row", na.rm = TRUE)
}

#' @export
pie_cat_source_summary_subbasin <- function(water_use_data, req_year, which_basin,subbasin,category_to_exclude){
  water_use_data %>%
    dplyr::filter(year == req_year, basin == which_basin, Subbasin == subbasin,category %nin% category_to_exclude) %>%
    dplyr::mutate(category = dplyr::recode(category, 'Nuclear Power' = "Thermoelectric"),
                  category = dplyr::recode(category, 'Thermal Power' = "Thermoelectric"),
                  category = dplyr::recode(category, 'Golf' = "Golf Course")) %>%
    dplyr::group_by(category,water_source_code) %>%
    dplyr::summarise(total_mgd = sum(total_mgd)) %>%
    tidyr::spread(water_source_code,total_mgd) %>%
    dplyr::rename(Groundwater = G,
                  Surface_water =S) %>% rowwise() %>%
    dplyr::mutate(total_mgd = sum(Groundwater,Surface_water, na.rm = TRUE)) %>%
    dplyr::mutate(.,Percentage_GW = ((Groundwater/sum(.$Groundwater, na.rm = TRUE))*100)) %>%
    dplyr::mutate(.,Percentage_SW = ((Surface_water/sum(.$Surface_water, na.rm = TRUE))*100)) %>%
    dplyr::mutate(.,Percentage_Total = ((total_mgd/sum(.$total_mgd, na.rm = TRUE))*100)) %>%
    dplyr::relocate(Percentage_GW, .after = Groundwater) %>%
    dplyr::relocate(Percentage_SW, .after = Surface_water) %>%
    janitor::adorn_totals("row", na.rm = TRUE)
}

#' @title Create pie plot for the summarized data by source
#' @description function for plotting withdrawal data by source as pie plots
#' @param pie_source_summary summarized water withdrawal data by source
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 coord_polar
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 theme
#' @return pie plot for summarized water withdrawal data by source
#' @export
pie_source_graph <- function (pie_source_summary) {
  ggplot2::ggplot(data = pie_source_summary, aes(x = "", y = total_mgd , fill = `Total_water_distribution`)) +
    geom_col() +
    # geom_text(aes(label = labels),
    #            color = c("white","white"),
    #            size = 5,
    #            position = position_stack(vjust = 0.8),# adjust the label's position within pie chart
    #            show.legend = FALSE,
    #            label.size = NA)+
    coord_polar(theta = "y") + # creates the pie chart
    # scale_fill_manual(values = c("Groundwater"="darkorange1","Surface Water"="midnightblue"))+
    theme_void()+
    theme(legend.position = "right", legend.text = element_text(size = 16), plot.title = element_text(size = 15, hjust = 0.5),
          legend.title = element_blank())+
    theme(panel.background = element_blank())+
    theme(plot.background = element_rect(fill = "white"))
}


#' @title Summarize data by category for pie charts
#' @description function for summarizing water withdrawals by category
#' @param water_use_data water withdrawal dataset saves after cleaning DHEC's original dataset
#' that have the listed columns within the select function (here "wu_monit_2000_2020" was used)
#' @param req_year select the required year for data analysis
#' @param basin select required spatial basin for data analysis
#' @param category_to_exclude water use category to exclude
#' @param source ground, surface, or total
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr recode
#' @importFrom dplyr summarise
#' @importFrom scales percent
#' @return dataframe for category wise summary of water withdrawals
#' @export

pie_category_summary <- function(water_use_data, category, req_year, which_basin, category_to_exclude, source){
  water_use_data %>%
    dplyr::mutate(category = dplyr::recode(category, "Thermal Power" = "Thermoelectric",
                                           "Nuclear Power" = "Thermoelectric", "Golf" = "Golf Course")) %>%
    dplyr::filter(year== req_year,basin == which_basin, category %nin% category_to_exclude, water_source_code %in% source) %>%
    dplyr::select(category, water_source_code, total_mgd) %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(total_mgd = sum(total_mgd, na.rm = TRUE)) %>%
    dplyr::mutate(percentage = total_mgd/sum(.$total_mgd, na.rm = TRUE)) %>%
    dplyr::mutate(labels = scales::percent(percentage, accuracy = 0.1))
}



#' @title Create pie plot for the summarized data by category
#' @description function for plotting withdrawal data by category as pie plots
#' @param pie_category_summary summarized water withdrawal data by category
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 coord_polar
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 theme
#' @return pie plot for summarized water withdrawal data by source
#' @export
pie_category_graph <- function(pie_category_summary){
  ggplot2::ggplot(data = pie_category_summary, aes(x = "", y = percentage , fill = `category`))+
    geom_col() +
    coord_polar(theta = "y") +
    theme_void()+
    theme(legend.position = "right", legend.text = element_text(size = 16), plot.title = element_text(size = 17, hjust = 0.5),
          legend.title = element_blank())+
    # theme(panel.background = element_rect(fill = "white", colour = "grey28",size = 1))+
    theme(plot.background = element_rect(fill = "white", color = NA))
}

#' @title Create bar plot for the summarized data by category
#' @description function for plotting withdrawal data by category as bar plots
#' @param pie_category_summary summarized water withdrawal data by category
#' @param source add the column for gw or sw
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 theme
#' @return bar plot for summarized water withdrawal data by source
#' @export
cat_bar_plot <- function(dat, y_col,x_col, percent_label_col){
  ggplot2::ggplot(data = dat, aes(y=!!as.name(y_col), x =!!as.name(x_col)))+
    geom_bar(stat = "identity", fill = "black", width = 0.5)+
    #theme(plot.title =element_text(size = 40))+
    # theme(plot.title =element_text(hjust = 0.5))+
    geom_text(aes(label =scales::percent(!!as.name(percent_label_col),scale = 1,accuracy = 0.1)), hjust= -0.1, size = 6)+
    labs(y= "Withdrawal (MGD)", x = "category")+
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(color = "gray"))+
    theme(axis.title = element_text(size = 16))+
    theme(axis.title = element_text(color = "black"))+
    theme(axis.text = element_text(size = 16))+
    theme(axis.text = element_text(color = "black"))+
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    theme(panel.background = element_rect(fill = "white", colour = "grey28",linewidth = 1, linetype = "solid"))+
    theme(plot.background = element_rect(fill = "white"))+
    coord_flip()
}
#' @title summarize data by year
#' @description function for trend plots of withdrawals by years
#' @param water_use_data water withdrawal dataset saves after cleaning DHEC's original dataset
#' that have the listed columns within the select function (here "wu_monit_2000_2020" was used)
#' @param req_year select the required years for data analysis
#' @param basin select required spatial basin for data analysis
#' @param category_to_exclude water use category to exclude
#' @param source ground, surface, or total
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr recode
#' @importFrom tidyr pivot_wider
#' @return summarized dataframe of water withdrawal data by years
#' @export
trend_plot_summary <- function(water_use_data,trend_year,which_basin,req_category){
  dat1 <- water_use_data %>%
    dplyr::select(year,category,basin, water_source_code,total_mgd) %>%
    dplyr::mutate(category = dplyr::recode(category, "Thermal Power" = "Thermoelectric",
                                           "Nuclear Power" = "Thermoelectric",
                                           "Golf" = "Golf Course")) %>%
    dplyr::filter(year %in% trend_year, basin==which_basin, category %in% req_category) %>%
    dplyr::filter(category %in% req_category, water_source_code %in% c("S","G")) %>%
    dplyr::group_by(year, water_source_code) %>%
    dplyr::summarise(total_mgd = sum(total_mgd, na.rm = TRUE))

  dat2 <- dat1 %>%
    dplyr::group_by(year) %>%
    summarise(total_mgd = sum(total_mgd)) %>%
    dplyr::mutate(water_source_code = "Total") %>%
    dplyr::bind_rows(dat1, .) %>%
    tidyr::pivot_wider(names_from = water_source_code, values_from = total_mgd)
}

#' @title Create trend bar plot for the summarized data by years
#' @description function for plotting withdrawals by years
#' @param trend_plot_summary summarized water withdrawal data by year
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 theme
#' @return trend bar plot for summarized water withdrawal data by years
#' @export
#################################work
trend_plot <-  function(dat, x_col, y_col){
  ggplot(data = dat, aes(x= !!as.name(x_col), y= !!as.name(y_col))) +
    geom_bar(stat="identity",fill = "black", width = 0.5)+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    # geom_text(aes(label = format(!!as.name(y_col),digits =2),vjust= -0.5, size = 10)) +
    # ylab("Withdrawals (MGD)")+
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    theme(legend.key=element_blank(),
          legend.position = "none")+
    #theme(plot.title =element_text(size = 40))+
    #theme(plot.title =element_text(hjust = 0.5))+
    theme(panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
    theme(axis.title = element_text(size = 16))+
    theme(axis.title = element_text(color = "black"))+
    theme(axis.text = element_text(size = 16))+
    theme(axis.text = element_text(color = "black"))+
    theme(axis.title.x = element_blank())+
    theme(panel.background = element_rect(fill = "white", colour = "grey28",size = 1, linetype = "solid"))+
    theme(plot.background = element_rect(fill = "white"))
}

# function from Alex's scwaterwithdrawal pkg
# the original function adds a date column that generates a last date for the
# given month and year to the wu database. In order to plot a stack bar plot,
# we need to have a fake date column that only has a single year.

pivot_withdrawal_reports <- function(dat) {
  tidyr::pivot_longer(
    data=dat,
    cols = c(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec),
    names_to="month", values_to="MGD",
    names_transform = stringr::str_to_title) %>%
    dplyr::mutate(fake_date=as.Date(paste('2021',month,'01'), '%Y %B %d'))
}

monthly_cat_trend_plot <- function(dat,cat) {
  year_color <- c("2011" = "#009292","2012" = "#D2B04C","2013" = "#744EC2",
                  "2014"= "#EC5656","2015" = "#4B4C4E","2016" = "#AAAAAA","2017" = "#B66DFF",
                  "2018" = "#12239E","2019" = "#A3623A", "2020" = "#E13012",
                  "2021" = "#5C0001")

  monthly_stack <- dat %>%
    dplyr::filter(category==cat) %>%
    ggplot2::ggplot(aes(x=fake_date, y= MGD,fill=as.factor(year)))+
    geom_bar(position="stack", stat="identity", width = 10)+
    scale_fill_manual(values= year_color)+
    scale_x_date(date_breaks = "1 month", date_labels = "%b")+
    scale_y_continuous(expand = expansion(mult = c(0, .1)), labels = comma)+
    theme(legend.title=element_blank(),
          legend.position = "bottom")+
    guides(fill = guide_legend(nrow = 1))+
    theme(panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
    theme(axis.title = element_text(size = 12))+
    theme(axis.title = element_text(color = "black"))+
    theme(axis.text = element_text(size = 12))+
    theme(axis.text = element_text(color = "black"))+
    theme(axis.title.x = element_blank())+
    theme(panel.background = element_rect(fill = "white", colour = "grey28",size = 1, linetype = "solid"))+
    theme(plot.background = element_rect(fill = "white"))+
    labs(y = "SW Withdrawal (MGD)")
  monthly_stack
}

monthly_cat_trend_line_plot <- function(dat,cat) {
  year_color <- c("2011" = "#009292","2012" = "#D2B04C","2013" = "#744EC2",
                  "2014"= "#EC5656","2015" = "#4B4C4E","2016" = "#AAAAAA","2017" = "#B66DFF",
                  "2018" = "#12239E","2019" = "#A3623A", "2020" = "#E13012",
                  "2021" = "#5C0001")

  monthly_line <- dat %>%
    dplyr::filter(category==cat) %>%
    ggplot2::ggplot(aes(x=fake_date, y= MGD,color=as.factor(year)))+
    geom_point() + geom_line()+
    scale_color_manual(values= year_color)+
    scale_x_date(date_breaks = "1 month", date_labels = "%b")+
    scale_y_continuous(expand = expansion(mult = c(0, .1)), labels = comma)+
    theme(legend.title=element_blank(),
          legend.position = "bottom")+
    guides(color = guide_legend(nrow = 1))+
    theme(panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
    theme(axis.title = element_text(size = 12))+
    theme(axis.title = element_text(color = "black"))+
    theme(axis.text = element_text(size = 12))+
    theme(axis.text = element_text(color = "black"))+
    theme(axis.title.x = element_blank())+
    theme(panel.background = element_rect(fill = "white", colour = "grey28",size = 1, linetype = "solid"))+
    theme(plot.background = element_rect(fill = "white"))+
    labs(y = "SW Withdrawal (MGD)")
  monthly_line
}
