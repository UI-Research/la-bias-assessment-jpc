library(knitr)
library(tidyverse)
library(urbnthemes)
library(sf)
library(grid)
library(gridExtra)
library(leaflet)
library(extrafont)
library(mapview)
library(htmlwidgets)
library(patchwork)
### Helper functions

plotr <- function(p = NULL, title = NULL, subtitle = NULL,
                  source = "",
                  endnote = "") {
  # Function that fills in logo, source, endnote, and title. Took out subtitle for now
  # Input:
  #   p: a ggplot
  #   title: String, Your plot's title
  #   subtitle: String, Your plot's subtitle
  #   source: String, Your plot's source
  #   endnote: String, Your plot's endnote
  
  titler <- function(title) {
    textGrob(title,
             x = unit(0, "npc"),
             hjust = 0,
             vjust = 0,
             gp = gpar(fontsize = 14, fontface="bold", fontfamily = "Times New Roman"))
  }
  subtitler <- function(subtitle) {
    textGrob(subtitle,
             x = unit(0, "npc"),
             hjust = 0,
             vjust = 0,
             gp = gpar(fontsize = 9.5, fontfamily = "Times New Roman"))
  }
  sourcer <- function(source) {
    grobTree(
      textGrob("Source: ",
               name = "source1",
               x = unit(0, "npc"),
               hjust = 0,
               vjust = 0,
               gp = gpar(fontsize = 8, fontfamily = "Times New Roman", fontface = "bold")),
      textGrob(source,
               x = unit(0, "npc") + grobWidth("source1"),
               hjust = 0,
               vjust = 0,
               gp = gpar(fontsize = 8, fontfamily = "Times New Roman"))
    )
  }
  noter <- function(endnote) {
    grobTree(
      textGrob("Notes: ",
               name = "note1",
               x = unit(0, "npc"),
               hjust = 0,
               vjust = 0,
               gp = gpar(fontsize = 8, fontfamily = "Times New Roman", fontface = "bold")),
      textGrob(endnote,
               x = unit(0, "npc") + grobWidth("note1"),
               hjust = 0,
               vjust = 0,
               gp = gpar(fontsize = 8, fontfamily = "Times New Roman"))
    )
  }
  caption <- grobTree(
    gp = gpar(fontsize = 7, hjust = 1),
    textGrob(label = "I N S T I T U T E",
             name = "caption1",
             x = unit(1, "npc"),
             y = unit(0, "npc"),
             hjust = 1,
             vjust = 0),
    textGrob(label = "U R B A N  ",
             x = unit(1, "npc") - grobWidth("caption1") - unit(0.01, "lines"),
             y = unit(0, "npc"),
             hjust = 1,
             vjust = 0,
             gp = gpar(col = "#1696d2")))
  grid.arrange(titler(title),
               #subtitler(subtitle),
               p,
               caption, 
               sourcer(source),
               #noter(endnote),
               heights = c(3, 27, 1, 1))
}

# Need to create custom annotation function to specify one facet to annotate in
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, 
                                ymax = Inf, dd) {
  layer(data = dd, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))}

### Graph Functions
make_demo_diff_plot = function(stats_filepath, title){
  stats = suppressMessages(read_csv(stats_filepath))
  
  var_names = tibble(statistic = c('pct_bach','pct_pov','pct_unemp','pct_white','pct_black','pct_anai',
                                       'pct_asian','pct_nh_pi','pct_other_race', 'pct_two_race', 'pct_hisp',
                                       'pct_smaller_races'),
                         full_name = c("Bachelor's degree or higher", "Families in poverty (last 12 months)",
                                       "Unemployment rate","White non-Hispanic ", "Black non-Hispanic ", 
                                       "Alaskan Native/American Indian non-Hispanic ", "Asian non-Hispanic ",
                                       "Native Hawaiian/Pacific Islander non-Hispanic ","Percent Other",
                                       "Two or more races", "Hispanic","Other race non-Hispanic"))
  var_names$full_name = str_wrap(var_names$full_name, width = 17)
  
  colors= c("#9d9d9d", "#ec008b")
  
  
  sd = stats %>% select(ends_with("sd")) %>%
    mutate(mean = c("Citywide Average","Data Implied Average")) %>%
    gather(statistic, value_sd, -mean) %>%
    mutate(statistic = str_replace_all(statistic, "cdp_",""), 
           statistic = str_replace_all(statistic, "_sd","")) %>% 
    spread(mean, value_sd) %>% 
    {colnames(.)[2] = "value_city_avg_sd"; .} %>% 
    {colnames(.)[3] = "value_data_avg_sd"; .}
  
  s = stats %>%
    select(-starts_with("hh"), -ends_with("sd")) %>%
    mutate(mean = c("Citywide Average","Data Implied Average"))%>%
    gather(statistic, value, - mean)  %>%
    spread(mean, value) %>% 
    {colnames(.)[2] = "value_city_avg"; .} %>% 
    {colnames(.)[3] = "value_data_avg"; .} %>% 
    left_join(sd, by = "statistic") %>%
    mutate(vartype = factor(ifelse(statistic %in% c('pct_bach', 'pct_unemp',
                                                    'pct_pov'), "econ variables",
                                   "race variables")),
           diff_city_data = round(value_data_avg - value_city_avg,2),
           value_avg_diff_sd = sqrt(value_city_avg_sd^2 + value_data_avg_sd^2))%>%
    left_join(var_names)%>% 
    #reorder factors for better plotting order
    {.$full_name <- factor(.$full_name, levels=c("Bachelor's degree\nor higher",
                          "Families in\npoverty (last 12\nmonths)", "Unemployment rate", 
                          "White non-\nHispanic", "Black non-\nHispanic", 
                          "Asian non-\nHispanic", "Alaskan Native/\nAmerican Indian\nnon-Hispanic",
                          "Native Hawaiian/\nPacific Islander\nnon-Hispanic","Two or more races",
                          "Other race non-\nHispanic", "Percent Other","Hispanic")); .}%>% 
    filter(!statistic %in% c("pct_anai","pct_nh_pi","pct_other_race","pct_two_race")) %>%
    # gather(mean, value, ends_with("avg")) %>%
    mutate(statistic = ifelse(statistic =="pct_smaller_races",
                              "pct_other", statistic),
           positive_diff = ifelse(diff_city_data>0, "Overrepresented", 
                                  "Underrepresented")) %>%             
    arrange(desc(statistic, vartype))
  
    overrep_grob=grobTree(textGrob("Overrepresented",x=0.02, y=0.51,hjust=0, rot =90,
                                 gp=gpar(col="#55b748",fontsize=11,fontface="plain", alpha = 0.9)))
    underrep_grob=grobTree(textGrob("Underrepresented",x=0.02, y=0.18,hjust=0, rot =90,
                                  gp=gpar(col="#db2b27",fontsize=11,fontface="plain", alpha = 0.8)))
  
  
  
  demo = ggplot(s, aes(x=full_name, y = diff_city_data, fill = positive_diff))+
    # geom_point(show.legend = F) +
    geom_bar(show.legend = F, stat = "identity", width = 0.4, alpha = 0.7) +
    geom_hline(yintercept = 0, size = 1.2, color = colors[1], alpha = 0.6)+
    geom_errorbar(aes(ymin = diff_city_data-3*value_avg_diff_sd, 
                      ymax = diff_city_data+3*value_avg_diff_sd),
                  alpha = 0.6, size = 1.05, width = 0.15,color = colors[1]) + 
    # geom_segment(aes(xend = full_name, yend =0), show.legend = F)+
    #the data.frame needs to replicate an actual data value bc it overwrites the data
    annotation_custom2(overrep_grob, dd = data.frame(vartype = "econ variables", 
                                                     full_name = s %>% 
                                                       filter(statistic == "pct_bach") %>%
                                                       pull(full_name),
                                                     diff_city_data = s %>% 
                                                       filter(statistic == "pct_bach") %>%
                                                       pull(diff_city_data),
                                                     positive_diff = "Overrepresented")) +
    annotation_custom2(underrep_grob, dd = data.frame(vartype = "econ variables", 
                                                      full_name = s %>% 
                                                        filter(statistic == "pct_bach") %>%
                                                        pull(full_name),
                                                      diff_city_data = s %>% 
                                                        filter(statistic == "pct_bach") %>%
                                                        pull(diff_city_data),
                                                      positive_diff = "Overrepresented")) +
    scale_fill_manual(values=c("Overrepresented" = "#55b748", 
                               "Underrepresented" = "#db2b27")) +
    labs(title = title, 
      y="", x="")+
    scale_y_continuous(labels = function(x) paste0(x, "%"), 
                       limits = c(-max(abs(s$diff_city_data)) -
                                    3.25*max(abs(s$value_avg_diff_sd)),
                                  max(abs(s$diff_city_data)) + 
                                    3.25*max(abs(s$value_avg_diff_sd)))) +
    facet_grid(.~vartype, scales  = "free_x", space = "free") + 
    # facet_wrap(vars(vartype), ncol=2, scales  = "free_x", 
    #            strip.position = "bottom", space = "free") +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          legend.position = "top",
          axis.ticks.x = element_blank(),
          plot.margin=grid::unit(c(0.01,0.01,0.01,0.01), "mm"))
  

  return(demo)
 
}

make_tract_bias_map=function(map_filepath, outpath= NA, save  = F, legend = T, title = ""){
  
  map_data = suppressMessages(read_csv(map_filepath))
  map_data = st_as_sf(map_data, wkt ="geometry")
  
  #getting boundary shape as multipolygon
  map_data_c = st_union(map_data)
  
  #setting up title for Leaflet in HTML
  g_title = paste0('<b><font face = "Lato" size ="3">', paste(title, " Geographic Bias"), '</font><b>')
  
  
  
  ### Note: We are setting the NA colors to a deeper pink than the color scale, 
  #but only because se manually confirmed that all prop_diffs in the data not
  #between [-0.05, 0.05] are above 0.05. 
  pal_tot = colorNumeric(palette = c("#12719e","#ececec","#af1f6b"),
                         domain=c(-0.05, 0.05), na.color = "#761548")
  
  
  map_plot = 
    leaflet(options = leafletOptions(zoomControl = F, attributionControl=F)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data= map_data %>% filter(!sig_diff),
                color = "#eaeaea",
                smoothFactor = 0.5,
                opacity = 0.6,
                fillOpacity = 0.5,
                weight = 0.9,
                fillColor = "#8e8e8e"
    ) %>%
    addPolygons(data=(map_data%>% filter(sig_diff)), 
                color ="#eaeaea",
                smoothFactor = 0.5,
                opacity = 0.5,
                fillOpacity = 0.5,
                weight = 0.9,
                fillColor = ~pal_tot((map_data%>% filter(sig_diff))$diff_prop)) %>%
    addPolygons(data = map_data_c,
                color = "black",
                smoothFactor = 0.5,
                opacity = 0.5,
                fillOpacity = 0,
                weight = 2) 
  
  if (legend){
    map_plot = map_plot %>% 
      addLegend("bottomright",
                colors =c("#8e8e8e"),
                labels= c("No significant bias"),
                title= "",
                opacity = 0.8)  %>%
      addLegend(position ="bottomright",
                pal= colorNumeric(palette = c("#af1f6b","#ececec","#12719e"),
                                  domain=c(-.05, .05)),
                values = c(-0.05,.05),
                opacity=0.8,
                title = "Tract Reporting Bias",
                labFormat = labelFormat(
                  suffix = "%",
                  transform = function(x) 100 * sort(x, decreasing = TRUE)))
  }
  
  if(!(title == "")){
    map_plot = map_plot %>% 
      addControl(g_title, position = "topleft")
  }
  
  if (save){
    mapshot(map_plot, file=paste0(outpath), remove_url = F) }
  
  return(map_plot)
}



lapd_arrests_demo = make_demo_diff_plot("data/output-data/arrests_2010_present.csv_stats.csv",
                                        title = "LAPD Arrests Demographic Bias")
lapd_crimes_demo = make_demo_diff_plot("data/output-data/crimes_2010_present.csv_stats.csv",
                                        title = "LAPD Crimes Demographic Bias")

lapd_arrests_geo = make_tract_bias_map("data/output-data/arrests_2010_present.csv_mapdata.csv",
                                       title = "LAPD Arrests")
lapd_crimes_geo = make_tract_bias_map("data/output-data/crimes_2010_present.csv_mapdata.csv",
                                       title = "LAPD Crimes")

ggsave("output/lapd_arrests_demo_bias.png", lapd_arrests_demo, width = 9, height = 5, units = "in")
ggsave("output/lapd_crimes_demo_bias.png", lapd_crimes_demo, width = 9, height = 5, units = "in")

mapshot(lapd_arrests_geo, url = "output/lapd_arrests_geo_bias.html", remove_url = F)
mapshot(lapd_crimes_geo, url = "output/lapd_crimes_geo_bias.html", remove_url = F)


# devtools::install_github("UrbanInstitute/urbnthemes@chartbook")
