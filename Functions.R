# --- Imports
library(sf)
library(readxl)
library(tidyverse)
library(ggvoronoi)
library(V8)
library(patchwork)
# --- Functions
# 'rs' stands for redistribution/redivision summary
# -- Helpers
rs_voronoi <- function(x,crs){
  polling_place_sfc <- st_as_sf(x$polling_places, coords = c("longitude","latitude"), crs = attr(x$polling_places,"crs")) %>%
    st_transform(crs)
  transformed_coordinates <- st_coordinates(polling_place_sfc)
  polling_place_sfc <- st_drop_geometry(polling_place_sfc) %>%
    cbind(transformed_coordinates)
  boundary_sp <- x$boundary %>% as_Spatial()
  st_as_sf(ggvoronoi::voronoi_polygon(polling_place_sfc,x = "X", y = "Y", outline = boundary_sp))
}
# Maybe requires a summary so if two independents get votes at the same booth, the adjustments work
rs_adjustment <- function(rs_election_results, rs_polling_places){
  # Adjusts for votes that cannot be tied to a polling place with coordinates
  if (!inherits(rs_election_results,"rs_election_results")){
    stop("'rs_election_results' must be of type 'rs_election_results'")
  }
  if (!inherits(rs_polling_places,"rs_polling_places")){
    stop("'rs_polling_places' must be of type 'rs_polling_places'")
  }
  df <- left_join(rs_election_results$results, select(rs_polling_places, pp_id, district, is_coord), by="pp_id") %>%
    group_by(group_code,district,is_coord) %>%
    mutate(district_pty_type_votes = sum(votes)) %>%
    ungroup(is_coord) %>%
    mutate(district_pty_votes = sum(votes)) %>%
    ungroup() %>%
    mutate(adj_ratio = district_pty_votes/district_pty_type_votes, adj_votes = votes*adj_ratio) %>%
    filter(is_coord == TRUE) %>%
    select(pp_id,group_code,votes,adj_votes) %>%
    group_by(pp_id) %>%
    mutate(vote_pct = votes/sum(votes), adj_vote_pct = adj_votes/sum(adj_votes)) %>%
    ungroup()
  rs_adjustment <- list(name = rs_election_results$name, results = df)
  class(rs_adjustment) <- c("rs_adjustment",class(rs_adjustment))
  rs_adjustment
}
rs_election <- function(name,boundaries,polling_places,election_result_list,crs){
  if (!is.character(name)){
    stop("'name' must be of type 'character'")
  }
  if (!inherits(boundaries,"rs_boundary_set")){
    stop("'boundaries' must be of type 'rs_boundary_set'")
  }
  if (!inherits(polling_places,"rs_polling_places")){
    stop("'polling_places' must be of type 'rs_polling_places'")
  }
  if (!inherits(election_result_list,"list")){
    stop("'election_result_list' must be of type 'list'")
  }
  else if (!all(sapply(election_result_list,function(x){"rs_election_results" %in% class(x)},simplify = TRUE))) {
    stop("'election_result_list' must only contain objects of type 'rs_election_results'")
  }
  else if (any(duplicated(sapply(election_result_list,function(x){x$name})))){
    stop("Every 'rs_election_results' in 'election_result_list' must have a unique name")
  }
  boundary_dist_names <- unique(boundaries$district)
  polling_place_dist_names <- unique(polling_places$district)
  if (all.equal(boundary_dist_names,polling_place_dist_names) != TRUE){
    warning(paste0("Some district names appear in one set but not the other\n",
                   "District names in boundary set but not in polling place list:",
                   paste(setdiff(boundary_dist_names,polling_place_dist_names), collapse = ", "),"\n",
                   "District names in polling place list but not in boundary set:",
                   paste(setdiff(polling_place_dist_names,boundary_dist_names), collapse = ", ")))
  }
  
  adj_results <- lapply(election_result_list,rs_adjustment, rs_polling_places = polling_places)
  
  sf_polling_places <- sapply(boundary_dist_names, function(x,sfc,polling_places){
    district_polling_places <- dplyr::filter(polling_places,is_coord == TRUE,district == x) %>% select(-district)
    boundary <- dplyr::filter(sfc,district == x) 
    return(list(boundary = boundary,polling_places = district_polling_places))
  }, sfc = boundaries, polling_places = polling_places, simplify = FALSE, USE.NAMES = TRUE)
  
  voronoi <- suppressWarnings(bind_rows(lapply(sf_polling_places,rs_voronoi,crs = crs)))
  
  missing_pp_id <- setdiff(unique(filter(polling_places,is_coord == TRUE)$pp_id),unique(voronoi$pp_id))
  
  if (length(missing_pp_id) != 0){
    warning("Polling places with id numbers: ",paste(missing_pp_id, collapse = ", ")," have been clipped from voronoi. Consider merging the results from these booths with nearby ones.")
  }
  
  election <- list(name = name,boundaries = boundaries,polling_places = polling_places, voronoi = voronoi, adj_results = adj_results)
  class(election) <- c("rs_election",class(election))
  election
}
print.rs_election <- function(x,...){
  cat("Election Name:", x$name,"\n\n")
  cat("Attached district map contains",nrow(x$boundaries),"districts\n")
  cat("    Names:",paste(head(x$boundaries$district,n=10),collapse = ", "),"\n\n")
  cat("Attached list of", nrow(x$polling_places), "polling places - With",sum(x$polling_places$is_coord),"polling places with coordinates\n")
  cat("    Voronoi diagram contains",nrow(x$voronoi),"polling places\n\n")
  cat(length(x$adj_results),"results attached:",paste(sapply(x$adj_results,function(y){y$name}), collapse = ", "),"\n")
  invisible(x)
}
#Consider asking for sources of data
rs_boundary_set <- function(sfc, names, to_crs=NULL){
  if (!inherits(sfc,"sf")){
    stop("'sfc' must be a Simple Feature. These can be generated using the 'sf' package.")
  }
  if (!is.character(sfc[[names]])){
    stop("'names' must be a column of 'sfc' of type 'character'")
  }
  if (!is.null(to_crs)){
    if (!is.numeric(to_crs)){
      stop("'to_crs' must be of type 'numeric'")
    }
    sfc <- sf::st_transform(sfc,to_crs)
  }
  sfc <- dplyr::rename(sfc,"district" = !! sym(names)) %>%
    dplyr::group_by(district) %>%
    dplyr::summarise(.groups = "drop")
  if (sf::st_is_longlat(sfc)){
    warning("Ensure that 'sfc' uses a projected CRS. If you're receiving warnings, use 'to_crs' to convert the shapefile into an appropriate projected CRS for the data.")
  }
  class(sfc) <- c("rs_boundary_set",class(sfc))
  sfc
}
rs_polling_places <- function(df,pp_id,dist_names,longitude = "longitude",latitude = "latitude",crs=4326){
  protected_column_names <- c("pp_id","district","longitude","latitude","is_coord")
  if (!inherits(df,"data.frame")){
    stop("'df' must be a Data Frame")
  }
  protected_column_names_used <- any(names(dplyr::select(df,!c(pp_id,dist_names,longitude,latitude))) %in% protected_column_names)
  if(protected_column_names_used){
    stop("Additional column names must not be any of \"", paste(protected_column_names, collapse = "\" \""),"\"")
  }
  df <- dplyr::rename(df,"pp_id" = !! sym(pp_id),"district" = !! sym(dist_names),"longitude" = !! sym(longitude),"latitude" = !! sym(latitude)) %>%
    dplyr::mutate(is_coord = !is.na(longitude)&!is.na(latitude)) %>%
    dplyr::relocate(pp_id,district,longitude,latitude,is_coord)
  attr(df, "crs") <- crs
  class(df) <- c("rs_polling_places",class(df))
  df
}
# Note only polling place id matters here, not the district
rs_election_results <- function(name, df, pp_id, group_code, votes){
  df <- dplyr::select(df,c(pp_id,group_code,votes)) %>%
    dplyr::rename("pp_id" = pp_id, "group_code" = group_code, "votes" = votes)
  rs_election_results <- list(name = name, results = df)
  class(rs_election_results) <- c("rs_election_results",class(rs_election_results))
  rs_election_results
}
rs_polcolpal_create <- function(df, breaks){
  input_length <- nrow(df)
  if (length(breaks) != 10){
    stop("'breaks' must be of length 10")
  }
  party_palette <- function(color){
    # Code with modification from https://hihayk.github.io/scale/
    # Made using Javascript, with two installed npm packages - Browserify and Color
    engine <- v8(global = "window")
    engine$source("out.js")
    engine$eval("function getColorsList(colorsAmount, colorsShiftAmount, mixColor, rotate, saturation, mainColor)  {
    const colorsList = []
     let step
     for (step = 0; step < colorsAmount; step++) {
         colorsList
          .push(color(mainColor).rotate((step + 1) / colorsAmount * -rotate)
          .saturate((step + 1) / colorsAmount * (saturation / 100))
          .mix(color(mixColor), (colorsShiftAmount / 100) * (step + 1) / colorsAmount)
          .hex())
      }
     return colorsList
    }")
    color_JS_String <- paste0("'",color,"'")
    darker_Col <- engine$eval(paste0("getColorsList(4, 64, 'black', 0, 64, ",color_JS_String,")")) %>% 
      str_split(",") %>%
      .[[1]]
    lighter_Col <- engine$eval(paste0("getColorsList(5, 80, 'white', 0, 80, ",color_JS_String,")")) %>% 
      str_split(",") %>%
      .[[1]] %>%
      rev()
    return(c(lighter_Col,color,darker_Col))
  }
  
  df2 <- data.frame(colour_code = -2:(input_length*10-1),
                    pty=c(NA,NA,rep(df[[1]],each=10)),
                    strength_code = c(NA,NA,rep(0:9,input_length)),
                    colour = c("#FFFFFF","#FFFFFF",unlist(lapply(df[[2]],party_palette))))
  
  choose_color <- function(pty_code,vote_pct,type = "colour_code"){
    if (any(is.na(c(pty_code,vote_pct)))|is.nan(vote_pct)){
      row_to_pick <- filter(df2, colour_code == -2)
    }
    else {
      strength <- findInterval(vote_pct, breaks, left.open = TRUE) - 1
      if (strength == -1){
        row_to_pick <- filter(df2, colour_code == -1)
      }
      else {
        row_to_pick <- filter(df2, pty == pty_code, strength_code == strength)
      }
    }
    row_to_pick[[type]]
  }
  ggplot_scale <- as.list(df2$colour)
  names(ggplot_scale) <- df2$colour_code
  legend <- function(square_size = 20, pty_codes){
    filtered_df <- filter(df2, colour_code >= 0, pty %in% pty_codes)
    plot <- ggplot(filtered_df) +
      geom_point(aes(fill=as.factor(colour_code),x=strength_code,y=pty),shape=22,size=square_size,color="white")+
      scale_fill_manual(values=filtered_df$colour)+
      scale_x_continuous(breaks = seq(0.5,8.5,1), labels = breaks[-1]*100) +
      coord_fixed(ratio = 1) + 
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none",
            axis.text.x = element_text(vjust = 3), 
            plot.background = element_rect(fill = "transparent", colour = "transparent"))
    return(plot)
  }
  polcolpal <- list(values = df2,choose_color = choose_color,ggplot_scale = ggplot_scale, legend = legend)
  class(polcolpal) <- c("rs_polcolpal", class(polcolpal))
  polcolpal
}
rs_election_as_sf <- function(election, result_name, polcolpal, adjustments=TRUE, mode = "plurality", single_group_code = NA){
  if (!(mode %in% c("plurality","single group"))){
    stop("'mode' must be one of \"plurality\" or \"single group\"")
  }
  result_column <- ifelse(adjustments,"adj_vote_pct","vote_pct")
  result <- election$adj_results[match(result_name,sapply(election$adj_results,function(x){x$name}))][[1]]$results %>%
    group_by(pp_id)
  if (mode == "plurality"){
    if (!is.na(single_group_code)){
      stop("'single_group_code' only works if 'mode' = \"single group\"\nTry setting 'mode' = \"single group\" or removing 'single_group_code' as an argument")
    }
    result_to_join <- slice_max(result, order_by = !!as.name(result_column), n=1,with_ties = FALSE)
  }
  if (mode == "single group"){
    result_to_join <- filter(result, group_code == single_group_code) %>%
      slice_max(order_by = !!as.name(result_column), n=1,with_ties = FALSE)
  }
  if (nrow(result_to_join) == 0){
    stop("There are no results to add to the map.\nOne possible cause is if 'single_group_code' doesn't refer to a group code in the result.")
  }
  if (!all(unique(result_to_join$group_code) %in% unique(polcolpal$values$pty))){
    stop("'polcolpal' must have a colour for each group code in the result")
  }
  rs_as_sf <- left_join(election$voronoi,result_to_join, by = "pp_id") %>%
    rowwise() %>%
    mutate(col_code = polcolpal$choose_color(group_code, vote_pct = !!as.name(result_column))) %>%
    ungroup()
  class(rs_as_sf) <- c("rs_election_as_sf",class(rs_as_sf))
  attr(rs_as_sf, "palette") <- polcolpal
  attr(rs_as_sf, "result_name") <- result_name
  rs_as_sf
}
rs_plot <- function(x, pdf_file_name = NA){
  plot <- ggplot(x) +
    geom_sf(aes(fill = as.factor(col_code)), color = "white") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_manual(values = attr(x,"palette")$ggplot_scale) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none") +
    attr(x,"palette")$legend(8,pty_codes = unique(x$group_code)) + 
    theme(panel.background = element_blank(),
          plot.background = element_blank()) +
    plot_layout(ncol = 1, widths = 1, heights = c(1.2,sqrt(2)-1.2)) &
    theme(plot.margin = unit(c(0,0,0,0),"pt"))
  if (is.na(pdf_file_name)){
    plot
  }
  else {
    ggsave(filename = pdf_file_name, plot = plot)
  }
}
rs_redistribution <- function(election, new_boundary_set, slivers_rm = TRUE){
  new_boundary_set <- rename(new_boundary_set, "to_district" = district)
  voronoi <- rename(election$voronoi, "from_district" = district)
  voronoi$initial_area <- st_area(voronoi)
  intersection <- st_intersection(voronoi,new_boundary_set)
  intersection$new_area <- st_area(intersection)
  intersection <- mutate(intersection,new_area_pct = as.double(new_area/initial_area))
  if (slivers_rm == TRUE){
    intersection <- filter(intersection,new_area_pct > 0.02)
  }
  intersection <- group_by(intersection,pp_id) %>%
    mutate(adj_new_area_pct = new_area_pct/(sum(new_area_pct))) %>%
    ungroup() %>%
    select(-c(initial_area,new_area,new_area_pct))
  
  rs_redistribution <- list(voronoi = intersection,adj_results = election$adj_results)
  class(rs_redistribution) <- c("rs_redistribution",class(rs_redistribution))
  rs_redistribution
}
summary.rs_redistribution <- function(object, ...){
  transfer_proportions <- select(st_drop_geometry(object$voronoi),pp_id,to_district,adj_new_area_pct)
  lapply(object$adj_results, function (x){
    x$results <- left_join(x$results,transfer_proportions, by = "pp_id") %>%
      group_by(group_code,to_district)  %>%
      summarise(votes = sum(adj_votes*adj_new_area_pct), .groups = "drop")
    x
  })
}
summary.rs_election <- function(object, ...){
  transfer_proportions <- select(st_drop_geometry(object$voronoi),pp_id,district)
  lapply(object$adj_results, function (x){
    x$results <- left_join(x$results,transfer_proportions, by = "pp_id") %>%
      group_by(group_code,district)  %>%
      summarise(votes = sum(adj_votes), .groups = "drop")
    x
  })
}
