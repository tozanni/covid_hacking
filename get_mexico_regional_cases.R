
#' Mexican Regional Case Counts
#'
#'
#' @description Fetches COVID data by region. Data is collated by: https://github.com/marianarf/covid19_mexico_analysis
#' @return A dataframe of Mexican regional case counts (by symptom onset date)
#' @export
#' @importFrom readr read_csv
#' @importFrom stringi stri_trans_general
#' @importFrom dplyr mutate select arrange group_by n lag ungroup left_join
#' @importFrom memoise cache_filesystem memoise
#
#
# Este extractor obtiene los primeros casos de cada estado. No toma en cuenta recuperados, solo nuevos confirmados.
#

get_mexico_regional_cases <- function() {
  path <- "https://raw.githubusercontent.com/marianarf/covid19_mexico_analysis/master/output_data/covid_mex_20200409.csv"
  
  ## Set up cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)
  
  cases0 <- mem_read(path) %>%
    #Quitar acentos en los nombres de los estados
    dplyr::mutate(Region=stringi::stri_trans_general(str = Region, id = "Latin-ASCII")) %>%
    
    # Usamos Date_Confirmed porque Date_Symptoms no trae un formato consistente
    dplyr::mutate(date=as.Date(Date_Confirmed, format = "%d/%m/%Y")) %>%
    
    #Crear columna local o importado
    dplyr::mutate(import_status = ifelse(Origin == "Contacto" , "local", "imported")) %>%
    dplyr::group_by(Region, import_status, date) %>%
    
    ## Sumar los casos individuales para obtener los nuevos casos del día por región
    dplyr::summarize(total = n()) %>% 
  
    #Limpieza final
    filter(!is.na(Region) & Region != "Region" )
    
  #Padding de los dias sin datos
  min_date <- min(cases0$date, na.rm = TRUE)
  max_date <- max(cases0$date, na.rm = TRUE) 
  all_dates <- data.frame(list(date=seq(min_date, max_date, by="day")))
  
  #Seleccionar regiones y status
  all_states <- cases0 %>% dplyr::select(Region, import_status) %>% dplyr::distinct()
  
  #Cross Join (con fake column)
  all_dates$fake <- 1
  all_states$fake <- 1
  
  all_states_dates <- dplyr::full_join(all_dates, all_states, by = "fake") %>% 
    dplyr::select(-fake)
  all_states_dates %>% View()
  
  #Full join y fill de NAs con 0
  cases <- merge(all_states_dates, cases0, all=T)
  cases$total[which(is.na(cases$total))] <- 0
    
  #La columna new_cases tiene los casos del dia y cases el acumulado
  cases1 <- cases %>% dplyr::select(date, state = Region, import_status, new_cases = total) %>%
    dplyr::group_by(state, import_status) %>%
    dplyr::mutate(cases = cumsum(new_cases))
  
  #cases1 %>% filter(state == "JALISCO") %>% View()
  #dplyr::select(-new_cases)
  # cases %>% View()
  return(cases1)
}


