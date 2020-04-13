
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
  
  cases <- mem_read(path) %>%
    #Quitar acentos en los nombres de los estados
    dplyr::mutate(Region=stringi::stri_trans_general(str = Region, id = "Latin-ASCII")) %>%
    
    # Usamos Date_Confirmed porque es la que requiere EpiNow
    dplyr::mutate(date=as.Date(Date_Confirmed, format = "%m/%d/%Y")) %>%
    
    #Crear columna local o importado
    dplyr::mutate(import_status = ifelse(Origin == "Contacto" , "local", "imported")) %>%
    dplyr::group_by(Region, import_status, date) %>%
    
    ## Sumar los casos individuales para obtener los nuevos casos del día por región
    dplyr::summarize(cases = n()) %>% 
    
    #Limpieza final
    filter(!is.na(Region) & Region != "Region" )

  return(cases)
}


