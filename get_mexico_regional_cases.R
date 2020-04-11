
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
        #Estandarizar acentos en los nombres de los estados
        dplyr::mutate(Region=stringi::stri_trans_general(str = Region, id = "Latin-ASCII")) %>%
        dplyr::mutate(date=as.Date(Date_Symptoms, format = "%d/%m/%Y")) %>%
        dplyr::group_by(Region, date) %>%

        ## Sumar los casos individuales para obtener los nuevos casos del día
        dplyr::summarize(total = n()) %>%

        #La columna new_cases tiene los casos del dia y cases el acumulado
        dplyr::select(date, state = Region, new_cases = total) %>%
        dplyr::group_by(state) %>%
        dplyr::mutate(cases = cumsum(new_cases))

  #cases <- cases %>%
  #  dplyr::left_join(regions, by = "region")

  return(cases)
}

