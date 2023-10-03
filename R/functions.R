#' Busca BDPA
#'
#' Função para realizar busca na BDPA
#'
#' @param query Expressão de busca para recuperar dados na BDPA. Aceita operadores boleanos como "AND","OR", "NOT" e outros.
#'
#'
#' @return dataframe with BDPA's data
#' @export
#'
#' @examples data <- busca_bdpa("pecuária AND mato grosso AND leiteira")
#' head(data)
#' @importFrom readxl read_xls
#' @importFrom utils URLencode download.file
#'
busca_bdpa <- function(query){
  link_bdpa <- function(query){
    query <- URLencode(query)
    a <- "https://www.bdpa.cnptia.embrapa.br/consulta/busca?b=ad&busca="
    b <- "&qFacets="
    c <- "&biblioteca=vazio&sort=&paginacao=t&paginaAtual=1&ig=tn"
    query <- paste(a, query, b, query, c, sep = "")
    return(query)
  }
  link <- link_bdpa(query)
  pasta_temp <- tempdir()
  pasta_temp <- paste(pasta_temp, "temp_bdpa.xlsx", sep = "/")
  download.file(link, pasta_temp, mode = "wb")
  suppressMessages(
    file <- readxl::read_xls(path = pasta_temp, sheet=1, col_names = FALSE)
  )
  cat(as.character(file[1,1]),
      "\n",
      as.character(file[2,1]),
      "\n",
      as.character(file[3,1]),
      "\n",
      as.character(file[4,1]),
      sep = "")
  colnames(file) <- file[6,]
  file <- file[-c(1:6), ]
  return(file)
}
data <- busca_bdpa("daniela maciel") #bug com nome de colunas

# query <- busca_bdpa("pecuária AND impacto OR ilpf")
# data <- download_bdpa(query)

