#' Análise do snap do R5
#'
#' Acha a distância do centroide de cada célula das grades utilizadas no projeto
#' até o ponto em que o snap do R5 de fato é feito. Importante: lembrar de mudar
#' o parâmetro de memória a ser utilizada pelo java:
#' \code{options(java.parameters = "-Xmx64G")}.
#'
#' @param munis Uma string. A sigla do município, como utilizado no projeto -
#'   uma sigla de três letras. Se \code{"todos"} (o padrão), roda a análise para
#'   todos os municípios do projeto.
#'
#' @section Detalhes:
#' Primeiro aplica a função \code{r5r::find_snap()} pra achar o snap dos
#' centroides das células das grades usadas no projeto.
#' Classifica esses snaps em quatro possíveis categorias:
#' \itemize{
#'   \item{Bom: dentro do hexágono, ou seja, a uma distância de até 174~ metros
#'         (tamanho da aresta do hexágono);}
#'   \item{Ok: dentro dos hexágonos imediatamente vizinhos, ou seja, a uma
#'         distãncia entre 174~ e 452~ metros (três vez a distância do centroide
#'         do hexágono ao ponto médio de sua aresta);}
#'   \item{Ruim: entre 452~ e 1600 metros; bem ruim, mas pelo menos achou algo.}
#'   \item{Péssimo: não conseguiu fazer o snap.}
#' }
#' Depois gera um relatório que inclui um mapa da distribuição de cada categoria
#' na cidade e uma tabela que resume o número de pessoas e oportunidades por
#' categoria. Salva esse relatório na pasta \code{resultados/analise_snap}.
#' Também salva a lista com a classificação do snap referente a cada célula das
#' grades, em formato \code{.rds}.
#'
#' @return Uma lista com a classificação dos snaps de cada cidade, em formato
#'   \code{c("data.table", "sf")}. Retornada de forma invisível.
#'
#' @examples
#' if (interactive()) {
#'
#'   options(java.parameters = "-Xmx64G")
#'
#'   munis = c("rio")
#'   analisar_snap(munis)
#'
#'   # ou roda pra todos
#'   analisar_snap()
#'
#' }
#'
#' @export
analisar_snap <- function(munis = "todos") {

  # munis_list é um objeto interno do pacote

  todos_munis <- munis_list$munis_df$abrev_muni

  # checando inputs

  if (identical(munis, "todos")) munis <- todos_munis

  checkmate::assert_names(munis, subset.of = todos_munis)

  # pega os dados agregados por hexágonos e seleciona as colunas desejadas

  grades <- lapply(
    munis,
    function(i)
      data.table::setDT(
        readRDS(
          paste0(
            "../../data/acesso_oport/hex_agregados/2019/hex_agregado_",
            i,
            "_09_2019.rds"
          )
        )
      )
  )
  grades <- lapply(
    grades,
    function(dt)
      dt[
        ,
        .(
          id_hex,
          pop = pop_total,
          emp = empregos_total,
          saude = saude_total,
          edu = edu_total,
          geom = geometry
        )
      ]
  )
  names(grades) <- munis

  # calcula o centroide das celulas
  # suprime os warnings relacionados aos atributos serem considerados constantes
  # nas geometrias e dos centroides não serem todos certinhos nesse datum

  suppressWarnings(
    centroides <- lapply(grades, function(i) sf::st_centroid(sf::st_as_sf(i)))
  )

  # prepara os inputs pra usar o {r5r}

  centroides <- lapply(
    centroides,
    function(df) sf::st_sf(id = df$id_hex, geom = df$geom)
  )

  # cada cidade precisa do seu próprio r5r_core

  endereco_grafos <- paste0("../../r5/graphs/2019/", munis)

  r5r_cores <- lapply(endereco_grafos, r5r::setup_r5, verbose = FALSE)
  names(r5r_cores) <- munis

  # calcula os snaps e classifica pela distância ao ponto original

  snaps <- mapply(r5r::find_snap, r5r_cores, centroides, SIMPLIFY = FALSE)
  snaps <- lapply(
    snaps,
    function(dt) dt[, classificacao := classificar_snap(distance)]
  )

  # "limpa" o r5r

  lapply(r5r_cores, r5r::stop_r5)
  rJava::.jgc(R.gc = TRUE)

  # puxa a classificação dos snaps pra grade

  grades <- lapply(
    seq_along(munis),
    function(i)
      grades[[i]][
        snaps[[i]],
        on = c(id_hex = "point_id"),
        classificacao := i.classificacao
      ]
  )

  # classifica hexágonos sem população e oportunidades como NA

  grades <- lapply(
    grades,
    function(dt)
      dt[pop == 0 & emp == 0 & saude == 0 & edu == 0, classificacao := NA]
  )
  names(grades) <- munis

  # cria uma pasta pra salvar os mapas

  if(!dir.exists("./resultados")) dir.create("./resultados")

  if(!dir.exists("./resultados/analise_snap"))
    dir.create("./resultados/analise_snap")

  # cria os relatórios e salva na pasta acima

  indices_muni <- setdiff(match(munis, munis_list$munis_df$abrev_muni), NA)

  invisible(
    mapply(
      criar_relatorio,
      grade = grades,
      sigla_muni = munis,
      nome_muni = munis_list$munis_df$name_muni[indices_muni],
      SIMPLIFY = FALSE
    )
  )

  return(invisible(grades))

}

#' Classificar snap pela distância
#'
#' Classifica cada snap pela distância entre o ponto original e o encontrado
#' pelo R5. São quatro as possíveis categorias:
#' \itemize{
#'   \item{Bom: dentro do hexágono, ou seja, a uma distância de até 174~ metros
#'         (tamanho da aresta do hexágono);}
#'   \item{Ok: dentro dos hexágonos imediatamente vizinhos, ou seja, a uma
#'         distãncia entre 174~ e 452~ metros (três vez a distância do centroide
#'         do hexágono ao ponto médio de sua aresta);}
#'   \item{Ruim: entre 452~ e 1600 metros; bem ruim, mas pelo menos achou algo.}
#'   \item{Péssimo: não conseguiu fazer o snap.}
#' }
#'
#' @param distancia Um vetor numérico. Distância entre o ponto e o snap.
#'
#' @return Um vector de factors com a classificação de cada snap.
#'
#' @keywords internal
classificar_snap <- function(distancia) {

  nome_categorias <- c(
    "Bom (<= 174 m)",
    "Ok (<= 452 m)",
    "Ruim (<= 1600 m)",
    "Péssimo (sem snap)"
  )

  categorias <- factor(nome_categorias, levels = nome_categorias)

  classificacao <- data.table::fcase(
    distancia <= 174, categorias[1],
    distancia <= 452, categorias[2],
    distancia <= 1600, categorias[3],
    default = categorias[4]
  )

  return(classificacao)

}



#' Criar relatório de análise dos snaps
#'
#' Cria um relatório de análise dos snaps. Inclui um mapa com a geolocalização
#' dos snaps e uma tabela que vê a quantidade de pessoas e oportunidades por
#' categoria da classificação.
#'
#' @param grade Um \code{c("data.table", "sf")} com a grade e dados do
#'   município.
#' @param sigla_muni Uma string. A sigla do município.
#' @param nome_muni Uma string. O nome do município.
#'
#' @return Retorna o \code{data.table} passado no parâmetro \code{grade}
#'   invisivelmente.
#'
#' @keywords internal
criar_relatorio <- function(grade, sigla_muni, nome_muni) {

  # cria caminho pro arquivo com normalizePath porque senão o rmarkdown
  # interpreta errado

  arquivo_resultado <- normalizePath(
    paste0("./resultados/analise_snap/relatorio_", sigla_muni, ".html")
  )

  # envia parâmetros pro esqueleto do relatório

  rmarkdown::render(
    system.file(
      "rmarkdown/esqueleto_analise_snap.Rmd",
      package = "aopcoisas"
    ),
    output_file = arquivo_resultado,
    params = list(
      grade = grade,
      nome_muni = nome_muni
    ),
    quiet = TRUE
  )

  return(grade)

}



#' Atualizar malhas viárias
#'
#' Atualiza as malhas viárias em formato \code{.pbf} e apaga o grafo gerado pelo
#' R5.
#'
#' @param munis Uma string. A sigla do município, como utilizado no projeto -
#'   uma sigla de três letras. Se \code{"todos"} (o padrão), roda a análise para
#'   todos os municípios do projeto.
#'
#' @return \code{NULL}, invisivelmente. Função chamada principalmente pelo seus
#' efeitos.
#'
#' @keywords internal
atualizar_pbfs <- function(munis = "todos") {

  todos_munis <- munis_list$munis_df$abrev_muni

  # checando inputs

  if (identical(munis, "todos")) munis <- todos_munis

  checkmate::assert_names(munis, subset.of = todos_munis)

  # move os .pbfs atualizados, na pasta data-raw/malha_viaria/2020 para as
  # pastas dos grafos do r5

  invisible(
    lapply(
      munis,
      function(i)
        file.copy(
          paste0("../../data-raw/malha_viaria/2020/", i, "/", i, "_2020.osm.pbf"),
          paste0("../../r5/graphs/2019/", i),
          overwrite = TRUE
        )
    )
  )

  # o grafo do r5 passa a ser inválido, então apaga os .dat e .mapbdb que antes
  # existiam na pasta
  # se der erro é porque não havia arquivos, então essa etapa pode ser pulada

  resultado_apagar <- tryCatch(
      apagar_arquivos_r5(munis),
      error = function(cnd) return(cnd)
  )

  return(invisible(NULL))

}



#' Apagar os arquivos gerados pelo R5
#'
#' Apaga os arquivos gerados pelo R5, em formato \code{.mapdb}, \code{.mapdb.p}
#' e \code{.dat}.
#'
#' @param munis Uma string. A sigla do município, como utilizado no projeto -
#'   uma sigla de três letras. Se \code{"todos"} (o padrão), roda a análise para
#'   todos os municípios do projeto.
#'
#' @return \code{NULL}, invisivelmente. Função chamada principalmente pelo seus
#' efeitos.
#'
#' @keywords internal
apagar_arquivos_r5 <- function(munis = "todos") {

  todos_munis <- munis_list$munis_df$abrev_muni

  # checando inputs

  if (identical(munis, "todos")) munis <- todos_munis

  checkmate::assert_names(munis, subset.of = todos_munis)

  # lista arquivos a serem apagados
  # o vapply gera uma matriz com os arquivos de cada cidade

  caminho_pastas <- paste0("../../r5/graphs/2019/", munis)

  arquivos_para_apagar <- vapply(
    caminho_pastas,
    function(i) {
      arquivos_na_pasta <- list.files(i)
      arquivos_desejados <- arquivos_na_pasta[
        grepl("\\.dat$", arquivos_na_pasta) |
          grepl("\\.mapdb", arquivos_na_pasta)
      ]
      file.path(i, arquivos_desejados)
    },
    character(3)
  )
  arquivos_para_apagar <- as.vector(arquivos_para_apagar)

  # apaga os arquivos

  a <- file.remove(arquivos_para_apagar)

  return(invisible(NULL))

}
