#' Análise da conectividade dos hexágonos como origens e destinos
#'
#' Calcula quantos destinos um hexágono pode alcançar, bem como quantas origens
#' conseguem alcançá-lo. Importante: lembrar de mudar o parâmetro de memória a
#' ser utilizada pelo java: `options(java.parameters = "-Xmx64G")`.
#'
#' @param munis Uma string. A sigla do município, como utilizado no projeto -
#'   uma sigla de três letras. Se `"todos"` (o padrão), roda a análise para
#'   todos os municípios do projeto.
#' @param n_cores Um numérico. O número de cores para usar no cálculo das
#'   matrizes de tempo de viagem. O padrão é 10 (metade dos cores do servidor).
#'
#' @section Detalhes:
#' A análise da acessibilidade levantou o questionamento de quantos hexágonos
#' poderiam estar isolados também no destino de um deslocamento, e não apenas em
#' sua origem. Essa análise mostra que todos os hexágonos isolados na origem
#' também o são na origem. A mesma "metodologia" utilizada na análise da
#' acessibilidade unitária, em que são calculadas as viagens também para
#' hexágonos que não são utilizados no projeto, se aplica nesta análise. Para
#' mais detalhes ver a documentação da função `analisar_acess_unitaria`.
#'
#' @return Uma lista com hexágonos que alcancem menos que 15 destinos ou que
#' sejam alcançados por menos de 15 origens.
#'
#' @examples
#' if (interactive()) {
#'
#'   options(java.parameters = "-Xmx64G")
#'
#'   munis = c("rio")
#'   analisar_conectividade_od(munis)
#'
#'   # ou roda pra todos
#'   analisar_acess_unitaria()
#'
#' }
#'
#' @export
analisar_conectividade_od <- function(munis = "todos", n_cores = 10) {

  # munis_list é um objeto interno do pacote

  todos_munis <- munis_list$munis_df$abrev_muni

  # checando inputs

  if (identical(munis, "todos")) munis <- todos_munis

  checkmate::assert_names(munis, subset.of = todos_munis)

  # lê a grade de cada uma das cidades, seleciona colunas de opoortunidades e
  # calcula o centroide de suas células
  # suprime os wanings relacionados ao cálculo dos centroides

  grades <- lapply(
    munis,
    function(i)
      readRDS(
        paste0("../../data/acesso_oport/hex_agregados/2019/hex_agregado_", i, "_09_2019.rds")
      )
  )

  grades <- lapply(
    grades,
    function(df)
      dplyr::select(
        df,
        id_hex,
        pop = pop_total,
        emp = empregos_total,
        saude = saude_total,
        edu = edu_total,
        geom = geometry
      )
  )

  suppressWarnings(
    centroides <- lapply(grades, sf::st_centroid)
  )

  # processa a tabela dos centroides pra que ela possar ser usado como origem/
  # destino na r5r::travel_time_matrix()

  centroides <- lapply(
    centroides,
    function(i) data.table::setDT(i)[, .(id = id_hex, geom)]
  )

  origens <- destinos <-  lapply(centroides, sf::st_as_sf)

  # roda r5r::setup_r5() pra cada cidade

  endereco_grafos <- paste0("../../r5/graphs/2019/", munis)

  r5r_cores <- lapply(endereco_grafos, r5r::setup_r5, verbose = FALSE)

  # calcula a acessibilidade em cada cidade

  ttms <- mapply(
    r5r::travel_time_matrix,
    r5r_cores,
    origens,
    destinos,
    MoreArgs = list(
      n_threads = n_cores,
      verbose = FALSE
    ),
    SIMPLIFY = FALSE
  )

  # "limpa" o r5r - suprime um warning relacionado ao lapply

  suppressWarnings(
    invisible(lapply(r5r_cores, r5r::stop_r5))
  )
  rJava::.jgc(R.gc = TRUE)

  # calcula o número de destinos que um hexágono alcança

  conexao_destinos <- lapply(
    ttms,
    function(dt) dt[, .(destinos_alcancados = .N), by = .(fromId)]
  )

  # calcula o número de origens que alcançam um hexágono

  conexao_origens <- lapply(
    ttms,
    function(dt) dt[, .(origens_alcancam = .N), by = .(toId)]
  )

  # puxa os dados de conectividade pra grade

  grades <- lapply(
    seq_along(munis),
    function(i){
      data.table::setDT(grades[[i]])[
        conexao_destinos[[i]],
        on = c(id_hex = "fromId"),
        destinos_alcancados := i.destinos_alcancados
      ]

      grades[[i]][
        conexao_origens[[i]],
        on = c(id_hex = "toId"),
        origens_alcancam := origens_alcancam
      ]
    }
  )

  # define a conectividade de células sem pessoas e oportunidades como NA

  grades <- lapply(
    grades,
    function(dt)
      dt[
        pop == 0 & emp == 0 & saude == 0 & edu == 0,
        `:=`(
          destinos_alcancados = NA,
          origens_alcancam = NA
        )
      ]
  )

  # cria uma pasta pra salvar os mapas

  if (!dir.exists("./resultados")) dir.create("./resultados")

  if (!dir.exists("./resultados/analise_conectividade_od"))
    dir.create("./resultados/analise_conectividade_od")

  # cria os relatórios e salva na pasta acima

  indices_muni <- setdiff(match(munis, munis_list$munis_df$abrev_muni), NA)

  invisible(
    mapply(
      criar_relatorio_conectividade_od,
      grade = grades,
      sigla_muni = munis,
      nome_muni = munis_list$munis_df$name_muni[indices_muni],
      SIMPLIFY = FALSE
    )
  )

  # salva também uma lista que identifica os hexágonos problemáticos (com
  # conectividade na origem ou no destino menor do que 15 e maior do que 0) em
  # cada cidade

  hexagonos_problematicos <- lapply(
    grades,
    function(dt)
      dt[
        (destinos_alcancados <= 15 & destinos_alcancados > 0) |
          (origens_que_alcancam <= 15 & origens_que_alcancam > 0)
      ]$id_hex
  )

  saveRDS(
    hexagonos_problematicos,
    "./resultados/analise_conectividade_od/hexagonos_problematicos.rds"
  )

  return(invisible(hexagonos_problematicos))

}



#' Criar relatório de conectividade dos hexágonos como origens e destinos
#'
#' Cria um relatório de conectividade dos hexágonos como origens e destinos.
#' Inclui mapas com a distribuição do número de destinos alcançados por hexágono
#' e com a distribuição do números de origens que alcançam cada hexágono, bem
#' como tabelas que ajudam a identificar hexágonos problemáticos.
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
criar_relatorio_conectividade_od <- function(grade, sigla_muni, nome_muni) {

  # cria caminho pro arquivo com normalizePath porque senão o rmarkdown
  # interpreta errado

  arquivo_resultado <- normalizePath(
    paste0("./resultados/analise_conectividade_od/relatorio_", sigla_muni, ".html")
  )

  # envia parâmetros pro esqueleto do relatório

  rmarkdown::render(
    system.file(
      "rmarkdown/esqueleto_analise_conectividade_od.Rmd",
      package = "aopcoisas"
    ),
    output_file = arquivo_resultado,
    params = list(
      grade = grade,
      nome_muni = nome_muni
    ),
    quiet = TRUE
  )

  return(invisible(grade))

}
