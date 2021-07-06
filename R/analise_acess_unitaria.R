#' Análise da acessibilidade unitária
#'
#' Calcula a acessibilidade para cada cidade como se houvesse uma única
#' oportunidade em cada uma de suas células. Importante: lembrar de mudar o
#' parâmetro de memória a ser utilizada pelo java:
#' \code{options(java.parameters = "-Xmx64G")}.
#'
#' @param munis Uma string. A sigla do município, como utilizado no projeto -
#'   uma sigla de três letras. Se \code{"todos"} (o padrão), roda a análise para
#'   todos os municípios do projeto.
#' @param n_cores Um numérico. O número de cores para usar no cálculo da
#'   acessibilidade. O padrão é 10 (metade dos cores do servidor).
#'
#' @section Detalhes:
#' Calcula a acessibilidade usando a r5r::accessibility() como sem em cada
#' hexágono houvesse uma oportunidade. Importante notar que literalmente todas
#' as células das cidades contém uma oportunidade, e não apenas as que são
#' utilizadas no roteamento do projeto, de fato. Embora os mapas gerados nos
#' relatórios apresentem as células sem população e oportunidades de emprego,
#' saúde e educação como NA, isso é feito em uma etapa de pós-processamento,
#' apenas pra não apresentar como problemáticos hexágonos que não são
#' considerados no projeto. Para ilustrar porque isso é importante, vale olhar
#' os relatórios gerados pra cidade de São Paulo:
#' \itemize{
#'   \item{\code{relatorio_spo_full}: O resultado retornado pela
#'         r5r::accessibility(). Calcula a acessibilidade mesmo para célculas
#'         que não são utilizadas no projeto AOP porque não têm oportunidades e
#'         pessoas;}
#'   \item{\code{relatorio_spo}: O resultado como apresentado para todas as
#'         cidades. Mostra os resultados comentados no item acima, mas retrata
#'         as células sem oportunidades e pessoas como NA. Ou seja, os hexágonos
#'         problemáticos são hexágonos considerados na análise e que precisam
#'         ser tratados.}
#'   \item{\code{relatorio_spo_antigo}: o resultado que é obtido quando se
#'         calcula a acessibilidade apenas considerando os hexágonos
#'         considerados no projeto. Muitos hexágonos passam a serem considerados
#'         problemáticos não porque são pouco conectados com seus vizinhos, mas
#'         porque não têm nenhum vizinhos (ou têm poucos) considerados no
#'         projeto. O problema desses hexágonos, portanto, não está relacionado
#'         à rede viária, mas sim ao isolamento, que é uma informação que se
#'         obtém das análises do projeto. Esses hexágonos, portanto, não
#'         devem ser corrigidos.}
#' }
#'
#' @return Uma lista com hexágonos que tenham acessibilidade unitária menor ou
#' igual a 15 e maior do que 0, pra cada cidade. Retornada de forma invisível.
#'
#' @examples
#' if (interactive()) {
#'
#'   options(java.parameters = "-Xmx64G")
#'
#'   munis = c("rio")
#'   analisar_acess_unitaria(munis)
#'
#'   # ou roda pra todos
#'   analisar_acess_unitaria()
#'
#' }
#'
#' @export
analisar_acess_unitaria <- function(munis = "todos", n_cores = 10) {

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
  # destino na r5r::accessibility()

  centroides <- lapply(
    centroides,
    function(i) data.table::setDT(i)[, .(id = id_hex, geom)]
  )

  origens <- lapply(centroides, sf::st_as_sf)
  destinos <- lapply(
    centroides,
    function(i)
      sf::st_as_sf(
        data.table::copy(i)[, oportunidades := 1]
      )
  )

  # roda r5r::setup_r5() pra cada cidade

  endereco_grafos <- paste0("../../r5/graphs/2019/", munis)

  r5r_cores <- lapply(endereco_grafos, r5r::setup_r5, verbose = FALSE)

  # calcula a acessibilidade em cada cidade

  acessibilidade <- mapply(
    r5r::accessibility,
    r5r_cores,
    origens,
    destinos,
    MoreArgs = list(
      opportunities_colname = "oportunidades",
      cutoffs = 120L,
      max_trip_duration = 120L,
      n_threads = n_cores,
      verbose = TRUE
    ),
    SIMPLIFY = FALSE
  )

  # "limpa" o r5r - suprime um warning relacionado ao lapply

  suppressWarnings(
    invisible(lapply(r5r_cores, r5r::stop_r5))
  )
  rJava::.jgc(R.gc = TRUE)

  # puxa a acessibilidade pra grade

  grades <- lapply(
    seq_along(munis),
    function(i)
      data.table::setDT(grades[[i]])[
        acessibilidade[[i]],
        on = c(id_hex = "from_id"),
        acessibilidade := i.accessibility
      ]
  )

  # define a acessibilidade de células sem pessoas e oportunidades como NA

  grades <- lapply(
    grades,
    function(dt)
      dt[pop == 0 & emp == 0 & saude == 0 & edu == 0, acessibilidade := NA]
  )

  # cria uma pasta pra salvar os mapas

  if (!dir.exists("./resultados")) dir.create("./resultados")

  if (!dir.exists("./resultados/analise_acess_unitaria"))
    dir.create("./resultados/analise_acess_unitaria")

  # cria os relatórios e salva na pasta acima

  indices_muni <- setdiff(match(munis, munis_list$munis_df$abrev_muni), NA)

  invisible(
    mapply(
      criar_relatorio_acess_unitaria,
      grade = grades,
      sigla_muni = munis,
      nome_muni = munis_list$munis_df$name_muni[indices_muni],
      SIMPLIFY = FALSE
    )
  )

  # salva também uma lista que identifica os hexágonso problemáticos (com
  # acessibilidade menor ou igual a 15 e maior do 0) em cada cidade

  hexagonos_problematicos <- lapply(
    grades,
    function(dt) dt[acessibilidade <= 15 & acessibilidade > 0]$id_hex
  )

  saveRDS(
    hexagonos_problematicos,
    "./resultados/analise_acess_unitaria/hexagonos_problematicos.rds"
  )

  return(invisible(hexagonos_problematicos))

}



#' Criar relatório de análise da acessibilidade unitária
#'
#' Cria um relatório de análise da acessibilidade unitária. Inclui um mapa com a
#' distribuição da acessibilidade, destacando quais são os hexágonos
#' problemáticos (com menos do que 15 de acessibilidade e mais do que 0).
#' Também apresenta uma tabela com hexágonos com acessibilidade menor do que 15,
#' dessa vez incluindo os com acessibilidade igual a 0. Hexágonos com
#' acessibilidade igual a 0 são aqueles em que o r5 não consegue fazer snap.
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
criar_relatorio_acess_unitaria <- function(grade, sigla_muni, nome_muni) {

  # cria caminho pro arquivo com normalizePath porque senão o rmarkdown
  # interpreta errado

  arquivo_resultado <- normalizePath(
    paste0("./resultados/analise_acess_unitaria/relatorio_", sigla_muni, ".html")
  )

  # envia parâmetros pro esqueleto do relatório

  rmarkdown::render(
    system.file(
      "rmarkdown/esqueleto_analise_acess_unitaria.Rmd",
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
