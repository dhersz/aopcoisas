munis_df <- data.table::setDT(
  tibble::tribble(
    ~code_muni, ~abrev_muni, ~name_muni,        ~abrev_estado,  ~map_plot_ratio_wh,
    2304400,    "for",       "Fortaleza",       "CE",           1.2,
    3550308,    "spo",       "Sao Paulo",       "SP",           0.65,
    3304557,    "rio",       "Rio de Janeiro",  "RJ",           1.91,
    4106902,    "cur",       "Curitiba",        "PR",           0.62,
    4314902,    "poa",       "Porto Alegre",    "RS",           0.75,
    3106200,    "bho",       "Belo Horizonte",  "MG",           0.69,
    5300108,    "bsb",       "Brasilia",        "DF",           1.71,
    2927408,    "sal",       "Salvador",        "BA",           1.36,
    1302603,    "man",       "Manaus",          "AM",           1.27,
    2611606,    "rec",       "Recife",          "PE",           0.68,
    5208707,    "goi",       "Goiania",         "GO",           0.93,
    1501402,    "bel",       "Belem",           "PA",           0.65,
    3518800,    "gua",       "Guarulhos",       "SP",           0.91,
    3509502,    "cam",       "Campinas",        "SP",           1.20,
    2111300,    "slz",       "Sao Luis",        "MA",           0.78,
    3304904,    "sgo",       "Sao Goncalo",     "RJ",           1.21,
    2704302,    "mac",       "Maceio",          "AL",           0.74,
    3301702,    "duq",       "Duque de Caxias", "RJ",           0.61,
    5002704,    "cgr",       "Campo Grande",    "MS",           0.87,
    2408102,    "nat",       "Natal",           "RN",           0.70
  )
)

munis_list <- list(munis_df = munis_df)

save(munis_list, file = "R/sysdata.rda", compress = "gzip")
