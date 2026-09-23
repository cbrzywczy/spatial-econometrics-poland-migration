options(scipen = 999, digits = 3)
Sys.setenv(LANG = "en")

# Ustalamy sciezke skryptu tak, zeby katalogi byly liczone jak w wersji Python.
get_script_path <- function() {
  # Szuka lokalizacji skryptu w trybie Rscript, source() oraz przy pracy interaktywnej z IDE.
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE))
  }
  frame_files <- vapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) {
      return(as.character(frame$ofile)[1])
    }
    NA_character_
  }, character(1))
  frame_files <- frame_files[!is.na(frame_files)]
  if (length(frame_files) > 0) {
    return(normalizePath(frame_files[[length(frame_files)]], winslash = "/", mustWork = FALSE))
  }

  # Przy uruchamianiu zaznaczenia z konsoli szukamy skryptu od bieżącego katalogu w górę.
  current_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  repeat {
    candidates <- c(
      file.path(current_dir, "bezdrog", "01_mnk_wmnk_mice.R"),
      file.path(current_dir, "01_mnk_wmnk_mice.R")
    )
    existing <- candidates[file.exists(candidates)]
    if (length(existing) > 0) {
      return(normalizePath(existing[[1]], winslash = "/", mustWork = TRUE))
    }
    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      break
    }
    current_dir <- parent_dir
  }
  normalizePath(file.path(getwd(), "01_mnk_wmnk_mice.R"), winslash = "/", mustWork = FALSE)
}

find_project_dir <- function(script_path) {
  # Ustala katalog projektu po folderze data i skrypcie w bezdrog, takze przy uruchamianiu zaznaczenia z IDE.
  start_dirs <- unique(c(
    dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE)),
    normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), "Licencjat"), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(dirname(getwd()), "Licencjat"), winslash = "/", mustWork = FALSE)
  ))

  for (start_dir in start_dirs) {
    current_dir <- start_dir
    repeat {
      candidate_project <- if (basename(current_dir) == "bezdrog") dirname(current_dir) else current_dir
      if (
        dir.exists(file.path(candidate_project, "data")) &&
          file.exists(file.path(candidate_project, "bezdrog", "01_mnk_wmnk_mice.R"))
      ) {
        return(normalizePath(candidate_project, winslash = "/", mustWork = TRUE))
      }
      parent_dir <- dirname(current_dir)
      if (identical(parent_dir, current_dir)) {
        break
      }
      current_dir <- parent_dir
    }
  }

  stop(
    "Nie znaleziono katalogu projektu. Ustaw working directory na katalog projektu albo uruchom caly plik bezdrog/WMNKmice.R.",
    call. = FALSE
  )
}

# Sciezki projektu — pojedyncze repo konsolidowane do jednego katalogu glownego.
get_script_dir_local <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
scripts_dir <- get_script_dir_local()
project_dir <- if (basename(scripts_dir) == "R") {
  normalizePath(file.path(scripts_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  scripts_dir
}
base_dir <- project_dir

# Ustawiamy katalogi danych, wynikow i plikow do Overleaf.
data_dir <- file.path(project_dir, "data")
granice_gmin_path <- file.path(data_dir, "A03_Granice_gmin.shp")
wydruki_dir <- file.path(base_dir, "wydruki")
wykresy_dir <- file.path(base_dir, "wykresy", "WMNKtestyzmiennych")
stargazer_dir <- file.path(base_dir, "stargazer", "WMNKtestyzmiennych")
stargazer_tex_dir <- file.path(stargazer_dir, "tex")
tabele_dir <- file.path(base_dir, "tabele", "WMNKtestyzmiennych")
mice_exports_dir <- file.path(base_dir, "mice")
# Wyniki do Overleaf zapisujemy do glownego katalogu projektu, nie do kopii w bezdrog.
overleaf_dir <- file.path(project_dir, "Overleaf")
overleaf_media_dir <- file.path(overleaf_dir, "media")
overleaf_tabele_dir <- file.path(overleaf_dir, "tabele")
overleaf_stargazer_dir <- file.path(overleaf_dir, "stargazer")
paper_dir <- file.path(project_dir, "licm2")

for (dir_path in c(
  wydruki_dir, wykresy_dir, stargazer_dir, stargazer_tex_dir, tabele_dir,
  mice_exports_dir,
  overleaf_media_dir, overleaf_tabele_dir, overleaf_stargazer_dir
)) {
  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
}

# Wczytujemy lokalne biblioteki R uzywane juz w projekcie.
local_r_lib <- file.path(base_dir, "Rlib")
project_r_lib <- file.path(project_dir, "Rlib")
dir.create(local_r_lib, showWarnings = FALSE, recursive = TRUE)
dir.create(project_r_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(unique(c(local_r_lib, project_r_lib, .libPaths())))

# Brakujace pakiety instalujemy krotko w petli do lokalnej biblioteki, a potem ladujemy wszystkie.
required_packages <- c("readxl", "mice", "stargazer")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, lib = local_r_lib, repos = "https://cloud.r-project.org")
  }
}
for (pkg in required_packages) {
  library(pkg, character.only = TRUE)
}
has_sf <- requireNamespace("sf", quietly = TRUE)

saldo_col <- "saldo migracji wewn\u0119trznych na 1000 ludno\u015Bci;og\u00F3\u0142em;2024;[osoba]"
przystanki_col <- paste0(
  "og\u00F3\u0142em (przystanki autobusowe (z trolejbusowymi) i tramwajowe, ",
  "przystanki wsp\u00F3lne dla tramwaj\u00F3w i autobus\u00F3w);og\u00F3\u0142em;2024;[szt.]"
)
drogi_col <- "o nawierzchni twardej;2024;[km]"
mieszkania_col <- "og\u00F3\u0142em;mieszkania;2024;[-]"
przychodnie_col <- "przychodnie na 10 tys. ludno\u015Bci;2024;[ob.]"
bezrobocie_col <- "og\u00F3\u0142em;2024;[%]"
powierzchnia_col <- "og\u00F3\u0142em w km2;2024;[km2]"
pop_gminy_col <- "gminy bez miast na prawach powiatu;miejsce zamieszkania;stan na 31 grudnia;og\u00F3\u0142em;2024;[osoba]"
pop_mnpp_col <- "miasta na prawach powiatu;miejsce zamieszkania;stan na 31 grudnia;og\u00F3\u0142em;2024;[osoba]"
wskaznik_g_col <- "Wska\u017Anik G"
m2_na_osobe_col <- "M2NaOsobe"

initial_ols_cols <- c(
  "logPrzystankiNa1000",
  "logWskaznikG",
  "logWynagrodzenia",
  "M2NaOsobe",
  "logMieszkaniaOddaneNa1000",
  "Przychodnie",
  "Bezrobocie",
  "Miejska",
  "Wiejska",
  "Odleglosc",
  "Odleglosc2"
)
final_wmnk_cols <- setdiff(initial_ols_cols, "logPrzystankiNa1000")
mice_impute_cols <- c("logMieszkaniaOddaneNa1000", "Przychodnie")
mice_predictor_cols <- unique(c(
  "ySaldo",
  initial_ols_cols,
  "logMieszkaniaNa1000",
  "MieszkaniaOddane",
  "logMieszkaniaOddane",
  "logM2NaOsobe",
  "Populacja"
))
mice_m <- 10
mice_maxit <- 2  # No chained dependency between impute targets, so 1-2 sweeps suffice.
mice_seed <- 20240517
mice_complete_action <- 1
save_pdf_plots <- TRUE

`%||%` <- function(x, y) {
  # Zwraca wartosc domyslna, gdy pierwszy argument jest NULL.
  if (is.null(x)) y else x
}

polish_variable_labels <- c(
  const = "Wyraz wolny",
  ySaldo = "Saldo migracji",
  logPrzystankiNa1000 = "log przystank\u00F3w na 1 tys.",
  logWskaznikG = "log Wska\u017Anika G",
  WskaznikG = "Wska\u017Anik G",
  logWynagrodzenia = "log wynagrodzenia",
  M2NaOsobe = "m\u00b2 na osob\u0119",
  logM2NaOsobe = "log m\u00b2 na osob\u0119",
  logMieszkaniaOddaneNa1000 = "log mieszka\u0144 oddanych na 1 tys.",
  logMieszkaniaNa1000 = "log mieszka\u0144 na 1 tys.",
  MieszkaniaOddane = "Mieszkania oddane",
  Przychodnie = "Przychodnie",
  Bezrobocie = "Bezrobocie",
  Miejska = "Gmina miejska",
  Wiejska = "Gmina wiejska",
  Odleglosc2 = "Odleg\u0142o\u015B\u0107\u00B2",
  Odleglosc = "Odleg\u0142o\u015B\u0107",
  residuals = "reszty",
  predictions = "warto\u015Bci dopasowane",
  sqrt_abs_resid = "sqrt(abs(reszt))"
)
polish_text_replacements <- c(
  "Dependent variable:" = "Zmienna obja\u015Bniana:",
  "Zmienna objasniana:" = "Zmienna obja\u015Bniana:",
  "Blad standardowy reszt" = "B\u0142\u0105d standardowy reszt",
  "Blad standardowy" = "B\u0142\u0105d standardowy",
  "Wspolczynniki" = "Wsp\u00F3\u0142czynniki",
  "Wspolczynnik" = "Wsp\u00F3\u0142czynnik",
  "Srednia" = "\u015Arednia",
  "srednia" = "\u015Brednia",
  "wartosci" = "warto\u015Bci",
  "gestosc" = "g\u0119sto\u015B\u0107",
  "ciepla" = "ciep\u0142a",
  "Pelny" = "Pe\u0142ny",
  "pelny" = "pe\u0142ny",
  "przystankow" = "przystank\u00F3w",
  "objasniana" = "obja\u015Bniana",
  "dostepny" = "dost\u0119pny",
  "Wplyw" = "Wp\u0142yw",
  "wplyw" = "wp\u0142yw",
  "odleglosci" = "odleg\u0142o\u015Bci",
  "odleglosc" = "odleg\u0142o\u015B\u0107",
  "krancowy" = "kra\u0144cowy",
  "sredniej" = "\u015Bredniej",
  "LACZNIE" = "\u0141\u0104CZNIE",
  "Lacznie" = "\u0141\u0105cznie",
  "laczenie" = "\u0142\u0105czenie",
  "laczna" = "\u0142\u0105czna",
  "Liczba obserwacji usunietych" = "Liczba obserwacji usuni\u0119tych"
)
polish_output_text <- function(text) {
  # Zamienia techniczne lub bezogonowe etykiety na wersje gotowe do tabel i wykresow.
  result <- as.character(text)
  variable_names <- names(polish_variable_labels)[order(nchar(names(polish_variable_labels)), decreasing = TRUE)]
  for (name in variable_names) {
    result <- gsub(name, polish_variable_labels[[name]], result, fixed = TRUE)
  }
  replacement_names <- names(polish_text_replacements)[order(nchar(names(polish_text_replacements)), decreasing = TRUE)]
  for (name in replacement_names) {
    result <- gsub(name, polish_text_replacements[[name]], result, fixed = TRUE)
  }
  result
}

polish_display_label <- function(value) {
  # Zwraca pojedyncza etykiete zmiennej z polskimi znakami.
  value <- as.character(value)
  if (value %in% names(polish_variable_labels)) {
    return(unname(polish_variable_labels[[value]]))
  }
  polish_output_text(gsub("_", " ", value, fixed = TRUE))
}

is_p_value_column <- function(column) {
  # Rozpoznaje kolumny p-value niezaleznie od zapisu z podkresleniem albo kropka.
  normalized <- gsub("[^a-z0-9]", "", tolower(as.character(column)))
  normalized %in% c("pvalue", "p") || grepl("pvalue$", normalized)
}

is_integer_like_column <- function(column) {
  # Kolumny licznikowe zostawiamy bez miejsc po przecinku, jak w tabelach w pracy.
  normalized <- gsub("[^a-z0-9]", "", tolower(as.character(column)))
  normalized %in% c("na", "n", "df", "dfnum", "dfden", "deltadf", "liczbaobserwacji")
}

format_tex_number <- function(value, column) {
  # Formatuje liczby do tabel LaTeX; p-value ma progowy zapis <0.001.
  if (length(value) == 0 || is.na(value)) {
    return("")
  }
  number <- as.numeric(value)
  if (is_p_value_column(column)) {
    return(ifelse(number < 0.001, "<0.001", sprintf("%.3f", number)))
  }
  if (is_integer_like_column(column) && isTRUE(all.equal(number, round(number)))) {
    return(as.character(as.integer(round(number))))
  }
  gsub(".", ",", sprintf("%.3f", number), fixed = TRUE)
}

format_tex_value <- function(value, column) {
  # Zamienia pojedyncza wartosc na tekst gotowy do tabeli LaTeX.
  if (length(value) == 0 || is.na(value)) {
    return("")
  }
  if (is.numeric(value) && !is.logical(value)) {
    return(format_tex_number(value, column))
  }
  polish_output_text(gsub("_", " ", as.character(value), fixed = TRUE))
}

polish_tex_column_name <- function(column) {
  # Ujednolica naglowki: bez podkreslen i z zapisem p-value.
  mapping <- c(
    p_value = "p-value",
    pvalue = "p-value",
    std_error = "Błąd standardowy",
    z_value = "Statystyka z",
    t_value = "Statystyka t",
    coefficient = "Współczynnik",
    estimate = "Współczynnik",
    statistic = "Statystyka",
    variable = "Zmienna",
    removed_variable = "Usunięta zmienna",
    removed_variable_pvalue = "p-value usuniętej zmiennej",
    n_obs = "Liczba obserwacji",
    num_regressors = "Liczba regresorów",
    regressors = "Regresory",
    aic = "AIC",
    bic = "BIC",
    r2 = "$R^2$",
    adj_r2 = "Skorygowane $R^2$",
    bp_pvalue = "p-value Breuscha-Pagana",
    white_pvalue = "p-value White'a",
    max_pvalue_without_const = "Najwyższe p-value bez stałej",
    all_variables_significant_5pct = "Wszystkie zmienne istotne na 5%",
    step = "Krok",
    model = "Model",
    test = "Test",
    statystyka = "Statystyka",
    min = "Min",
    max = "Max",
    srednia = "Średnia",
    na = "NA"
  )
  key <- as.character(column)
  normalized <- gsub("[^a-z0-9]", "", tolower(key))
  if (key %in% names(mapping)) {
    return(unname(mapping[[key]]))
  }
  if (normalized %in% names(mapping)) {
    return(unname(mapping[[normalized]]))
  }
  label <- gsub("_", " ", key, fixed = TRUE)
  polish_output_text(gsub("\\bp value\\b", "p-value", label, ignore.case = TRUE))
}

format_dataframe_for_latex <- function(df) {
  # Przygotowuje cala ramke do stabilnego eksportu LaTeX.
  formatted <- as.data.frame(lapply(names(df), function(column) {
    vapply(df[[column]], format_tex_value, character(1), column = column)
  }), stringsAsFactors = FALSE, check.names = FALSE)
  names(formatted) <- vapply(names(df), polish_tex_column_name, character(1))
  formatted
}

strip_latex_comments <- function(lines) {
  # Usuwa komentarze LaTeX z linii zrodlowych.
  sub("%.*$", "", lines)
}

extract_latex_refs <- function(lines, command_name) {
  # Wyciaga sciezki z komend \input i \includegraphics.
  pattern <- paste0("\\\\", command_name, "(\\[[^]]*\\])?\\{([^}]*)\\}")
  matches <- gregexpr(pattern, lines, perl = TRUE)
  raw_matches <- regmatches(lines, matches)
  refs <- unlist(lapply(raw_matches, function(line_matches) {
    if (length(line_matches) == 0 || identical(line_matches, character(0))) {
      return(character())
    }
    sub(pattern, "\\2", line_matches, perl = TRUE)
  }), use.names = FALSE)
  gsub("\\\\", "/", refs)
}

read_maintest_refs <- function() {
  # Buduje liste plikow realnie wczytywanych przez licm2/maintest.tex.
  maintest_path <- file.path(paper_dir, "maintest.tex")
  refs <- list(media = character(), tabele = character(), stargazer = character())
  if (!file.exists(maintest_path)) {
    return(refs)
  }
  text <- strip_latex_comments(readLines(maintest_path, warn = FALSE, encoding = "UTF-8"))
  graphics_refs <- extract_latex_refs(text, "includegraphics")
  input_refs <- extract_latex_refs(text, "input")

  input_refs <- ifelse(grepl("\\.[A-Za-z0-9]+$", input_refs), input_refs, paste0(input_refs, ".tex"))
  refs$media <- basename(graphics_refs[grepl("^media/", graphics_refs)])
  refs$tabele <- basename(input_refs[grepl("^tabele/", input_refs)])
  refs$stargazer <- basename(input_refs[grepl("^stargazer/", input_refs)])
  refs
}

maintest_refs <- read_maintest_refs()
publish_dirs <- list(
  media = overleaf_media_dir,
  tabele = overleaf_tabele_dir,
  stargazer = overleaf_stargazer_dir
)

overleaf_r_media_files <- c(
  "mapa_ciepla_ysaldoR.png",
  "mapy_ciepla_zmiennych_4x3R.png",
  "mapa_ciepla_logM2NaOsobeR.png",
  "mapa_reszt_finalnego_wmnkR.png",
  "korelacje_zmiennych_pearsonR.png",
  "korelacje_zmiennych_spearmanR.png",
  "diagnostyka_reszt_2x2_finalR.png",
  "reszty_vs_zmienne_4x3_finalR.png",
  "reszty_vs_zmienna_logWskaznikGR.png",
  "reszty_vs_zmienna_logWynagrodzeniaR.png",
  "reszty_vs_zmienna_M2NaOsobeR.png",
  "reszty_vs_zmienna_logMieszkaniaOddaneNa1000R.png",
  "reszty_vs_zmienna_PrzychodnieR.png",
  "reszty_vs_zmienna_BezrobocieR.png",
  "reszty_vs_zmienna_MiejskaR.png",
  "reszty_vs_zmienna_WiejskaR.png",
  "reszty_vs_zmienna_OdlegloscR.png",
  "reszty_vs_zmienna_Odleglosc2R.png",
  "reszty_vs_wartosci_dopasowane_finalR.png",
  "scale_location_finalR.png",
  "histogram_reszt_finalR.png",
  "qq_plot_finalR.png"
)

overleaf_r_table_files <- c(
  "statystyki_opisowe_zmiennych_przed_MNKR.tex",
  "testy_diagnostyczne_model2_wmnkR.tex"
)

overleaf_r_stargazer_files <- c(
  "modele_ols_podstawowy_i_bez_przystankowR.tex",
  "modele_ols_podstawoweR.tex",
  "model_funkcji_wariancjiR.tex",
  "WydrukiMNKfullR.tex",
  "modele_chowR.tex"
)

maintest_uses <- function(subdir, filename) {
  # Sprawdza, czy plik jest zaleznoscia glownego pliku pracy.
  filename %in% (maintest_refs[[subdir]] %||% character())
}

publish_if_maintest_uses <- function(source_path, subdir) {
  # Publikuje wszystkie wyniki R do Overleaf, bo maintest.tex moze jeszcze nie wskazywac nazw z sufiksem R.
  if (!file.exists(source_path)) {
    return(FALSE)
  }
  target_dir <- publish_dirs[[subdir]]
  dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
  target_path <- file.path(target_dir, basename(source_path))
  source_norm <- normalizePath(source_path, winslash = "/", mustWork = TRUE)
  target_norm <- normalizePath(target_path, winslash = "/", mustWork = FALSE)
  if (source_norm != target_norm) {
    file.copy(source_path, target_path, overwrite = TRUE)
  }
  TRUE
}

ensure_stargazer_tex_file <- function(filename) {
  # Nie tworzymy tabel modelowych ze starego tekstu, bo maja wygladac jak eksport Stargazer.
  tex_path <- file.path(stargazer_tex_dir, filename)
  file.exists(tex_path)
}

copy_hardcoded_overleaf_file <- function(source_dir, target_subdir, filename) {
  # Kopiuje pojedynczy jawnie wskazany plik wynikowy do katalogu Overleaf.
  if (identical(target_subdir, "stargazer")) {
    ensure_stargazer_tex_file(filename)
  }
  source_path <- file.path(source_dir, filename)
  if (!file.exists(source_path)) {
    message(sprintf("Brak pliku do Overleaf: %s", source_path))
    return(FALSE)
  }
  publish_if_maintest_uses(source_path, target_subdir)
}

sync_r_outputs_to_overleaf <- function() {
  # Kopiuje do Overleaf tylko twardo wskazane wyniki odpowiadajace WMNKtestyzmiennych.
  for (filename in overleaf_r_media_files) {
    copy_hardcoded_overleaf_file(wykresy_dir, "media", filename)
  }
  for (filename in overleaf_r_table_files) {
    copy_hardcoded_overleaf_file(tabele_dir, "tabele", filename)
  }
  for (filename in overleaf_r_stargazer_files) {
    copy_hardcoded_overleaf_file(stargazer_tex_dir, "stargazer", filename)
  }
}

safe_plot_name <- function(value) {
  # Upraszcza nazwy zmiennych do bezpiecznych nazw plikow graficznych.
  gsub("[^[:alnum:]]", "_", as.character(value))
}

r_output_stem <- function(name) {
  # Dopisuje sufiks R do nazwy wynikowej bez podwajania tego sufiksu.
  if (endsWith(as.character(name), "R")) {
    return(as.character(name))
  }
  paste0(name, "R")
}

r_output_filename <- function(filename) {
  # Dopisuje sufiks R przed rozszerzeniem pliku wynikowego.
  ext <- tools::file_ext(filename)
  if (identical(ext, "")) {
    return(r_output_stem(filename))
  }
  stem <- substr(filename, 1, nchar(filename) - nchar(ext) - 1)
  paste0(r_output_stem(stem), ".", ext)
}

r_output_file_path <- function(path) {
  # Buduje sciezke wynikowa z sufiksem R w nazwie pliku.
  file.path(dirname(path), r_output_filename(basename(path)))
}

r_output_path <- function(dir_path, filename) {
  # Buduje sciezke wynikowa w katalogu z sufiksem R w nazwie pliku.
  file.path(dir_path, r_output_filename(filename))
}

save_plot_device <- function(name, width, height, dpi = 300, code) {
  # Zapisuje wykres jako PDF (wektor — ostre etykiety w bezjc.tex) i opcjonalnie
  # jako PNG do szybkiego podgladu. Publikujemy PDF.
  name <- r_output_stem(name)
  pdf_path <- file.path(wykresy_dir, paste0(name, ".pdf"))
  grDevices::cairo_pdf(pdf_path, width = width, height = height, family = "serif")
  ok <- FALSE
  tryCatch(
    {
      code()
      ok <- TRUE
    },
    finally = {
      grDevices::dev.off()
    }
  )
  if (ok) {
    publish_if_maintest_uses(pdf_path, "media")
  }
  if (isTRUE(save_pdf_plots)) {
    png_path <- file.path(wykresy_dir, paste0(name, ".png"))
    grDevices::png(png_path, width = width, height = height, units = "in", res = dpi)
    code()
    grDevices::dev.off()
  }
  invisible(pdf_path)
}

html_escape <- function(x) {
  # Escapuje tekst przed zapisem HTML.
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

write_html_table <- function(df, path, digits = 3) {
  # Zapisuje prosta tabele HTML z formatowaniem liczbowym jak pandas.to_html.
  path <- r_output_file_path(path)
  formatted <- as.data.frame(lapply(names(df), function(column_name) {
    column <- df[[column_name]]
    if (is.numeric(column)) {
      ifelse(is.na(column), "", sprintf(paste0("%.", digits, "f"), column))
    } else {
      ifelse(is.na(column), "", polish_output_text(as.character(column)))
    }
  }), stringsAsFactors = FALSE, check.names = FALSE)
  names(formatted) <- vapply(names(df), polish_tex_column_name, character(1))
  header <- paste(sprintf("<th>%s</th>", html_escape(names(formatted))), collapse = "")
  rows <- apply(formatted, 1, function(row) {
    paste0("<tr>", paste(sprintf("<td>%s</td>", html_escape(row)), collapse = ""), "</tr>")
  })
  html <- c("<table border=\"1\" class=\"dataframe\">", "<thead>", paste0("<tr>", header, "</tr>"),
            "</thead>", "<tbody>", rows, "</tbody>", "</table>")
  writeLines(html, path, useBytes = TRUE)
}

df_to_string <- function(df) {
  # Zamienia ramke danych na tekst podobny do pandas.to_string(index=False).
  display_df <- df
  names(display_df) <- vapply(names(display_df), polish_tex_column_name, character(1))
  display_df[] <- lapply(display_df, function(column) {
    if (is.character(column)) {
      polish_output_text(column)
    } else {
      column
    }
  })
  paste(capture.output(print(display_df, row.names = FALSE)), collapse = "\n")
}

save_overleaf_table <- function(filename, latex) {
  # Zapisuje tabele w bezdrog i publikuje ja do Overleaf, jesli uzywa jej maintest.tex.
  base_filename <- as.character(filename)
  filename <- r_output_stem(base_filename)
  tex_path <- file.path(tabele_dir, paste0(filename, ".tex"))
  writeLines(enc2utf8(latex), tex_path, useBytes = TRUE)
  publish_if_maintest_uses(tex_path, "tabele")
}

dataframe_to_latex_table <- function(df, caption, column_format = NULL) {
  # Tworzy prosta tabele LaTeX z ramki danych.
  formatted <- format_dataframe_for_latex(df)
  if (is.null(column_format)) {
    column_format <- paste(rep("l", ncol(formatted)), collapse = "")
  }
  header <- paste(names(formatted), collapse = " & ")
  body <- apply(formatted, 1, function(row) paste(row, collapse = " & "))
  tabular <- c(
    paste0("\\begin{tabular}{", column_format, "}"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    paste0(body, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}"
  )
  c(
    "\\begin{table}[H]",
    "\\centering",
    paste0("\\caption{", polish_output_text(caption), "}"),
    tabular,
    "\\end{table}",
    ""
  )
}

display_coef_name <- function(name) {
  # Ujednolica nazwe wyrazu wolnego do zapisu statsmodels.
  ifelse(name == "(Intercept)", "const", name)
}

model_summary_text <- function(model) {
  # Zapisuje tekstowe podsumowanie modelu.
  paste(capture.output(summary(model)), collapse = "\n")
}

model_summary_html <- function(model) {
  # Zapisuje podsumowanie modelu jako prosty HTML.
  paste0("<html><body><pre>", html_escape(model_summary_text(model)), "</pre></body></html>")
}

latex_escape <- function(value) {
  # Ucieka znaki specjalne w komorkach tekstowych tabel LaTeX.
  text <- as.character(value)
  text <- gsub("\\\\", "\\\\textbackslash{}", text)
  text <- gsub("&", "\\\\&", text, fixed = TRUE)
  text <- gsub("%", "\\\\%", text, fixed = TRUE)
  text <- gsub("_", "\\\\_", text, fixed = TRUE)
  text <- gsub("#", "\\\\#", text, fixed = TRUE)
  text <- gsub("\\^", "\\\\textasciicircum{}", text)
  text
}

model_coefficient_table_latex <- function(models, title = NULL) {
  # Buduje awaryjna tabele LaTeX ze wspolczynnikami, gdy pakiet stargazer nie obsluzy modeli.
  rows <- character()
  for (idx in seq_along(models)) {
    coef_table <- summary(models[[idx]])$coefficients
    for (row_idx in seq_len(nrow(coef_table))) {
      variable <- display_coef_name(rownames(coef_table)[[row_idx]])
      rows <- c(
        rows,
        paste(
          latex_escape(sprintf("Model %s", idx)),
          latex_escape(polish_display_label(variable)),
          format_tex_number(coef_table[row_idx, "Estimate"], "coefficient"),
          format_tex_number(coef_table[row_idx, "Std. Error"], "std_error"),
          format_tex_number(coef_table[row_idx, "t value"], "t_value"),
          format_tex_number(coef_table[row_idx, "Pr(>|t|)"], "p_value"),
          sep = " & "
        )
      )
    }
  }
  c(
    "\\begin{table}[H]",
    "\\centering",
    paste0("\\caption{", latex_escape(polish_output_text(title %||% "Wspolczynniki modeli")), "}"),
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{llrrrr}",
    "\\toprule",
    "Model & Zmienna & Wspolczynnik & Blad standardowy & Statystyka t & p-value \\\\",
    "\\midrule",
    paste0(rows, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "\\end{table}",
    ""
  )
}

format_stargazer_number <- function(value) {
  # Formatuje liczby w tabelach modelowych z kropka, tak jak robi to Stargazer.
  if (is.na(value) || !is.finite(value)) {
    return("")
  }
  sprintf("%.3f", as.numeric(value))
}

format_stargazer_integer <- function(value) {
  # Formatuje liczniki obserwacji i stopni swobody w stylu tabel modelowych.
  if (is.na(value) || !is.finite(value)) {
    return("")
  }
  as.character(as.integer(round(as.numeric(value))))
}

stargazer_stars <- function(p_value) {
  # Zwraca oznaczenia istotnosci zgodne z progami uzywanymi w tabelach Stargazer.
  if (is.na(p_value) || !is.finite(p_value)) {
    return("$^{}$")
  }
  if (p_value < 0.01) {
    "$^{***}$"
  } else if (p_value < 0.05) {
    "$^{**}$"
  } else if (p_value < 0.1) {
    "$^{*}$"
  } else {
    "$^{}$"
  }
}

model_coef_lookup <- function(model) {
  # Pobiera wspolczynniki modelu w strukturze wygodnej do tabeli wielomodelowej.
  coef_table <- summary(model)$coefficients
  rownames(coef_table) <- vapply(rownames(coef_table), display_coef_name, character(1))
  coef_table
}

model_stat_rows <- function(models) {
  # Buduje dolne wiersze statystyk modelowych w stylu Stargazer.
  observation_row <- c("Liczba obserwacji", vapply(models, function(model) format_stargazer_integer(length(residuals(model))), character(1)))
  r2_row <- c("$R^2$", vapply(models, function(model) format_stargazer_number(summary(model)$r.squared), character(1)))
  adj_r2_row <- c("Skorygowane $R^2$", vapply(models, function(model) format_stargazer_number(summary(model)$adj.r.squared), character(1)))
  resid_row <- c(
    "Błąd standardowy reszt",
    vapply(models, function(model) {
      model_summary <- summary(model)
      paste0(format_stargazer_number(model_summary$sigma), " (df=", format_stargazer_integer(model$df.residual), ")")
    }, character(1))
  )
  f_row <- c(
    "Statystyka F",
    vapply(models, function(model) {
      fstat <- summary(model)$fstatistic
      if (is.null(fstat)) {
        return("")
      }
      p_value <- pf(fstat[["value"]], fstat[["numdf"]], fstat[["dendf"]], lower.tail = FALSE)
      paste0(
        format_stargazer_number(fstat[["value"]]),
        stargazer_stars(p_value),
        " (df=",
        format_stargazer_integer(fstat[["numdf"]]),
        "; ",
        format_stargazer_integer(fstat[["dendf"]]),
        ")"
      )
    }, character(1))
  )
  list(observation_row, r2_row, adj_r2_row, resid_row, f_row)
}

model_table_latex <- function(models, title = NULL, covariate_order = NULL, column_labels = NULL, resize = FALSE) {
  # Tworzy tabele modeli w ukladzie zblizonym do Pythonowego Stargazera.
  model_args <- if (is.list(models) && !inherits(models, "lm")) models else list(models)
  coef_tables <- lapply(model_args, model_coef_lookup)
  if (!is.null(covariate_order)) {
    ordered_terms <- covariate_order[covariate_order %in% unique(unlist(lapply(coef_tables, rownames)))]
  } else {
    ordered_terms <- unique(unlist(lapply(coef_tables, rownames)))
  }
  column_count <- length(model_args)
  tabular_spec <- paste0("@{\\extracolsep{5pt}}l", paste(rep("c", column_count), collapse = ""))
  model_numbers <- if (is.null(column_labels)) {
    paste0("(", seq_len(column_count), ")")
  } else {
    as.character(column_labels)
  }
  rows <- c(
    paste0("\\begin{table}[H] \\centering"),
    paste0("  \\caption{", polish_output_text(title %||% "Modele"), "}")
  )
  if (isTRUE(resize)) {
    rows <- c(rows, "\\resizebox{\\textwidth}{!}{%")
  }
  rows <- c(
    rows,
    paste0("\\begin{tabular}{", tabular_spec, "}"),
    "\\\\[-1.8ex]\\hline",
    "\\hline \\\\[-1.8ex]",
    paste0("& \\multicolumn{", column_count, "}{c}{\\textit{Zmienna objaśniana: Saldo migracji}} \\\\"),
    paste0("\\cline{2-", column_count + 1, "}"),
    paste0("\\\\[-1.8ex] & ", paste(model_numbers, collapse = " & "), " \\\\"),
    "\\hline \\\\[-1.8ex]"
  )
  for (term in ordered_terms) {
    coef_values <- character()
    se_values <- character()
    for (coef_table in coef_tables) {
      if (term %in% rownames(coef_table)) {
        coef_values <- c(coef_values, paste0(format_stargazer_number(coef_table[term, "Estimate"]), stargazer_stars(coef_table[term, "Pr(>|t|)"])))
        se_values <- c(se_values, paste0("(", format_stargazer_number(coef_table[term, "Std. Error"]), ")"))
      } else {
        coef_values <- c(coef_values, "")
        se_values <- c(se_values, "")
      }
    }
    rows <- c(
      rows,
      paste0(" ", polish_display_label(term), " & ", paste(coef_values, collapse = " & "), " \\\\"),
      paste0("& ", paste(se_values, collapse = " & "), " \\\\")
    )
  }
  rows <- c(rows, "\\hline \\\\[-1.8ex]")
  for (stat_row in model_stat_rows(model_args)) {
    rows <- c(rows, paste0(" ", stat_row[[1]], " & ", paste(stat_row[-1], collapse = " & "), " \\\\"))
  }
  rows <- c(
    rows,
    "\\hline",
    "\\hline \\\\[-1.8ex]",
    paste0("\\textit{Uwaga:} & \\multicolumn{", column_count, "}{r}{$^{*}$p-value$<$0.100; $^{**}$p-value$<$0.050; $^{***}$p-value$<$0.010} \\\\"),
    "\\end{tabular}"
  )
  if (isTRUE(resize)) {
    rows <- c(rows, "}")
  }
  c(rows, "\\end{table}", "")
}

save_stargazer_models <- function(
  models,
  filename,
  covariate_order = NULL,
  title = NULL,
  column_labels = NULL
) {
  # Zapisuje modele do LaTeX bez HTML i bez recznego skladania tabel.
  base_filename <- as.character(filename)
  filename <- r_output_stem(base_filename)
  if (FALSE) {
    lines <- c("Pakiet stargazer nie jest dostępny w tym interpreterze.", "")
    for (idx in seq_along(models)) {
      lines <- c(lines, sprintf("MODEL %s", idx), model_summary_text(models[[idx]]), "")
    }
    writeLines(lines, file.path(stargazer_dir, paste0(filename, ".txt")), useBytes = TRUE)
    writeLines(lines, file.path(stargazer_tex_dir, paste0(filename, ".txt")), useBytes = TRUE)
    tex_path <- file.path(stargazer_tex_dir, paste0(filename, ".tex"))
    if (file.exists(tex_path)) {
      file.remove(tex_path)
    }
    return(invisible(NULL))
  }

  {
      # Stargazer w R przy do.call oczekuje modeli jako osobnych argumentow, nie jako jednej zagniezdzonej listy.
      model_args <- if (is.list(models) && !inherits(models, "lm")) models else list(models)
      order_r <- NULL
      labels_r <- NULL
      if (!is.null(covariate_order)) {
        # Wyraz wolny R-owy stargazer obsluguje osobno, wiec nie przekazujemy go w order.
        covariates_for_order <- covariate_order[covariate_order != "const"]
        if (length(covariates_for_order) > 0) {
          order_r <- covariates_for_order
          labels_r <- vapply(covariates_for_order, polish_display_label, character(1))
        }
      }
      title_r <- if (is.null(title)) NULL else polish_output_text(title)

      # Generujemy LaTeX bezposrednim wywolaniem stargazera.
      if (length(model_args) == 1) {
        latex <- if (is.null(order_r)) {
          capture.output(stargazer::stargazer(model_args[[1]], type = "latex", title = title_r, header = FALSE, digits = 3, intercept.top = TRUE, intercept.bottom = FALSE))
        } else {
          capture.output(stargazer::stargazer(model_args[[1]], type = "latex", title = title_r, order = order_r, covariate.labels = labels_r, header = FALSE, digits = 3, intercept.top = TRUE, intercept.bottom = FALSE))
        }
      } else if (length(model_args) == 2) {
        latex <- if (is.null(order_r)) {
          capture.output(stargazer::stargazer(model_args[[1]], model_args[[2]], type = "latex", title = title_r, header = FALSE, digits = 3, intercept.top = TRUE, intercept.bottom = FALSE))
        } else {
          capture.output(stargazer::stargazer(model_args[[1]], model_args[[2]], type = "latex", title = title_r, order = order_r, covariate.labels = labels_r, header = FALSE, digits = 3, intercept.top = TRUE, intercept.bottom = FALSE))
        }
      } else if (length(model_args) == 3) {
        latex <- if (is.null(order_r)) {
          capture.output(stargazer::stargazer(model_args[[1]], model_args[[2]], model_args[[3]], type = "latex", title = title_r, header = FALSE, digits = 3, intercept.top = TRUE, intercept.bottom = FALSE))
        } else {
          capture.output(stargazer::stargazer(model_args[[1]], model_args[[2]], model_args[[3]], type = "latex", title = title_r, order = order_r, covariate.labels = labels_r, header = FALSE, digits = 3, intercept.top = TRUE, intercept.bottom = FALSE))
        }
      } else if (length(model_args) == 4) {
        latex <- if (is.null(order_r)) {
          capture.output(stargazer::stargazer(model_args[[1]], model_args[[2]], model_args[[3]], model_args[[4]], type = "latex", title = title_r, header = FALSE, digits = 3, intercept.top = TRUE, intercept.bottom = FALSE))
        } else {
          capture.output(stargazer::stargazer(model_args[[1]], model_args[[2]], model_args[[3]], model_args[[4]], type = "latex", title = title_r, order = order_r, covariate.labels = labels_r, header = FALSE, digits = 3, intercept.top = TRUE, intercept.bottom = FALSE))
        }
      } else {
        stop("Stargazer w tym skrypcie jest ustawiony na maksymalnie 4 modele.", call. = FALSE)
      }
      latex <- gsub("Dependent variable:", "Zmienna obja\u015Bniana:", latex, fixed = TRUE)
      latex <- gsub(" Observations &", " Liczba obserwacji &", latex, fixed = TRUE)
      latex <- gsub(" Adjusted R$^{2}$ &", " Skorygowane $R^2$ &", latex, fixed = TRUE)
      latex <- gsub(" Adjusted $R^2$ &", " Skorygowane $R^2$ &", latex, fixed = TRUE)
      latex <- gsub(" Residual Std. Error &", " B\u0142\u0105d standardowy reszt &", latex, fixed = TRUE)
      latex <- gsub(" F Statistic &", " Statystyka F &", latex, fixed = TRUE)
      latex <- gsub("\\textit{Note:}", "\\textit{Uwaga:}", latex, fixed = TRUE)
      latex <- gsub("Constant", "Wyraz wolny", latex, fixed = TRUE)
      latex <- polish_output_text(latex)
      latex <- gsub(
        "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01",
        "$^{*}$p-value$<$0.100; $^{**}$p-value$<$0.050; $^{***}$p-value$<$0.010",
        latex,
        fixed = TRUE
      )
      if (base_filename %in% c("modele_chow", "WydrukiMNKfull")) {
        # Duze tabele Stargazera skalujemy w LaTeX do szerokosci tekstu.
        latex <- sub("\\\\begin\\{table\\}\\[!htbp\\]", "\\\\begin{table}[H]", latex)
        latex <- sub("\\\\begin\\{tabular\\}\\{", "\\\\resizebox{\\\\textwidth}{!}{%\n\\\\begin{tabular}{", latex)
        latex <- sub("\\\\end\\{tabular\\}", "\\\\end{tabular}\n}", latex)
      }
      tex_path <- file.path(stargazer_tex_dir, paste0(filename, ".tex"))
      writeLines(enc2utf8(latex), tex_path, useBytes = TRUE)
      publish_if_maintest_uses(tex_path, "stargazer")
  }
  invisible(NULL)
}

norm_code <- function(x) {
  # Normalizuje kody TERYT do siedmiu cyfr.
  if (is.numeric(x)) {
    x <- format(x, scientific = FALSE, trim = TRUE)
  } else {
    x <- as.character(x)
  }
  x <- gsub("\\.0$", "", x)
  x <- trimws(x)
  ifelse(
    is.na(x),
    NA_character_,
    paste0(vapply(pmax(0, 7 - nchar(x)), function(n) paste(rep("0", n), collapse = ""), character(1)), x)
  )
}

read_csv_semicolon <- function(path) {
  # Wczytuje pliki CSV z separatorem srednikiem i przecinkiem dziesietnym.
  tryCatch(
    read.csv2(path, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(exc) read.csv2(path, check.names = FALSE, stringsAsFactors = FALSE)
  )
}

read_csv_comma <- function(path) {
  # Wczytuje pliki CSV z separatorem przecinkiem.
  tryCatch(
    read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(exc) read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  )
}

select_existing_cols <- function(df, cols) {
  # Wybiera kolumny i jasno przerywa, gdy brakuje wymaganej nazwy.
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Brak kolumn: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  df[, cols, drop = FALSE]
}

left_merge <- function(x, y, by = "Kod") {
  # Scala ramki jak pandas.merge(how='left'), zachowujac kolejnosc lewej ramki.
  order_col <- ".left_merge_order__"
  x[[order_col]] <- seq_len(nrow(x))
  merged <- merge(x, y, by = by, all.x = TRUE, sort = FALSE)
  merged <- merged[order(merged[[order_col]]), , drop = FALSE]
  merged[[order_col]] <- NULL
  row.names(merged) <- NULL
  merged
}

replace_infinite_na <- function(df) {
  # Zamienia nieskonczonosci po logarytmach na NA.
  numeric_cols <- vapply(df, is.numeric, logical(1))
  df[numeric_cols] <- lapply(df[numeric_cols], function(column) {
    column[is.infinite(column)] <- NA_real_
    column
  })
  df
}

load_model_data <- function() {
  # Wczytuje dane z BDL, GUS i Ministerstwa Finansow.
  if (!dir.exists(data_dir)) {
    stop(sprintf(
      "Nie znaleziono katalogu danych: %s. Uruchom caly skrypt WMNKmice.R z katalogu projektu albo ustaw poprawny working directory.",
      data_dir
    ), call. = FALSE)
  }
  czyn_przyst <- read_csv_semicolon(file.path(data_dir, "CzynnePrzystanki.csv"))
  dl_drog <- read_csv_semicolon(file.path(data_dir, "DlugoscDrog.csv"))
  med_wynagr_path <- Sys.glob(file.path(data_dir, "MedianaWynagrod*Brutto2024.csv"))[[1]]
  med_wynagr <- read_csv_semicolon(med_wynagr_path)
  mieszk_odd <- read_csv_semicolon(file.path(data_dir, "MieszkaniaOddane.csv"))
  przych_na_10k <- read_csv_semicolon(file.path(data_dir, "PrzychodnieNa10k.csv"))
  udz_bezrob <- read_csv_semicolon(file.path(data_dir, "UdzialBezrobotnych.csv"))
  odleglosci <- read_csv_comma(file.path(data_dir, "odleglosci_km.csv"))
  saldo <- read_csv_semicolon(file.path(data_dir, "SaldoMigracjiNa1k.csv"))
  populacja <- read_csv_semicolon(file.path(data_dir, "PopulacjaGmin.csv"))
  powierzchnia <- read_csv_semicolon(file.path(data_dir, "PowierzchniaGmin.csv"))
  m2_mieszkania <- read_csv_semicolon(file.path(data_dir, "M2NaOsobe_MieszkaniaNa1000Mieszkancow.csv"))
  wskaznik_g <- readxl::read_excel(file.path(data_dir, "WskaznikG.xlsx"))
  wskaznik_g <- as.data.frame(wskaznik_g, stringsAsFactors = FALSE, check.names = FALSE)

  for (obj_name in c(
    "czyn_przyst", "dl_drog", "med_wynagr", "mieszk_odd", "przych_na_10k",
    "udz_bezrob", "odleglosci", "saldo", "populacja", "powierzchnia", "m2_mieszkania"
  )) {
    obj <- get(obj_name)
    obj$Kod <- norm_code(obj$Kod)
    assign(obj_name, obj)
  }

  data <- data.frame(Kod = czyn_przyst$Kod, stringsAsFactors = FALSE)
  data <- left_merge(data, select_existing_cols(saldo, c("Kod", saldo_col)))
  data <- left_merge(data, select_existing_cols(czyn_przyst, c("Kod", przystanki_col)))
  data <- left_merge(data, select_existing_cols(dl_drog, c("Kod", drogi_col)))
  data <- left_merge(data, select_existing_cols(przych_na_10k, c("Kod", przychodnie_col)))
  data <- left_merge(data, select_existing_cols(udz_bezrob, c("Kod", bezrobocie_col)))
  data <- left_merge(data, select_existing_cols(odleglosci, c("Kod", "odleglosc")))
  data <- left_merge(data, select_existing_cols(powierzchnia, c("Kod", powierzchnia_col)))

  # Populacja jest rozbita na dwie kolumny, wiec sumujemy je do jednego mianownika.
  populacja <- select_existing_cols(populacja, c("Kod", pop_gminy_col, pop_mnpp_col))
  populacja$Populacja <- populacja[[pop_gminy_col]] + populacja[[pop_mnpp_col]]
  data <- left_merge(data, populacja[, c("Kod", "Populacja"), drop = FALSE])

  # Z miesiecznych kolumn wynagrodzen liczymy srednia roczna.
  med_wynagr$srednia <- rowMeans(med_wynagr[, 3:ncol(med_wynagr), drop = FALSE], na.rm = TRUE)
  data <- left_merge(data, med_wynagr[, c("Kod", "srednia"), drop = FALSE])
  data <- left_merge(data, select_existing_cols(mieszk_odd, c("Kod", mieszkania_col)))

  # Z pliku bierzemy tylko powierzchnie mieszkan na osobe.
  names(m2_mieszkania)[3] <- m2_na_osobe_col
  data <- left_merge(data, select_existing_cols(m2_mieszkania, c("Kod", m2_na_osobe_col)))

  data$Kod <- norm_code(data$Kod)

  # Normalizujemy kody TERYT we Wskazniku G przed scaleniem.
  names(wskaznik_g)[names(wskaznik_g) == "Kod gminy"] <- "Kod"
  wskaznik_g$Kod <- norm_code(wskaznik_g$Kod)
  wskaznik_g$Kod6 <- substr(wskaznik_g$Kod, 1, 6)
  data <- left_merge(data, select_existing_cols(wskaznik_g, c("Kod", wskaznik_g_col)))

  # Braki Wskaznika G uzupelniamy po pierwszych szesciu cyfrach TERYT.
  data$Kod6 <- substr(data$Kod, 1, 6)
  g_fallback <- wskaznik_g[!is.na(wskaznik_g[[wskaznik_g_col]]), c("Kod6", wskaznik_g_col), drop = FALSE]
  g_fallback <- g_fallback[!duplicated(g_fallback$Kod6), , drop = FALSE]
  names(g_fallback)[names(g_fallback) == wskaznik_g_col] <- paste0(wskaznik_g_col, "_po_Kod6")
  data <- left_merge(data, g_fallback, by = "Kod6")
  g_missing <- is.na(data[[wskaznik_g_col]])
  data[[wskaznik_g_col]][g_missing] <- data[[paste0(wskaznik_g_col, "_po_Kod6")]][g_missing]
  data$Kod6 <- NULL
  data[[paste0(wskaznik_g_col, "_po_Kod6")]] <- NULL

  data$ySaldo <- data[[saldo_col]]
  data$logPrzystankiNaKm2 <- log(data[[przystanki_col]] / data[[powierzchnia_col]])
  data$logPrzystankiNa1000 <- log(data[[przystanki_col]] / data$Populacja * 1000)
  data$logDrogi <- log1p(data[[drogi_col]])
  data$logDrogiNaKm2 <- log1p(data[[drogi_col]] / data[[powierzchnia_col]])
  data$logWynagrodzenia <- log(data$srednia)
  data$logMieszkania <- log(data[[mieszkania_col]])
  # Liczbe mieszkan oddanych zachowujemy tez w wartosciach bezwzglednych.
  data$MieszkaniaOddane <- data[[mieszkania_col]]
  # Dla dodatnich wartosci stosujemy log(x), bez dodawania jedynki.
  data$logMieszkaniaOddane <- log(data$MieszkaniaOddane)
  data$logMieszkaniaNaKm2 <- log(data[[mieszkania_col]] / data[[powierzchnia_col]])
  data$logMieszkaniaNa1000 <- log(data[[mieszkania_col]] / data$Populacja * 1000)
  # Tworzymy log mieszkan oddanych na 1000 mieszkancow.
  data$logMieszkaniaOddaneNa1000 <- log(data[[mieszkania_col]] / data$Populacja * 1000)
  data$M2NaOsobe <- data[[m2_na_osobe_col]]
  # Logarytm powierzchni na osobe zapisujemy do osobnej mapy.
  data$logM2NaOsobe <- log(data$M2NaOsobe)
  data$Przychodnie <- data[[przychodnie_col]]
  data$Bezrobocie <- data[[bezrobocie_col]]
  data$Miejska <- as.integer(substr(data$Kod, nchar(data$Kod), nchar(data$Kod)) == "1")
  data$Wiejska <- as.integer(substr(data$Kod, nchar(data$Kod), nchar(data$Kod)) == "2")
  data$Odleglosc <- data$odleglosc
  data$Odleglosc2 <- data$Odleglosc^2
  data$WskaznikG <- data[[wskaznik_g_col]]
  data$logWskaznikG <- log(data$WskaznikG)

  data <- replace_infinite_na(data)
  row.names(data) <- as.character(seq_len(nrow(data)) - 1)
  data
}

drop_na_cols <- function(data, cols) {
  # Usuwa wiersze z brakami w dokladnie wskazanych kolumnach.
  data[complete.cases(data[, cols, drop = FALSE]), cols, drop = FALSE]
}

is_usable_mice_predictor <- function(column) {
  # Sprawdza, czy kolumna ma wystarczajaco duzo informacji dla predykcji MICE.
  observed <- column[!is.na(column)]
  length(observed) > 1 && length(unique(observed)) > 1
}

save_mice_imputation_report <- function(report, imputed_values, imputations = NULL) {
  # Zapisuje raport z imputacji, zeby bylo widac, ile brakow zostalo uzupelnionych.
  write.csv(
    report,
    r_output_path(wydruki_dir, "WMNKmice_imputacja_brakow.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  if (length(imputed_values) > 0) {
    write.csv(
      do.call(rbind, imputed_values),
      r_output_path(wydruki_dir, "WMNKmice_imputowane_wartosci.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }
  if (!is.null(imputations$loggedEvents) && nrow(imputations$loggedEvents) > 0) {
    write.csv(
      imputations$loggedEvents,
      r_output_path(wydruki_dir, "WMNKmice_logged_events.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }
}

mice_completed_bundle_path <- function() {
  # Zwraca wspolna sciezke do listy 10 imputowanych zbiorow uzywanej przez pozostale skrypty R.
  file.path(mice_exports_dir, "WMNKmice_completed_datasets.rds")
}

save_mice_completed_bundle <- function(bundle) {
  # Zapisuje obiekt MICE i 10 kompletnych zbiorow, aby SDEM i GWR korzystaly z tej samej imputacji.
  dir.create(mice_exports_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(bundle, mice_completed_bundle_path())
  write.csv(
    bundle$report,
    file.path(mice_exports_dir, "WMNKmice_completed_datasets_manifest.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  invisible(bundle)
}

load_mice_completed_bundle <- function() {
  # Wczytuje wspolny artefakt imputacji zapisany przez WMNKmice.R.
  readRDS(mice_completed_bundle_path())
}

impute_model_missing_with_mice <- function(data, return_all = FALSE, export_all = FALSE) {
  # Uzupelnia MICE tylko braki w zmiennych, ktore blokowaly pelna probe modelu.
  # Przy return_all=TRUE zwraca rowniez 10 kompletnych zbiorow do poolowania wynikow.
  missing_required <- setdiff(mice_impute_cols, names(data))
  if (length(missing_required) > 0) {
    stop(sprintf("Brak kolumn do imputacji MICE: %s", paste(missing_required, collapse = ", ")), call. = FALSE)
  }

  missing_before <- colSums(is.na(data[, mice_impute_cols, drop = FALSE]))
  target_cols <- names(missing_before)[missing_before > 0]
  empty_report <- data.frame(
    variable = mice_impute_cols,
    missing_before = as.integer(missing_before[mice_impute_cols]),
    missing_after = as.integer(missing_before[mice_impute_cols]),
    imputed = 0L,
    method = "",
    m = mice_m,
    maxit = mice_maxit,
    seed = mice_seed,
    completed_dataset = mice_complete_action,
    stringsAsFactors = FALSE
  )
  if (length(target_cols) == 0) {
    save_mice_imputation_report(empty_report, list())
    completed_datasets <- replicate(mice_m, data, simplify = FALSE)
    names(completed_datasets) <- paste0("imp", seq_len(mice_m))
    bundle <- list(
      single = data,
      completed_datasets = completed_datasets,
      mids = NULL,
      report = empty_report,
      imputed_values = list(),
      impute_cols = mice_impute_cols,
      predictor_cols = character(0),
      m = mice_m,
      maxit = mice_maxit,
      seed = mice_seed
    )
    if (export_all) {
      save_mice_completed_bundle(bundle)
    }
    if (return_all) {
      return(bundle)
    }
    return(data)
  }

  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  predictor_cols <- unique(c(mice_impute_cols, intersect(mice_predictor_cols, numeric_cols)))
  bad_targets <- target_cols[vapply(data[, target_cols, drop = FALSE], function(column) sum(!is.na(column)) < 2, logical(1))]
  if (length(bad_targets) > 0) {
    stop(sprintf("Za malo danych obserwowanych do imputacji MICE: %s", paste(bad_targets, collapse = ", ")), call. = FALSE)
  }

  usable_predictors <- predictor_cols[vapply(data[, predictor_cols, drop = FALSE], function(column) {
    # Do predykcji bierzemy tylko kompletne kolumny, zeby pmm nie zwracal NA dla wierszy imputowanych.
    !any(is.na(column)) && is_usable_mice_predictor(column)
  }, logical(1))]
  predictor_cols <- unique(c(mice_impute_cols, usable_predictors))
  impute_input <- data[, predictor_cols, drop = FALSE]

  method <- mice::make.method(impute_input)
  method[] <- ""
  method[target_cols] <- "pmm"
  predictor_matrix <- mice::make.predictorMatrix(impute_input)
  predictor_matrix[,] <- 0
  for (target_col in target_cols) {
    predictor_matrix[target_col, setdiff(usable_predictors, target_col)] <- 1
  }

  imputations <- mice::mice(
    impute_input,
    m = mice_m,
    maxit = mice_maxit,
    method = method,
    predictorMatrix = predictor_matrix,
    printFlag = FALSE,
    seed = mice_seed
  )
  # Bierzemy jedna kompletna baze, zeby zachowac dotychczasowy jednoprzebiegowy pipeline WMNK.
  completed <- mice::complete(imputations, action = mice_complete_action)
  completed_datasets <- mice::complete(imputations, action = "all")
  completed_datasets <- lapply(completed_datasets, function(completed_part) {
    # Do kazdej imputacji doklejamy pelny zbior z nieimputowanymi kolumnami i stala kolejnoscia Kod.
    completed_data <- data
    for (target_col in target_cols) {
      missing_mask <- is.na(data[[target_col]])
      completed_data[[target_col]][missing_mask] <- completed_part[[target_col]][missing_mask]
    }
    completed_data
  })
  names(completed_datasets) <- paste0("imp", seq_along(completed_datasets))

  imputed_values <- list()
  imputed_values_all <- list()
  for (target_col in target_cols) {
    missing_mask <- is.na(data[[target_col]])
    imputed_values[[target_col]] <- data.frame(
      Kod = data$Kod[missing_mask],
      variable = target_col,
      imputed_value = completed[[target_col]][missing_mask],
      stringsAsFactors = FALSE
    )
    imputed_values_all[[target_col]] <- do.call(rbind, lapply(seq_along(completed_datasets), function(imp_id) {
      data.frame(
        .imp = imp_id,
        Kod = data$Kod[missing_mask],
        variable = target_col,
        imputed_value = completed_datasets[[imp_id]][[target_col]][missing_mask],
        stringsAsFactors = FALSE
      )
    }))
    data[[target_col]][missing_mask] <- completed[[target_col]][missing_mask]
  }

  missing_after <- colSums(is.na(data[, mice_impute_cols, drop = FALSE]))
  report <- data.frame(
    variable = mice_impute_cols,
    missing_before = as.integer(missing_before[mice_impute_cols]),
    missing_after = as.integer(missing_after[mice_impute_cols]),
    imputed = as.integer(missing_before[mice_impute_cols] - missing_after[mice_impute_cols]),
    method = ifelse(mice_impute_cols %in% target_cols, method[mice_impute_cols], ""),
    m = mice_m,
    maxit = mice_maxit,
    seed = mice_seed,
    completed_dataset = mice_complete_action,
    stringsAsFactors = FALSE
  )
  save_mice_imputation_report(report, imputed_values, imputations)
  if (length(imputed_values_all) > 0) {
    write.csv(
      do.call(rbind, imputed_values_all),
      file.path(mice_exports_dir, "WMNKmice_imputowane_wartosci_10_imputacji.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }
  if (any(missing_after[mice_impute_cols] > 0)) {
    stop("Imputacja MICE nie uzupelnila wszystkich brakow w zmiennych modelu.", call. = FALSE)
  }
  bundle <- list(
    single = data,
    completed_datasets = completed_datasets,
    mids = imputations,
    report = report,
    imputed_values = imputed_values_all,
    impute_cols = mice_impute_cols,
    predictor_cols = predictor_cols,
    m = mice_m,
    maxit = mice_maxit,
    seed = mice_seed
  )
  if (export_all) {
    save_mice_completed_bundle(bundle)
  }
  if (return_all) {
    return(bundle)
  }
  data
}

fit_lm_for_columns <- function(data, x_cols, response = "ySaldo", weights = NULL) {
  # Estymuje model liniowy dla zadanej listy regresorow.
  formula <- as.formula(paste(response, "~", paste(x_cols, collapse = " + ")))
  if (is.null(weights)) {
    lm(formula, data = data)
  } else {
    lm(formula, data = data, weights = weights)
  }
}

statsmodels_loglike <- function(model) {
  # Liczy log-likelihood zgodny z formula statsmodels dla OLS/WLS.
  resid <- residuals(model)
  n <- length(resid)
  weights <- weights(model)
  if (is.null(weights)) {
    weights <- rep(1, n)
  }
  ssr <- sum(weights * resid^2)
  if (!is.finite(ssr) || ssr <= 0) {
    return(NA_real_)
  }
  -n / 2 * log(ssr) - n / 2 * (1 + log(2 * pi / n)) + 0.5 * sum(log(weights))
}

statsmodels_aic <- function(model) {
  # Liczy AIC z taka sama kara jak statsmodels.
  llf <- statsmodels_loglike(model)
  -2 * llf + 2 * length(coef(model))
}

statsmodels_bic <- function(model) {
  # Liczy BIC z taka sama kara jak statsmodels.
  llf <- statsmodels_loglike(model)
  -2 * llf + log(length(residuals(model))) * length(coef(model))
}

model_r2 <- function(model) {
  # Pobiera R2 z podsumowania modelu.
  unname(summary(model)$r.squared)
}

model_adj_r2 <- function(model) {
  # Pobiera skorygowane R2 z podsumowania modelu.
  unname(summary(model)$adj.r.squared)
}

model_coef_dataframe <- function(model, step = NULL, variant = NULL) {
  # Buduje ramke wspolczynnikow zgodna z eksportem Pythona.
  coefs <- as.data.frame(summary(model)$coefficients, stringsAsFactors = FALSE)
  names(coefs) <- c("coefficient", "std_error", "t_value", "p_value")
  coefs$variable <- vapply(row.names(coefs), display_coef_name, character(1))
  coefs <- coefs[, c("variable", "coefficient", "std_error", "t_value", "p_value"), drop = FALSE]
  if (!is.null(step)) {
    coefs <- cbind(step = step, coefs)
  }
  if (!is.null(variant)) {
    coefs <- cbind(variant = variant, coefs)
  }
  row.names(coefs) <- NULL
  coefs
}

coef_value <- function(model, variable, field = "estimate") {
  # Pobiera wybrana statystyke wspolczynnika po nazwie statsmodels.
  name <- ifelse(variable == "const", "(Intercept)", variable)
  coefs <- summary(model)$coefficients
  column <- switch(
    field,
    estimate = "Estimate",
    std_error = "Std. Error",
    t_value = "t value",
    p_value = "Pr(>|t|)"
  )
  unname(coefs[name, column])
}

design_matrix_from_data <- function(data, x_cols) {
  # Tworzy macierz regresorow z kolumna stalej nazwanej jak w statsmodels.
  x <- cbind(const = 1, as.matrix(data[, x_cols, drop = FALSE]))
  storage.mode(x) <- "double"
  x
}

has_constant_column <- function(exog) {
  # Sprawdza, czy macierz zawiera kolumne stala.
  any(apply(exog, 2, function(column) max(abs(column - column[1]), na.rm = TRUE) < 1e-10))
}

auxiliary_ols_stats <- function(y, exog) {
  # Liczy R2 i F dla regresji pomocniczej bez automatycznego dodawania stalej.
  fit <- lm.fit(x = exog, y = y)
  resid <- fit$residuals
  n <- length(y)
  rank <- fit$rank
  centered <- has_constant_column(exog)
  tss <- if (centered) sum((y - mean(y))^2) else sum(y^2)
  if (!is.finite(tss) || tss <= 0) {
    return(list(r2 = NA_real_, fvalue = NA_real_, fpvalue = NA_real_, rank = rank, df_resid = n - rank))
  }
  ssr <- sum(resid^2)
  r2 <- 1 - ssr / tss
  df_model <- if (centered) rank - 1 else rank
  df_resid <- n - rank
  if (df_model <= 0 || df_resid <= 0 || !is.finite(r2) || r2 >= 1) {
    fvalue <- NA_real_
    fpvalue <- NA_real_
  } else {
    fvalue <- (r2 / df_model) / ((1 - r2) / df_resid)
    fpvalue <- pf(fvalue, df_model, df_resid, lower.tail = FALSE)
  }
  list(r2 = r2, fvalue = fvalue, fpvalue = fpvalue, rank = rank, df_resid = df_resid)
}

bp_test <- function(resid, exog) {
  # Odtwarza test Breuscha-Pagana/Koenkera z het_breuschpagan.
  y <- resid^2
  stats <- auxiliary_ols_stats(y, exog)
  df <- qr(exog)$rank - 1
  lm_stat <- length(resid) * stats$r2
  c(
    "lagrange multiplier statistic" = lm_stat,
    "p-value" = pchisq(lm_stat, df = df, lower.tail = FALSE),
    "f-value" = stats$fvalue,
    "f p-value" = stats$fpvalue
  )
}

full_rank_exog <- function(exog) {
  # Usuwa liniowo zalezne kolumny pomocniczej macierzy testowej.
  exog <- as.matrix(exog)
  qr_obj <- qr(exog)
  if (qr_obj$rank == 0) {
    return(exog[, integer(), drop = FALSE])
  }
  keep <- sort(qr_obj$pivot[seq_len(qr_obj$rank)])
  exog[, keep, drop = FALSE]
}

safe_white <- function(resid, exog) {
  # Test White'a wymaga macierzy bez liniowo zaleznych kolumn.
  tryCatch(
    {
      exog <- full_rank_exog(exog)
      nvars <- ncol(exog)
      pair_index <- which(upper.tri(matrix(TRUE, nvars, nvars), diag = TRUE), arr.ind = TRUE)
      aux <- sapply(seq_len(nrow(pair_index)), function(idx) {
        exog[, pair_index[idx, 1]] * exog[, pair_index[idx, 2]]
      })
      aux <- as.matrix(aux)
      stats <- auxiliary_ols_stats(resid^2, aux)
      df <- qr(aux)$rank - 1
      lm_stat <- length(resid) * stats$r2
      c(
        "lagrange multiplier statistic" = lm_stat,
        "p-value" = pchisq(lm_stat, df = df, lower.tail = FALSE),
        "f-value" = stats$fvalue,
        "f p-value" = stats$fpvalue
      )
    },
    error = function(exc) c(
      "lagrange multiplier statistic" = NA_real_,
      "p-value" = NA_real_,
      "f-value" = NA_real_,
      "f p-value" = NA_real_
    )
  )
}

jarque_bera_test <- function(resid) {
  # Liczy statystyke Jarque-Bera, skosnosc i kurtoze jak statsmodels.
  resid <- as.numeric(resid)
  n <- length(resid)
  centered <- resid - mean(resid)
  m2 <- mean(centered^2)
  skewness <- mean(centered^3) / (m2^(3 / 2))
  kurtosis <- mean(centered^4) / (m2^2)
  jb <- n / 6 * (skewness^2 + (kurtosis - 3)^2 / 4)
  c(
    "The Jarque-Bera test statistic" = jb,
    "p-value" = pchisq(jb, df = 2, lower.tail = FALSE),
    "skewness" = skewness,
    "kurtosis" = kurtosis
  )
}

reset_test <- function(model, power = 3) {
  # Liczy test RESET tak jak statsmodels: Wald chi-kwadrat dla poteg wartosci dopasowanych.
  y <- model.response(model.frame(model))
  x <- model.matrix(model)
  fitted_values <- fitted(model)
  powers <- 2:power
  z <- cbind(x, sapply(powers, function(p) fitted_values^p))
  # statsmodels refituje rozszerzony model bez przekazywania wag z modelu bazowego.
  unrestricted <- lm.fit(x = z, y = y)
  q <- ncol(z) - ncol(x)
  gamma_idx <- (ncol(z) - q + 1):ncol(z)
  df_resid <- length(y) - unrestricted$rank
  sigma2 <- sum(unrestricted$residuals^2) / df_resid
  cov_unrestricted <- sigma2 * solve(crossprod(z))
  gamma <- unrestricted$coefficients[gamma_idx]
  cov_gamma <- cov_unrestricted[gamma_idx, gamma_idx, drop = FALSE]
  statistic <- as.numeric(t(gamma) %*% solve(cov_gamma, gamma))
  list(fvalue = statistic, pvalue = pchisq(statistic, df = q, lower.tail = FALSE))
}

f_test_single_zero <- function(model, variable) {
  # Liczy F-test dla pojedynczego ograniczenia beta=0.
  coefs <- summary(model)$coefficients
  t_value <- unname(coefs[variable, "t value"])
  fvalue <- t_value^2
  list(fvalue = fvalue, pvalue = pf(fvalue, 1, df.residual(model), lower.tail = FALSE))
}

variance_inflation_factor_matrix <- function(exog) {
  # Liczy VIF tak jak statsmodels dla kazdej kolumny macierzy z dodana stala.
  exog <- as.matrix(exog)
  result <- numeric(ncol(exog))
  for (idx in seq_len(ncol(exog))) {
    y <- exog[, idx]
    x <- exog[, -idx, drop = FALSE]
    fit <- lm.fit(x = x, y = y)
    ssr <- sum(fit$residuals^2)
    tss <- if (has_constant_column(x)) sum((y - mean(y))^2) else sum(y^2)
    r2 <- ifelse(tss <= 0, 1, 1 - ssr / tss)
    result[idx] <- 1 / (1 - r2)
  }
  names(result) <- colnames(exog)
  result
}

fit_variant <- function(
  data,
  model_name,
  include_roads = TRUE,
  g_variant = "base_log",
  housing_variant = "base_m2",
  sample_label = "pelna_proba"
) {
  # Estymuje wariant modelu: OLS i WLS na tej samej probie bez usuwania obserwacji.
  x_cols <- build_x_cols(include_roads, g_variant, housing_variant)
  required_cols <- c("Kod", "ySaldo", x_cols)
  model_data <- drop_na_cols(data, required_cols)
  ols <- fit_lm_for_columns(model_data, x_cols)

  model_data$log_u2 <- log(pmax(residuals(ols)^2, .Machine$double.xmin))
  variance_model <- fit_lm_for_columns(model_data, x_cols, response = "log_u2")
  weights_wls <- 1 / exp(fitted(variance_model))
  wls <- fit_lm_for_columns(model_data, x_cols, weights = weights_wls)

  exog <- model.matrix(wls)
  bp <- bp_test(residuals(wls), exog)
  white <- safe_white(residuals(wls), exog)
  pvalues <- summary(wls)$coefficients[, "Pr(>|t|)"]
  pvalues_without_const <- pvalues[names(pvalues) != "(Intercept)"]
  max_pvalue_without_const <- max(pvalues_without_const)
  insignificant_variables_5pct <- paste(names(pvalues_without_const)[pvalues_without_const >= 0.05], collapse = ",")

  list(
    model = model_name,
    sample = sample_label,
    variable = "bez_drog",
    wskaznik_g = g_variant,
    housing = housing_variant,
    n_obs = nrow(model_data),
    ols_aic = statsmodels_aic(ols),
    ols_bic = statsmodels_bic(ols),
    ols_r2 = model_r2(ols),
    wls_aic = statsmodels_aic(wls),
    wls_bic = statsmodels_bic(wls),
    wls_r2 = model_r2(wls),
    wls_adj_r2 = model_adj_r2(wls),
    bp_pvalue = unname(bp["p-value"]),
    white_pvalue = unname(white["p-value"]),
    all_variables_significant_5pct = max_pvalue_without_const < 0.05,
    max_pvalue_without_const = max_pvalue_without_const,
    insignificant_variables_5pct = insignificant_variables_5pct,
    coef_przystanki_na_1000 = coef_value(wls, "logPrzystankiNa1000"),
    pvalue_przystanki_na_1000 = coef_value(wls, "logPrzystankiNa1000", "p_value"),
    coef_drogi_na_1000 = NA_real_,
    pvalue_drogi_na_1000 = NA_real_,
    coef_mieszkania_na_1000 = coef_value(wls, "logMieszkaniaNa1000"),
    pvalue_mieszkania_na_1000 = coef_value(wls, "logMieszkaniaNa1000", "p_value"),
    coef_wskaznik_g = NA_real_,
    pvalue_wskaznik_g = NA_real_,
    coef_log_wskaznik_g = coef_value(wls, "logWskaznikG"),
    pvalue_log_wskaznik_g = coef_value(wls, "logWskaznikG", "p_value"),
    coef_m2_na_osobe = coef_value(wls, "M2NaOsobe"),
    pvalue_m2_na_osobe = coef_value(wls, "M2NaOsobe", "p_value"),
    model_object = wls
  )
}

fit_wls_for_columns <- function(data, x_cols) {
  # Estymuje pojedynczy model WMNK dla zadanej listy zmiennych bez usuwania obserwacji.
  required_cols <- c("Kod", "ySaldo", x_cols)
  model_data <- drop_na_cols(data, required_cols)
  ols <- fit_lm_for_columns(model_data, x_cols)

  model_data$log_u2 <- log(pmax(residuals(ols)^2, .Machine$double.xmin))
  variance_model <- fit_lm_for_columns(model_data, x_cols, response = "log_u2")
  weights_wls <- 1 / exp(fitted(variance_model))
  wls <- fit_lm_for_columns(model_data, x_cols, weights = weights_wls)
  exog <- model.matrix(wls)
  bp <- bp_test(residuals(wls), exog)
  white <- safe_white(residuals(wls), exog)

  list(
    x_cols = x_cols,
    model_data = model_data,
    ols = ols,
    variance_model = variance_model,
    wls = wls,
    bp = bp,
    white = white
  )
}

# --- MICE / Rubin pooling dla finalnego WMNK ---
fit_wls_per_imputation <- function(completed_datasets, x_cols) {
  lapply(completed_datasets, function(d) fit_wls_for_columns(d, x_cols)$wls)
}

pool_wls_rubin <- function(fits) {
  # Rubin pooling dla listy lm/wls; zwraca data.frame z estimate, std.error, t, p, fmi, df.
  m <- length(fits)
  coefs <- lapply(fits, function(fit) summary(fit)$coefficients)
  terms <- rownames(coefs[[1]])
  out <- lapply(terms, function(term) {
    qbar <- mean(vapply(coefs, function(co) co[term, "Estimate"], numeric(1)))
    ubar <- mean(vapply(coefs, function(co) co[term, "Std. Error"]^2, numeric(1)))
    b <- stats::var(vapply(coefs, function(co) co[term, "Estimate"], numeric(1)))
    total_var <- ubar + (1 + 1/m) * b
    se <- sqrt(total_var)
    riv <- (1 + 1/m) * b / ubar
    lambda <- (1 + 1/m) * b / total_var
    df_old <- (m - 1) / lambda^2
    fmi <- lambda
    tstat <- qbar / se
    pval <- 2 * pt(-abs(tstat), df = pmax(df_old, 1))
    data.frame(
      term = term, estimate = qbar, std_error = se,
      statistic = tstat, p_value = pval, fmi = fmi,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

save_pooled_final_wmnk_stargazer <- function(mice_bundle, x_cols) {
  # Buduje pulę WLS po m imputacjach MICE i zapisuje tabelę współczynników Rubinem do paper/stargazer/.
  if (is.null(mice_bundle$completed_datasets)) {
    message("Brak completed_datasets w mice_bundle; pominięto pooled-WMNK.")
    return(invisible(NULL))
  }
  fits <- fit_wls_per_imputation(mice_bundle$completed_datasets, x_cols)
  pooled <- pool_wls_rubin(fits)
  pooled$Zmienna <- vapply(pooled$term, polish_display_label, character(1))
  pooled_out <- pooled[, c("Zmienna", "estimate", "std_error", "statistic", "p_value", "fmi")]
  names(pooled_out) <- c("Zmienna", "Współczynnik", "Błąd std.", "t", "p-value", "FMI")
  write.csv(
    pooled_out,
    r_output_path(wydruki_dir, "WMNK_final_pooled_rubin_R.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  # LaTeX
  fmt <- function(x) ifelse(is.na(x), "--", formatC(x, digits = 4, format = "f"))
  body <- paste(
    apply(pooled_out, 1, function(row) {
      paste(c(row[["Zmienna"]], fmt(as.numeric(row[["Współczynnik"]])),
              fmt(as.numeric(row[["Błąd std."]])), fmt(as.numeric(row[["t"]])),
              fmt(as.numeric(row[["p-value"]])), fmt(as.numeric(row[["FMI"]]))),
            collapse = " & ")
    }),
    collapse = " \\\\\n"
  )
  m <- length(mice_bundle$completed_datasets)
  latex <- paste0(
    "\\begin{table}[H]\n\\centering\n",
    "\\caption{Finalny WMNK po pooligu Rubina (m=", m, " imputacji MICE)}\n",
    "\\label{tab:wmnk_final_pooled_rubin}\n",
    "\\begin{tabular}{lrrrrr}\n\\hline\n",
    "Zmienna & Współczynnik & Błąd std. & t & p-value & FMI \\\\\n\\hline\n",
    body, " \\\\\n\\hline\n",
    "\\end{tabular}\n\\end{table}\n"
  )
  paper_dir <- file.path(base_dir, "paper", "stargazer")
  if (!dir.exists(paper_dir)) dir.create(paper_dir, recursive = TRUE)
  writeLines(enc2utf8(latex), file.path(paper_dir, "WMNK_final_pooled_rubin_R.tex"), useBytes = TRUE)
  invisible(pooled_out)
}

load_gminy_for_map <- function(message_filename, map_description) {
  # Wczytuje geometrie gmin; gdy brakuje silnika GIS, nie przerywa eksportu tabel.
  if (!isTRUE(has_sf)) {
    message <- paste0(
      "Nie zapisano ", map_description, ", bo w środowisku R nie jest dostępny pakiet sf."
    )
    writeLines(message, r_output_path(wydruki_dir, message_filename), useBytes = TRUE)
    return(NULL)
  }
  tryCatch(
    sf::st_read(granice_gmin_path, quiet = TRUE),
    error = function(exc) {
      message <- paste0(
        "Nie zapisano ", map_description, ", bo sf nie wczytal pliku SHP: ",
        conditionMessage(exc)
      )
      writeLines(message, r_output_path(wydruki_dir, message_filename), useBytes = TRUE)
      NULL
    }
  )
}

twoslope_values <- function(values, vmin, vmax) {
  # Skaluje wartosci jak TwoSlopeNorm z centrum w zerze.
  if (isTRUE(vmin < 0 && vmax > 0)) {
    ifelse(values < 0, 0.5 * (values - vmin) / (0 - vmin), 0.5 + 0.5 * values / vmax)
  } else {
    (values - vmin) / (vmax - vmin)
  }
}

format_legend_tick <- function(values) {
  # Formatuje etykiety legendy bez sztucznego przesuwania kolorow w ramkach.
  formatted <- ifelse(abs(values) >= 100, sprintf("%.0f", values), sprintf("%.3f", values))
  formatted <- gsub(".", ",", formatted, fixed = TRUE)
  formatted <- sub("(,[0-9]*?)0+$", "\\1", formatted)
  sub(",$", "", formatted)
}

draw_heatmap_legend <- function(vmin, vmax, palette, legend_label) {
  # Rysuje ciagly pasek legendy z jedna ramka, bez osobnych czarnych obwodek dla kazdego koloru.
  usr <- par("usr")
  dx <- diff(usr[1:2])
  dy <- diff(usr[3:4])
  x0 <- usr[1] + 0.035 * dx
  x1 <- usr[1] + 0.060 * dx
  y0 <- usr[3] + 0.055 * dy
  y1 <- usr[3] + 0.355 * dy
  y_edges <- seq(y0, y1, length.out = length(palette) + 1)
  rect(
    xleft = x0,
    ybottom = y_edges[-length(y_edges)],
    xright = x1,
    ytop = y_edges[-1],
    col = palette,
    border = NA,
    xpd = NA
  )
  rect(x0, y0, x1, y1, border = "black", lwd = 0.7, xpd = NA)

  ticks <- pretty(c(vmin, vmax), n = 5)
  ticks <- ticks[ticks >= vmin & ticks <= vmax]
  tick_positions <- y0 + twoslope_values(ticks, vmin, vmax) * (y1 - y0)
  segments(x1, tick_positions, x1 + 0.010 * dx, tick_positions, lwd = 0.7, xpd = NA)
  text(x1 + 0.014 * dx, tick_positions, labels = format_legend_tick(ticks), adj = c(0, 0.5), cex = 0.65, xpd = NA)
  text(x0, y1 + 0.030 * dy, labels = polish_output_text(legend_label), adj = c(0, 0), font = 2, cex = 0.75, xpd = NA)
}

plot_sf_heatmap <- function(map_data, column, title, filename, legend_label = column, dpi = 220, limits = NULL) {
  # Zapisuje mape ciepla jako rastrowy PNG.
  values <- map_data[[column]]
  observed <- values[!is.na(values)]
  if (!is.null(limits)) {
    vmin <- limits[[1]]
    vmax <- limits[[2]]
  } else if (length(observed) == 0) {
    vmin <- 0
    vmax <- 1
  } else {
    vmin <- min(observed)
    vmax <- max(observed)
  }
  palette <- grDevices::colorRampPalette(c("#3b4cc0", "#f7f7f7", "#b40426"))(256)
  scaled <- twoslope_values(values, vmin, vmax)
  color_index <- pmin(256, pmax(1, floor(scaled * 255) + 1))
  colors <- palette[color_index]
  colors[is.na(values)] <- "#eeeeee"

  save_plot_device(filename, width = 9, height = 9, dpi = dpi, code = function() {
    old_par <- par(mar = c(0.2, 0.2, 2.2, 0.2))
    on.exit(par(old_par), add = TRUE)
    plot(sf::st_geometry(map_data), col = colors, border = "white", lwd = 0.08, main = polish_output_text(title))
    draw_heatmap_legend(vmin, vmax, palette, legend_label)
  })
}

save_ysaldo_heatmap <- function(data) {
  # Zapisuje mape ciepla salda migracji.
  gminy <- load_gminy_for_map("mapa_ciepla_ysaldo_brak_geopandas.txt", "mapy ciepła salda migracji")
  if (is.null(gminy)) {
    return(invisible(NULL))
  }
  saldo_data <- data[!is.na(data$ySaldo), c("Kod", "ySaldo"), drop = FALSE]
  saldo_data$Kod <- norm_code(saldo_data$Kod)
  gminy$Kod <- norm_code(gminy$JPT_KOD_JE)
  map_data <- merge(gminy, saldo_data, by = "Kod", all.x = TRUE, sort = FALSE)
  plot_sf_heatmap(map_data, "ySaldo", "Mapa ciepła salda migracji", "mapa_ciepla_ysaldo", "Saldo migracji")
}

save_single_variable_heatmap <- function(data, column, filename, title) {
  # Zapisuje osobna mape ciepla dla jednej zmiennej.
  gminy <- load_gminy_for_map(paste0(filename, "_brak_geopandas.txt"), paste0("mapy ciepła ", polish_display_label(column)))
  if (is.null(gminy)) {
    return(invisible(NULL))
  }
  variable_data <- data[!is.na(data[[column]]), c("Kod", column), drop = FALSE]
  variable_data$Kod <- norm_code(variable_data$Kod)
  gminy$Kod <- norm_code(gminy$JPT_KOD_JE)
  map_data <- merge(gminy, variable_data, by = "Kod", all.x = TRUE, sort = FALSE)
  plot_sf_heatmap(map_data, column, title, filename, polish_display_label(column))
}

save_final_wmnk_residual_map <- function(fit) {
  # Zapisuje mape reszt finalnego modelu WMNK.
  gminy <- load_gminy_for_map("mapa_reszt_finalnego_wmnk_brak_geopandas.txt", "mapy reszt finalnego WMNK")
  if (is.null(gminy)) {
    return(invisible(NULL))
  }
  residual_data <- fit$model_data[, "Kod", drop = FALSE]
  residual_data$Kod <- norm_code(residual_data$Kod)
  residual_data$reszty_wmnk <- as.numeric(residuals(fit$wls))
  gminy$Kod <- norm_code(gminy$JPT_KOD_JE)
  map_data <- merge(gminy, residual_data, by = "Kod", all.x = TRUE, sort = FALSE)
  vmax <- max(abs(map_data$reszty_wmnk), na.rm = TRUE)
  vmax <- ifelse(is.finite(vmax), vmax, 1)
  map_data$reszty_wmnk <- pmax(-vmax, pmin(vmax, map_data$reszty_wmnk))
  plot_sf_heatmap(
    map_data,
    "reszty_wmnk",
    "Mapa reszt finalnego modelu WMNK",
    "mapa_reszt_finalnego_wmnk",
    "Reszty WMNK",
    limits = c(-vmax, vmax)
  )
}

save_variable_heatmap_panel <- function(data, variable_cols) {
  # Tworzy plansze map ciepla dla zmiennej objasnianej i regresorow.
  gminy <- load_gminy_for_map("plansza_map_ciepla_zmiennych_brak_geopandas.txt", "planszy map ciepła zmiennych")
  if (is.null(gminy)) {
    return(invisible(NULL))
  }
  variable_data <- data[, c("Kod", variable_cols), drop = FALSE]
  variable_data$Kod <- norm_code(variable_data$Kod)
  gminy$Kod <- norm_code(gminy$JPT_KOD_JE)
  map_data <- merge(gminy, variable_data, by = "Kod", all.x = TRUE, sort = FALSE)
  palette <- grDevices::colorRampPalette(c("#3b4cc0", "#f7f7f7", "#b40426"))(256)

  save_plot_device("mapy_ciepla_zmiennych_4x3", width = 15, height = 18, dpi = 300, code = function() {
    old_par <- par(mfrow = c(4, 3), mar = c(0.2, 0.2, 2.8, 0.2), oma = c(0, 0, 3, 0), cex.main = 1.6)
    on.exit(par(old_par), add = TRUE)
    for (column in variable_cols) {
      values <- map_data[[column]]
      observed <- values[!is.na(values)]
      if (length(observed) == 0) {
        plot.new()
        title(column, cex.main = 0.8)
        next
      }
      vmin <- min(observed)
      vmax <- max(observed)
      scaled <- twoslope_values(values, vmin, vmax)
      color_index <- pmin(256, pmax(1, floor(scaled * 255) + 1))
      colors <- palette[color_index]
      colors[is.na(values)] <- "#eeeeee"
      plot(sf::st_geometry(map_data), col = colors, border = "white", lwd = 0.03, main = polish_display_label(column))
    }
    for (idx in seq_len(12 - length(variable_cols))) {
      plot.new()
    }
    mtext("Mapy ciepła zmiennych modelu", outer = TRUE, cex = 1.8)
  })
}

save_correlation_plot <- function(corr_df, name, title) {
  # Zapisuje macierz korelacji jako obraz.
  save_plot_device(name, width = 9.6, height = 8.4, dpi = 300, code = function() {
    old_par <- par(mar = c(15, 15, 4, 1.5))
    on.exit(par(old_par), add = TRUE)
    palette <- grDevices::colorRampPalette(c("#3b4cc0", "#f7f7f7", "#b40426"))(256)
    mat <- as.matrix(corr_df)
    image(seq_len(ncol(mat)), seq_len(nrow(mat)), t(mat[nrow(mat):1, ]), col = palette, zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "")
    axis(1, at = seq_len(ncol(mat)), labels = vapply(colnames(mat), polish_display_label, character(1)), las = 2, cex.axis = 1.05)
    axis(2, at = seq_len(nrow(mat)), labels = rev(vapply(rownames(mat), polish_display_label, character(1))), las = 2, cex.axis = 1.05)
    title(polish_output_text(title), cex.main = 1.8)
  })
}

save_final_wmnk_plots <- function(data, fit, x_cols) {
  # Generuje wykresy diagnostyczne finalnego WMNK.
  model_data <- fit$model_data
  model_wls_data <- fit$model_data
  wls <- fit$wls

  # W korelacjach pomijamy kwadrat odleglosci, zeby nie dublowac zmiennej.
  corr_cols <- c("logPrzystankiNa1000", x_cols[x_cols != "Odleglosc2"])
  corr_data <- drop_na_cols(data, corr_cols)
  for (corr_method in c("pearson", "spearman")) {
    corr_df <- cor(corr_data, method = corr_method)
    save_correlation_plot(corr_df, paste0("korelacje_zmiennych_", corr_method), paste0("Korelacje zmiennych (", tools::toTitleCase(corr_method), ")"))
  }

  residuals_wls <- as.numeric(residuals(wls))
  fitted_wls <- as.numeric(fitted(wls))
  model_wls_data$residuals <- residuals_wls
  model_wls_data$predictions <- fitted_wls
  model_wls_data$sqrt_abs_resid <- sqrt(abs(residuals_wls))

  # Laczymy cztery wykresy diagnostyczne reszt w jedna plansze.
  save_plot_device("diagnostyka_reszt_2x2_final", width = 12, height = 9, dpi = 300, code = function() {
    old_par <- par(mfrow = c(2, 2), cex.main = 1.6, cex.lab = 1.2, cex.axis = 1.1)
    on.exit(par(old_par), add = TRUE)
    hist(residuals_wls, breaks = 30, probability = TRUE, col = "gray80", border = "black", main = "Histogram reszt WMNK", xlab = "reszty", ylab = "gęstość")
    if (stats::sd(residuals_wls) > 0) {
      x_grid <- seq(min(residuals_wls), max(residuals_wls), length.out = 200)
      lines(x_grid, dnorm(x_grid, mean(residuals_wls), stats::sd(residuals_wls)), lwd = 1.2)
    }
    plot(fitted_wls, residuals_wls, pch = 16, cex = 0.7, col = grDevices::adjustcolor("black", 0.65), main = "Reszty vs wartości dopasowane", xlab = "wartości dopasowane", ylab = "reszty")
    abline(h = 0, lwd = 1)
    if (length(fitted_wls) > 1) {
      lines(lowess(fitted_wls, residuals_wls, f = 0.25), col = "green", lwd = 1.4)
    }
    qqnorm(residuals_wls, main = "QQ plot reszt WMNK")
    qqline(residuals_wls, col = "red")
    plot(fitted_wls, model_wls_data$sqrt_abs_resid, pch = 16, cex = 0.7, col = grDevices::adjustcolor("black", 0.65), main = "Scale-location", xlab = "wartości dopasowane", ylab = "sqrt(abs(reszt))")
    if (length(fitted_wls) > 1) {
      lines(lowess(fitted_wls, model_wls_data$sqrt_abs_resid, f = 0.25), col = "green", lwd = 1.4)
    }
  })

  # Laczymy wykresy reszt wzgledem regresorow w jedna plansze.
  plot_cols <- c(x_cols[!x_cols %in% c("Miejska", "Wiejska")], intersect(c("Miejska", "Wiejska"), x_cols))
  save_plot_device("reszty_vs_zmienne_4x3_final", width = 15, height = 18, dpi = 300, code = function() {
    old_par <- par(mfrow = c(4, 3))
    on.exit(par(old_par), add = TRUE)
    for (idx in seq_len(12)) {
      if (idx <= length(plot_cols)) {
        column <- plot_cols[[idx]]
        label <- polish_display_label(column)
        plot(model_wls_data[[column]], residuals_wls, pch = 16, cex = 0.6, col = grDevices::adjustcolor("black", 0.6), xlab = label, ylab = "reszty", main = paste("Reszty vs", label))
        abline(h = 0, lwd = 0.9)
      } else {
        plot.new()
      }
    }
  })

  for (column in x_cols) {
    save_plot_device(paste0("reszty_vs_zmienna_", safe_plot_name(column)), width = 7, height = 5, dpi = 300, code = function() {
      label <- polish_display_label(column)
      plot(model_wls_data[[column]], residuals_wls, pch = 16, cex = 0.7, col = grDevices::adjustcolor("black", 0.65), xlab = label, ylab = "reszty", main = paste("Reszty WMNK vs", label))
      abline(h = 0, lwd = 1)
    })
  }

  save_plot_device("reszty_vs_wartosci_dopasowane_final", width = 7, height = 5, dpi = 300, code = function() {
    plot(fitted_wls, residuals_wls, pch = 16, cex = 0.7, col = grDevices::adjustcolor("black", 0.65), xlab = "wartości dopasowane", ylab = "reszty", main = "Reszty vs wartości dopasowane")
    abline(h = 0, lwd = 1)
    if (length(fitted_wls) > 1) {
      lines(lowess(fitted_wls, residuals_wls, f = 0.25), col = "green", lwd = 1.4)
    }
  })

  save_plot_device("scale_location_final", width = 7, height = 5, dpi = 300, code = function() {
    plot(fitted_wls, model_wls_data$sqrt_abs_resid, pch = 16, cex = 0.7, col = grDevices::adjustcolor("black", 0.65), xlab = "wartości dopasowane", ylab = "sqrt(abs(reszt))", main = "Scale-location")
    if (length(fitted_wls) > 1) {
      lines(lowess(fitted_wls, model_wls_data$sqrt_abs_resid, f = 0.25), col = "green", lwd = 1.4)
    }
  })

  save_plot_device("histogram_reszt_final", width = 7, height = 5, dpi = 300, code = function() {
    hist(residuals_wls, breaks = 30, probability = TRUE, col = "gray80", border = "black", main = "Histogram reszt WMNK", xlab = "reszty")
    if (stats::sd(residuals_wls) > 0) {
      x_grid <- seq(min(residuals_wls), max(residuals_wls), length.out = 200)
      lines(x_grid, dnorm(x_grid, mean(residuals_wls), stats::sd(residuals_wls)), lwd = 1.2)
    }
  })

  save_plot_device("qq_plot_final", width = 7, height = 5, dpi = 300, code = function() {
    qqnorm(residuals_wls, main = "QQ plot reszt WMNK")
    qqline(residuals_wls, col = "red")
  })

}

save_final_wmnk_stargazer <- function(fit, x_cols, initial_ols) {
  # Zapisuje zestawy modeli do tabel Stargazer.
  order <- c("const", x_cols)
  save_stargazer_models(
    list(fit$ols, fit$wls),
    "modele_ols_podstawowe",
    covariate_order = order,
    title = "Model 2 OLS i finalny WMNK bez usuwania obserwacji"
  )
  save_stargazer_models(
    list(fit$variance_model),
    "model_funkcji_wariancji",
    title = "Funkcja wariancji dla finalnego WMNK bez drog"
  )
}

run_chow_test_for_final_model <- function(fit, x_cols) {
  # Liczy test Chowa dla finalnej proby modelu bez usuwania obserwacji.
  model_data <- fit$model_data
  model_data$Kod <- as.character(model_data$Kod)
  chow_cols <- x_cols[!x_cols %in% c("Miejska", "Wiejska")]
  chow_data <- drop_na_cols(model_data, c("Kod", "ySaldo", chow_cols))

  groups <- list(
    wiejskie = chow_data[endsWith(chow_data$Kod, "2"), , drop = FALSE],
    miejsko_wiejskie = chow_data[endsWith(chow_data$Kod, "3"), , drop = FALSE],
    miejskie = chow_data[endsWith(chow_data$Kod, "1"), , drop = FALSE]
  )
  model_full <- fit_lm_for_columns(chow_data, chow_cols)
  group_models <- list()
  for (name in names(groups)) {
    if (nrow(groups[[name]]) > 0) {
      group_models[[name]] <- fit_lm_for_columns(groups[[name]], chow_cols)
    }
  }

  rss_full <- sum(residuals(model_full)^2)
  rss_groups <- sum(vapply(group_models, function(model) sum(residuals(model)^2), numeric(1)))
  n_groups <- sum(vapply(group_models, function(model) length(residuals(model)), numeric(1)))
  group_count <- length(group_models)
  k <- length(coef(model_full))
  df_num <- k * (group_count - 1)
  df_den <- n_groups - group_count * k
  f_chow <- ((rss_full - rss_groups) / df_num) / (rss_groups / df_den)
  p_value <- pf(f_chow, df_num, df_den, lower.tail = FALSE)
  result <- data.frame(
    F_chow = f_chow,
    p_value = p_value,
    df_num = as.integer(df_num),
    df_den = as.integer(df_den),
    n_full = as.integer(length(residuals(model_full))),
    n_wiejskie = ifelse("wiejskie" %in% names(group_models), as.integer(length(residuals(group_models$wiejskie))), 0L),
    n_miejsko_wiejskie = ifelse("miejsko_wiejskie" %in% names(group_models), as.integer(length(residuals(group_models$miejsko_wiejskie))), 0L),
    n_miejskie = ifelse("miejskie" %in% names(group_models), as.integer(length(residuals(group_models$miejskie))), 0L)
  )
  write.csv(result, r_output_path(wydruki_dir, "WMNKtestyzmiennych_chow_test.csv"), row.names = FALSE, fileEncoding = "UTF-8")

  model_order <- c(list(model_full), group_models[intersect(c("wiejskie", "miejsko_wiejskie", "miejskie"), names(group_models))])
  save_stargazer_models(
    model_order,
    "modele_chow",
    covariate_order = c("const", chow_cols),
    title = "Modele do testu Chowa dla finalnego modelu WMNK"
  )
  list(result = as.list(result[1, , drop = FALSE]), models = model_order, x_cols = chow_cols)
}

print_named_test <- function(test_names, values) {
  # Wypisuje liste par nazwa-wartosc podobna do statsmodels.compat.lzip.
  print(data.frame(name = test_names, value = as.numeric(values), row.names = NULL))
}

print_final_wmnk_diagnostics <- function(data, fit, x_cols, model_row, coef_df, vif_df, initial_ols = NULL) {
  # Wypisuje na konsole statystyki, testy i wyniki hipotez.
  model_data <- fit$model_data
  ols <- fit$ols
  variance_model <- fit$variance_model
  wls <- fit$wls
  diagnostic_cols <- c("ySaldo", x_cols)
  bp_white_names <- c("lagrange multiplier statistic", "p-value", "f-value", "f p-value")
  jb_names <- c("The Jarque-Bera test statistic", "p-value", "skewness", "kurtosis")

  cat("Braki danych\n")
  print(colSums(is.na(data[, diagnostic_cols, drop = FALSE])))
  cat("Liczba obserwacji wykorzystanych w modelu to:", nrow(model_data), "\n")
  cat("średnia\n")
  print(colMeans(model_data[, diagnostic_cols, drop = FALSE], na.rm = TRUE))
  cat("odchylenie standardowe\n")
  print(vapply(model_data[, diagnostic_cols, drop = FALSE], stats::sd, numeric(1), na.rm = TRUE))
  cat("min\n")
  print(vapply(model_data[, diagnostic_cols, drop = FALSE], min, numeric(1), na.rm = TRUE))
  cat("max\n")
  print(vapply(model_data[, diagnostic_cols, drop = FALSE], max, numeric(1), na.rm = TRUE))

  if (!is.null(initial_ols)) {
    cat("\nMODEL OLS POCZATKOWY\n")
    print(summary(initial_ols))
    cat("\nMODEL OLS BEZ logPrzystankiNa1000\n")
  } else {
    cat("\nMODEL OLS POCZATKOWY\n")
  }
  print(summary(ols))
  tryCatch(
    {
      reset_ols <- reset_test(ols, power = 3)
      cat("\n TEST RESET (fitted) \n")
      cat("p-value:", reset_ols$pvalue, "\n")
    },
    error = function(exc) cat("\n TEST RESET (fitted) pominiety:", conditionMessage(exc), "\n")
  )

  cat("Test Breusha-Pagana:\n")
  print_named_test(bp_white_names, bp_test(residuals(ols), model.matrix(ols)))
  cat("Test White'a:\n")
  print_named_test(bp_white_names, safe_white(residuals(ols), model.matrix(ols)))
  cat("ZMIENNE HETEROSKEDASTYCZNOSC\n")
  print(summary(variance_model))
  cat("Model z macierzą odporną White'a HC0\n")
  if (requireNamespace("sandwich", quietly = TRUE) && requireNamespace("lmtest", quietly = TRUE)) {
    print(lmtest::coeftest(ols, vcov. = sandwich::vcovHC(ols, type = "HC0")))
  } else {
    cat("Pakiety sandwich/lmtest niedostepne.\n")
  }
  cat("Test Jarque-Bera\n")
  print_named_test(jb_names, jarque_bera_test(residuals(ols)))
  cat("Test na łączną nieistotność\n")
  cat("F-stat:", unname(summary(ols)$fstatistic["value"]), "p-value:",
      pf(summary(ols)$fstatistic["value"], summary(ols)$fstatistic["numdf"], summary(ols)$fstatistic["dendf"], lower.tail = FALSE), "\n")
  cat("Zmienne łącznie istotne\n")
  cat(" VIF \n")
  names(vif_df)[names(vif_df) == "vif"] <- "VIF"
  print(vif_df[order(-vif_df$VIF), , drop = FALSE], row.names = FALSE)

  cat("\nFINALNY MODEL WMNK\n")
  print(summary(wls))
  for (name in names(coef(wls))) {
    cat(sprintf("%s: %.20f\n", display_coef_name(name), coef(wls)[[name]]))
  }

  tryCatch(
    {
      reset_wls <- reset_test(wls, power = 3)
      cat("\n TEST RESET (fitted) best \n")
      cat("p-value:", reset_wls$pvalue, "\n")
    },
    error = function(exc) cat("\n TEST RESET (fitted) best pominiety:", conditionMessage(exc), "\n")
  )

  cat("Test Breusha-Pagana best:\n")
  print_named_test(bp_white_names, fit$bp)
  cat("Test White'a best:\n")
  print_named_test(bp_white_names, fit$white)
  cat("Test Jarque-Bera best\n")
  print_named_test(jb_names, jarque_bera_test(residuals(wls)))
  cat("Test na łączną nieistotność 1\n")
  cat("F-stat best:", unname(summary(wls)$fstatistic["value"]), "p-value:",
      pf(summary(wls)$fstatistic["value"], summary(wls)$fstatistic["numdf"], summary(wls)$fstatistic["dendf"], lower.tail = FALSE), "\n")
  cat("Zmienne łącznie istotne\n")

  # Testuje hipotezy na finalnej specyfikacji bez drog.
  cat("\n=== H2: Test Wiejska > 0 ===\n")
  if ("Wiejska" %in% names(coef(wls))) {
    beta_wiejska <- coef_value(wls, "Wiejska")
    se_wiejska <- coef_value(wls, "Wiejska", "std_error")
    t_stat_wiejska <- beta_wiejska / se_wiejska
    p_value_wiejska <- 1 - pt(t_stat_wiejska, df = df.residual(wls))
    cat(sprintf("Współczynnik Wiejska: %.6f\n", beta_wiejska))
    cat(sprintf("Błąd standardowy: %.6f\n", se_wiejska))
    cat(sprintf("t-statystyka: %.4f\n", t_stat_wiejska))
    cat(sprintf("p-value (test jednostronny >0): %.6f\n", p_value_wiejska))
    cat(ifelse(p_value_wiejska < 0.05, "Hipoteza H2 potwierdzona: Wiejska > 0 (istotnie)", "Hipoteza H2 NIE potwierdzona"), "\n")
  }

  cat("\n=== H3: Test logWynagrodzenia istotne i > 0 ===\n")
  if ("logWynagrodzenia" %in% names(coef(wls))) {
    beta_wyn <- coef_value(wls, "logWynagrodzenia")
    se_wyn <- coef_value(wls, "logWynagrodzenia", "std_error")
    t_stat_wyn <- beta_wyn / se_wyn
    p_value_wyn <- coef_value(wls, "logWynagrodzenia", "p_value")
    p_value_wyn_one_sided <- 1 - pt(t_stat_wyn, df = df.residual(wls))
    cat(sprintf("Współczynnik logWynagrodzenia: %.6f\n", beta_wyn))
    cat(sprintf("Błąd standardowy: %.6f\n", se_wyn))
    cat(sprintf("t-statystyka: %.4f\n", t_stat_wyn))
    cat(sprintf("p-value (test dwustronny): %.6f\n", p_value_wyn))
    cat(sprintf("p-value (test jednostronny >0): %.6f\n", p_value_wyn_one_sided))
    cat(ifelse(p_value_wyn_one_sided < 0.05, "Hipoteza H3 potwierdzona: logWynagrodzenia > 0 (istotnie)", "Hipoteza H3 NIE potwierdzona"), "\n")
    cat("\nInterpretacja: wzrost wynagrodzeń o 1% powoduje wzrost salda migracji\n")
    cat(sprintf("o %.6f na 1000 ludności\n", beta_wyn / 100))
    cat(sprintf("Wzrost wynagrodzeń o 10%% -> wzrost salda o %.4f na 1000 ludności\n", beta_wyn * 10 / 100))
  }

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("TESTOWANIE HIPOTEZY H1 (ZŁOŻONEJ)\n")
  cat(strrep("=", 60), "\n", sep = "")
  if (all(c("Odleglosc", "Odleglosc2") %in% names(coef(wls)))) {
    beta_odl <- coef_value(wls, "Odleglosc")
    beta_odl2 <- coef_value(wls, "Odleglosc2")
    odl_mean <- mean(model_data$Odleglosc)
    marginal_effect <- beta_odl + 2 * beta_odl2 * odl_mean
    marginal_effects <- beta_odl + 2 * beta_odl2 * model_data$Odleglosc
    pct_negative <- mean(marginal_effects < 0) * 100
    cat("\n=== H1a: Wpływ odległości jest ujemny ===\n")
    cat(sprintf("Średnia odległość: %.2f km\n", odl_mean))
    cat(sprintf("Efekt krańcowy w średniej odległości: %.6f\n", marginal_effect))
    cat(sprintf("Procent obserwacji z ujemnym efektem krańcowym: %.2f%%\n", pct_negative))
    h1a_confirmed <- marginal_effect < 0 && pct_negative > 50
    cat(ifelse(h1a_confirmed, "H1a potwierdzona: wpływ odległości jest ujemny (w średniej)", "H1a NIE potwierdzona"), "\n")

    cat("\n=== H1b: Wpływ odległości jest nieliniowy ===\n")
    f_test_nonlinear <- f_test_single_zero(wls, "Odleglosc2")
    cat("Test F dla nieliniowosci (^2):\n")
    cat(sprintf("F-statystyka: %.4f\n", f_test_nonlinear$fvalue))
    cat(sprintf("p-value: %.6f\n", f_test_nonlinear$pvalue))
    h1b_confirmed <- f_test_nonlinear$pvalue < 0.05
    cat(ifelse(h1b_confirmed, "H1b potwierdzona: Zaleznosc jest nieliniowa (istotnie)", "H1b NIE potwierdzona"), "\n")

    cat("\n", strrep("=", 60), "\n", sep = "")
    cat("PODSUMOWANIE HIPOTEZY H1 (ZŁOŻONEJ)\n")
    cat(strrep("=", 60), "\n", sep = "")
    cat("\nH1a: Wpływ odległości jest UJEMNY\n")
    cat(sprintf("  - Efekt krańcowy w średniej odległości: %.6f\n", marginal_effect))
    cat(sprintf("  - Procent obserwacji z ujemnym efektem: %.2f%%\n", pct_negative))
    cat(sprintf("  - Wniosek: %s\n", ifelse(h1a_confirmed, "POTWIERDZONA", "ODRZUCONA")))
    cat("\nH1b: Wpływ odległości jest NIELINIOWY\n")
    cat(sprintf("  - F-test (^2 = 0): p-value = %.6f\n", f_test_nonlinear$pvalue))
    cat(sprintf("  - Wniosek: %s\n", ifelse(h1b_confirmed, "POTWIERDZONA", "ODRZUCONA")))
    cat("\n", strrep("-", 60), "\n", sep = "")
    cat("H1 (ŁĄCZNIE): Wpływ odległości jest UJEMNY i NIELINIOWY\n")
    cat(sprintf("Wniosek koncowy: %s\n", ifelse(h1a_confirmed && h1b_confirmed, "HIPOTEZA H1 POTWIERDZONA", "HIPOTEZA H1 ODRZUCONA")))
    cat(strrep("=", 60), "\n", sep = "")
  }

  cat("\nPODSUMOWANIE FINALNE\n")
  cat(df_to_string(model_row), "\n")
  cat("\nWspółczynniki:\n")
  cat(df_to_string(coef_df), "\n")
}

save_descriptive_statistics_tex <- function(data, x_cols) {
  # Zapisuje statystyki opisowe do tabeli LaTeX.
  diagnostic_cols <- c("ySaldo", "logPrzystankiNa1000", x_cols)
  rows <- lapply(diagnostic_cols, function(column) {
    series <- data[[column]]
    row <- data.frame(
      Zmienna = column,
      Min = min(series, na.rm = TRUE),
      `Średnia` = mean(series, na.rm = TRUE),
      Max = max(series, na.rm = TRUE),
      `Odchylenie standardowe` = stats::sd(series, na.rm = TRUE),
      NA_count = sum(is.na(series)),
      check.names = FALSE
    )
    names(row)[names(row) == "NA_count"] <- "NA"
    row
  })
  stats_df <- do.call(rbind, rows)
  # Zawijamy dlugi naglowek, zeby tabela statystyk opisowych miescila sie w pracy.
  descriptive_latex <- dataframe_to_latex_table(stats_df, "Statystyki opisowe zmiennych", column_format = "lrrrrr")
  descriptive_latex <- gsub(
    "Odchylenie standardowe",
    "\\shortstack{Odchylenie\\\\standardowe}",
    descriptive_latex,
    fixed = TRUE
  )
  save_overleaf_table(
    "statystyki_opisowe_zmiennych_przed_MNK",
    descriptive_latex
  )
}

save_wmnk_diagnostic_tests_tex <- function(fit) {
  # Zapisuje testy diagnostyczne do tabeli LaTeX.
  scalar_stat <- function(value) {
    # Sprowadza skalar lub jednoelementowa tablice statystyki do liczby.
    tryCatch(as.numeric(value)[1], error = function(exc) NA_real_)
  }

  format_stat <- function(prefix, value) {
    # Formatuje statystyke testowa z przecinkiem dziesietnym i symbolem testu.
    paste0("$", prefix, " = ", gsub(".", ",", sprintf("%.4f", as.numeric(value)), fixed = TRUE), "$")
  }
  format_pvalue <- function(value) {
    # Formatuje p-value zgodnie z zapisem w tabelach pracy.
    value <- as.numeric(value)
    if (is.na(value)) {
      return("")
    }
    if (value < 0.001) {
      return("$< 0,001$")
    }
    paste0("$", gsub(".", ",", sprintf("%.3f", value), fixed = TRUE), "$")
  }
  test_decision <- function(pvalue) {
    # Wpisuje decyzje testowa dla poziomu istotnosci 5%.
    ifelse(as.numeric(pvalue) < 0.05, "Odrzucamy $H_0$", "Brak podstaw do odrzucenia $H_0$")
  }
  make_test_row <- function(model, test, hypothesis, stat_prefix, stat_value, pvalue) {
    # Buduje pojedynczy wiersz tabeli w ukladzie zgodnym z czescia opisowa pracy.
    paste(
      model,
      test,
      hypothesis,
      format_stat(stat_prefix, stat_value),
      format_pvalue(pvalue),
      test_decision(pvalue),
      sep = " & "
    )
  }

  model2_bp <- bp_test(residuals(fit$ols), model.matrix(fit$ols))
  model2_jb <- jarque_bera_test(residuals(fit$ols))
  model2_reset <- tryCatch(reset_test(fit$ols, power = 3), error = function(exc) list(fvalue = NA_real_, pvalue = NA_real_))
  wmnk_jb <- jarque_bera_test(residuals(fit$wls))
  wmnk_reset <- tryCatch(reset_test(fit$wls, power = 3), error = function(exc) list(fvalue = NA_real_, pvalue = NA_real_))

  rows <- c(
    make_test_row("Model 2", "RESET fitted", "Poprawna forma funkcyjna", "F", scalar_stat(model2_reset$fvalue), model2_reset$pvalue),
    make_test_row("Model 2", "Breuscha-Pagana", "Homoskedastyczno\u015b\u0107 reszt", "LM", as.numeric(model2_bp[1]), as.numeric(model2_bp[2])),
    make_test_row("Model 2", "Jarque-Bera", "Normalno\u015b\u0107 reszt", "JB", as.numeric(model2_jb[1]), as.numeric(model2_jb[2])),
    "\\midrule",
    make_test_row("Model WMNK", "RESET fitted", "Poprawna forma funkcyjna", "F", scalar_stat(wmnk_reset$fvalue), wmnk_reset$pvalue),
    make_test_row("Model WMNK", "Jarque-Bera", "Normalno\u015b\u0107 reszt", "JB", as.numeric(wmnk_jb[1]), as.numeric(wmnk_jb[2]))
  )
  row_lines <- ifelse(rows == "\\midrule", rows, paste0(rows, " \\\\"))
  diagnostic_latex <- c(
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{Testy diagnostyczne modelu 2 oraz finalnego modelu WMNK}",
    "\\begin{tabular}{lllrrl}",
    "\\toprule",
    "Model & Test & Hipoteza zerowa & Statystyka & $p$-value & Decyzja \\\\",
    "\\midrule",
    row_lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}",
    ""
  )
  save_overleaf_table(
    "testy_diagnostyczne_model2_wmnk",
    diagnostic_latex
  )
}

backward_elimination_wmnk_bez_drog <- function(data) {
  # Usuwa kolejno najmniej istotne zmienne z modelu WMNK.
  current_cols <- build_x_cols(include_roads = TRUE, g_variant = "base_log")
  step_rows <- list()
  coef_rows <- list()
  step <- 0

  repeat {
    fit <- fit_wls_for_columns(data, current_cols)
    wls <- fit$wls
    pvalues <- summary(wls)$coefficients[, "Pr(>|t|)"]
    pvalues_without_const <- pvalues[names(pvalues) != "(Intercept)"]
    max_pvalue <- max(pvalues_without_const)
    variable_to_remove <- names(which.max(pvalues_without_const))
    all_significant <- max_pvalue < 0.05

    step_rows[[length(step_rows) + 1]] <- data.frame(
      step = step,
      removed_variable = ifelse(all_significant, "", variable_to_remove),
      removed_variable_pvalue = ifelse(all_significant, NA_real_, max_pvalue),
      n_obs = nrow(fit$model_data),
      num_regressors = length(current_cols),
      regressors = paste(current_cols, collapse = ","),
      aic = statsmodels_aic(wls),
      bic = statsmodels_bic(wls),
      r2 = model_r2(wls),
      adj_r2 = model_adj_r2(wls),
      bp_pvalue = as.numeric(fit$bp["p-value"]),
      white_pvalue = as.numeric(fit$white["p-value"]),
      max_pvalue_without_const = max_pvalue,
      all_variables_significant_5pct = all_significant,
      stringsAsFactors = FALSE
    )
    coef_rows[[length(coef_rows) + 1]] <- model_coef_dataframe(wls, step = step)

    if (all_significant || length(current_cols) == 1) {
      return(list(steps = do.call(rbind, step_rows), coefficients = do.call(rbind, coef_rows), wls = wls))
    }
    current_cols <- current_cols[current_cols != variable_to_remove]
    step <- step + 1
  }
}

compare_housing_stock_and_new_flats <- function(data) {
  # Porownuje warianty zmiennych mieszkaniowych w tej samej specyfikacji.
  base_cols <- c(
    "logPrzystankiNa1000",
    "logWynagrodzenia",
    "M2NaOsobe",
    "Przychodnie",
    "Bezrobocie",
    "Miejska",
    "Wiejska",
    "Odleglosc",
    "Odleglosc2",
    "logWskaznikG"
  )
  housing_variants <- list(
    bez_mieszkan = character(),
    oddane_raw = "MieszkaniaOddane",
    oddane_log = "logMieszkaniaOddane"
  )
  rows <- list()
  coef_rows <- list()
  vif_rows <- list()
  for (variant_name in names(housing_variants)) {
    housing_cols <- housing_variants[[variant_name]]
    x_cols <- c(base_cols, housing_cols)
    fit <- fit_wls_for_columns(data, x_cols)
    wls <- fit$wls
    pvalues <- summary(wls)$coefficients[, "Pr(>|t|)"]
    pvalues_without_const <- pvalues[names(pvalues) != "(Intercept)"]
    rows[[length(rows) + 1]] <- data.frame(
      variant = variant_name,
      housing_variables = paste(housing_cols, collapse = ","),
      n_obs = nrow(fit$model_data),
      aic = statsmodels_aic(wls),
      bic = statsmodels_bic(wls),
      r2 = model_r2(wls),
      adj_r2 = model_adj_r2(wls),
      bp_pvalue = as.numeric(fit$bp["p-value"]),
      white_pvalue = as.numeric(fit$white["p-value"]),
      max_pvalue_without_const = max(pvalues_without_const),
      insignificant_variables_5pct = paste(names(pvalues_without_const)[pvalues_without_const >= 0.05], collapse = ","),
      stringsAsFactors = FALSE
    )
    coef_rows[[length(coef_rows) + 1]] <- model_coef_dataframe(wls, variant = variant_name)
    x_vif <- design_matrix_from_data(fit$model_data, x_cols)
    vif_values <- variance_inflation_factor_matrix(x_vif)
    vif_rows[[length(vif_rows) + 1]] <- data.frame(
      variant = variant_name,
      regressor = names(vif_values),
      vif = as.numeric(vif_values),
      n = nrow(fit$model_data),
      stringsAsFactors = FALSE
    )
  }
  comparison <- do.call(rbind, rows)
  comparison <- comparison[order(comparison$aic, comparison$bic), , drop = FALSE]
  row.names(comparison) <- NULL
  write.csv(comparison, r_output_path(wydruki_dir, "WMNK_mieszkania_oddane_zasob_porownanie.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_html_table(comparison, file.path(wydruki_dir, "WMNK_mieszkania_oddane_zasob_porownanie.html"))
  write.csv(do.call(rbind, coef_rows), r_output_path(wydruki_dir, "WMNK_mieszkania_oddane_zasob_wspolczynniki.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(do.call(rbind, vif_rows), r_output_path(wydruki_dir, "WMNK_mieszkania_oddane_zasob_vif.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  comparison
}

add_g_comparison_to_baseline <- function(comparison) {
  # Dodaje porownanie z modelem bez Wskaznika G.
  group_cols <- c("sample", "variable", "housing")
  baseline <- comparison[comparison$wskaznik_g == "none", c(group_cols, "wls_aic", "wls_bic", "wls_r2"), drop = FALSE]
  names(baseline)[names(baseline) == "wls_aic"] <- "baseline_wls_aic"
  names(baseline)[names(baseline) == "wls_bic"] <- "baseline_wls_bic"
  names(baseline)[names(baseline) == "wls_r2"] <- "baseline_wls_r2"
  comparison <- merge(comparison, baseline, by = group_cols, all.x = TRUE, sort = FALSE)
  comparison$delta_aic_vs_baseline <- comparison$wls_aic - comparison$baseline_wls_aic
  comparison$delta_bic_vs_baseline <- comparison$wls_bic - comparison$baseline_wls_bic
  comparison$delta_r2_vs_baseline <- comparison$wls_r2 - comparison$baseline_wls_r2
  comparison
}

add_housing_comparison_to_baseline <- function(comparison) {
  # Dodaje porownanie z modelem bez dodatkowej zmiennej mieszkaniowej.
  group_cols <- c("sample", "variable", "wskaznik_g")
  baseline <- comparison[comparison$housing == "none", c(group_cols, "wls_aic", "wls_bic", "wls_r2", "wls_adj_r2"), drop = FALSE]
  names(baseline)[names(baseline) == "wls_aic"] <- "baseline_no_housing_wls_aic"
  names(baseline)[names(baseline) == "wls_bic"] <- "baseline_no_housing_wls_bic"
  names(baseline)[names(baseline) == "wls_r2"] <- "baseline_no_housing_wls_r2"
  names(baseline)[names(baseline) == "wls_adj_r2"] <- "baseline_no_housing_wls_adj_r2"
  comparison <- merge(comparison, baseline, by = group_cols, all.x = TRUE, sort = FALSE)
  comparison$delta_aic_vs_no_housing <- comparison$wls_aic - comparison$baseline_no_housing_wls_aic
  comparison$delta_bic_vs_no_housing <- comparison$wls_bic - comparison$baseline_no_housing_wls_bic
  comparison$delta_r2_vs_no_housing <- comparison$wls_r2 - comparison$baseline_no_housing_wls_r2
  comparison$delta_adj_r2_vs_no_housing <- comparison$wls_adj_r2 - comparison$baseline_no_housing_wls_adj_r2
  comparison
}

build_x_cols <- function(include_roads, g_variant = "base_log", housing_variant = "base_m2") {
  # Buduje wspolna liste regresorow dla estymacji i VIF.
  x_cols <- c(
    "logPrzystankiNa1000",
    "logWynagrodzenia",
    "M2NaOsobe",
    "logWskaznikG",
    "logMieszkaniaNa1000",
    "MieszkaniaOddane",
    "Przychodnie",
    "Bezrobocie",
    "Miejska",
    "Wiejska",
    "Odleglosc",
    "Odleglosc2"
  )
  if (!g_variant %in% c("base_log", "log", "none")) {
    stop(sprintf("Nieznany wariant Wskaznika G: %s", g_variant), call. = FALSE)
  }
  if (!housing_variant %in% c("base_m2", "none")) {
    stop(sprintf("Nieznany wariant zasobow mieszkaniowych: %s", housing_variant), call. = FALSE)
  }
  x_cols
}

save_correlation_and_vif <- function(data, base_variants) {
  # Zapisuje korelacje i VIF dla porownywanych specyfikacji.
  correlation_cols <- c(
    "ySaldo",
    "logPrzystankiNa1000",
    "logWynagrodzenia",
    "logMieszkaniaNa1000",
    "Przychodnie",
    "Bezrobocie",
    "Miejska",
    "Wiejska",
    "Odleglosc",
    "Odleglosc2",
    "WskaznikG",
    "logWskaznikG",
    "M2NaOsobe"
  )
  corr_data <- drop_na_cols(data, correlation_cols)
  for (corr_method in c("pearson", "spearman")) {
    corr_df <- as.data.frame(cor(corr_data, method = corr_method))
    write.csv(corr_df, r_output_path(wydruki_dir, paste0("WMNKtestyzmiennych_korelacje_", corr_method, ".csv")), fileEncoding = "UTF-8")
    write_html_table(corr_df, file.path(wydruki_dir, paste0("WMNKtestyzmiennych_korelacje_", corr_method, ".html")))
  }

  vif_rows <- list()
  for (variant in base_variants) {
    for (g_variant in c("base_log")) {
      for (housing_variant in c("base_m2")) {
        x_cols <- build_x_cols(variant$include_roads, g_variant, housing_variant)
        model_data <- drop_na_cols(variant$data, x_cols)
        x_vif <- design_matrix_from_data(model_data, x_cols)
        vif_values <- variance_inflation_factor_matrix(x_vif)
        vif_rows[[length(vif_rows) + 1]] <- data.frame(
          model = variant$model_name,
          sample = variant$sample_label,
          variable_group = "bez_drog",
          wskaznik_g = g_variant,
          housing = housing_variant,
          regressor = names(vif_values),
          vif = as.numeric(vif_values),
          n = nrow(model_data),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  vif_df <- do.call(rbind, vif_rows)
  write.csv(vif_df, r_output_path(wydruki_dir, "WMNKtestyzmiennych_vif.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_html_table(vif_df, file.path(wydruki_dir, "WMNKtestyzmiennych_vif.html"))
}

main <- function() {
  data <- load_model_data()
  # Przed estymacja tworzymy 10 imputacji MICE pelnego zbioru WMNK i zapisujemy je dla modeli przestrzennych.
  mice_bundle <- impute_model_missing_with_mice(data, return_all = TRUE, export_all = TRUE)
  data <- mice_bundle$single
  # Najpierw liczymy pelny OLS, a potem finalny WMNK bez przystankow.
  initial_model_data <- drop_na_cols(data, c("Kod", "ySaldo", initial_ols_cols))
  initial_ols <- fit_lm_for_columns(initial_model_data, initial_ols_cols)
  fit <- fit_wls_for_columns(data, final_wmnk_cols)
  wls <- fit$wls

  model_row <- data.frame(
    model = "WMNK_final_bez_logPrzystankiNa1000",
    regressors = paste(final_wmnk_cols, collapse = ","),
    n_obs = nrow(fit$model_data),
    aic = statsmodels_aic(wls),
    bic = statsmodels_bic(wls),
    r2 = model_r2(wls),
    adj_r2 = model_adj_r2(wls),
    bp_pvalue = as.numeric(fit$bp["p-value"]),
    white_pvalue = as.numeric(fit$white["p-value"]),
    stringsAsFactors = FALSE
  )
  coef_df <- model_coef_dataframe(wls)
  x_vif <- design_matrix_from_data(fit$model_data, final_wmnk_cols)
  vif_values <- variance_inflation_factor_matrix(x_vif)
  vif_df <- data.frame(
    regressor = names(vif_values),
    vif = as.numeric(vif_values),
    n = nrow(fit$model_data),
    stringsAsFactors = FALSE
  )

  save_descriptive_statistics_tex(data, final_wmnk_cols)
  save_wmnk_diagnostic_tests_tex(fit)

  save_stargazer_models(
    list(initial_ols, fit$ols),
    "modele_ols_podstawowy_i_bez_przystankow",
    covariate_order = c("const", initial_ols_cols),
    title = "Model OLS podstawowy i model bez logPrzystankiNa1000"
  )

  write.csv(model_row, r_output_path(wydruki_dir, "WMNK_final_bez_logPrzystankiNa1000.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_html_table(model_row, file.path(wydruki_dir, "WMNK_final_bez_logPrzystankiNa1000.html"))
  write.csv(coef_df, r_output_path(wydruki_dir, "WMNK_final_bez_logPrzystankiNa1000_wspolczynniki.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_html_table(coef_df, file.path(wydruki_dir, "WMNK_final_bez_logPrzystankiNa1000_wspolczynniki.html"))
  write.csv(vif_df, r_output_path(wydruki_dir, "WMNK_final_bez_logPrzystankiNa1000_vif.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  writeLines(model_summary_text(wls), r_output_path(wydruki_dir, "WMNK_final_bez_logPrzystankiNa1000_summary.txt"), useBytes = TRUE)
  writeLines(model_summary_html(wls), r_output_path(wydruki_dir, "WMNK_final_bez_logPrzystankiNa1000_summary.html"), useBytes = TRUE)

  save_ysaldo_heatmap(data)
  save_variable_heatmap_panel(data, c("ySaldo", final_wmnk_cols[final_wmnk_cols != "Odleglosc2"]))
  save_single_variable_heatmap(data, "logM2NaOsobe", "mapa_ciepla_logM2NaOsobe", "Mapa ciepła logM2NaOsobe")
  save_final_wmnk_residual_map(fit)
  save_final_wmnk_plots(data, fit, final_wmnk_cols)
  save_final_wmnk_stargazer(fit, final_wmnk_cols, initial_ols)
  # Pełna pula Rubina po m=10 imputacjach MICE (oprócz wyników z imputacji 1).
  save_pooled_final_wmnk_stargazer(mice_bundle, final_wmnk_cols)

  # Zapisuje trzy etapy estymacji do jednej tabeli LaTeX.
  save_stargazer_models(
    list(initial_ols, fit$ols, fit$wls),
    "WydrukiMNKfull",
    covariate_order = c("const", initial_ols_cols),
    title = "Pe\u0142ny OLS, OLS bez przystank\u00F3w i finalny WMNK"
  )
  chow <- run_chow_test_for_final_model(fit, final_wmnk_cols)
  print_final_wmnk_diagnostics(data, fit, final_wmnk_cols, model_row, coef_df, vif_df, initial_ols = initial_ols)
  cat(sprintf(
    "Test Chowa dla finalnego modelu WMNK: F=%.4f, p-value=%.6f, df=(%s; %s)\n",
    chow$result$F_chow,
    chow$result$p_value,
    chow$result$df_num,
    chow$result$df_den
  ))

  # Nadpisuje stare nazwy plikow wynikami modelu finalnego.
  model_row_old <- model_row
  names(model_row_old)[names(model_row_old) == "model"] <- "step"
  write.csv(model_row_old, r_output_path(wydruki_dir, "WMNK_bez_logPrzystankiNa1000_eliminacja_5pct.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  coef_df_with_step <- cbind(step = "final", coef_df)
  write.csv(coef_df_with_step, r_output_path(wydruki_dir, "WMNK_bez_logPrzystankiNa1000_eliminacja_5pct_wspolczynniki.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  writeLines(model_summary_text(wls), r_output_path(wydruki_dir, "WMNK_bez_logPrzystankiNa1000_final_5pct_summary.txt"), useBytes = TRUE)
  writeLines(model_summary_html(wls), r_output_path(wydruki_dir, "WMNK_bez_logPrzystankiNa1000_final_5pct_summary.html"), useBytes = TRUE)

  lines <- c(
    "FINALNY MODEL WMNK BEZ LOGPRZYSTANKINA1000",
    "",
    "Regresory:",
    paste(vapply(final_wmnk_cols, polish_display_label, character(1)), collapse = ", "),
    "",
    df_to_string(model_row),
    "",
    "Współczynniki:",
    df_to_string(coef_df_with_step)
  )
  writeLines(lines, r_output_path(wydruki_dir, "WMNKtestyzmiennych_porownanie.txt"), useBytes = TRUE)
  sync_r_outputs_to_overleaf()
  # Wypisuje koncowy raport tak samo, jak zapisuje go do pliku tekstowego.
  cat("\n")
  cat(paste(lines, collapse = "\n"))
  cat("\n")
  cat("\nZapisano wspolne imputacje MICE do:", mice_completed_bundle_path(), "\n")
  # Informujemy w konsoli, ze caly program zakonczyl obliczenia i eksporty.
  print("Koniec MICE!!")
}

if (sys.nframe() == 0) {
  main()
}

