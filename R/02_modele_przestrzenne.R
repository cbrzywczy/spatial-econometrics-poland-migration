# --- Kodowanie UTF-8 ---
Sys.setlocale("LC_CTYPE", "pl_PL.UTF-8")
options(encoding = "UTF-8")
options(scipen = 999, digits = 3)
Sys.setenv(LANG = "en")

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
data_dir <- file.path(project_dir, "data")

wydruki_dir <- file.path(base_dir, "wydruki")
wykresy_dir <- file.path(base_dir, "wykresy")
spatial_r_wykresy_dir <- file.path(wykresy_dir, "spatial_R_bez_logDrogi")
stargazer_dir <- file.path(base_dir, "stargazer", "wyborprzest_bez_logDrogi")
stargazer_tex_dir <- file.path(stargazer_dir, "tex")
tabele_dir <- file.path(base_dir, "tabele", "wyborprzest_bez_logDrogi")
overleaf_dir <- file.path(base_dir, "Overleaf")
overleaf_media_dir <- file.path(overleaf_dir, "media")
overleaf_tabele_dir <- file.path(overleaf_dir, "tabele")
overleaf_stargazer_dir <- file.path(overleaf_dir, "stargazer")

dir.create(wydruki_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(wykresy_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(spatial_r_wykresy_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(stargazer_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(stargazer_tex_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tabele_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(overleaf_media_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(overleaf_tabele_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(overleaf_stargazer_dir, showWarnings = FALSE, recursive = TRUE)
SAVE_PDF_PLOTS <- TRUE

# Wczytujemy liste plikow realnie uzywanych przez licm2/maintest.tex.
source(file.path(scripts_dir, "publish_helpers.R"), encoding = "UTF-8")
init_maintest_publisher(project_dir, overleaf_dir)

# Pakiety instalujemy lokalnie, zeby nie wymagac zmian w globalnej bibliotece R.
local_r_lib <- file.path(base_dir, "Rlib")
project_r_lib <- file.path(project_dir, "Rlib")
dir.create(local_r_lib, showWarnings = FALSE, recursive = TRUE)
dir.create(project_r_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(local_r_lib, project_r_lib, .libPaths()))

requiredPackages <- c(
  "sf",
  "spdep",
  "spatialreg",
  "dplyr",
  "stringr",
  "lmtest",
  "readxl",
  "stargazer",
  "mice"
)

# Wczytujemy pakiety potrzebne do modeli przestrzennych i zapisu tabel.
for (pkg in requiredPackages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, lib = local_r_lib, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

MICE_IMPUTE_COLS <- c("logMieszkaniaOddaneNa1000", "Przychodnie")
MICE_PREDICTOR_COLS <- unique(c(
  "ySaldo",
  "logPrzystankiNa1000",
  "logWskaznikG",
  "logWynagrodzenia",
  "M2NaOsobe",
  "logMieszkaniaOddaneNa1000",
  "Przychodnie",
  "Bezrobocie",
  "Miejska",
  "Wiejska",
  "logMieszkaniaNa1000",
  "mieszkania",
  "MieszkaniaOddaneNa1000",
  "Populacja"
))
MICE_M <- 10
MICE_MAXIT <- 2  # No chained dependency between impute targets, so 1-2 sweeps suffice.
MICE_SEED <- 20240517
MICE_COMPLETE_ACTION <- 1
MICE_REPORT_PREFIX <- "wyborprzest_bez_logDrogi_mice"
WMNK_MICE_SCRIPT <- file.path(scripts_dir, "01_mnk_wmnk_mice.R")
WMNK_MICE_BUNDLE_PATH <- file.path(base_dir, "mice", "WMNKmice_completed_datasets.rds")

load_wmnk_mice_bundle <- function() {
  # Wczytuje wspolne 10 imputacji z WMNKmice.R; gdy ich nie ma, tworzy je z pelnego zbioru WMNK.
  if (file.exists(WMNK_MICE_BUNDLE_PATH)) {
    return(readRDS(WMNK_MICE_BUNDLE_PATH))
  }
  wmnk_env <- new.env(parent = globalenv())
  sys.source(WMNK_MICE_SCRIPT, envir = wmnk_env)
  raw_data <- wmnk_env$load_model_data()
  wmnk_env$impute_model_missing_with_mice(raw_data, return_all = TRUE, export_all = TRUE)
}

prepare_mice_model_data <- function(bundle, model_cols) {
  # Ustala wspolna probe dla wszystkich imputacji bez zmiany kolejnosci jednostek w obrebie imputacji.
  completed <- bundle$completed_datasets
  if (is.null(completed) || length(completed) == 0) {
    stop("Brak listy completed_datasets w artefakcie WMNKmice.", call. = FALSE)
  }
  missing_cols <- setdiff(model_cols, names(completed[[1]]))
  if (length(missing_cols) > 0) {
    stop(sprintf("W imputowanym zbiorze WMNK brakuje kolumn: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  complete_codes <- lapply(completed, function(d) {
    d$Kod[complete.cases(d[, model_cols, drop = FALSE])]
  })
  common_codes <- Reduce(intersect, complete_codes)
  lapply(completed, function(d) {
    current <- d[d$Kod %in% common_codes, model_cols, drop = FALSE]
    current[match(common_codes, current$Kod), , drop = FALSE]
  })
}

is_usable_mice_predictor <- function(column) {
  # Sprawdza, czy predyktor ma wystarczajaco informacji dla modelu imputacji.
  observed <- column[!is.na(column)]
  length(observed) > 1 && length(unique(observed)) > 1
}

save_mice_imputation_report <- function(report, imputed_values, imputations = NULL) {
  # Zapisuje raport MICE, zeby mozna bylo opisac uzupelnienie brakow w pracy.
  write.csv(
    report,
    file.path(wydruki_dir, paste0(MICE_REPORT_PREFIX, "_imputacja_brakow.csv")),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  if (length(imputed_values) > 0) {
    write.csv(
      do.call(rbind, imputed_values),
      file.path(wydruki_dir, paste0(MICE_REPORT_PREFIX, "_imputowane_wartosci.csv")),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }
  logged_events <- imputations$loggedEvents
  if (!is.null(logged_events) && nrow(logged_events) > 0) {
    write.csv(
      logged_events,
      file.path(wydruki_dir, paste0(MICE_REPORT_PREFIX, "_logged_events.csv")),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }
}

impute_model_missing_with_mice <- function(data) {
  # Uzupelnia MICE tylko te zmienne, ktore ograniczaja probe modeli bez logDrogi.
  missing_required <- setdiff(MICE_IMPUTE_COLS, names(data))
  if (length(missing_required) > 0) {
    stop(sprintf("Brakuje kolumn do imputacji MICE: %s", paste(missing_required, collapse = ", ")))
  }

  missing_before <- colSums(is.na(data[, MICE_IMPUTE_COLS, drop = FALSE]))
  target_cols <- names(missing_before)[missing_before > 0]
  if (length(target_cols) == 0) {
    empty_report <- data.frame(
      variable = MICE_IMPUTE_COLS,
      missing_before = as.integer(missing_before),
      missing_after = as.integer(missing_before),
      imputed = 0L,
      method = "",
      m = MICE_M,
      maxit = MICE_MAXIT
    )
    save_mice_imputation_report(empty_report, list())
    return(data)
  }

  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  predictor_cols <- unique(c(MICE_IMPUTE_COLS, intersect(MICE_PREDICTOR_COLS, numeric_cols)))
  bad_targets <- target_cols[vapply(data[, target_cols, drop = FALSE], function(column) {
    sum(!is.na(column)) < 2
  }, logical(1))]
  if (length(bad_targets) > 0) {
    stop(sprintf("Za malo obserwacji do imputacji MICE dla: %s", paste(bad_targets, collapse = ", ")))
  }

  usable_predictors <- predictor_cols[vapply(data[, predictor_cols, drop = FALSE], function(column) {
    !any(is.na(column)) && is_usable_mice_predictor(column)
  }, logical(1))]
  predictor_cols <- unique(c(MICE_IMPUTE_COLS, usable_predictors))
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
    m = MICE_M,
    maxit = MICE_MAXIT,
    method = method,
    predictorMatrix = predictor_matrix,
    printFlag = FALSE,
    seed = MICE_SEED
  )
  completed <- mice::complete(imputations, action = MICE_COMPLETE_ACTION)

  imputed_values <- list()
  for (target_col in target_cols) {
    missing_idx <- which(is.na(data[[target_col]]))
    data[missing_idx, target_col] <- completed[missing_idx, target_col]
    imputed_values[[target_col]] <- data.frame(
      Kod = data$Kod[missing_idx],
      variable = target_col,
      imputed_value = data[missing_idx, target_col]
    )
  }

  missing_after <- colSums(is.na(data[, MICE_IMPUTE_COLS, drop = FALSE]))
  report <- data.frame(
    variable = MICE_IMPUTE_COLS,
    missing_before = as.integer(missing_before[MICE_IMPUTE_COLS]),
    missing_after = as.integer(missing_after[MICE_IMPUTE_COLS]),
    imputed = as.integer(missing_before[MICE_IMPUTE_COLS] - missing_after[MICE_IMPUTE_COLS]),
    method = ifelse(MICE_IMPUTE_COLS %in% target_cols, method[MICE_IMPUTE_COLS], ""),
    m = MICE_M,
    maxit = MICE_MAXIT
  )
  save_mice_imputation_report(report, imputed_values, imputations)

  if (any(missing_after[target_cols] > 0)) {
    stop("Imputacja MICE nie uzupelnila wszystkich brakow w zmiennych modelowych.")
  }
  data
}

norm_code <- function(x) {
  # Normalizuje kody TERYT do siedmiu cyfr.
  x <- as.character(x)
  x <- gsub("\\.0$", "", x)
  stringr::str_pad(trimws(x), width = 7, side = "left", pad = "0")
}
read_gus <- function(name) {
  # Wczytuje pliki GUS z separatorem srednikiem.
  read.csv2(file.path(data_dir, name), check.names = FALSE, stringsAsFactors = FALSE)
}

latex_escape <- function(x) {
  # Zabezpiecza znaki specjalne przed bledami w LaTeX-u.
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_%&#])", "\\\\\\1", x, perl = TRUE)
  x
}

format_tex_value <- function(x) {
  # Formatuje wartosci przed wpisaniem ich do tabeli LaTeX.
  format_one <- function(value, column_name = "") {
    if (length(value) == 0 || is.na(value)) {
      return("")
    }
    if (is.logical(value)) {
      return(ifelse(value, "tak", "nie"))
    }
    if (is.numeric(value)) {
      if (!is.finite(value)) {
        return(as.character(value))
      }
      if (is_p_value_column(column_name)) {
        return(ifelse(value < 0.001, "<0.001", formatC(value, format = "f", digits = 3, decimal.mark = ".")))
      }
      if (is_integer_like_column(column_name) && abs(value - round(value)) < .Machine$double.eps^0.5) {
        return(as.character(as.integer(round(value))))
      }
      return(formatC(value, format = "f", digits = 3, decimal.mark = ","))
    }
    latex_escape(gsub("_", " ", as.character(value), fixed = TRUE))
  }
  column_name <- attr(x, "column_name", exact = TRUE)
  if (is.null(column_name)) {
    column_name <- ""
  }
  vapply(x, format_one, character(1), column_name = column_name)
}

is_p_value_column <- function(column_name) {
  # Rozpoznaje kolumny p-value niezaleznie od zapisu technicznego.
  normalized <- gsub("[^a-z0-9]", "", tolower(as.character(column_name)))
  normalized %in% c("p", "pvalue") || grepl("pvalue$", normalized)
}

is_integer_like_column <- function(column_name) {
  # Kolumny licznikowe zostawiamy bez miejsc po przecinku.
  normalized <- gsub("[^a-z0-9]", "", tolower(as.character(column_name)))
  normalized %in% c("n", "na", "df", "deltadf", "liczbaobserwacji")
}

format_tex_column <- function(x, column_name) {
  # Przekazuje nazwe kolumny do formatowania wartosci.
  attr(x, "column_name") <- column_name
  format_tex_value(x)
}

polish_tex_column_name <- function(column_name) {
  # Zamienia techniczne nazwy kolumn na polskie etykiety bez podkreslen.
  mapping <- c(
    variable = "Zmienna",
    model = "Model",
    test = "Test",
    statistic = "Statystyka",
    method = "Metoda",
    parameter = "Parametr",
    p_value = "p-value",
    pvalue = "p-value",
    df = "df",
    logLik = "Log-likelihood",
    pseudo_R2 = "Pseudo R2",
    resid_moran_I = "I Morana reszt",
    resid_moran_p = "p-value reszt",
    restriction = "Ograniczenie",
    estimate = "Współczynnik",
    std_error = "Błąd standardowy",
    z_value = "Statystyka z",
    direct = "Bezpośredni",
    indirect = "Pośredni",
    total = "Całkowity",
    classification = "Klasyfikacja"
  )
  if (column_name %in% names(mapping)) {
    return(unname(mapping[[column_name]]))
  }
  normalized <- gsub("[^a-z0-9]", "", tolower(as.character(column_name)))
  if (normalized == "pvalue") {
    return("p-value")
  }
  label <- gsub("_", " ", as.character(column_name), fixed = TRUE)
  label <- gsub("p value", "p-value", label, fixed = TRUE)
  label
}

format_tex_dataframe <- function(df) {
  # Przygotowuje ramke danych do zapisu w LaTeX.
  formatted <- as.data.frame(Map(format_tex_column, df, names(df)), stringsAsFactors = FALSE, check.names = FALSE)
  names(formatted) <- vapply(names(df), polish_tex_column_name, character(1))
  formatted
}

format_p_value_tex <- function(value) {
  # Formatuje p-value zgodnie z wymogiem progu <0.001.
  ifelse(is.na(value), "", ifelse(value < 0.001, "<0.001", formatC(value, format = "f", digits = 3, decimal.mark = ".")))
}

format_number_tex <- function(value, digits = 3) {
  # Formatuje zwykle liczby z przecinkiem dziesietnym.
  ifelse(is.na(value), "", formatC(value, format = "f", digits = digits, decimal.mark = ","))
}

write_overleaf_table <- function(df, file_stub, caption, align = NULL) {
  # Zapisuje ramke danych jako tabele LaTeX w bezdrog i publikuje wybrane pliki do Overleaf.
  out_path <- file.path(tabele_dir, paste0(file_stub, ".tex"))
  df <- as.data.frame(df, check.names = FALSE)
  formatted <- format_tex_dataframe(df)
  if (is.null(align)) {
    align <- paste0("l", paste(rep("r", max(0, ncol(formatted) - 1)), collapse = ""))
  }
  lines <- c(
    "\\begin{table}[H]",
    "\\centering",
    paste0("\\caption{", gsub("Wspolczynniki", "Współczynniki", caption, fixed = TRUE), "}"),
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste(latex_escape(names(formatted)), collapse = " & "),
    "\\\\",
    "\\midrule"
  )
  body <- apply(formatted, 1, function(row) paste(row, collapse = " & "))
  lines <- c(lines, paste0(body, " \\\\"), "\\bottomrule", "\\end{tabular}", "\\end{table}", "")
  writeLines(lines, out_path, useBytes = TRUE)
  publish_if_maintest_uses(out_path, "tabele")
  invisible(out_path)
}

write_overleaf_stargazer_table <- function(df, file_stub, caption) {
  # Zapisuje prosta tabele modelowa w bezdrog/stargazer i publikuje wybrane pliki do Overleaf.
  # Uzywane przy tabelach wspolczynnikow, gdy chcemy miec pelna kontrole nad formatem LaTeX.
  out_path <- file.path(stargazer_tex_dir, paste0(file_stub, ".tex"))
  df <- as.data.frame(df, check.names = FALSE)
  formatted <- format_tex_dataframe(df)
  align <- paste0("l", paste(rep("r", max(0, ncol(formatted) - 1)), collapse = ""))
  lines <- c(
    "\\begin{table}[H]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste(latex_escape(names(formatted)), collapse = " & "),
    "\\\\",
    "\\midrule"
  )
  body <- apply(formatted, 1, function(row) paste(row, collapse = " & "))
  lines <- c(lines, paste0(body, " \\\\"), "\\bottomrule", "\\end{tabular}", "\\end{table}", "")
  writeLines(lines, out_path, useBytes = TRUE)
  publish_if_maintest_uses(out_path, "stargazer")
  invisible(out_path)
}

model_coef_table <- function(model) {
  # Pobiera wspolczynniki modelu przestrzennego do tabeli LaTeX.
  # Funkcja ujednolica nazwy kolumn z roznych klas modeli spatialreg.
  coef_matrix <- as.data.frame(coef(summary(model)))
  coef_matrix$Zmienna <- rownames(coef_matrix)
  coef_matrix$Zmienna <- ifelse(coef_matrix$Zmienna == "(Intercept)", "Wyraz wolny", coef_matrix$Zmienna)
  coef_matrix$Zmienna <- sub("^lag\\.", "WX ", coef_matrix$Zmienna)
  rownames(coef_matrix) <- NULL
  names(coef_matrix) <- sub("Pr\\(>\\|z\\|\\)", "p_value", names(coef_matrix))
  names(coef_matrix) <- sub("Estimate", "estimate", names(coef_matrix))
  names(coef_matrix) <- sub("Std. Error", "std_error", names(coef_matrix), fixed = TRUE)
  names(coef_matrix) <- sub("z value", "z_value", names(coef_matrix), fixed = TRUE)
  coef_matrix[, c("Zmienna", setdiff(names(coef_matrix), "Zmienna"))]
}

save_stargazer_or_summary <- function(models, file_stub, title) {
  # Zapisuje modele jako HTML i LaTeX; w razie bledu zapisuje zwykly tekst.
  # Dzieki temu skrypt zawsze zostawia czytelny wynik, nawet gdy stargazer nie obsluzy klasy modelu.
  html_path <- file.path(stargazer_dir, paste0(file_stub, ".html"))
  tex_path <- file.path(stargazer_tex_dir, paste0(file_stub, ".tex"))
  txt_path <- file.path(stargazer_dir, paste0(file_stub, ".txt"))
  tex_txt_path <- file.path(stargazer_tex_dir, paste0(file_stub, ".txt"))
  result <- tryCatch(
    {
      do.call(
        stargazer::stargazer,
        c(models, list(type = "html", title = title, out = html_path))
      )
      do.call(
        stargazer::stargazer,
        c(models, list(type = "latex", title = title, out = tex_path))
      )
      publish_if_maintest_uses(tex_path, "stargazer")
      TRUE
    },
    error = function(e) {
      for (fallback_path in c(txt_path, tex_txt_path)) {
        sink(fallback_path)
        cat("Stargazer nie obsluzyl tego zestawu modeli:\n")
        cat(conditionMessage(e), "\n\n")
        for (name in names(models)) {
          cat("\n====================\n")
          cat(name, "\n")
          cat("====================\n")
          print(summary(models[[name]]))
        }
        sink()
      }
      FALSE
    }
  )
  invisible(result)
}

save_spatial_plot <- function(file_stub, width = 8, height = 8, plot_expr) {
  # Zapisuje wykres jako PDF (wektor — ostre etykiety w bezjc.tex) i opcjonalnie
  # jako PNG do podgladu. Publikujemy PDF, zeby pdflatex go zlapal.
  pdf_path <- file.path(spatial_r_wykresy_dir, paste0(file_stub, ".pdf"))
  png_path <- file.path(spatial_r_wykresy_dir, paste0(file_stub, ".png"))
  plot_code <- substitute(plot_expr)

  grDevices::cairo_pdf(pdf_path, width = width, height = height, family = "serif")
  par(family = "serif", mar = c(4.2, 4.4, 3.8, 1.2), cex = 0.95, cex.main = 1.8, cex.lab = 1.25, cex.axis = 1.1)
  eval(plot_code, envir = parent.frame())
  dev.off()
  publish_if_maintest_uses(pdf_path, "media")

  if (SAVE_PDF_PLOTS) {
    png(png_path, width = width * 300, height = height * 300, res = 300, type = "cairo")
    par(family = "serif", mar = c(4.2, 4.4, 3.8, 1.2), cex = 0.95, cex.main = 1.8, cex.lab = 1.25, cex.axis = 1.1)
    eval(plot_code, envir = parent.frame())
    dev.off()
  }
}

plot_moran_scatter_R <- function(diag_df, file_stub, title_text, x_label, y_label) {
  # Rysuje wykres rozrzutu Morana z kolorami dla czterech cwiartek.
  # Nachylenie linii regresji odpowiada wizualnej sile autokorelacji przestrzennej.
  colors <- c(HH = "#d73027", LH = "#fdae61", LL = "#4575b4", HL = "#91bfdb")
  save_spatial_plot(file_stub, width = 8, height = 8, {
    plot(
      diag_df$z,
      diag_df$wz,
      type = "n",
      xlab = x_label,
      ylab = y_label,
      main = title_text
    )
    abline(h = 0, v = 0, col = "black", lwd = 1)
    for (label in names(colors)) {
      idx <- diag_df$moran_quadrant == label
      points(diag_df$z[idx], diag_df$wz[idx], pch = 16, cex = 0.65, col = adjustcolor(colors[[label]], 0.75))
    }
    fit <- lm(wz ~ z, data = diag_df)
    abline(fit, col = "black", lty = 2, lwd = 1.2)
    legend("topleft", legend = names(colors), col = colors, pch = 16, bty = "n")
  })
}

plot_category_map_R <- function(gmina_sf, diag_df, category_col, file_stub, title_text, colors, legend_title) {
  # Rysuje mape kategorii przestrzennych po kodzie TERYT.
  # Uzywamy jej dla cwiartek Morana i etykiet LISA, czyli wynikow dyskretnych.
  map_data <- merge(
    gmina_sf[, c("JPT_KOD_JE")],
    diag_df[, c("Kod", category_col)],
    by.x = "JPT_KOD_JE",
    by.y = "Kod",
    all.x = TRUE,
    sort = FALSE
  )
  vals <- as.character(map_data[[category_col]])
  fill <- colors[vals]
  fill[is.na(fill)] <- "#d9d9d9"
  map_plot <- st_simplify(map_data, dTolerance = 150, preserveTopology = TRUE)

  save_spatial_plot(file_stub, width = 8.4, height = 8.4, {
    plot(st_geometry(map_plot), col = fill, border = "grey55", lwd = 0.12, main = title_text)
    legend(
      "bottomleft",
      legend = names(colors),
      fill = colors,
      title = legend_title,
      bty = "n",
      cex = 0.9
    )
  })
}

# ============================================================
# 1. Budowa zbioru danych do modeli przestrzennych
# ============================================================
# Budujemy dane dla modeli przestrzennych bez drog i bez odleglosci.
# Ten blok scala dane GUS/BDL/MF po kodzie TERYT i tworzy zmienne zgodne
# ze specyfikacja uzywana pozniej w modelach przestrzennych.
czyn_przyst <- read_gus("CzynnePrzystanki.csv")
med_wynagr_path <- Sys.glob(file.path(data_dir, "MedianaWynagrod*Brutto2024.csv"))[1]
med_wynagr <- read.csv2(med_wynagr_path, check.names = FALSE, stringsAsFactors = FALSE)
mieszk_odd <- read_gus("MieszkaniaOddane.csv")
przych_na_10k <- read_gus("PrzychodnieNa10k.csv")
udz_bezrob <- read_gus("UdzialBezrobotnych.csv")
saldo <- read_gus("SaldoMigracjiNa1k.csv")
populacja <- read_gus("PopulacjaGmin.csv")
m2_mieszkania <- read_gus("M2NaOsobe_MieszkaniaNa1000Mieszkancow.csv")
wskaznik_g <- readxl::read_excel(file.path(data_dir, "WskaznikG.xlsx"))

data_full <- data.frame(Kod = norm_code(czyn_przyst$Kod))
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(saldo$Kod), ySaldo = saldo[[3]]),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full$Przystanki <- as.numeric(czyn_przyst[[3]])
# Populacja jest rozbita na dwie kolumny, wiec sumujemy je do jednego mianownika.
populacja$Populacja <- as.numeric(populacja[[3]]) + as.numeric(populacja[[4]])
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(populacja$Kod), Populacja = populacja$Populacja),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full$logPrzystankiNa1000 <- log(data_full$Przystanki / data_full$Populacja * 1000)
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(wskaznik_g[["Kod gminy"]]), WskaznikG = as.numeric(wskaznik_g[["Wskaźnik G"]])),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full$logWskaznikG <- log(data_full$WskaznikG)

# Z miesiecznych kolumn wynagrodzen liczymy srednia roczna.
med_wynagr$srednia <- rowMeans(med_wynagr[, 3:ncol(med_wynagr)], na.rm = TRUE)
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(med_wynagr$Kod), logWynagrodzenia = log(med_wynagr$srednia)),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(mieszk_odd$Kod), mieszkania = as.numeric(mieszk_odd[[3]])),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full$logMieszkaniaNa1000 <- log(data_full$mieszkania / data_full$Populacja * 1000)
data_full$MieszkaniaOddaneNa1000 <- data_full$mieszkania / data_full$Populacja * 1000
data_full$logMieszkaniaOddaneNa1000 <- log(data_full$MieszkaniaOddaneNa1000)
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(przych_na_10k$Kod), Przychodnie = przych_na_10k[[3]]),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(udz_bezrob$Kod), Bezrobocie = udz_bezrob[[3]]),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
# Dodajemy metraz na osobe; odleglosc nie wchodzi do tych modeli.
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(m2_mieszkania$Kod), M2NaOsobe = as.numeric(m2_mieszkania[[3]])),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full$Miejska <- as.integer(substr(data_full$Kod, 7, 7) == "1")
data_full$Wiejska <- as.integer(substr(data_full$Kod, 7, 7) == "2")
# Po scaleniu czyscimy wartosci nieskonczone po logarytmach.
numeric_cols <- setdiff(names(data_full), "Kod")
for (col in numeric_cols) {
  data_full[[col]] <- as.numeric(data_full[[col]])
  data_full[[col]][!is.finite(data_full[[col]])] <- NA
}

x_cols <- c(
  "logWskaznikG",
  "logWynagrodzenia",
  "M2NaOsobe",
  "logMieszkaniaOddaneNa1000",
  "Przychodnie",
  "Bezrobocie",
  "Miejska",
  "Wiejska"
)

# Wczytujemy imputacje z pelnego zbioru WMNK i dopiero potem ustalamy probe do macierzy W.
wmnk_mice_bundle <- load_wmnk_mice_bundle()
model_cols <- c("Kod", "ySaldo", x_cols)
mice_model_data_list <- prepare_mice_model_data(wmnk_mice_bundle, model_cols)
data_full <- if (!is.null(wmnk_mice_bundle$single)) wmnk_mice_bundle$single else wmnk_mice_bundle$completed_datasets[[1]]
data_model <- mice_model_data_list[[1]]

# Wczytujemy granice gmin, naprawiamy geometrie i laczymy dane opisowe z mapa.
# CRS 2180 jest ukladem metrycznym, wygodnym do analiz przestrzennych dla Polski.
gmina <- st_read(file.path(data_dir, "A03_Granice_gmin.shp"), quiet = TRUE)
gmina$JPT_KOD_JE <- norm_code(gmina$JPT_KOD_JE)
gmina <- st_make_valid(gmina)
gmina <- st_transform(gmina, 2180)
gmina <- merge(gmina, data_model, by.x = "JPT_KOD_JE", by.y = "Kod", all = FALSE, sort = FALSE)
gmina <- gmina[!st_is_empty(gmina), ]
mice_model_data_list <- lapply(mice_model_data_list, function(d) {
  # Po zbudowaniu geometrii ustawiamy identyczna kolejnosc jednostek jak w macierzy sasiedztwa.
  matched <- d[match(gmina$JPT_KOD_JE, d$Kod), , drop = FALSE]
  if (any(is.na(matched$Kod)) || !identical(matched$Kod, gmina$JPT_KOD_JE)) {
    stop("Nie udalo sie dopasowac imputowanych danych do kolejnosci geometrii.", call. = FALSE)
  }
  row.names(matched) <- NULL
  matched
})
model_data <- mice_model_data_list[[1]]

cont_nb <- poly2nb(gmina, queen = TRUE)
cont_listw <- nb2listw(cont_nb, style = "W", zero.policy = TRUE)

eq <- as.formula(paste("ySaldo ~", paste(x_cols, collapse = " + ")))

# ============================================================
# 2. Estymacja modeli globalnych i testy wyboru modelu
# ============================================================
# Liczymy OLS oraz testy LM/Rao dla zaleznosci przestrzennej.
model_ols <- lm(eq, data = model_data)
lm_moran <- lm.morantest(model_ols, cont_listw, zero.policy = TRUE)
lm_tests <- lm.RStests(model_ols, cont_listw, test = "all", zero.policy = TRUE)

# Estymujemy zestaw porownywanych modeli przestrzennych.
# SLX zawiera przestrzenne opoznienia regresorow WX.
model_slx <- lmSLX(eq, data = model_data, listw = cont_listw, zero.policy = TRUE)
print(summary(model_slx))

# SAR zawiera przestrzenne opoznienie zmiennej objasnianej Wy.
model_sar <- lagsarlm(eq, data = model_data, listw = cont_listw, tol.solve = 1e-50, zero.policy = TRUE)
print(summary(model_sar))

# SEM modeluje autokorelacje przestrzenna w skladniku losowym.
model_sem <- errorsarlm(eq, data = model_data, listw = cont_listw, tol.solve = 1e-50, zero.policy = TRUE)
print(summary(model_sem))

# SDEM laczy SEM z przestrzennymi opoznieniami regresorow WX.
model_sdem <- errorsarlm(
  eq,
  data = model_data,
  listw = cont_listw,
  etype = "emixed",
  tol.solve = 1e-50,
  zero.policy = TRUE
)
print(summary(model_sdem))

# SDM laczy SAR z przestrzennymi opoznieniami regresorow WX.
model_sdm <- lagsarlm(
  eq,
  data = model_data,
  listw = cont_listw,
  type = "mixed",
  tol.solve = 1e-50,
  zero.policy = TRUE
)
print(summary(model_sdm))

# SAC/SARAR zawiera jednoczesnie Wy i przestrzenna autokorelacje bledu.
model_sac <- sacsarlm(
  eq,
  data = model_data,
  listw = cont_listw,
  type = "sac",
  tol.solve = 1e-50,
  zero.policy = TRUE
)
print(summary(model_sac))

# GNS/Manski zawiera pelny zestaw komponentow: Wy, WX i blad przestrzenny.
model_gns <- sacsarlm(
  eq,
  data = model_data,
  listw = cont_listw,
  type = "sacmixed",
  tol.solve = 1e-50,
  zero.policy = TRUE
)
print(summary(model_gns))

# Test Walda sprawdza laczna istotnosc zmiennych opoznionych przestrzennie.
wald_joint_zero <- function(model, coef_names, test_name) {
  available_names <- intersect(coef_names, names(coef(model)))
  missing_names <- setdiff(coef_names, available_names)
  if (length(missing_names) > 0) {
    stop(
      paste(
        "Brak wspolczynnikow w modelu dla testu Walda:",
        paste(missing_names, collapse = ", ")
      )
    )
  }

  beta <- coef(model)[available_names]
  vc <- vcov(model)[available_names, available_names, drop = FALSE]
  statistic <- as.numeric(t(beta) %*% solve(vc, beta))
  df <- length(available_names)
  p_value <- pchisq(statistic, df = df, lower.tail = FALSE)

  data.frame(
    test = test_name,
    restriction = paste(paste0(available_names, " = 0"), collapse = "; "),
    statistic = statistic,
    df = df,
    p_value = p_value,
    method = "Wald chi-square",
    row.names = NULL
  )
}

extract_coef_for_rubin <- function(model, imp_id, model_name) {
  # Wyciaga estymaty i bledy standardowe z modelu, zeby polaczyc wyniki zamiast danych.
  ct <- as.data.frame(coef(summary(model)), check.names = FALSE)
  estimate_col <- grep("^Estimate$", names(ct), value = TRUE)[1]
  se_col <- grep("Std\\. Error|Std. Error", names(ct), value = TRUE)[1]
  if (is.na(estimate_col) || is.na(se_col)) {
    stop(sprintf("Nie rozpoznano kolumn Estimate/Std. Error dla modelu %s.", model_name), call. = FALSE)
  }
  data.frame(
    .imp = imp_id,
    model = model_name,
    term = rownames(ct),
    estimate = as.numeric(ct[[estimate_col]]),
    std.error = as.numeric(ct[[se_col]]),
    row.names = NULL
  )
}

pool_rubin_table <- function(tab, dfcom = Inf) {
  # Stosuje reguly Rubina: qbar, ubar, b oraz t = ubar + (1 + 1/m) * b.
  pooled_rows <- lapply(split(tab, tab$term), function(part) {
    part <- part[order(part$.imp), , drop = FALSE]
    m <- length(unique(part$.imp))
    qbar <- mean(part$estimate)
    ubar <- mean(part$std.error^2)
    b <- stats::var(part$estimate)
    if (is.na(b)) {
      b <- 0
    }
    total <- ubar + (1 + 1 / m) * b
    se_total <- sqrt(total)
    riv <- ifelse(ubar > 0, ((1 + 1 / m) * b) / ubar, NA_real_)
    lambda <- ifelse(total > 0, ((1 + 1 / m) * b) / total, NA_real_)
    df <- if (is.na(riv) || riv == 0) Inf else (m - 1) * (1 + 1 / riv)^2
    statistic <- qbar / se_total
    p_value <- if (is.finite(df)) 2 * pt(abs(statistic), df = df, lower.tail = FALSE) else 2 * pnorm(abs(statistic), lower.tail = FALSE)
    critical <- if (is.finite(df)) qt(0.975, df = df) else qnorm(0.975)
    fmi <- ifelse(is.finite(df), (riv + 2 / (df + 3)) / (riv + 1), lambda)
    data.frame(
      term = part$term[1],
      m = m,
      estimate = qbar,
      ubar = ubar,
      b = b,
      total_var = total,
      std.error = se_total,
      statistic = statistic,
      df = df,
      p_value = p_value,
      conf.low = qbar - critical * se_total,
      conf.high = qbar + critical * se_total,
      riv = riv,
      lambda = lambda,
      fmi = fmi,
      row.names = NULL
    )
  })
  do.call(rbind, pooled_rows)
}

extract_lambda_for_rubin <- function(model, imp_id) {
  # Lambda z SDEM jest parametrem przestrzennego bledu, wiec poolujemy go osobno jak skalar.
  lambda_value <- as.numeric(model$lambda)
  lambda_se <- model$lambda.se
  if (is.null(lambda_se) || length(lambda_se) == 0 || is.na(lambda_se)) {
    lambda_se <- tryCatch(as.numeric(summary(model)$lambda.se), error = function(e) NA_real_)
  }
  data.frame(
    .imp = imp_id,
    model = "SDEM",
    term = "lambda",
    estimate = lambda_value,
    std.error = as.numeric(lambda_se),
    row.names = NULL
  )
}

extract_autoreg_for_rubin <- function(model, imp_id, model_name) {
  # Pooluje skalary autoregresyjne rho (lagsarlm/sacsarlm) i lambda (errorsarlm/sacsarlm)
  # razem ze wspolczynnikami beta, zeby Rubin objal wszystkie estymowane parametry modelu.
  rows <- list()
  pick_se <- function(value, se_attr) {
    if (is.null(se_attr) || length(se_attr) == 0 || any(is.na(se_attr))) {
      return(NA_real_)
    }
    as.numeric(se_attr[1])
  }
  if (!is.null(model$rho)) {
    rho_se <- pick_se(model$rho, model$rho.se)
    if (is.na(rho_se)) {
      rho_se <- tryCatch(as.numeric(summary(model)$rho.se), error = function(e) NA_real_)
    }
    rows[[length(rows) + 1]] <- data.frame(
      .imp = imp_id, model = model_name, term = "rho",
      estimate = as.numeric(model$rho), std.error = rho_se,
      row.names = NULL
    )
  }
  if (!is.null(model$lambda)) {
    lam_se <- pick_se(model$lambda, model$lambda.se)
    if (is.na(lam_se)) {
      lam_se <- tryCatch(as.numeric(summary(model)$lambda.se), error = function(e) NA_real_)
    }
    rows[[length(rows) + 1]] <- data.frame(
      .imp = imp_id, model = model_name, term = "lambda",
      estimate = as.numeric(model$lambda), std.error = lam_se,
      row.names = NULL
    )
  }
  if (length(rows) == 0) {
    return(data.frame(.imp = integer(0), model = character(0), term = character(0),
                      estimate = numeric(0), std.error = numeric(0)))
  }
  do.call(rbind, rows)
}

fit_spatial_mice_and_pool <- function(model_name, fit_fn, mice_data_list) {
  # Refit modelu na kazdej z 10 imputacji i pool Rubina dla bet + parametrow autoregresyjnych.
  # CSVs wynikowe: <prefix>_<MODEL>_mice_coefficients_by_imp_R.csv i ..._mice_rubin_pooled_R.csv.
  models <- lapply(seq_along(mice_data_list), function(imp_id) {
    cat(sprintf("%s MICE: estymacja imputacji %s/%s\n",
                model_name, imp_id, length(mice_data_list)))
    flush.console()
    fit_fn(mice_data_list[[imp_id]])
  })
  coef_tab <- do.call(rbind, Map(
    extract_coef_for_rubin, models, seq_along(models),
    MoreArgs = list(model_name = model_name)
  ))
  auto_tab <- do.call(rbind, Map(
    extract_autoreg_for_rubin, models, seq_along(models),
    MoreArgs = list(model_name = model_name)
  ))
  pooled_parts <- list(pool_rubin_table(coef_tab, dfcom = Inf))
  if (!is.null(auto_tab) && nrow(auto_tab) > 0) {
    pooled_parts[[length(pooled_parts) + 1]] <- pool_rubin_table(auto_tab, dfcom = Inf)
  }
  pooled <- do.call(rbind, pooled_parts)
  out_prefix <- "wyborprzest_bez_logDrogi"
  write.csv(
    coef_tab,
    file.path(wydruki_dir, sprintf("%s_%s_mice_coefficients_by_imp_R.csv", out_prefix, model_name)),
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  if (nrow(auto_tab) > 0) {
    write.csv(
      auto_tab,
      file.path(wydruki_dir, sprintf("%s_%s_mice_autoreg_by_imp_R.csv", out_prefix, model_name)),
      row.names = FALSE, fileEncoding = "UTF-8"
    )
  }
  write.csv(
    pooled,
    file.path(wydruki_dir, sprintf("%s_%s_mice_rubin_pooled_R.csv", out_prefix, model_name)),
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  list(models = models, coef = coef_tab, autoreg = auto_tab, pooled = pooled)
}

sdem_wx_coef_names <- paste0("lag.", x_cols)
wald_sdem_wx <- wald_joint_zero(
  model_sdem,
  sdem_wx_coef_names,
  "SDEM WX joint significance"
)
write.csv(wald_sdem_wx, file.path(wydruki_dir, "wyborprzest_bez_logDrogi_sdem_wald_wx_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Estymujemy kazdy model przestrzenny na 10 imputacjach i laczymy tylko wyniki, nie dane.
spatial_fit_fns <- list(
  SLX = function(d) lmSLX(eq, data = d, listw = cont_listw, zero.policy = TRUE),
  SAR = function(d) lagsarlm(eq, data = d, listw = cont_listw, tol.solve = 1e-50, zero.policy = TRUE),
  SEM = function(d) errorsarlm(eq, data = d, listw = cont_listw, tol.solve = 1e-50, zero.policy = TRUE),
  SDEM = function(d) errorsarlm(eq, data = d, listw = cont_listw, etype = "emixed", tol.solve = 1e-50, zero.policy = TRUE),
  SDM = function(d) lagsarlm(eq, data = d, listw = cont_listw, type = "mixed", tol.solve = 1e-50, zero.policy = TRUE),
  SAC = function(d) sacsarlm(eq, data = d, listw = cont_listw, type = "sac", tol.solve = 1e-50, zero.policy = TRUE),
  GNS = function(d) sacsarlm(eq, data = d, listw = cont_listw, type = "sacmixed", tol.solve = 1e-50, zero.policy = TRUE)
)
spatial_mice <- lapply(names(spatial_fit_fns), function(nm) {
  fit_spatial_mice_and_pool(nm, spatial_fit_fns[[nm]], mice_model_data_list)
})
names(spatial_mice) <- names(spatial_fit_fns)

# SDEM jest wyrozniony w tekscie pracy, wiec zachowujemy oryginalne nazwy obiektow i tabele tex.
sdem_mice_models <- spatial_mice$SDEM$models
sdem_coef_for_pooling <- spatial_mice$SDEM$coef
sdem_lambda_for_pooling <- spatial_mice$SDEM$autoreg
sdem_rubin_pooled <- spatial_mice$SDEM$pooled
sdem_rubin_pooled_tex <- data.frame(
  Zmienna = ifelse(sdem_rubin_pooled$term == "(Intercept)", "Wyraz wolny", sub("^lag\\.", "WX ", sdem_rubin_pooled$term)),
  estimate = sdem_rubin_pooled$estimate,
  std_error = sdem_rubin_pooled$std.error,
  statistic = sdem_rubin_pooled$statistic,
  p_value = sdem_rubin_pooled$p_value,
  fmi = sdem_rubin_pooled$fmi,
  row.names = NULL
)

model_list <- list(
  SLX = model_slx,
  SAR = model_sar,
  SEM = model_sem,
  SDEM = model_sdem,
  SDM = model_sdm,
  SAC = model_sac,
  GNS = model_gns
)
all_models_for_stargazer <- c(list(OLS = model_ols), model_list)
save_stargazer_or_summary(
  all_models_for_stargazer,
  "wyborprzest_bez_logDrogi_all_models",
  "Modele przestrzenne z finalnymi zmiennymi WMNK bez odleglosci"
)
save_stargazer_or_summary(
  list(OLS = model_ols),
  "wyborprzest_bez_logDrogi_ols",
  "Model OLS z wyborprzest_bez_logDrogi.R"
)
for (model_name in names(model_list)) {
  save_stargazer_or_summary(
    setNames(list(model_list[[model_name]]), model_name),
    paste0("wyborprzest_bez_logDrogi_", tolower(model_name)),
    paste("Model", model_name, "z wyborprzest_bez_logDrogi.R")
  )
}

pseudo_r2 <- function(model) {
  resid <- residuals(model)
  1 - sum(resid^2, na.rm = TRUE) / sum((model_data$ySaldo - mean(model_data$ySaldo))^2, na.rm = TRUE)
}

moran_resid <- function(model) {
  mt <- moran.test(residuals(model), cont_listw, zero.policy = TRUE)
  c(
    I = unname(mt$estimate[["Moran I statistic"]]),
    p = mt$p.value
  )
}

moran_values <- do.call(rbind, lapply(model_list, moran_resid))
comparison <- data.frame(
  model = names(model_list),
  logLik = sapply(model_list, function(m) as.numeric(logLik(m))),
  AIC = sapply(model_list, AIC),
  BIC = sapply(model_list, BIC),
  pseudo_R2 = sapply(model_list, pseudo_r2),
  resid_moran_I = moran_values[, "I"],
  resid_moran_p = moran_values[, "p"],
  row.names = NULL
)
comparison <- comparison[order(comparison$AIC, comparison$BIC), ]

# ============================================================
# 3. Porownania modeli: AIC/BIC, LR, Wald i reszty
# ============================================================
# Reczny LR jest potrzebny dla porownan z lmSLX(): model SLX z lmSLX()
# nie dziedziczy po klasach Sarlm ani lm, ktore sa wymagane przez anova.Sarlm().
# Dlatego statystyke LR liczymy bezposrednio z roznicy logLik modeli.
lr_comparison_table <- function(model_small, model_large, df_diff, test_name) {
  ll_small <- as.numeric(logLik(model_small))
  ll_large <- as.numeric(logLik(model_large))
  statistic <- 2 * (ll_large - ll_small)
  p_value <- pchisq(statistic, df = df_diff, lower.tail = FALSE)
  data.frame(
    test = test_name,
    logLik_small = ll_small,
    logLik_large = ll_large,
    df = df_diff,
    statistic = statistic,
    p_value = p_value,
    row.names = NULL
  )
}

# Testy LR sprawdzaja, czy prostszy model wyraznie pogarsza dopasowanie.
cat("\nANOVA/LR: GNS/Manski vs SAR\n")
anova_gns_sar <- anova(model_gns, model_sar)
print(anova_gns_sar)
cat("\nANOVA/LR: SAR vs GNS/Manski\n")
anova_sar_gns <- anova(model_sar, model_gns)
print(anova_sar_gns)
cat("\nANOVA/LR: GNS/Manski vs SEM\n")
anova_gns_sem <- anova(model_gns, model_sem)
print(anova_gns_sem)
cat("\nANOVA/LR: GNS/Manski vs SDEM\n")
anova_gns_sdem <- anova(model_gns, model_sdem)
print(anova_gns_sdem)
cat("\nANOVA/LR: GNS/Manski vs SDM\n")
anova_gns_sdm <- anova(model_gns, model_sdm)
print(anova_gns_sdm)
cat("\nANOVA/LR: GNS/Manski vs SAC\n")
anova_gns_sac <- anova(model_gns, model_sac)
print(anova_gns_sac)
cat("\nANOVA/LR: SAC vs SAR\n")
anova_sac_sar <- anova(model_sac, model_sar)
print(anova_sac_sar)
cat("\nANOVA/LR: SAC vs SEM\n")
anova_sac_sem <- anova(model_sac, model_sem)
print(anova_sac_sem)

# Dodatkowo porownujemy SDEM z prostszymi wariantami.
cat("\nANOVA/LR: SDEM vs SAR\n")
anova_sdem_sar <- anova(model_sdem, model_sar)
print(anova_sdem_sar)
cat("\nANOVA/LR: SDEM vs SLX\n")
anova_sdem_slx <- lr_comparison_table(model_slx, model_sdem, 1, "SLX_vs_SDEM")
print(anova_sdem_slx)
cat("\nANOVA/LR: SDEM vs SEM\n")
anova_sdem_sem <- anova(model_sdem, model_sem)
print(anova_sdem_sem)

# Zapisujemy wskazanie kryteriow AIC/BIC, ale nie uzywamy go do dalszej analizy.
admissible <- comparison[!is.na(comparison$AIC) & comparison$resid_moran_p > 0.05, ]
if (nrow(admissible) > 0) {
  selected_model_name <- admissible[order(admissible$AIC, admissible$BIC), "model"][1]
} else {
  selected_model_name <- comparison[order(comparison$AIC, comparison$BIC), "model"][1]
}

# Do dalszej diagnostyki i interpretacji wymuszamy SDEM, mimo ze kryteria moga wskazywac GNS.
best_model_name <- "SDEM"
best_model <- model_list[[best_model_name]]

# Sprawdzamy globalna autokorelacje przestrzenna salda migracji.
# Te wyniki opisuja sama zmienna zalezna, zanim przejdziemy do reszt modeli.
global_moran <- moran.test(model_data$ySaldo, cont_listw, zero.policy = TRUE)
global_geary <- geary.test(model_data$ySaldo, cont_listw, zero.policy = TRUE)
join_binary <- factor(ifelse(model_data$ySaldo >= 0, "nonnegative", "negative"))
global_join <- joincount.multi(join_binary, cont_listw, zero.policy = TRUE)
global_join_test <- joincount.test(join_binary, cont_listw, zero.policy = TRUE, alternative = "greater")

# Zmieniamy wyniki testow na ramki danych do tabel LaTeX.
moran_to_df <- function(moran_obj, variable_name, alternative_name) {
  data.frame(
    variable = variable_name,
    alternative = alternative_name,
    statistic = unname(moran_obj$statistic),
    moran_i = unname(moran_obj$estimate[["Moran I statistic"]]),
    expectation = unname(moran_obj$estimate[["Expectation"]]),
    variance = unname(moran_obj$estimate[["Variance"]]),
    p_value = moran_obj$p.value,
    method = moran_obj$method,
    row.names = NULL
  )
}

joincount_to_df <- function(join_obj, variable_name) {
  join_df <- as.data.frame(join_obj, check.names = FALSE)
  join_df$classification <- rownames(join_df)
  rownames(join_df) <- NULL
  join_df <- join_df[, c("classification", setdiff(names(join_df), "classification"))]
  join_df$variable <- variable_name
  join_df[, c("variable", setdiff(names(join_df), "variable"))]
}

joincount_test_to_df <- function(join_test_obj, variable_name) {
  # Zapisuje formalne testy BB z joincount.test(), ktore zawieraja p-value dla kazdego koloru.
  level_names <- names(join_test_obj)
  if (is.null(level_names)) {
    level_names <- paste0("level_", seq_along(join_test_obj))
  }

  rows <- lapply(seq_along(join_test_obj), function(idx) {
    test_obj <- join_test_obj[[idx]]
    estimate_names <- names(test_obj$estimate)
    observed_name <- grep("Same colour|Joincount|statistic", estimate_names, value = TRUE)[1]
    expectation_name <- grep("Expectation", estimate_names, value = TRUE)[1]
    variance_name <- grep("Variance", estimate_names, value = TRUE)[1]

    data.frame(
      variable = variable_name,
      color = level_names[idx],
      statistic = as.numeric(test_obj$statistic[1]),
      p_value = as.numeric(test_obj$p.value[1]),
      observed = if (is.na(observed_name)) NA_real_ else as.numeric(test_obj$estimate[[observed_name]][1]),
      expectation = if (is.na(expectation_name)) NA_real_ else as.numeric(test_obj$estimate[[expectation_name]][1]),
      variance = if (is.na(variance_name)) NA_real_ else as.numeric(test_obj$estimate[[variance_name]][1]),
      alternative = as.character(test_obj$alternative[1]),
      method = as.character(test_obj$method[1]),
      row.names = NULL
    )
  })

  do.call(rbind, rows)
}

geary_to_df <- function(geary_obj, variable_name) {
  data.frame(
    variable = variable_name,
    statistic = unname(geary_obj$estimate[["Geary C statistic"]]),
    expectation = unname(geary_obj$estimate[["Expectation"]]),
    variance = unname(geary_obj$estimate[["Variance"]]),
    p_value = geary_obj$p.value,
    method = geary_obj$method,
    alternative = geary_obj$alternative,
    row.names = NULL
  )
}

ols_residuals <- as.numeric(residuals(model_ols))
# Dla reszt OLS liczymy kilka wariantow testu Morana, zeby pokazac kierunek autokorelacji.
ols_residuals_moran_alternatives <- c("greater", "two.sided", "less")
ols_residuals_moran_alternative_rows <- do.call(
  rbind,
  lapply(ols_residuals_moran_alternatives, function(alternative_name) {
    moran_to_df(
      moran.test(ols_residuals, cont_listw, alternative = alternative_name, zero.policy = TRUE),
      "reszty_ols",
      alternative_name
    )
  })
)
ols_geary <- geary.test(ols_residuals, cont_listw, zero.policy = TRUE)
ols_join_binary <- factor(ifelse(ols_residuals >= 0, "nonnegative", "negative"))
ols_join <- joincount.multi(ols_join_binary, cont_listw, zero.policy = TRUE)
ols_join_test <- joincount.test(ols_join_binary, cont_listw, zero.policy = TRUE, alternative = "greater")

geary_rows <- rbind(
  geary_to_df(global_geary, "ySaldo"),
  geary_to_df(ols_geary, "reszty_ols")
)
join_rows <- rbind(
  joincount_to_df(global_join, "ySaldo"),
  joincount_to_df(ols_join, "reszty_ols")
)
join_test_rows <- rbind(
  joincount_test_to_df(global_join_test, "ySaldo"),
  joincount_test_to_df(ols_join_test, "reszty_ols")
)

# Liczymy podstawowe testy LR dla modeli zagniezdzonych.
lr_pair <- function(model_small, model_large, df_diff) {
  stat <- 2 * (as.numeric(logLik(model_large)) - as.numeric(logLik(model_small)))
  p_value <- pchisq(stat, df = df_diff, lower.tail = FALSE)
  c(statistic = stat, p_value = p_value, df = df_diff)
}

lr_tests <- data.frame(
  test = c("OLS_vs_SAR", "OLS_vs_SEM", "SAR_vs_SDM", "SEM_vs_SDEM"),
  statistic = c(
    lr_pair(model_ols, model_sar, 1)["statistic"],
    lr_pair(model_ols, model_sem, 1)["statistic"],
    lr_pair(model_sar, model_sdm, length(x_cols))["statistic"],
    lr_pair(model_sem, model_sdem, length(x_cols))["statistic"]
  ),
  p_value = c(
    lr_pair(model_ols, model_sar, 1)["p_value"],
    lr_pair(model_ols, model_sem, 1)["p_value"],
    lr_pair(model_sar, model_sdm, length(x_cols))["p_value"],
    lr_pair(model_sem, model_sdem, length(x_cols))["p_value"]
  ),
  df = c(1, 1, length(x_cols), length(x_cols)),
  row.names = NULL
)

# ============================================================
# 4. Efekty bezposrednie, posrednie i laczne
# ============================================================
# Przygotowujemy funkcje do efektow i diagnostyki modelu.
# W modelach przestrzennych interpretujemy nie tylko beta, ale tez efekty przez sasiedztwo.
lagged_x <- as.data.frame(sapply(x_cols, function(v) {
  lag.listw(cont_listw, model_data[[v]], zero.policy = TRUE)
}))
names(lagged_x) <- paste0("lag_", x_cols)

coef_value <- function(model, nm) {
  vals <- coef(model)
  if (nm %in% names(vals)) {
    unname(vals[[nm]])
  } else {
    NA_real_
  }
}

coef_p_value <- function(model, nm) {
  # Pobiera p-value wspolczynnika z tabeli summary(model).
  coef_matrix <- as.data.frame(coef(summary(model)), check.names = FALSE)
  if (!nm %in% rownames(coef_matrix)) {
    return(NA_real_)
  }
  p_col <- grep("^Pr|p.value|p_value", names(coef_matrix), value = TRUE)[1]
  if (is.na(p_col)) {
    return(NA_real_)
  }
  as.numeric(coef_matrix[nm, p_col])
}

lag_coef_name <- function(model, variable) {
  # Wyszukuje nazwe wspolczynnika WX odpowiadajacego zmiennej X.
  candidates <- c(paste0("lag.", variable), paste0("lag_", variable), paste0("lag.", make.names(variable)))
  hit <- candidates[candidates %in% names(coef(model))]
  if (length(hit) == 0) {
    NA_character_
  } else {
    hit[1]
  }
}

total_effect_p_value <- function(model, direct_name, indirect_name) {
  # Testuje istotnosc sumy efektu bezposredniego i posredniego.
  if (is.na(indirect_name)) {
    return(coef_p_value(model, direct_name))
  }
  coef_names <- names(coef(model))
  if (!direct_name %in% coef_names || !indirect_name %in% coef_names) {
    return(NA_real_)
  }
  covariance <- vcov(model)
  variance_total <- covariance[direct_name, direct_name] +
    covariance[indirect_name, indirect_name] +
    2 * covariance[direct_name, indirect_name]
  if (!is.finite(variance_total) || variance_total <= 0) {
    return(NA_real_)
  }
  total_value <- coef_value(model, direct_name) + coef_value(model, indirect_name)
  z_value <- total_value / sqrt(variance_total)
  2 * pnorm(abs(z_value), lower.tail = FALSE)
}

significance_stars <- function(p_value) {
  # Zamienia p-value na gwiazdki istotnosci stosowane w tabelach.
  ifelse(
    is.na(p_value),
    "",
    ifelse(p_value < 0.01, "***", ifelse(p_value < 0.05, "**", ifelse(p_value < 0.10, "*", "")))
  )
}

format_effect_with_stars <- function(value, p_value) {
  # Formatuje wartosc efektu i dopisuje gwiazdki na podstawie p-value.
  ifelse(
    is.na(value),
    "",
    paste0(formatC(value, format = "f", digits = 3, decimal.mark = ","), significance_stars(p_value))
  )
}

impact_table_manual <- function(model, model_name) {
  # Dla SDEM/SLX liczymy efekty z samych wspolczynnikow X i WX.
  rows <- lapply(x_cols, function(v) {
    lag_name <- lag_coef_name(model, v)
    direct <- coef_value(model, v)
    indirect <- if (is.na(lag_name)) 0 else coef_value(model, lag_name)
    direct_p <- coef_p_value(model, v)
    indirect_p <- if (is.na(lag_name)) NA_real_ else coef_p_value(model, lag_name)
    total <- direct + indirect
    total_p <- total_effect_p_value(model, v, lag_name)
    data.frame(
      variable = v,
      direct = direct,
      direct_p_value = direct_p,
      direct_stars = significance_stars(direct_p),
      indirect = indirect,
      indirect_p_value = indirect_p,
      indirect_stars = significance_stars(indirect_p),
      total = total,
      total_p_value = total_p,
      total_stars = significance_stars(total_p),
      model = model_name,
      row.names = NULL
    )
  })
  do.call(rbind, rows)
}

impact_table_for_tex <- function(impact_df) {
  # Przygotowuje wersje tabeli efektow z gwiazdkami dopisanymi do wartosci.
  data.frame(
    Zmienna = impact_df$variable,
    Bezposredni = format_effect_with_stars(impact_df$direct, impact_df$direct_p_value),
    Posredni = format_effect_with_stars(impact_df$indirect, impact_df$indirect_p_value),
    Calkowity = format_effect_with_stars(impact_df$total, impact_df$total_p_value),
    row.names = NULL,
    check.names = FALSE
  )
}

parse_impacts_output <- function(impact_lines, model_name) {
  # Odczytuje tabele efektow z tekstowego wydruku impacts().
  header_idx <- grep("Direct\\s+Indirect\\s+Total", impact_lines)
  if (length(header_idx) == 0) {
    return(NULL)
  }

  rows <- list()
  for (line in impact_lines[(header_idx[1] + 1):length(impact_lines)]) {
    stripped <- trimws(line)
    if (stripped == "" || grepl("^=", stripped)) {
      next
    }

    parts <- strsplit(stripped, "\\s+")[[1]]
    if (length(parts) < 4) {
      next
    }

    numeric_idx <- (length(parts) - 2):length(parts)
    values <- suppressWarnings(as.numeric(parts[numeric_idx]))
    if (any(is.na(values))) {
      next
    }

    variable_parts <- parts[seq_len(length(parts) - 3)]
    variable <- paste(variable_parts, collapse = " ")
    variable <- sub("\\s+dy/dx$", "", variable)
    rows[[length(rows) + 1]] <- data.frame(
      variable = variable,
      direct = values[1],
      indirect = values[2],
      total = values[3],
      model = model_name,
      row.names = NULL
    )
  }

  if (length(rows) == 0) {
    NULL
  } else {
    do.call(rbind, rows)
  }
}

impact_table <- function(model, model_name) {
  # Dla modeli z opoznionym y liczymy pelne efekty funkcja impacts().
  if (model_name %in% c("SAR", "SDM", "GNS")) {
    impact_obj <- impacts(model, listw = cont_listw, zero.policy = TRUE)
    impact_lines <- capture.output(print(impact_obj))
    writeLines(
      impact_lines,
      con = file.path(wydruki_dir, paste0("wyborprzest_bez_logDrogi_", tolower(model_name), "_full_impacts_R.txt"))
    )

    parsed <- parse_impacts_output(impact_lines, model_name)
    if (!is.null(parsed)) {
      return(parsed)
    }

    stop("Nie udalo sie automatycznie sparsowac wyniku impacts(); sprawdz plik TXT z pelnym wydrukiem.")
  }

  # Dla pozostalych modeli wystarcza tabela wspolczynnikow X i WX.
  impact_table_manual(model, model_name)
}

best_residuals <- as.numeric(residuals(best_model))
best_fitted <- as.numeric(fitted(best_model))

if (best_model_name %in% c("SLX", "SDEM", "SDM", "GNS")) {
  best_exog <- cbind(model_data[, x_cols], lagged_x)
} else {
  best_exog <- model_data[, x_cols]
}
best_bp_data <- data.frame(best_residuals = best_residuals, best_exog, check.names = TRUE)
best_bp <- bptest(best_residuals ~ 1, varformula = ~ . - best_residuals, data = best_bp_data)

best_impacts <- impact_table(best_model, best_model_name)
write.csv(best_impacts, file.path(wydruki_dir, "wyborprzest_bez_logDrogi_best_model_impacts_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")

test_estimate_value_local <- function(test_obj, name_pattern) {
  # Pobiera wartosc estymatora z obiektu testu, takze przy uruchamianiu fragmentu skryptu z IDE.
  estimate_name <- grep(name_pattern, names(test_obj$estimate), value = TRUE)[1]
  if (is.na(estimate_name)) {
    return(NA_real_)
  }
  as.numeric(test_obj$estimate[[estimate_name]][1])
}

write_spatial_overleaf_tables <- function() {
  # Zapisuje docelowe tabele do Overleaf od razu po policzeniu wynikow modelowych.
  # Ten blok jest przed mapami, zeby dlugie rysowanie nie blokowalo eksportu tabel.
  test_estimate_value_local <- function(test_obj, name_pattern) {
    estimate_name <- grep(name_pattern, names(test_obj$estimate), value = TRUE)[1]
    if (is.na(estimate_name)) {
      return(NA_real_)
    }
    as.numeric(test_obj$estimate[[estimate_name]][1])
  }

  table_value_by_name_local <- function(df, name_pattern) {
    column_name <- grep(name_pattern, names(df), value = TRUE)[1]
    if (is.na(column_name)) {
      return(NA_real_)
    }
    as.numeric(df[[column_name]][1])
  }

  join_total_row_local <- function(join_obj) {
    join_df <- as.data.frame(join_obj, check.names = FALSE)
    join_df$classification <- rownames(join_df)
    rownames(join_df) <- NULL
    if (any(join_df$classification == "Jtot")) {
      join_df[join_df$classification == "Jtot", , drop = FALSE][1, , drop = FALSE]
    } else {
      join_df[1, , drop = FALSE]
    }
  }

  join_p_value_from_row_local <- function(join_row) {
    statistic <- table_value_by_name_local(join_row, "Std|z-value|statistic")
    p_value <- table_value_by_name_local(join_row, "^Pr|p.value|p_value")
    if (is.na(p_value) && !is.na(statistic)) {
      p_value <- 2 * pnorm(abs(statistic), lower.tail = FALSE)
    }
    p_value
  }

  join_class_label_local <- function(classification, variable_name) {
    base_label <- ifelse(variable_name == "ySaldo", "ySaldo", "reszta")
    if (classification == "negative:negative") {
      return(paste0("$", base_label, " < 0$ -- $", base_label, " < 0$"))
    }
    if (classification == "nonnegative:nonnegative") {
      return(paste0("$", base_label, " >= 0$ -- $", base_label, " >= 0$"))
    }
    if (classification == "nonnegative:negative") {
      return(paste0("$", base_label, " >= 0$ -- $", base_label, " < 0$"))
    }
    classification
  }

  join_components_for_tex_local <- function(join_df, variable_name) {
    selected <- join_df[join_df$variable == variable_name, , drop = FALSE]
    data.frame(
      Klasyfikacja = vapply(selected$classification, join_class_label_local, character(1), variable_name = variable_name),
      Obserwowane = selected$Joincount,
      Oczekiwane = selected$Expected,
      Wariancja = selected$Variance,
      z = selected[["z-value"]],
      row.names = NULL,
      check.names = FALSE
    )
  }

  test_verdict_local <- function(p_value) {
    ifelse(is.na(p_value), "", ifelse(p_value < 0.05, "Odrzucamy H0", "Brak podstaw do odrzucenia H0"))
  }

  global_join_row <- join_total_row_local(global_join)
  global_join_statistic <- table_value_by_name_local(global_join_row, "Std|z-value|statistic")
  global_join_p_value <- join_p_value_from_row_local(global_join_row)
  global_moran_i <- test_estimate_value_local(global_moran, "Moran I")
  global_geary_c <- test_estimate_value_local(global_geary, "Geary C")
  global_auto_table_pdf <- data.frame(
    Test = c("I Morana", "C Geary'ego", "Join-count"),
    "Hipoteza zerowa H0" = c(
      "Brak dodatniej autokorelacji przestrzennej zmiennej ySaldo",
      "Brak autokorelacji przestrzennej; C = 1",
      "Losowe rozmieszczenie gmin wedlug znaku salda migracji"
    ),
    Statystyka = c(
      paste0("I=", format_number_tex(global_moran_i)),
      paste0("C=", format_number_tex(global_geary_c)),
      paste0("z=", format_number_tex(global_join_statistic))
    ),
    "p-value" = format_p_value_tex(c(as.numeric(global_moran$p.value[1]), as.numeric(global_geary$p.value[1]), global_join_p_value)),
    Werdykt = test_verdict_local(c(as.numeric(global_moran$p.value[1]), as.numeric(global_geary$p.value[1]), global_join_p_value)),
    row.names = NULL,
    check.names = FALSE
  )
  write_overleaf_table(global_auto_table_pdf, "globalna_autokorelacja_ysaldo", "Globalna autokorelacja przestrzenna zmiennej zaleznej ySaldo", align = "@{}p{3cm}p{5.2cm}p{3cm}p{2cm}p{2.8cm}@{}")
  write_overleaf_table(join_components_for_tex_local(join_rows, "ySaldo"), "join_count_ysaldo_skladowe", "Skladowe testu join-count dla zmiennej zaleznej ySaldo", align = "@{}lrrrr@{}")

  ols_moran_row <- ols_residuals_moran_alternative_rows[ols_residuals_moran_alternative_rows$alternative == "greater", , drop = FALSE][1, , drop = FALSE]
  ols_geary_row <- geary_rows[geary_rows$variable == "reszty_ols", , drop = FALSE][1, , drop = FALSE]
  ols_join_row <- join_total_row_local(ols_join)
  ols_join_statistic <- table_value_by_name_local(ols_join_row, "Std|z-value|statistic")
  ols_join_p_value <- join_p_value_from_row_local(ols_join_row)
  ols_residual_autocorr_table <- data.frame(
    Test = c("I Morana dla reszt MNK", "C Geary'ego dla reszt MNK", "Join-count dla znaku reszt MNK"),
    H0 = c(
      "Brak dodatniej autokorelacji przestrzennej",
      "Brak autokorelacji przestrzennej; C = 1",
      "Losowy rozklad dodatnich i ujemnych reszt"
    ),
    H1 = c(
      "Wystepuje dodatnia autokorelacja przestrzenna",
      "Wystepuje autokorelacja przestrzenna",
      "Wystepuje przestrzenne grupowanie znakow reszt"
    ),
    Statystyka = c(
      paste0("I=", format_number_tex(ols_moran_row$moran_i)),
      paste0("C=", format_number_tex(ols_geary_row$statistic)),
      paste0("z=", format_number_tex(ols_join_statistic))
    ),
    "p-value" = format_p_value_tex(c(ols_moran_row$p_value, ols_geary_row$p_value, ols_join_p_value)),
    row.names = NULL,
    check.names = FALSE
  )
  write_overleaf_table(ols_residual_autocorr_table, "testy_autokorelacji_reszt_mnk", "Testy autokorelacji przestrzennej dla reszt modelu MNK", align = "@{}p{3cm}p{3.5cm}p{3.5cm}p{2.8cm}p{1.8cm}@{}")
  write_overleaf_table(join_components_for_tex_local(join_rows, "reszty_ols"), "join_count_reszty_mnk_skladowe", "Skladowe testu join-count dla znaku reszt modelu MNK", align = "@{}lrrrr@{}")

  lm_rows_local <- data.frame(
    test = names(lm_tests),
    statistic = sapply(lm_tests, function(x) unname(x$statistic)),
    p_value = sapply(lm_tests, function(x) x$p.value),
    row.names = NULL
  )
  lm_interpretation_local <- function(test_name, p_value) {
    if (test_name == "RSerr") {
      return(ifelse(p_value < 0.05, "Istotna zaleznosc typu spatial error w tescie podstawowym", "Brak istotnosci typu spatial error w tescie podstawowym"))
    }
    if (test_name == "RSlag") {
      return(ifelse(p_value < 0.05, "Istotna zaleznosc typu spatial lag w tescie podstawowym", "Brak istotnosci typu spatial lag w tescie podstawowym"))
    }
    if (test_name == "adjRSerr") {
      return(ifelse(p_value < 0.05, "Po korekcie pozostaje istotny skladnik spatial error", "Po korekcie brak istotnosci dla czystego spatial error"))
    }
    if (test_name == "adjRSlag") {
      return(ifelse(p_value < 0.05, "Po korekcie pozostaje istotny skladnik spatial lag", "Po korekcie brak istotnosci dla czystego spatial lag"))
    }
    ifelse(p_value < 0.05, "Laczny test wskazuje na istotna zaleznosc przestrzenna", "Laczny test nie wskazuje na istotna zaleznosc przestrzenna")
  }
  lm_rows_tex <- data.frame(
    Test = lm_rows_local$test,
    Statystyka = lm_rows_local$statistic,
    "p-value" = lm_rows_local$p_value,
    Interpretacja = mapply(lm_interpretation_local, lm_rows_local$test, lm_rows_local$p_value),
    row.names = NULL,
    check.names = FALSE
  )
  write_overleaf_table(lm_rows_tex, "testy_lm_rao_wybor_modelu", "Testy Rao score/LM dla wyboru typu modelu przestrzennego", align = "@{}lrrp{6.2cm}@{}")

  comparison_tex <- comparison[, c("model", "AIC", "BIC", "logLik")]
  names(comparison_tex) <- c("Model", "AIC", "BIC", "Log-likelihood")
  write_overleaf_table(comparison_tex, "porownanie_modeli_przestrzennych_aic_bic_loglik", "Porownanie modeli przestrzennych wedlug kryteriow informacyjnych", align = "lrrr")

  anova_outputs_local <- list(
    "GNS/Manski vs SAR" = anova_gns_sar,
    "GNS/Manski vs SEM" = anova_gns_sem,
    "GNS/Manski vs SDEM" = anova_gns_sdem,
    "GNS/Manski vs SDM" = anova_gns_sdm,
    "GNS/Manski vs SAC" = anova_gns_sac,
    "SDEM vs SAR" = anova_sdem_sar,
    "SDEM vs SLX" = anova_sdem_slx,
    "SDEM vs SEM" = anova_sdem_sem
  )
  anova_lr_row_local <- function(nm) {
    anova_df <- as.data.frame(anova_outputs_local[[nm]], check.names = FALSE)
    lr_col <- grep("L.Ratio|statistic", names(anova_df), value = TRUE)[1]
    p_col <- grep("^p-value$|p_value|Pr", names(anova_df), value = TRUE)[1]
    df_col <- grep("^df$|^Df$", names(anova_df), value = TRUE)[1]
    lr_values <- suppressWarnings(as.numeric(anova_df[[lr_col]]))
    p_values <- suppressWarnings(as.numeric(anova_df[[p_col]]))
    df_values <- if (!is.na(df_col)) suppressWarnings(as.numeric(anova_df[[df_col]])) else NA_real_
    lr_value <- lr_values[is.finite(lr_values)][1]
    p_value <- p_values[is.finite(p_values)][1]
    delta_df <- if (sum(is.finite(df_values)) >= 2) {
      abs(diff(df_values[is.finite(df_values)][1:2]))
    } else {
      df_values[is.finite(df_values)][1]
    }
    simple_model <- trimws(sub("^.* vs ", "", nm))
    comparison_name <- gsub("GNS/Manski", "GNS", nm, fixed = TRUE)
    simple_model <- gsub("GNS/Manski", "GNS", simple_model, fixed = TRUE)
    data.frame(
      Porownanie = comparison_name,
      "Model prostszy" = simple_model,
      "Delta df" = delta_df,
      LR = lr_value,
      "p-value" = p_value,
      Wniosek = ifelse(p_value < 0.05, "brak podstaw do redukcji", "redukcja dopuszczalna"),
      row.names = NULL,
      check.names = FALSE
    )
  }
  anova_lr_redukcja_pdf <- dplyr::bind_rows(lapply(names(anova_outputs_local), anova_lr_row_local))
  write_overleaf_table(anova_lr_redukcja_pdf, "anova_lr_redukcja_gns", "Testy ANOVA/LR dla redukcji modeli GNS i SDEM", align = "lrrrrl")

  wald_sdem_wx_tex <- data.frame(
    Test = "Test Walda komponentu WX modelu SDEM",
    "Hipoteza zerowa H0" = "Wszystkie parametry przy przestrzennie opoznionych zmiennych objasniajacych sa rowne zero",
    Statystyka = wald_sdem_wx$statistic,
    df = wald_sdem_wx$df,
    "p-value" = wald_sdem_wx$p_value,
    Wniosek = ifelse(wald_sdem_wx$p_value < 0.05, "komponent WX istotny", "brak lacznej istotnosci komponentu WX"),
    row.names = NULL,
    check.names = FALSE
  )
  write_overleaf_table(wald_sdem_wx_tex, "testy_komponentow_SDEM", "Test komponentu WX modelu SDEM", align = "@{}p{3.4cm}p{6cm}rrrp{3.2cm}@{}")

  pseudo_r2_tex <- comparison[order(-comparison$pseudo_R2), c("model", "pseudo_R2")]
  names(pseudo_r2_tex) <- c("Model", "Pseudo R2")
  write_overleaf_table(pseudo_r2_tex, "porownanie_modeli_pseudo_r2", "Porownanie modeli przestrzennych wedlug pseudo R2")

  best_moran_local <- moran.test(best_residuals, cont_listw, zero.policy = TRUE)
  # Wyciagamy I Morana bezposrednio z obiektu testu, zeby fragment tabeli dzialal tez uruchamiany osobno.
  best_moran_i_local_name <- grep("Moran I", names(best_moran_local$estimate), value = TRUE)[1]
  best_moran_i_local <- if (is.na(best_moran_i_local_name)) {
    NA_real_
  } else {
    as.numeric(best_moran_local$estimate[[best_moran_i_local_name]][1])
  }
  best_residual_tests_tex <- data.frame(
    Test = c(paste("I Morana reszt", best_model_name), "Breusch-Pagan dla reszt SDEM"),
    "Hipoteza zerowa H0" = c(
      "Brak autokorelacji przestrzennej reszt",
      "Homoskedastycznosc skladnika losowego"
    ),
    Statystyka = c(
      paste0("I=", format_number_tex(best_moran_i_local)),
      format_number_tex(unname(best_bp$statistic))
    ),
    "p-value" = format_p_value_tex(c(as.numeric(best_moran_local$p.value[1]), best_bp$p.value)),
    Wniosek = c(
      ifelse(as.numeric(best_moran_local$p.value[1]) < 0.05, "reszty skorelowane przestrzennie", "brak istotnej autokorelacji reszt"),
      ifelse(best_bp$p.value < 0.05, "heteroskedastycznosc pozostaje", "brak podstaw do odrzucenia homoskedastycznosci")
    ),
    row.names = NULL,
    check.names = FALSE
  )
  write_overleaf_table(best_residual_tests_tex, "testy_morana_bp_reszty_SDEM", "Diagnostyka reszt modelu SDEM", align = "@{}p{3.4cm}p{5.4cm}p{2.4cm}p{1.8cm}p{4cm}@{}")
  write_overleaf_stargazer_table(sdem_rubin_pooled_tex, "wyborprzest_bez_logDrogi_SDEM_coefficients", "Współczynniki modelu SDEM po regułach Rubina")
  write_overleaf_stargazer_table(impact_table_for_tex(best_impacts), "SDEM_impacts", "Efekty bezpośrednie, pośrednie i całkowite modelu SDEM")
}

write_spatial_overleaf_tables()

# ============================================================
# 5. Diagnostyka reszt i mapy autokorelacji lokalnej
# ============================================================
# Liczymy Morana i LISA dla reszt OLS oraz modelu przestrzennego.
residual_diagnostics <- function(values, prefix) {
  z <- as.numeric(scale(values))
  wz <- lag.listw(cont_listw, z, zero.policy = TRUE)
  quadrant <- ifelse(
    z >= 0 & wz >= 0, "HH",
    ifelse(z < 0 & wz >= 0, "LH", ifelse(z < 0 & wz < 0, "LL", "HL"))
  )
  lisa <- localmoran(values, cont_listw, zero.policy = TRUE)
  lisa_p_col <- grep("^Pr", colnames(lisa), value = TRUE)[1]
  lisa_q <- ifelse(
    z >= 0 & wz >= 0, 1,
    ifelse(z < 0 & wz >= 0, 2, ifelse(z < 0 & wz < 0, 3, 4))
  )
  lisa_label <- ifelse(lisa[, lisa_p_col] < 0.05, c("HH", "LH", "LL", "HL")[lisa_q], "NS")
  data.frame(
    Kod = gmina$JPT_KOD_JE,
    prefix = prefix,
    value = values,
    z = z,
    wz = wz,
    moran_quadrant = quadrant,
    local_I = lisa[, "Ii"],
    local_I_p = lisa[, lisa_p_col],
    lisa_label = lisa_label,
    row.names = NULL
  )
}

resid_ols_diag <- residual_diagnostics(residuals(model_ols), "reszty_ols")
resid_best_diag <- residual_diagnostics(best_residuals, paste0("reszty_", tolower(best_model_name)))
ysaldo_diag <- residual_diagnostics(model_data$ySaldo, "ysaldo")
residual_diag <- rbind(resid_ols_diag, resid_best_diag)

# Zapisujemy wykres Morana, mape cwiartek i mape LISA.
moran_colors <- c(HH = "#d73027", LH = "#fdae61", LL = "#4575b4", HL = "#91bfdb")
lisa_colors <- c(HH = "#d73027", LL = "#4575b4", LH = "#fdae61", HL = "#91bfdb", NS = "#d9d9d9")

plot_moran_scatter_R(
  ysaldo_diag,
  "ysaldo_moran_scatter_quadrants_R",
  "Wykres rozrzutu I Morana: ySaldo",
  "ySaldo (z-score)",
  "Opóźnienie przestrzenne ySaldo"
)
plot_category_map_R(
  gmina,
  ysaldo_diag,
  "moran_quadrant",
  "ysaldo_moran_quadrant_map_R",
  "Mapa kwadrantów Morana: ySaldo",
  moran_colors,
  "Ćwiartka"
)
plot_category_map_R(
  gmina,
  ysaldo_diag,
  "lisa_label",
  "ysaldo_lisa_cluster_map_R",
  "Mapa klastrów LISA: ySaldo",
  lisa_colors,
  "LISA"
)

plot_moran_scatter_R(
  resid_ols_diag,
  "reszty_ols_moran_scatter_quadrants_R",
  "Wykres rozrzutu I Morana: reszty MNK",
  "reszty MNK (z-score)",
  "Opóźnienie przestrzenne reszt MNK"
)
plot_category_map_R(
  gmina,
  resid_ols_diag,
  "moran_quadrant",
  "reszty_ols_moran_quadrant_map_R",
  "Mapa kwadrantów Morana: reszty MNK",
  moran_colors,
  "Ćwiartka"
)
plot_category_map_R(
  gmina,
  resid_ols_diag,
  "lisa_label",
  "reszty_ols_lisa_cluster_map_R",
  "Mapa klastrów LISA: reszty MNK",
  lisa_colors,
  "LISA"
)

best_prefix <- paste0("reszty_", tolower(best_model_name))
plot_moran_scatter_R(
  resid_best_diag,
  paste0(best_prefix, "_moran_scatter_quadrants_R"),
  paste("Wykres rozrzutu I Morana:", best_prefix),
  paste(best_prefix, "(z-score)"),
  paste("Opóźnienie przestrzenne:", best_prefix)
)
plot_category_map_R(
  gmina,
  resid_best_diag,
  "moran_quadrant",
  paste0(best_prefix, "_moran_quadrant_map_R"),
  paste("Mapa kwadrantów Morana:", best_prefix),
  moran_colors,
  "Ćwiartka"
)
plot_category_map_R(
  gmina,
  resid_best_diag,
  "lisa_label",
  paste0(best_prefix, "_lisa_cluster_map_R"),
  paste("Mapa klastrów LISA:", best_prefix),
  lisa_colors,
  "LISA"
)

gmina$reszty_ols <- as.numeric(residuals(model_ols))
gmina$pred_ols <- as.numeric(fitted(model_ols))
gmina$reszty_best <- best_residuals
gmina$pred_best <- best_fitted
gmina$best_model <- best_model_name

best_diag_wide <- resid_best_diag[, c("Kod", "z", "wz", "moran_quadrant", "local_I", "local_I_p", "lisa_label")]
names(best_diag_wide) <- c(
  "JPT_KOD_JE",
  "best_resid_z",
  "best_resid_wz",
  "best_resid_quad",
  "best_local_I",
  "best_local_I_p",
  "best_lisa_label"
)
gmina <- merge(gmina, best_diag_wide, by = "JPT_KOD_JE", all.x = TRUE, sort = FALSE)

lm_rows <- data.frame(
  # Zbieramy testy LM/Rao do jednej tabeli, bo lm.RStests() zwraca liste obiektow htest.
  test = names(lm_tests),
  statistic = sapply(lm_tests, function(x) unname(x$statistic)),
  p_value = sapply(lm_tests, function(x) x$p.value),
  row.names = NULL
)

# ============================================================
# 6. Eksport tabel roboczych CSV i raportow tekstowych
# ============================================================
# Te pliki sa pelnym zapisem wynikow do sprawdzenia poza LaTeX-em.
write.csv(lm_rows, file.path(wydruki_dir, "wyborprzest_bez_logDrogi_lm_tests_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(comparison, file.path(wydruki_dir, "wyborprzest_bez_logDrogi_model_comparison_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")
anova_outputs <- list(
  # Zestawiamy wszystkie redukcje modeli w jednej liscie, zeby zapisac je jednym raportem.
  "GNS/Manski vs SAR" = anova_gns_sar,
  "SAR vs GNS/Manski" = anova_sar_gns,
  "GNS/Manski vs SEM" = anova_gns_sem,
  "GNS/Manski vs SDEM" = anova_gns_sdem,
  "GNS/Manski vs SDM" = anova_gns_sdm,
  "GNS/Manski vs SAC" = anova_gns_sac,
  "SAC vs SAR" = anova_sac_sar,
  "SAC vs SEM" = anova_sac_sem,
  "SDEM vs SAR" = anova_sdem_sar,
  "SDEM vs SLX" = anova_sdem_slx,
  "SDEM vs SEM" = anova_sdem_sem
)
sink(file.path(wydruki_dir, "wyborprzest_bez_logDrogi_anova_reductions_R.txt"), split = TRUE)
for (name in names(anova_outputs)) {
  cat("\nANOVA/LR:", name, "\n")
  print(anova_outputs[[name]])
}
sink()
write.csv(lr_tests, file.path(wydruki_dir, "wyborprzest_bez_logDrogi_lr_tests_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(wald_sdem_wx, file.path(wydruki_dir, "wyborprzest_bez_logDrogi_sdem_wald_wx_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(best_impacts, file.path(wydruki_dir, "wyborprzest_bez_logDrogi_best_model_impacts_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(residual_diag, file.path(wydruki_dir, "wyborprzest_bez_logDrogi_residual_lisa_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(ols_residuals_moran_alternative_rows, file.path(wydruki_dir, "wyborprzest_bez_logDrogi_ols_residuals_moran_alternatives_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(geary_rows, file.path(wydruki_dir, "wyborprzest_bez_logDrogi_geary_tests_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(join_rows, file.path(wydruki_dir, "wyborprzest_bez_logDrogi_joincount_tests_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(join_test_rows, file.path(wydruki_dir, "wyborprzest_bez_logDrogi_joincount_formal_tests_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")

bp_rows <- data.frame(
  # Breusch-Pagan dla reszt modelu analizowanego dalej, czyli wymuszonego SDEM.
  test = "Breusch-Pagan best model residuals",
  statistic = unname(best_bp$statistic),
  p_value = best_bp$p.value,
  parameter = unname(best_bp$parameter),
  method = best_bp$method,
  row.names = NULL
)
write.csv(bp_rows, file.path(wydruki_dir, "wyborprzest_bez_logDrogi_best_bp_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ============================================================
# 7. Tabele LaTeX publikowane do Overleaf
# ============================================================
# Zapisujemy tabele LaTeX uzywane w rozdziale o analizie przestrzennej.
test_estimate_value <- function(test_obj, name_pattern) {
  # Wyciaga pojedyncza wartosc estymatora z obiektu htest niezaleznie od pelnej nazwy pola.
  estimate_name <- grep(name_pattern, names(test_obj$estimate), value = TRUE)[1]
  if (is.na(estimate_name)) {
    return(NA_real_)
  }
  as.numeric(test_obj$estimate[[estimate_name]][1])
}

table_value_by_name <- function(df, name_pattern) {
  # Wyciaga pojedyncza wartosc z pierwszej kolumny pasujacej do wzorca nazwy.
  column_name <- grep(name_pattern, names(df), value = TRUE)[1]
  if (is.na(column_name)) {
    return(NA_real_)
  }
  as.numeric(df[[column_name]][1])
}

global_join_df <- as.data.frame(global_join, check.names = FALSE)
global_join_df$classification <- rownames(global_join_df)
# joincount.multi() zwraca kilka skladowych; do tabeli zbiorczej bierzemy wiersz laczny Jtot.
global_join_row <- if (any(global_join_df$classification == "Jtot")) {
  global_join_df[global_join_df$classification == "Jtot", , drop = FALSE][1, , drop = FALSE]
} else {
  global_join_df[1, , drop = FALSE]
}
global_join_statistic <- table_value_by_name(global_join_row, "Std|z-value|statistic")
global_join_p_value <- table_value_by_name(global_join_row, "^Pr|p.value|p_value")
if (is.na(global_join_p_value) && !is.na(global_join_statistic)) {
  # joincount.multi() w niektorych wersjach zwraca tylko z-value, wiec p-value liczymy z rozkladu normalnego.
  global_join_p_value <- 2 * pnorm(abs(global_join_statistic), lower.tail = FALSE)
}
global_auto_table <- data.frame(
  # Tabela laczy trzy klasyczne testy autokorelacji zmiennej zaleznej.
  test = c("I Morana", "C Geary'ego", "Join-count"),
  statistic = c(
    test_estimate_value(global_moran, "Moran I"),
    test_estimate_value(global_geary, "Geary C"),
    global_join_statistic
  ),
  p_value = c(as.numeric(global_moran$p.value[1]), as.numeric(global_geary$p.value[1]), global_join_p_value),
  method = c(as.character(global_moran$method[1]), as.character(global_geary$method[1]), "Join-count"),
  row.names = NULL
)
# Eksport do Overleaf wykonuje ponizszy blok w ukladzie zgodnym z tabela wzorcowa.

join_total_row <- function(join_obj) {
  # Wybiera laczny wiersz Jtot z wyniku joincount.multi().
  join_df <- as.data.frame(join_obj, check.names = FALSE)
  join_df$classification <- rownames(join_df)
  rownames(join_df) <- NULL
  if (any(join_df$classification == "Jtot")) {
    join_df[join_df$classification == "Jtot", , drop = FALSE][1, , drop = FALSE]
  } else {
    join_df[1, , drop = FALSE]
  }
}

join_p_value_from_row <- function(join_row) {
  # W razie braku p-value w obiekcie join-count liczy je z przyblizenia normalnego.
  statistic <- table_value_by_name(join_row, "Std|z-value|statistic")
  p_value <- table_value_by_name(join_row, "^Pr|p.value|p_value")
  if (is.na(p_value) && !is.na(statistic)) {
    p_value <- 2 * pnorm(abs(statistic), lower.tail = FALSE)
  }
  p_value
}

join_class_label <- function(classification, variable_name) {
  # Zamienia techniczne etykiety klas join-count na opis uzywany w tabelach.
  base_label <- ifelse(variable_name == "ySaldo", "ySaldo", "reszta")
  if (classification == "negative:negative") {
    return(paste0("$", base_label, " < 0$ -- $", base_label, " < 0$"))
  }
  if (classification == "nonnegative:nonnegative") {
    return(paste0("$", base_label, " >= 0$ -- $", base_label, " >= 0$"))
  }
  if (classification == "nonnegative:negative") {
    return(paste0("$", base_label, " >= 0$ -- $", base_label, " < 0$"))
  }
  classification
}

join_components_for_tex <- function(join_df, variable_name) {
  # Buduje zwarta tabele skladowych join-count bez kolumn technicznych.
  selected <- join_df[join_df$variable == variable_name, , drop = FALSE]
  data.frame(
    Klasyfikacja = vapply(selected$classification, join_class_label, character(1), variable_name = variable_name),
    Obserwowane = selected$Joincount,
    Oczekiwane = selected$Expected,
    Wariancja = selected$Variance,
    z = selected[["z-value"]],
    row.names = NULL,
    check.names = FALSE
  )
}

test_verdict <- function(p_value) {
  # Zwraca krotki wniosek dla testu przy poziomie istotnosci 5%.
  ifelse(is.na(p_value), "", ifelse(p_value < 0.05, "Odrzucamy H0", "Brak podstaw do odrzucenia H0"))
}

global_join_row <- join_total_row(global_join)
global_join_statistic <- table_value_by_name(global_join_row, "Std|z-value|statistic")
global_join_p_value <- join_p_value_from_row(global_join_row)
global_moran_i <- test_estimate_value(global_moran, "Moran I")
global_geary_c <- test_estimate_value(global_geary, "Geary C")
global_auto_table_pdf <- data.frame(
  # Tabela laczy trzy klasyczne testy autokorelacji zmiennej zaleznej w ukladzie z pracy.
  Test = c("I Morana", "C Geary'ego", "Join-count"),
  "Hipoteza zerowa H0" = c(
    "Brak dodatniej autokorelacji przestrzennej zmiennej ySaldo",
    "Brak autokorelacji przestrzennej; C = 1",
    "Losowe rozmieszczenie gmin wedlug znaku salda migracji"
  ),
  Statystyka = c(
    paste0("I=", format_number_tex(global_moran_i)),
    paste0("C=", format_number_tex(global_geary_c)),
    paste0("z=", format_number_tex(global_join_statistic))
  ),
  "p-value" = format_p_value_tex(c(as.numeric(global_moran$p.value[1]), as.numeric(global_geary$p.value[1]), global_join_p_value)),
  Werdykt = test_verdict(c(as.numeric(global_moran$p.value[1]), as.numeric(global_geary$p.value[1]), global_join_p_value)),
  row.names = NULL,
  check.names = FALSE
)
write_overleaf_table(global_auto_table_pdf, "globalna_autokorelacja_ysaldo", "Globalna autokorelacja przestrzenna zmiennej zaleznej ySaldo", align = "@{}p{3cm}p{5.2cm}p{3cm}p{2cm}p{2.8cm}@{}")

write_overleaf_table(join_components_for_tex(join_rows, "ySaldo"), "join_count_ysaldo_skladowe", "Skladowe testu join-count dla zmiennej zaleznej ySaldo", align = "@{}lrrrr@{}")

ols_moran_row <- ols_residuals_moran_alternative_rows[ols_residuals_moran_alternative_rows$alternative == "greater", , drop = FALSE][1, , drop = FALSE]
ols_geary_row <- geary_rows[geary_rows$variable == "reszty_ols", , drop = FALSE][1, , drop = FALSE]
ols_join_row <- join_total_row(ols_join)
ols_join_statistic <- table_value_by_name(ols_join_row, "Std|z-value|statistic")
ols_join_p_value <- join_p_value_from_row(ols_join_row)
ols_residual_autocorr_table <- data.frame(
  Test = c("I Morana dla reszt MNK", "C Geary'ego dla reszt MNK", "Join-count dla znaku reszt MNK"),
  H0 = c(
    "Brak dodatniej autokorelacji przestrzennej",
    "Brak autokorelacji przestrzennej; C = 1",
    "Losowy rozklad dodatnich i ujemnych reszt"
  ),
  H1 = c(
    "Wystepuje dodatnia autokorelacja przestrzenna",
    "Wystepuje autokorelacja przestrzenna",
    "Wystepuje przestrzenne grupowanie znakow reszt"
  ),
  Statystyka = c(
    paste0("I=", format_number_tex(ols_moran_row$moran_i)),
    paste0("C=", format_number_tex(ols_geary_row$statistic)),
    paste0("z=", format_number_tex(ols_join_statistic))
  ),
  "p-value" = format_p_value_tex(c(ols_moran_row$p_value, ols_geary_row$p_value, ols_join_p_value)),
  row.names = NULL,
  check.names = FALSE
)
write_overleaf_table(ols_residual_autocorr_table, "testy_autokorelacji_reszt_mnk", "Testy autokorelacji przestrzennej dla reszt modelu MNK", align = "@{}p{3cm}p{3.5cm}p{3.5cm}p{2.8cm}p{1.8cm}@{}")

write_overleaf_table(join_components_for_tex(join_rows, "reszty_ols"), "join_count_reszty_mnk_skladowe", "Skladowe testu join-count dla znaku reszt modelu MNK", align = "@{}lrrrr@{}")

lm_interpretation <- function(test_name, p_value) {
  # Dopisuje krotka interpretacje testow LM/Rao.
  if (test_name == "RSerr") {
    return(ifelse(p_value < 0.05, "Istotna zaleznosc typu spatial error w tescie podstawowym", "Brak istotnosci typu spatial error w tescie podstawowym"))
  }
  if (test_name == "RSlag") {
    return(ifelse(p_value < 0.05, "Istotna zaleznosc typu spatial lag w tescie podstawowym", "Brak istotnosci typu spatial lag w tescie podstawowym"))
  }
  if (test_name == "adjRSerr") {
    return(ifelse(p_value < 0.05, "Po korekcie pozostaje istotny skladnik spatial error", "Po korekcie brak istotnosci dla czystego spatial error"))
  }
  if (test_name == "adjRSlag") {
    return(ifelse(p_value < 0.05, "Po korekcie pozostaje istotny skladnik spatial lag", "Po korekcie brak istotnosci dla czystego spatial lag"))
  }
  ifelse(p_value < 0.05, "Laczny test wskazuje na istotna zaleznosc przestrzenna", "Laczny test nie wskazuje na istotna zaleznosc przestrzenna")
}
lm_rows_tex <- data.frame(
  Test = lm_rows$test,
  Statystyka = lm_rows$statistic,
  "p-value" = lm_rows$p_value,
  Interpretacja = mapply(lm_interpretation, lm_rows$test, lm_rows$p_value),
  row.names = NULL,
  check.names = FALSE
)
write_overleaf_table(lm_rows_tex, "testy_lm_rao_wybor_modelu", "Testy Rao score/LM dla wyboru typu modelu przestrzennego", align = "@{}lrrp{6.2cm}@{}")

comparison_tex <- comparison[, c("model", "AIC", "BIC", "logLik")]
names(comparison_tex) <- c("Model", "AIC", "BIC", "Log-likelihood")
write_overleaf_table(comparison_tex, "porownanie_modeli_przestrzennych_aic_bic_loglik", "Porownanie modeli przestrzennych wedlug kryteriow informacyjnych", align = "lrrr")

anova_lr_row <- function(nm) {
  # Z pojedynczego wyniku ANOVA/LR zostawia tylko wiersz decyzyjny.
  anova_df <- as.data.frame(anova_outputs[[nm]], check.names = FALSE)
  lr_col <- grep("L.Ratio|statistic", names(anova_df), value = TRUE)[1]
  p_col <- grep("^p-value$|p_value|Pr", names(anova_df), value = TRUE)[1]
  df_col <- grep("^df$|^Df$", names(anova_df), value = TRUE)[1]
  lr_values <- suppressWarnings(as.numeric(anova_df[[lr_col]]))
  p_values <- suppressWarnings(as.numeric(anova_df[[p_col]]))
  df_values <- if (!is.na(df_col)) suppressWarnings(as.numeric(anova_df[[df_col]])) else NA_real_
  lr_value <- lr_values[is.finite(lr_values)][1]
  p_value <- p_values[is.finite(p_values)][1]
  delta_df <- if (sum(is.finite(df_values)) >= 2) {
    abs(diff(df_values[is.finite(df_values)][1:2]))
  } else {
    df_values[is.finite(df_values)][1]
  }
  simple_model <- trimws(sub("^.* vs ", "", nm))
  comparison_name <- gsub("GNS/Manski", "GNS", nm, fixed = TRUE)
  simple_model <- gsub("GNS/Manski", "GNS", simple_model, fixed = TRUE)
  data.frame(
    Porownanie = comparison_name,
    "Model prostszy" = simple_model,
    "Delta df" = delta_df,
    LR = lr_value,
    "p-value" = p_value,
    Wniosek = ifelse(p_value < 0.05, "brak podstaw do redukcji", "redukcja dopuszczalna"),
    row.names = NULL,
    check.names = FALSE
  )
}
# Bierzemy nazwy z istniejacej listy wynikow ANOVA/LR, zeby eksport nie zalezal
# od recznie utrzymywanej zmiennej pomocniczej.
lr_names <- names(anova_outputs)
anova_lr_redukcja_pdf <- dplyr::bind_rows(lapply(lr_names, anova_lr_row))
write_overleaf_table(anova_lr_redukcja_pdf, "anova_lr_redukcja_gns", "Testy ANOVA/LR dla redukcji modeli GNS i SDEM", align = "lrrrrl")

wald_sdem_wx_tex <- data.frame(
  Test = "Test Walda komponentu WX modelu SDEM",
  "Hipoteza zerowa H0" = "Wszystkie parametry przy przestrzennie opoznionych zmiennych objasniajacych sa rowne zero",
  Statystyka = wald_sdem_wx$statistic,
  df = wald_sdem_wx$df,
  "p-value" = wald_sdem_wx$p_value,
  Wniosek = ifelse(wald_sdem_wx$p_value < 0.05, "komponent WX istotny", "brak lacznej istotnosci komponentu WX"),
  row.names = NULL,
  check.names = FALSE
)
write_overleaf_table(wald_sdem_wx_tex, "testy_komponentow_SDEM", "Test komponentu WX modelu SDEM", align = "@{}p{3.4cm}p{6cm}rrrp{3.2cm}@{}")

pseudo_r2_tex <- comparison[order(-comparison$pseudo_R2), c("model", "pseudo_R2")]
names(pseudo_r2_tex) <- c("Model", "Pseudo R2")
write_overleaf_table(pseudo_r2_tex, "porownanie_modeli_pseudo_r2", "Porownanie modeli przestrzennych wedlug pseudo R2")

best_moran <- moran.test(best_residuals, cont_listw, zero.policy = TRUE)
# Diagnostyka reszt SDEM laczy autokorelacje przestrzenna i heteroskedastycznosc.
best_residual_tests <- rbind(
  data.frame(
    test = paste("I Morana reszt", best_model_name),
    statistic = test_estimate_value(best_moran, "Moran I"),
    p_value = as.numeric(best_moran$p.value[1]),
    parameter = NA_real_,
    method = as.character(best_moran$method[1]),
    row.names = NULL
  ),
  bp_rows
)
best_residual_tests_tex <- data.frame(
  # Tabela koncowa ma uklad z pracy: test, hipoteza, statystyka, p-value i wniosek.
  Test = c(paste("I Morana reszt", best_model_name), "Breusch-Pagan dla reszt SDEM"),
  "Hipoteza zerowa H0" = c(
    "Brak autokorelacji przestrzennej reszt",
    "Homoskedastycznosc skladnika losowego"
  ),
  Statystyka = c(
    paste0("I=", format_number_tex(test_estimate_value(best_moran, "Moran I"))),
    format_number_tex(unname(best_bp$statistic))
  ),
  "p-value" = format_p_value_tex(c(as.numeric(best_moran$p.value[1]), best_bp$p.value)),
  Wniosek = c(
    ifelse(as.numeric(best_moran$p.value[1]) < 0.05, "reszty skorelowane przestrzennie", "brak istotnej autokorelacji reszt"),
    ifelse(best_bp$p.value < 0.05, "heteroskedastycznosc pozostaje", "brak podstaw do odrzucenia homoskedastycznosci")
  ),
  row.names = NULL,
  check.names = FALSE
)
write_overleaf_table(best_residual_tests_tex, "testy_morana_bp_reszty_SDEM", "Diagnostyka reszt modelu SDEM", align = "@{}p{3.4cm}p{5.4cm}p{2.4cm}p{1.8cm}p{4cm}@{}")
write_overleaf_stargazer_table(sdem_rubin_pooled_tex, "wyborprzest_bez_logDrogi_SDEM_coefficients", "Współczynniki modelu SDEM po regułach Rubina")
write_overleaf_stargazer_table(impact_table_for_tex(best_impacts), "SDEM_impacts", "Efekty bezpośrednie, pośrednie i całkowite modelu SDEM")

# ============================================================
# 8. Warstwa przestrzenna i koncowy raport tekstowy
# ============================================================
# Warstwa GPKG przechowuje reszty, dopasowania i lokalne statystyki.
st_write(
  gmina,
  file.path(wydruki_dir, "wyborprzest_bez_logDrogi_best_model_R.gpkg"),
  layer = "best_model_residuals",
  delete_layer = TRUE,
  quiet = TRUE
)

# Raport tekstowy zbiera najwazniejsze wydruki konsolowe z calego skryptu.
sink(file.path(wydruki_dir, "wyborprzest_bez_logDrogi_summary_R.txt"), split = TRUE)
cat("WYBOR MODELU PRZESTRZENNEGO\n\n")
cat("Dane: benchmark: best MNK, bez drog oraz bez Odleglosc i Odleglosc2.\n")
cat("Zmienne niezalezne:", paste(x_cols, collapse = ", "), "\n")
cat("Liczba obserwacji:", nrow(model_data), "\n\n")
cat("Globalne statystyki przestrzenne ySaldo:\n")
print(global_moran)
print(global_geary)
print(global_join)
print(global_join_test)
cat("\n")
cat("Moran reszt OLS:\n")
print(lm_moran)
cat("\nMoran reszt OLS dla alternatyw greater / two.sided / less:\n")
print(ols_residuals_moran_alternative_rows)
cat("\nGeary C dla reszt OLS:\n")
print(ols_geary)
cat("\nJoin-count dla znaku reszt OLS:\n")
print(ols_join)
print(ols_join_test)
cat("\nTabele Moran, Geary C i join-count zapisane do CSV:\n")
print(ols_residuals_moran_alternative_rows)
print(geary_rows)
print(join_rows)
print(join_test_rows)
cat("\nTesty LM/Rao score:\n")
print(lm_tests)
cat("\nPorownanie modeli:\n")
print(comparison)
cat("\nANOVA / LR redukcji modeli przestrzennych:\n")
for (name in names(anova_outputs)) {
  cat("\nANOVA/LR:", name, "\n")
  print(anova_outputs[[name]])
}
cat("\nModel wskazany przez kryteria:", selected_model_name, "\n")
cat("\nModel analizowany dalej:", best_model_name, "\n")
print(summary(best_model))
cat("\nTesty LR:\n")
print(lr_tests)
cat("\nTest Walda dla lacznej istotnosci komponentu WX w modelu SDEM:\n")
print(wald_sdem_wx)
cat("\nSDEM po 10 imputacjach MICE - reguly Rubina:\n")
cat("Dla kazdego parametru policzono qbar=mean(Q_i), ubar=mean(SE_i^2), b=var(Q_i), total_var=ubar+(1+1/m)*b oraz SE=sqrt(total_var).\n")
cat("Kolumny riv/lambda/fmi pokazuja odpowiednio wzrost wariancji, udzial wariancji miedzy imputacjami i frakcje brakujacej informacji.\n")
print(sdem_rubin_pooled)
cat("\nEfekty najlepszego modelu:\n")
print(best_impacts)
cat("\nBreusch-Pagan dla reszt najlepszego modelu:\n")
print(best_bp)
cat("\nMoran i LISA dla reszt OLS oraz najlepszego modelu - zapisane do CSV:\n")
print(table(residual_diag$prefix, residual_diag$lisa_label))
sink()

print(comparison)
cat("\nModel wskazany przez kryteria:", selected_model_name, "\n")
cat("\nModel analizowany dalej:", best_model_name, "\n")
cat("\nSDEM po 10 imputacjach MICE - reguly Rubina:\n")
cat("qbar=mean(Q_i), ubar=mean(SE_i^2), b=var(Q_i), total_var=ubar+(1+1/m)*b, SE=sqrt(total_var).\n")
print(sdem_rubin_pooled)
