# Publishing helper: copies selected outputs into the canonical locations
# consumed by bezjc.tex (media/, tabele/, stargazer/ at project root).
# The original setup wrote everything to a separate Overleaf/ tree; the
# consolidated repo now uses these dirs directly.

.publisher_env <- new.env(parent = emptyenv())
.publisher_env$project_dir <- NULL

init_maintest_publisher <- function(project_dir, overleaf_dir = NULL) {
  .publisher_env$project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  for (sub in c("media", "tabele", "stargazer")) {
    dir.create(file.path(.publisher_env$project_dir, sub),
               showWarnings = FALSE, recursive = TRUE)
  }
  invisible(NULL)
}

publish_if_maintest_uses <- function(path, kind = "tabele") {
  # kind in {"tabele", "stargazer", "media"} — copy file to <project>/<kind>/
  # with its existing basename.
  if (is.null(.publisher_env$project_dir) || !file.exists(path)) {
    return(invisible(path))
  }
  dest_dir <- file.path(.publisher_env$project_dir, kind)
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(dest_dir, basename(path))
  ok <- tryCatch(file.copy(path, dest, overwrite = TRUE),
                 error = function(e) FALSE)
  invisible(if (isTRUE(ok)) dest else path)
}
