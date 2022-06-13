

#' Normalize file directory
#'
#' Normalize file directory
#'
#' Simple wrapper over `normalizePath()` that preserves NULLs and applies
#' pandoc-friendly defaults, and is intended to operate only on the directory
#' of a file and not the filename itself.
#'
#' It may have different output compared to `normalize_path()`
#' in these scenarios:
#'
#' If `path` is a symbolic link to a file, the path returned will
#' use the directory name of the symlinked file, and the actual name
#' of the symlink file. In contrast, `normalizePath()` and `normalize_path()`
#' both return the directory name of the source file, and the file
#' name of the source file.
#'
#' Consider this example:
#'
#' Symbolic link file `"symlink.rmd"` is pointing to the source file
#' which is located at this path:
#' `"/some_folder/elsewhere/source_file.rmd"`.
#'
#' This file may be described as follows:
#'
#' `symlink.rmd -> /some_folder/elsewhere/source_file.rmd`
#'
#' In this case:
#'
#' `normalizaPath("symlink.rmd")` will return:
#'
#' > `"/some_folder/elsewhere/source_file.rmd"`
#'
#' Instead, `normalize_directory("symlink.rmd")` will return:
#'
#' > `"/current_path/here/symlink.rmd"`
#'
#' @return `character` path to the input `path`. When input `path` is `NULL`,
#'    this function returns `NULL`.
#'
#' @param path `character` path to a file, directory, or symlink.
#' @param winslash,mustWork arguments are passed to `base::normalizePath()`.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' devtools::load_all()
#'
#' normalize_directory("LICENSE.md")
#' normalize_path("LICENSE.md")
#' normalizePath("LICENSE.md")
#'
#' normalize_directory(c("LICENSE.md", "R"))
#' normalize_directory(".")
#'
#' normalize_directory(".././rmarkdown")
#' normalize_path(".././rmarkdown")
#' normalizePath(".././rmarkdown")
#'
#' normalize_directory("../")
#' normalize_directory("./../rmarkdown")
#'
#' file.exists(normalize_directory("LICENSE.md"))
#' file.exists(normalize_directory("."))
#'
#' # compare missing file output to normalizePath()
#' normalizePath("../LICENSE.md")
#' normalize_directory("../LICENSE.md")
#'
#' normalizePath("../LICENSE.md", mustWork=TRUE)
#' normalize_directory("../LICENSE.md", mustWork=TRUE)
#'
#' # demonstrate utility with symlinks
#' temp_path <- tempdir();
#' temp_subdir <- file.path(temp_path, "source_subdir")
#' project_subdir <- file.path(temp_path, "project_subdir")
#' temp_file <- file.path(temp_subdir, "temp_file.rmd")
#' temp_file
#' symlink_file <- file.path(project_subdir, "symlink_file.rmd")
#' symlink_file
#'
#' dir.create(temp_subdir)
#' dir.create(project_subdir)
#' file.create(temp_file)
#' file.symlink(from=temp_file, to=symlink_file)
#'
#' file.exists(c(temp_file, symlink_file))
#' normalizePath(c(temp_file, symlink_file))
#' normalize_directory(c(temp_file, symlink_file))
#'
#' file.exists(normalize_directory(c(temp_file, symlink_file)))
#' unlink(c(temp_file, symlink_file))
#' unlink(c(temp_subdir, project_subdir))
#'
#' @export
normalize_directory <- function
(path,
 winslash = "/",
 mustWork = NA,
 ...)
{
  if (!is.null(path)) {
    path_dirname <- dirname(path);
    path_basename <- basename(path);
    if (dir.exists(path) || path_dirname == path_basename) {
      normalizePath(path = path,
        winslash = winslash,
        mustWork = mustWork)
    } else {
      outpath <- file.path(
        normalizePath(path = path_dirname,
          winslash = winslash,
          mustWork = mustWork),
        path_basename)
      if (mustWork %in% TRUE) {
        outpath_exists <- file.exists(outpath);
        if (any(!outpath_exists)) {
          which_err <- head(which(!outpath_exists), 1);
          stop(paste0("path[",
            which_err,
            ']="',
            outpath[which_err],
            '": No such file or directory'
            ))
        }
      }
      outpath
    }
  }
}

#' Simple wrapper to abs_path to accomodate symlinks
#'
#' Simple wrapper to abs_path to accomodate symlinks
#'
#' `normalizePath()` doesn't work if the path contains Unicode characters that
#' cannot be represented in the current system locale, even if the file exists.
#' This function uses `normalize_directory()` to try to accomodate
#' symlink filenames. If it fails, it calls `fs::path_abs()` which
#' appears to be a more robust equivalent to `normalize_directory()`.
#'
#' @param x `character` path to file.
#'
#' @export
abs_directory <- function
(x)
{
  if (!file.exists(x)) {
    stop("The file '", x, "' does not exist.")
  }

  res <- normalize_directory(path = x,
    mustWork = FALSE)

  if (file.exists(res)) return(res)
  if (!requireNamespace('fs', quietly = TRUE)) warning(
    'normalizePath() cannot make the path(s) absolute. The fs package is required.'
  )
  as.character(fs::path_abs(x))
}
