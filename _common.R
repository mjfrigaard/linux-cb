# upgrade pak
# pak::pak('mjfrigaard/shinypak', ask = FALSE, upgrade = TRUE)

options(width = 50L, pillar.width = 50L)

readme_stuff <- list.files(
    path = "data",
    pattern = "html|files",
    full.names = TRUE,
    include.dirs = TRUE,
    all.files = TRUE)

sapply(X = readme_stuff, FUN = unlink, force = TRUE, recursive = TRUE)

co_box <- function(
  color = "b",
  header = "header",
  contents = "Your text",
  size = "1.05",
  hsize = "1.10",
  fold = FALSE,
  look = "default") {
  
  if (look == "simple") {
    look <- "simple"
  } else if (look == "minimal") {
    look <- "minimal"
  } else {
    look <- "default"
  }
  
  fold <- tolower(as.character(fold))
  size <- as.character(size)
  
  class <- switch(color,
    b = "note",
    g = "tip",
    r = "important",
    o = "warning",
    y = "caution",
    stop("Invalid `type`", call. = FALSE)
  )
  
  switch(color,
    b = cat(paste0(
      "\n\n",
      ":::: {.callout-", class, " collapse='", fold, "'", " appearance='", look, "' icon=false}", "\n\n",
      "## [", header, "]{style='font-weight: bold; font-size: ", hsize, "em;'}\n\n",
      "::: {style='font-size: ", size, "em; color: #282b2d;'}\n\n",
      "\n", glue::glue_collapse(contents), "\n\n",
      "::: \n\n",
      "::::", "\n"
    )),
    g = cat(paste0(
      "\n\n",
      ":::: {.callout-", class, " collapse='", fold, "'", " appearance='", look, "' icon=false}", "\n\n",
      "## [", header, "]{style='font-weight: bold; font-size: ", hsize, "em;'}\n\n",
      "::: {style='font-size: ", size, "em; color: #282b2d;'}\n\n",
      "\n", glue::glue_collapse(contents), "\n\n",
      "::: \n\n",
      "::::", "\n"
    )),
    y = cat(paste0(
      "\n\n",
      ":::: {.callout-", class, " collapse='", fold, "'", " appearance='", look, "' icon=false}", "\n\n",
      "## [", header, "]{style='font-weight: bold; font-size: ", hsize, "em;'}\n\n",
      "::: {style='font-size: ", size, "em; color: #282b2d;'}\n\n",
      "\n", glue::glue_collapse(contents), "\n\n",
      "::: \n\n",
      "::::", "\n"
    )),
    o = cat(paste0(
      "\n\n",
      ":::: {.callout-", class, " collapse='", fold, "'", " appearance='", look, "' icon=false}", "\n\n",
      "## [", header, "]{style='font-weight: bold; font-size: ", hsize, "em;'}\n\n",
      "::: {style='font-size: ", size, "em; color: #282b2d;'}\n\n",
      "\n", glue::glue_collapse(contents), "\n\n",
      "::: \n\n",
      "::::", "\n"
    )),
    r = cat(paste0(
      "\n\n",
      ":::: {.callout-", class, " collapse='", fold, "'", " appearance='", look, "' icon=false}", "\n\n",
      "## [", header, "]{style='font-weight: bold; font-size: ", hsize, "em;'}\n\n",
      "::: {style='font-size: ", size, "em; color: #282b2d;'}\n\n",
      "\n", glue::glue_collapse(contents), "\n\n",
      "::: \n\n",
      "::::", "\n"
    )),
    stop("Invalid `type`", call. = FALSE)
  )
}

git_contrib_box <- function(
                repo = 'fm-unix', 
                header = "See a typo, error, or something missing?",
                contents = "Please open an issue on ",
                hsize = "1.15", 
                size = "1.00", 
                fold = FALSE) {
  
  git_repo_root <- "https://github.com/mjfrigaard/"
  new_issue <- "/issues/new"
  
  fold <- tolower(fold)
  
  gh_repo_link <- paste0("[GitHub.]", "(", git_repo_root, repo, new_issue, ")")
  gh_repo_link
  
  cat(paste0(
        "\n\n",
        ":::: {.callout-note", " collapse='", fold, "'", " appearance='simple' icon=false}", "\n\n",
        "## [", header, "]{style='font-weight: bold; font-size: ", hsize, "em;'}\n\n",
        "::: {style='font-size: ", size, "em; color: #282b2d;'}\n\n",
        "\n", 
        glue::glue_collapse(contents), 
        gh_repo_link,
        "\n\n",
        "::: \n\n",
        "::::", "\n"
      ))
  
  
}

hot_key <- function(fun = "L") {
  if (fun == "all") {
glue::glue("\n:::: {{layout='[ 15, 33, 16 ]'}}

::: {{#first-column}}

:::

::: {{#second-column}}

::: {{style='font-weight: bold; font-size: 1.15em' layout-valign='bottom'}}

<br><br>

<kbd>Ctrl/Cmd</kbd> + <kbd>Shift</kbd> + <kbd>L</kbd> / <kbd>D</kbd> / <kbd>B</kbd>
:::

:::

::: {{#third-column}}

:::

::::")
  } else if (fun == 'tf') {
glue::glue("\n:::: {{layout='[ 30, 50, 20 ]'}}

::: {{#first-column}}

:::

::: {{#second-column}}

::: {{style='font-weight: bold; font-size: 1.15em' layout-valign='bottom'}}

<br>

<kbd>Ctrl/Cmd</kbd> + <kbd>T</kbd>
:::

:::

::: {{#third-column}}

:::

::::")
  } else if (fun == 'cf') {
glue::glue("\n:::: {{layout='[ 30, 50, 20 ]'}}

::: {{#first-column}}

:::

::: {{#second-column}}

::: {{style='font-weight: bold; font-size: 1.15em' layout-valign='bottom'}}

<br>

<kbd>Ctrl/Cmd</kbd> + <kbd>Shift</kbd> + <kbd>R</kbd>
:::

:::

::: {{#third-column}}

:::

::::")
  } else {
glue::glue("\n:::: {{layout='[ 30, 50, 20 ]'}}

::: {{#first-column}}

:::

::: {{#second-column}}

::: {{style='font-weight: bold; font-size: 1.25em' layout-valign='bottom'}}

<br>

<kbd>Ctrl/Cmd</kbd> + <kbd>Shift</kbd> + <kbd>{fun}</kbd>
:::

:::

::: {{#third-column}}

:::

::::")
  }
}
