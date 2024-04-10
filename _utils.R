# install.packages('xml2', 'rvest', 'tidyverse')
library(xml2)
library(rvest)
library(tidyverse)
library(janitor)

scrape_wiki_tbl <- function(wiki_url, tbl_string) {
  wiki_html <- xml2::read_html(wiki_url)
  wiki_html_tables <- wiki_html %>% rvest::html_nodes(css = "table")
  relevant_tables <- wiki_html_tables[base::grep(tbl_string, wiki_html_tables)]
   janitor::clean_names(
     rvest::html_table(relevant_tables[[1]], fill = TRUE)
   )
}

# download wikipedia tables for text manipulation
video_game_hof <- scrape_wiki_tbl(
  wiki_url = 'https://en.wikipedia.org/wiki/World_Video_Game_Hall_of_Fame', 
  tbl_string = "Inductees")
video_game_hof <- video_game_hof |> 
  dplyr::mutate(
    game = str_remove_all(game, "\\*")
  ) |> 
  dplyr::select(!ends_with('ref'))
vroom::vroom_write(
  x = video_game_hof, 
  file = "data/vg_hof.tsv", 
  delim = "\t", 
  eol = '\n')


tallest_trees <- scrape_wiki_tbl(
  wiki_url = 'https://en.wikipedia.org/wiki/List_of_tallest_trees', 
  tbl_string = "trees")
trees <- dplyr::filter(tallest_trees, 
  # drop first row
  class != "Class") |> 
  dplyr::rename(
    ht_meters = height,
    ht_feet = height_2
  ) |> 
  dplyr::mutate(
    name = dplyr::if_else(
      condition = tree_name == "", true = 
      NA_character_, 
      false = tree_name)
  ) |> 
  tidyr::separate(col = species, 
    into = c('tree', 'species'), 
    sep = "\\(") |> 
  dplyr::mutate(
    tree = stringr::str_trim(tree, "both"),
    species = stringr::str_remove_all(species, "note 1"),
    species = stringr::str_remove_all(species, "\\)|\\[|\\]"),
  ) |> 
  dplyr::select(
    tree, species, class, 
    dplyr::starts_with('ht'),
    location, 
    continent,
    name, 
    !dplyr::contains('ref'), 
    -tree_name)
  
vroom::vroom_write(
  x = trees, 
  file = "data/trees.tsv", 
  delim = "\t", 
  eol = '\n')

music_videos <- scrape_wiki_tbl(
    wiki_url = "https://en.wikipedia.org/wiki/List_of_most_expensive_music_videos",
    tbl_string = "Most")
music_videos <- dplyr::filter(music_videos, # drop first row
  rank != "Rank") |> 
  dplyr::rename(
    cost_nominal = cost_est,
    cost_adj = cost_est_2,
    artists = artist_s
  ) |> 
dplyr::mutate(
    title = stringr::str_remove_all(title, '\"')
  ) |> 
  dplyr::select(!contains('ref'))
# music_videos |> View()
vroom::vroom_write(
  x = music_videos, 
  file = "data/music_vids.tsv", 
  delim = "\t", 
  eol = '\n')