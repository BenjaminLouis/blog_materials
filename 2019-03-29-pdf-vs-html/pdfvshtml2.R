library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(purrr)
library(pdftools)


#### PDF
  

metapdf <- pdf_data("static/blog/2019-03-29-pdf-vs-html_files/test_form.pdf") %>%
  bind_rows(.id = "page") %>%
  group_by(page, y) %>%
  mutate(lines = str_c(text, collapse = " ")) %>%
  filter(x == min(x) & height == 10) %>% 
  ungroup() %>%
  mutate(sentences_group = paste0("sent", cumsum(str_detect(lines, "(^\\d{1,2}\\.\\s|^[:upper:]([:lower:]|\\s))")))) %>%
  group_by(sentences_group) %>%
  mutate(sentences = str_c(lines, collapse = " ")) %>%
  distinct(sentences_group, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(question_bloc = paste0("bloc", cumsum(str_detect(lines, "^\\d{1,2}\\.\\s")))) %>%
  filter(question_bloc != "bloc0") %>%
  group_by(question_bloc) %>%
  mutate(types = case_when("Check all that apply." %in% sentences ~ "multiple",
                           "Mark only one oval." %in% sentences ~ "unique",
                           "Mark only one oval per row." %in% sentences ~ "grid",
                           length(sentences) == 1 ~ "free")) %>%
  filter(!is.element(sentences, c("Check all that apply.", "Mark only one oval.",
                                  "Mark only one oval per row."))) %>%
  mutate(id = 1:n()) 

columns <- metapdf %>% 
  filter(types == "grid" & id == 2) %>% 
  pull(sentences) %>% 
  str_split("\\s") %>% 
  unlist
rows <- metapdf %>% 
  filter(types == "grid" & !id %in% 1:2) %>% 
  pull(sentences)
#mutate_at
  #filter(!(types == "grid" & id == 2)) %>%
metapdf <- metapdf %>% 
  mutate(choices = list(sentences[-1])) %>% 
  ungroup %>%
  filter(id == 1) %>%
  select(quest = sentences, types, choices) 

metapdf$choices[metapdf$types == "grid"] <- list(c(row = rows, column = columns))

# Cleaning
metapdf <- metapdf %>% mutate(quest = str_replace(quest, "\\*", "") %>% str_trim)


#### HTML

library(rvest)


# Importing html form
form <- xml2::read_html("static/blog/2019-03-29-pdf-vs-html_files/test_form.html")


questchoices <- form %>%
  html_nodes(".freebirdFormeditorViewItemContent") %>%
  keep(~html_nodes(.x, ".freebirdFormeditorViewItemMinimizedTitleRow") %>% 
         html_text() %>% 
         length() != 0)


quest <- questchoices %>%
  map(~html_nodes(.x, ".freebirdFormeditorViewItemMinimizedTitleRow") %>% 
        html_text()) 

# Isolating possible choices
get_choices_and_types <- function(.nd) {
  
  # Multiple choices
  if (length(html_nodes(.nd, ".freebirdFormeditorViewQuestionBodyChoicelistbodyOmniList")) != 0) {
    
    choices <- html_nodes(.nd, ".exportInput") %>%
      keep(~html_attr(.x, "aria-label") == "option value") %>% 
      html_attr(.,"value")
    # For "Other" option
    other <- html_nodes(.nd, ".freebirdFormeditorViewOmnilistListPlaceholderOption") %>% 
      html_attr(., "style") != "display:none"
    if (other) choices <- c(choices, "Other:")
    
    dltype <- html_nodes(.nd, ".freebirdFormeditorViewQuestionBodyChoicelistbodyOmniList") %>% 
      html_attr("data-list-type")
    if (dltype == "1") type <- "multiple" else type <- "unique"
    
  } else if (length(html_nodes(.nd, ".freebirdFormeditorViewQuestionBodyGridbodyRow")) != 0) {
    
    choices <- c(
      row = html_nodes(.nd, xpath = './/*[@class="freebirdFormeditorViewQuestionBodyGridbodyRow"]') %>%
        html_text(),
      column = html_nodes(.nd, ".freebirdFormeditorViewQuestionBodyGridbodyColumnHeader") %>%
        html_children() %>% 
        keep(~html_attr(.x, "class") == "freebirdFormeditorViewQuestionBodyGridbodyCell") %>% 
        html_text()
    )
    
    type <- "grid"
    
  } else {
    choices <- character(0)
    type <- "free"
  }
  return(tibble(types = type, choices = list(choices)))
}
choices_types <- map_dfr(questchoices, get_choices_and_types)


# Saving in a tibble
metahtml <- bind_cols(quest = paste0(1:9, ". ", unlist(quest)), choices_types)

# Cleaning
metahtml <- metahtml %>% mutate(quest = str_replace(quest, "\\*", "") %>% str_trim)

# IDENTIQUE ?
map2(metapdf, metahtml, all.equal)



# #### HTML
# 
# xpath <- function(.cl, exact = FALSE) {
#   if (exact) {
#     paste0('.//*[@class = \"', .cl, '\"]')
#   } else {
#     paste0('.//*[contains(concat( \" \", @class, \" \" ), concat( \" \", \"', .cl, '\", \" \" ))]')
#   }
# }
# 
# 
# # Importing html form
# form <- xml2::read_html("static/blog/2019-03-29-pdf-vs-html_files/test_form.html")
# 
# # Getting question-choices elements
# questchoices <- form %>%
#   xml_find_all(xpath("freebirdFormeditorViewItemContent")) %>%
#   keep(~xml_find_all(.x, xpath("freebirdFormeditorViewItemMinimizedTitleRow")) %>% 
#          xml_text %>% 
#          length != 0)
#   
# # Isolating questions
# quest <- questchoices %>%
#   map(~xml_find_all(.x, xpath("freebirdFormeditorViewItemMinimizedTitleRow")) %>% 
#         xml_text) 
#   
# 
# # Isolating possible choices
# get_choices_and_types <- function(.nd) {
#   
#   # Multiple choices
#   if (length(xml_find_all(.nd, xpath("freebirdFormeditorViewQuestionBodyChoicelistbodyOmniList"))) != 0) {
#     
#     choices <- xml_find_all(.nd, xpath("exportInput")) %>%
#       keep(~xml_attr(.x,"aria-label") == "option value") %>% 
#       xml_attr(.,"value")
#     # For "Other" option
#     other <- xml_find_all(.nd, xpath("freebirdFormeditorViewOmnilistListPlaceholderOption")) %>% 
#       xml_attr(., "style") != "display:none"
#     if (other) choices <- c(choices, "Other:")
#     
#     dltype <- xml_find_all(.nd, xpath("freebirdFormeditorViewQuestionBodyChoicelistbodyOmniList")) %>% 
#       xml_attr("data-list-type")
#     if (dltype == "1") type <- "multiple" else type <- "unique"
#     
#   } else if (length(xml_find_all(.nd, xpath("freebirdFormeditorViewQuestionBodyGridbodyRow"))) != 0) {
#     
#     choices <- c(
#       row = xml_find_all(.nd, xpath("freebirdFormeditorViewQuestionBodyGridbodyRow", exact = TRUE)) %>%
#         xml_text,
#       column = xml_find_all(.nd, xpath("freebirdFormeditorViewQuestionBodyGridbodyColumnHeader")) %>%
#         xml_children %>% 
#         keep(~xml_attr(.x, "class") == "freebirdFormeditorViewQuestionBodyGridbodyCell") %>% 
#         xml_text
#     )
#     
#     type <- "grid"
#     
#   } else {
#     choices <- character(0)
#     type <- "free"
#   }
#   return(tibble(types = type, choices = list(choices)))
# }
# choices_types <- map_dfr(questchoices, get_choices_and_types)
# 
# 
# # Saving in a tibble
# metahtml <- bind_cols(quest = paste0(1:9, ". ", unlist(quest)), choices_types)
# 
# # Cleaning
# metahtml <- metahtml %>% mutate(quest = str_replace(quest, "\\*", "") %>% str_trim)
# 
# # IDENTIQUE ?
# map2(metapdf, metahtml, all.equal)
