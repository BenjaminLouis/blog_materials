## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE)

## ----package-------------------------------------------------------------
library(dplyr)
library(purrr)
library(stringr)

## ----pdfimport-----------------------------------------------------------
library(pdftools)
metapdf <- pdf_data("2019-04-03-pdf-vs-html_files/test_form.pdf")
head(metapdf[[1]])

## ----bindpages-----------------------------------------------------------
library(pdftools)
metapdf <- bind_rows(metapdf, .id = "page")

## ----pdflines------------------------------------------------------------
metapdf <- metapdf %>%
  filter(height == 10) %>%
  group_by(page, y) %>%
  mutate(lines = str_c(text, collapse = " ")) %>%
  distinct(lines, .keep_all = TRUE) %>%
  ungroup()

## ----pdfsentences--------------------------------------------------------
metapdf <- metapdf %>%
  mutate(sentences_group = str_detect(lines,"(^\\d{1,2}\\.\\s|^[:upper:]([:lower:]|\\s))") %>% 
           cumsum() %>% 
           paste0("sent", .)) %>%
  group_by(sentences_group) %>%
  mutate(sentences = str_c(lines, collapse = " ")) %>%
  distinct(sentences_group, .keep_all = TRUE) %>%
  ungroup()
select(metapdf, lines, sentences_group, sentences)

## ----pdfblocs------------------------------------------------------------
metapdf <- metapdf %>%
  mutate(question_bloc = paste0("bloc", cumsum(str_detect(lines, "^\\d{1,2}\\.\\s")))) %>%
  filter(question_bloc != "bloc0")

## ----pdftypes------------------------------------------------------------
metapdf <- metapdf %>%
  group_by(question_bloc) %>%
  mutate(types = case_when("Check all that apply." %in% sentences ~ "multiple",
                           "Mark only one oval." %in% sentences ~ "unique",
                           "Mark only one oval per row." %in% sentences ~ "grid",
                           length(sentences) == 1 ~ "free")) %>%
  filter(!is.element(sentences, c("Check all that apply.", "Mark only one oval.",
                                  "Mark only one oval per row."))) %>%
  mutate(id = 1:n()) 

## ----pdfchoices----------------------------------------------------------
# Grid question
columns <- metapdf %>% 
  filter(types == "grid" & id == 2) %>% 
  pull(sentences) %>% 
  str_split("\\s") %>% 
  unlist()
rows <- metapdf %>% 
  filter(types == "grid" & !id %in% 1:2) %>% 
  pull(sentences)
# Creating questions and choices columns
metapdf <- metapdf %>% 
  mutate(choices = list(sentences[-1])) %>% 
  ungroup %>%
  filter(id == 1) %>%
  select(questions = sentences, types, choices) 
# Adding choices values for grid question
metapdf$choices[metapdf$types == "grid"] <- list(c(row = rows, column = columns))

## ----pdfclean------------------------------------------------------------
metapdf <- metapdf %>% mutate(questions = str_replace(questions, "\\*", "") %>% str_trim)
metapdf

## ----htmlimport----------------------------------------------------------
library(rvest)
form <- read_html("2019-04-03-pdf-vs-html_files/test_form.html")
form
form %>% html_nodes("body") %>% html_children() %>% length()

## ----htmlblocs-----------------------------------------------------------
prefix <- function(.x) {paste0(".freebirdFormeditorView", .x)}
question_blocs <- form %>%
  html_nodes(prefix("ItemContent")) %>%
  keep(~html_nodes(.x, prefix("ItemMinimizedTitleRow")) %>% 
         length() != 0)

## ----htmlquestions-------------------------------------------------------
questions <- question_blocs %>%
  html_nodes(prefix("ItemMinimizedTitleRow")) %>%
  html_text() 
questions

## ----htmlchoices---------------------------------------------------------
# To make code smaller
get_xpath <- function(.y) {
  cl <- str_replace(prefix(.y), "\\.", "")
  paste0('.//*[@class="', cl,'"]')
}
# To get choices and types
get_choices_and_types <- function(.nd) {
  
  # Multiple choices questions
  if (length(html_nodes(.nd, prefix("QuestionBodyChoicelistbodyOmniList"))) != 0) {
    # Answer choices
    choices <- html_nodes(.nd, ".exportInput") %>%
      keep(~html_attr(.x, "aria-label") == "option value") %>% 
      html_attr(.,"value")
    # For "Other" option
    other <- html_nodes(.nd, prefix("OmnilistListPlaceholderOption")) %>% 
      html_attr(., "style") != "display:none"
    if (other) choices <- c(choices, "Other:")
    # Question type
    dltype <- html_nodes(.nd, prefix("QuestionBodyChoicelistbodyOmniList")) %>% 
      html_attr("data-list-type")
    if (dltype == "1") type <- "multiple" else type <- "unique"
  
  # Grid question  
  } else if (length(html_nodes(.nd, prefix("QuestionBodyGridbodyRow"))) != 0) {
    # Answer choices
    choices <- c(
      row = html_nodes(.nd, xpath = get_xpath("QuestionBodyGridbodyRow")) %>%
        html_text(),
      column = html_nodes(.nd, prefix("QuestionBodyGridbodyColumnHeader")) %>%
        html_nodes(xpath = get_xpath("QuestionBodyGridbodyCell")) %>%  
        html_text()
    )
    # Question type
    type <- "grid"
  
  # Free question  
  } else {
    choices <- character(0)
    type <- "free"
  }
  return(tibble(types = type, choices = list(choices)))
}
choices_types <- map_dfr(question_blocs, get_choices_and_types)

## ----htmlclean-----------------------------------------------------------
metahtml <- bind_cols(questions = paste0(1:9, ". ", unlist(questions)), choices_types)
metahtml <- mutate(metahtml, questions = str_replace(questions, "\\*", "") %>% str_trim)
metahtml

## ----conclusion----------------------------------------------------------
map2(metapdf, metahtml, all.equal)

