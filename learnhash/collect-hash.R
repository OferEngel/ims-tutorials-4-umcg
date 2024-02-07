library(tidyverse)
library(learnrhash)
library(purrr)
library(stringr)

# Errors
df.err <-  read_csv(
  "hash.csv",
  show_col_types = FALSE
) |>
  filter(!str_detect(hash, "Qlp"))


df.orig <-  read_csv(
  "hash.csv",
  show_col_types = FALSE
) |>
  filter(str_detect(hash, "Qlp"))

# This function extracts the names and correct answers.
get_correct <- function(df) {

  fix_empty_state_obj <- function(obj) {
    if (length(obj) == 0) {
      list(
        list(
          id = NA_character_,
          type = NA_character_,
          data = NULL
        )
      )
    } else {
      obj
    }
  }

  # df <- df.orig |> slice(1)
  hash <- "hash"
  submt.raw <- df |>
    select(hash) |>
      rename(hash = all_of(hash)) %>%
      mutate(
        hash = lapply(all_of(hash), learnrhash::decode_obj),
        hash = lapply(all_of(hash), fix_empty_state_obj)
      ) %>%
      unnest_longer(all_of(hash)) %>% pull(hash)

  tryCatch(
    {
      submt <- submt.raw |>
          filter(type %in% c("question", "identifier"))

      uid <- submt |>
        filter(type == "identifier") |>
        unnest(answer)  |>
        pivot_wider(
          names_from = label, values_from = answer
          ) |>
        dplyr::select(student_name, student_id)

      n.crct <- submt |>
        drop_na(correct) |>
        filter(correct == TRUE) |>
        nrow()

      uid |> add_column(correct = n.crct)
    }
    ,
    error = function(cond) {
      tribble(
        ~student_name, ~student_id, ~correct,
        "error" , "NA", 0
      )

    },
    warning = function(cond) {
      tribble(
        ~student_name, ~student_id, ~correct,
        "error" , "NA", 0
      )
    }
  )

}



res <- pmap(df.orig |> slice(seq(nrow(df.orig))) |>
       select(hash), ~ tibble(hash = ..1) |> get_correct()) |>
  bind_rows()

df.orig |>
  select(-hash) |>
  add_column(res) |>
  summarize(
    n = n(),
    correct = max(correct),
    .by = c(Email, lesson)
    ) |>
  print(n = 300)



