`%>%` <- magrittr::`%>%`

add_github_logo <- function(url) {
  gsub(
    pattern = "(.*)https://github.com/(.*)", 
    replacement = '\\1[<i class="fa fa-github"></i> GitHub](https://github.com/\\2)', 
    x = url
  )
}

profil_section <- function(xlsx = "data/cv.xlsx", sheet = "profil") {
  readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::filter(show == 1) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    dplyr::mutate(
      level = purrr::map_chr(
        .x = level, 
        .f = ~ paste(rep("#", each = as.numeric(.x) + 2), collapse = "")
      )
    ) %>% 
    glue::glue_data('{level} {title}\n\n{paragraph}\n\n')
}

contact_section <- function(xlsx = "data/cv.xlsx", sheet = "contact") {
  readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    glue::glue_data(
      '## Contact Info {{#contact}}',
      '\n\n',
      '- <i class="fa fa-user" style="color: #4169e1;"></i> {position}',
      '\n',
      '- <i class="fa fa-map-marker" style="color: #4169e1;"></i> {city}',
      '\n',
      '- <i class="fa fa-envelope" style="color: #4169e1;"></i> [{gsub("@", " [at] ", email)}](mailto:{email})',
      '\n',
      '- <i class="fa fa-phone" style="color: #4169e1;"></i> {phone}',
      '\n',
      '- <i class="fa fa-github" style="color: #4169e1;"></i> [{github}](https://github.com/{github})',
      '\n',
      '- <i class="fa fa-website" style="color: #4169e1;"></i> My website: {website}',
      
      '\n\n'
    )
}

skills_section <- function(xlsx = "data/cv.xlsx", sheet = "skills") {
  readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    dplyr::group_by(level) %>% 
    dplyr::summarise(
      what = as.character(glue::glue_collapse(what, sep = ", ", last = " and ")), 
      .groups = "drop"
    ) %>% 
    tidyr::pivot_wider(names_from = level, values_from = what) %>% 
    glue::glue_data(
      '## Computer Skills {{#skills}}',
      "\n\n",
      '- <u style="color: #40ff00;">*Advanced:*</u> {advanced}',
      '\n',
      '- <u style="color: #ff8000;">*Intermediate:*</u> {intermediate}',
      '\n',
      '- <u style="color: #df3a01;">*Basic:*</u> {basic}',
      '\n\n\n\n',
      '## Language Skills',
      "\n\n",
      '- <u style="color: #40ff00;">*French:*</u> {French}',
      '\n',
      '- <u style="color: #ff8000;">*English:*</u> {English}',
      '\n',
      '- <u style="color: #df3a01;">*Spanish:*</u> {Spanish}',
      '\n\n'
    )
}

disclaimer_section <- function(text = NULL) {
  glue::glue(
    '## Disclaimer {{#disclaimer}}',
    if (is.null(text)) '\n\n' else '\n\n{text}\n\n',
    'Last updated on {Sys.Date()}.\n\n'
  )
}

sidebar <- function(
  png = "pictures/cv.png", 
  contact = contact_section(), 
  skills = skills_section(), 
  disclaimer = disclaimer_section()
) {
  cat(
    '# Aside\n',
    '```{{r, out.extra = \'style="width=226px;" id="picture"\'}}',
    'knitr::include_graphics({png})',
     '```',
    contact,
    skills, 
    disclaimer,
    sep = "\n\n"
  )
}

title_section <- function(author = NULL) {
  c(
    "# Main",
    glue::glue('## {author} {{#title}}')
  )
}

education_section <- function(xlsx = "data/cv.xlsx", sheet = "education", page_break_after = FALSE) {
  text <- readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::slice(dplyr::n():1) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    glue::glue_data(.sep = '\n\n',
      '### {degree}',
      '{university}',
      '{city}',
      '{start} - {end}',
      '{description}',
      '\n\n'
    )
  
  if (page_break_after) {
    c("## Education {data-icon=graduation-cap data-concise=true .break-after-me}", text)
  } else {
    c("## Education {data-icon=graduation-cap data-concise=true}", text)
  }
}

experience_section <- function(xlsx = "data/cv.xlsx", sheet = "experience", page_break_after = FALSE) {
  text <- readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::slice(dplyr::n():1) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    glue::glue_data(.sep = '\n\n',
      '### {position}',
      '{institute}',
      '{city}',
      '{start} - {end}',
      'Activities: *{activities}*',
      '\n\n'
    )
  
  if (page_break_after) {
    c("## Professional & Research Experience {data-icon=laptop .break-after-me}", text)
  } else {
    c("## Professional & Research Experience {data-icon=laptop}", text)
  }
}

