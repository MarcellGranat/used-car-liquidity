doc <- read_lines("manuscript.tex")

# markdown does not allow to add figure notes by default
# this script modifies the generated manuscript.tex code

# replace the simple `includegraphics ...` lines w complete latex snippets
# including the captions, labels & notes from figure-labels.txt

fig_labels <- read_lines("markdown/figure-labels.txt") %>%
  tibble(data = .) %>% 
  mutate(
    r = cumsum(data == "") + 1,
    name = gsub(":.*", "", data),
    name = ifelse(name %in% c("title", "comment", "alignment"), name, "id"),
    data = str_remove(data, str_c(name, ": |:")),
  ) %>% 
  filter(data != "")

fig_labels

for (i in unique(fig_labels$r)) {
  
  fig_labels %>% 
    filter(r == i) %>% 
    pull(data, name = name) %>% 
    iwalk(~ assign(.y, .x, envir = globalenv()))
  
  row_contains <- which(str_detect(doc, id) & str_detect(doc, "includegraphics"))
  
  doc <- append(doc, str_c("\\begin{figure}", alignment), after = row_contains - 1)
  doc <- append(doc, "\\centering", after = row_contains)
  doc <- append(doc, str_c("\\caption{", title, " ", comment, "}\\label{fig:", id, "}"), after = row_contains + 2)
  doc <- append(doc, "\\end{figure}", after = row_contains + 3)
  doc <- append(doc, "", after = row_contains + 4)
  
}

full_page_figure <- 2 # figure* = full page figure

for (i in full_page_figure) {
  row_begin <- which(str_detect(doc, "begin\\{figure\\}"))[i]
  doc[row_begin] <- str_replace(doc[row_begin], "figure", "figure*")
  row_end <- which(str_detect(doc, "end\\{figure\\}"))[i]
  doc[row_end] <- str_replace(doc[row_end], "figure", "figure*")
}

write_lines(doc, file = "manuscript.tex")

tools::texi2pdf("manuscript.tex")
system("open manuscript.pdf")





