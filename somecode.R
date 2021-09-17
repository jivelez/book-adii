
require(bookdown)
bookdown::render_book('index.Rmd')
browseURL('docs/index.html')
usethis:::browse_github()