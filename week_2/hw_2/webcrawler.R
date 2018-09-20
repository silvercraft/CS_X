library(rvest)

page.source <- read_html("https://www.reddit.com/reddits/")

kw = html_nodes(page.source, ".title")
html_text(kw)
html_attr(kw,"href")