---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/parsing-html.md"
---

{{< edit_this_page >}}

# How to Parse HTML with Haskell

## What & Why?

Parsing HTML lets you extract data from web pages. As a Haskell programmer, this means you can automate data scraping, mine websites for information, or build your own page analysis tools.

## How to:

We'll use the `tagsoup` library for parsing. Install it by running `cabal install tagsoup`.

Here's a basic example to extract titles from an HTML string:

```Haskell
import Text.HTML.TagSoup

extractTitles :: String -> [String]
extractTitles = 
    map innerText . sections (~== "<title>") . parseTags

main :: IO ()
main = do
    let html = "<html><head><title>My Site</title></head></html>"
    print $ extractTitles html
```

Run this code and you'll see `["My Site"]` as the output.

## Deep Dive

`tagsoup` gives you tools to parse data from websites in a simple, flexible manner. It was inspired by Python's BeautifulSoup, making it part of Haskell’s effort to incorporate functionality of other languages.

An alternative approach for stricter HTML parsing is `html-conduit` library which uses DOM structured parsing. But this requires your HTML to be well-formed.

On performance, `tagsoup` is fairly efficient – it depends on text size rather than its structure. Its main focus is on robustness and simplicity, assuming real-world HTML is a wild and messy space.

## See Also

- [TagSoup library](http://hackage.haskell.org/package/tagsoup)
- BeautifulSoup (Python) for comparison: [Beautiful Soup 4 Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- Other HTML parsers in Haskell: [html-conduit](http://hackage.haskell.org/package/xml-conduit)