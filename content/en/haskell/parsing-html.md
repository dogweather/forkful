---
title:                "Parsing HTML"
date:                  2024-01-20T15:31:49.800254-07:00
html_title:           "Bash recipe: Parsing HTML"
simple_title:         "Parsing HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML means extracting data from HTML documents—HTML is the scaffolding of the web, after all. Programmers parse HTML to automate data scraping, to migrate content, or to transform it into different formats.

## How to:

Let's get our hands dirty with some code, using the `tagsoup` library to parse a simple HTML snippet. First, make sure to install the package from Hackage via `cabal install tagsoup`.

```Haskell
import Text.HTML.TagSoup

-- Let's parse a simple HTML snippet
let html = "<html><body><p>Hello, Haskell!</p></body></html>"

-- Parse it
let parsedHtml = parseTags html

-- Find paragraphs
let paragraphs = partitions (~== "<p>") parsedHtml

-- Get the text from the first paragraph
let firstParagraphText = innerText $ head paragraphs

-- Voila!
print firstParagraphText
```

Sample output:
```
"Hello, Haskell!"
```
This snippet parses an HTML string, hunts down paragraph tags, and prints the text contained within the first paragraph. Neat and sweet.

## Deep Dive

Parsing HTML in Haskell hasn't always been as streamlined as it is today. Once upon a time, folks rolled their own parsers or wrestled with lower-level libraries, parsing HTML like it was the Wild West.

These days, you've got options. `tagsoup`, as we've used, is great for when HTML structure is more suggestion than rule—it's tolerant of real-world messy HTML. If you're looking for more rigor, `html-conduit` combined with `xml-conduit` from the `conduit` package might be your jam. They use a streaming approach and are pickier about structure.

Under the hood, these libraries convert HTML into a tree or a soup of tags. They provide handy functions to query and manipulate this data, making HTML parsing less of a headache. Think of them as a treasure map, where X marks the paragraph tag.

## See Also

- [`tagsoup` on Hackage](https://hackage.haskell.org/package/tagsoup)
- [`html-conduit` on Hackage](https://hackage.haskell.org/package/html-conduit)
- [Beautiful Soup documentation](https://www.crummy.com/software/BeautifulSoup/) - While not Haskell, Beautiful Soup's approach to 'tag soup' has influenced similar libraries in the Haskell world.
- [XPath and XQuery Functions and Operators on W3C](https://www.w3.org/TR/xpath-functions/) - Deep dive into standards can inform about the structure and querying of XML/HTML documents, useful for understanding the parsing strategies in the background.
