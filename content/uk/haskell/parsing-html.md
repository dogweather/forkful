---
title:                "Парсинг HTML"
date:                  2024-01-20T15:32:15.306535-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Parsing HTML isn't rocket science—it's just extracting data from HTML markup. Programmers do it for web scraping, data mining, or when they need to interact with web pages not designed for programmatic access.

## How to: (Як це зробити:)
Let's parse HTML in Haskell using the `tagsoup` library. Install it first:

```bash
cabal update
cabal install tagsoup
```

Here's a snippet that fetches and parses a simple HTML document:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup
import Network.HTTP.Conduit (simpleHttp)

main :: IO ()
main = do
    html <- simpleHttp "http://example.com"
    let tags = parseTags html
    let links = [ deTag x | TagOpen "a" atts <- tags, x <- atts, fst x == "href" ]
    print links
    
deTag :: (String, String) -> String
deTag = snd

```

This simple program will print out all hyperlinks on the "http://example.com" homepage.

## Deep Dive (Поглиблений Аналіз)
Haskell's `tagsoup` has been around since the early 2000s. It's quirky but practical. `tagsoup` allows for lenient parsing, meaning it can handle real-world, messy HTML well.

There are alternatives like `html-conduit`, which is built on top of the streaming library `conduit` for more memory-efficient parsing, especially useful for large documents.

Key implementation detail: `tagsoup` parses HTML into a list of tags, treating the document as text, not a tree—this is different from libraries that build a DOM.

## See Also (Дивіться також)
For more on `tagsoup`:
- Hackage: https://hackage.haskell.org/package/tagsoup

For an alternative approach with `html-conduit`:
- Tutorial: https://www.yesodweb.com/book/xml

For general concepts around web scraping in Haskell:
- Blog post: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/tagsoup
