---
title:                "Parsing html"
html_title:           "Haskell recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the process of analyzing and extracting data from HTML code. As a markup language, HTML is used to create the structure and content of web pages. Programmers use parsing to retrieve specific information from these pages, such as text, images, and links. This is especially useful for things like web scraping, data extraction, and automated testing.

## How to:

To demonstrate parsing HTML in Haskell, let's first import the `tagsoup` library which provides functions for parsing HTML and XML. 

```Haskell
import Text.HTML.TagSoup
```

Next, we can use the `parseTags` function to parse a given HTML string and return a list of `Tag`s. For example, let's parse the HTML from the Wikipedia homepage:

```Haskell
tags <- parseTags <$> getResponseBody "https://en.wikipedia.org/wiki/Main_Page"
```

We can now manipulate this list of tags to extract the data we need. For instance, we can use the `find` function to retrieve the content of the page's title:

```Haskell
let titleTag = find (~== "<title>") tags
let title = fromTagText <$> titleTag
-- Output: "Wikipedia, the free encyclopedia"
```

We can also use `filterTags` to search for specific tags and attributes. For example, let's find all `img` tags with a specific class attribute:

```Haskell
let images = filterTags (\tag -> tag ~== ("<img class=\"thumbimage\"/>") ) tags
-- Output: [TagOpen "img" [("class","thumbimage")]]
```

These are just a few examples of what you can do with the `tagsoup` library for parsing HTML in Haskell. For more options and functions, be sure to check out the official documentation.

## Deep Dive:

HTML parsing has been a common task in web development for decades. In the early days, developers would often use regular expressions to parse HTML. However, this approach is error-prone and not very flexible. That's why libraries like `tagsoup` were created to provide more robust and reliable parsing options.

An alternative to using a library is to write your own parsers using a parser combinator library such as `parsec` or `attoparsec`. These libraries use a more functional approach to parsing, allowing you to define parsers as composable functions. However, this can be more challenging and time-consuming for simple HTML parsing tasks.

In terms of implementation, `tagsoup` uses a SAX (Simple API for XML) parser, which is known for its efficiency and low memory usage. The library also provides options for customizing how certain tags or attributes are parsed, making it a versatile tool for handling different types of HTML.

## See Also:

- [Official `tagsoup` documentation](https://hackage.haskell.org/package/tagsoup)
- [`parsec` library for writing parsers in Haskell](https://hackage.haskell.org/package/parsec)
- [`attoparsec` library for high-performance parsing](https://hackage.haskell.org/package/attoparsec)
- [Learn You a Haskell - Parsing Basic Expressions](http://learnyouahaskell.com/starting-out#im-a-list-comprehension)