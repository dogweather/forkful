---
title:                "Parsing html"
html_title:           "Elm recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Why
Parsing HTML, or the process of extracting information from HTML code, is a crucial task in web development. Whether you want to scrape data from a website, build a web scraper, or create a web page, understanding how to parse HTML is essential.

## How To
To parse HTML in Elm, we can use the `elm/parser` library. First, let's import the library:
```Elm
import Html exposing (Html)
import Parser exposing (Parser, (|.), succeed, oneOf, map, Parser)
import Parser.RegularExpressions exposing (regex)
```
Next, we define a type for our parse tree. This will help us organize the information we parse later on.
```Elm
type HtmlNode
    = Tag String HtmlNodeList
    | Text String
    | Comment String
    | Doctype String

type HtmlNodeList
    = SingleNode HtmlNode
    | NodeList List HtmlNode
```
We can now start writing our parser. Let's start with a function that parses a tag. We use the `regex` function to match the opening and closing tags, and the `oneOf` function to match any character inside the tag.
```Elm
parseTag : Parser HtmlNode
parseTag =
    regex "^<([a-z]+)>" |> map (\tag -> Tag tag (SingleNode (Text tag)))
```
Next, we can write a parser for text between tags.
```Elm
parseText : Parser HtmlNode
parseText =
    regex "^(.*?)<" |> map Text
```
We can also parse comments and doctypes using similar functions.
```Elm
parseComment : Parser HtmlNode
parseComment =
    regex "^(<!--.*?-->)" |> map Comment
    
parseDoctype : Parser HtmlNode
parseDoctype =
    regex "^(<!doctype.*?)>" |> map (\doctype -> Doctype doctype)
```
Finally, we can combine all our parsers into one using the `oneOf` function.
```Elm
parseHtml : Parser HtmlNode
parseHtml =
    oneOf [ parseTag, parseText, parseComment, parseDoctype ]
```
We can now test our parser by passing in a string of HTML code and seeing the output.
```Elm
Parser.run parseHtml "<h1>Hello, world!</h1>"
--> Tag "h1" (SingleNode (Text "Hello, world!"))
```

## Deep Dive
The `elm/parser` library also provides many useful functions for parsing more complex HTML structures, such as attributes, nested tags, and self-closing tags. We can also use the `Parser.map` function to transform our parsed data into a more organized format.

For a more in-depth understanding of parsing HTML in Elm, I highly recommend reading the official documentation for the `elm/parser` library.

## See Also
- [Elm Parser Library Documentation](https://package.elm-lang.org/packages/elm/parser/latest/)
- [Parsing HTML with elm/parser](https://dev.to/werner/practical-applying-parser-theory-in-elm-parsing-html-with-elmparser-54f1) by Werner Echezuría