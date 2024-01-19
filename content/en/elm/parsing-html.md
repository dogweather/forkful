---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML transforms web content into a tree-like structure that your program can work with. It helps in data scraping, automated testing, manipulation and rendering of web content.

## How to:

Elm has html-parser library for parsing HTML. Here's a simple example:

```Elm
import Html.Parser exposing (..)
import Html.Parser.Util exposing (..)

parseDiv = node "div" (succeed ()) 
```

This bit of code defines a parser that'll recognize `<div>` elements. Let's parse!

```Elm
import Html.Parser exposing (..)
import Html.Parser.Util exposing (..)

main =
    parse parseDiv "<div></div>"
```

Upon running, you shall get the following output:

```Elm
Ok( () )
```

The `Ok` indicates a successful parse with the parsed content as its argument.

## Deep Dive 

Elm's approach to parsing maintains the language's commitment to type safety and functional principles. Elm's HTML parser was introduced in version 0.17. It's built upon basic building blocks like 'node', 'text', and 'attribute' that allows complex and flexible parsing behavior.

Alternatives to Elm's built-in HTML parser include using ports to delegate the parsing work to Javascript, or other independent Elm libraries. Yet, Elm's native parser often provides enough functionality for most use-cases while maintaining the language's key principles.

Under the hood, Elm's HTML parser leverages a parsing approach that ensures input is processed in one pass, resulting in efficiency and speed. 

## See Also 

For more detailed info, refer to:

1. Elm's [official guide](http://guide.elm-lang.org/) for version details.
2. [HTML Parser package](https://package.elm-lang.org/packages/eeue56/elm-html-parser/latest/) documentation.
3. Deeper dive into parser theory at [Wikipedia](https://en.wikipedia.org/wiki/Parsing).