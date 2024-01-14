---
title:                "Elm recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract information from a website but found yourself struggling with messy and inconsistent HTML code? Parsing HTML can be a daunting task, but with Elm, it can become a simple and straightforward process.

## How To

To begin parsing HTML in Elm, we will need to use the [elm/parser](https://package.elm-lang.org/packages/elm/parser/latest/) package. This package provides a set of functions and types specifically designed for parsing HTML.

First, we will import the necessary modules from the package:

```Elm
import Html.Parser exposing (..)
import Html.Parser.Attribute as Attr
import Html.Parser.Html as H
```

Next, we need to define a parser for our desired HTML element. In this example, we will be parsing a <ul> element with two <li> elements inside.

```Elm
listParser : Parser String (List String)
listParser = 
    H.tagName "ul" 
        |> andMap (H.list (H.tagName "li" (Attr.property "textContent")))
```

The `tagName` function specifies the HTML element we want to parse, while `andMap` and `list` allow us to get the elements inside the <ul> element.

To run our parser on some HTML code, we can use the [`decode` function](https://package.elm-lang.org/packages/elm/parser/latest/Html-Parser#decode) from the `elm/json` package:

```Elm
decode listParser listHtml
```

Where `listHtml` is a string containing our desired HTML code. If the parsing is successful, we will get a `Result` containing a list of strings representing the content of the <li> elements. For example, if our `listHtml` variable contained:

```html
<ul>
    <li>Item 1</li>
    <li>Item 2</li>
</ul>
```
Our output would be: `Ok ["Item 1","Item 2"]`.

## Deep Dive

Although our example may seem simple, it can be expanded to handle more complex HTML structures. For instance, we could have nested elements or elements with multiple attributes. With the Elm parser package, we can define custom parsers for any HTML structure we may encounter.

We can also use the [`map` function](https://package.elm-lang.org/packages/elm/parser/latest/Parser#map) to transform the parsed result into a desired type. For instance, if we wanted to convert the list of strings into a list of integers, we could use the following code:

```Elm
map (String.toInt >> Result.toMaybe) (decode listParser listHtml)
```

## See Also

- [elm/parser package](https://package.elm-lang.org/packages/elm/parser/latest/)
- [elm/json package](https://package.elm-lang.org/packages/elm/json/latest/)
- [HTML element reference](https://developer.mozilla.org/en-US/docs/Web/HTML/Element)