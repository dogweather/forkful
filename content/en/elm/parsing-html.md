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

## What & Why?

Parsing HTML is the process of breaking down HTML code into its individual components to be processed by a program. Programmers do this in order to extract specific information from a webpage or to manipulate the HTML code itself.

## How to:

```Elm
-- Import the appropriate library
import Html.Parser

-- Create a function to parse the HTML
parseHTML : String -> Result String (List (Html Parser.Node))
parseHTML str =
    -- Use the `parse` function from the Html.Parser library
    Html.Parser.parse str

-- Use the function to parse an HTML string and print the results
Html.Parser.fromString "<h1>Welcome!</h1>"
    |> parseHTML
    |> case of
        -- Handle the success case where the HTML is valid
        Ok nodes ->
            Debug.toString nodes

        -- Handle the error case where the HTML is invalid
        Err error ->
            "Error: " ++ error
```

The output of this code would be:

Ok [Elm.Node (tagName "h1") [] [Elm.TextNode "Welcome!"]]

This output is a list of nodes, with each node representing an element in the HTML code. You can also use this output to access specific elements or attributes within the HTML.

## Deep Dive:

Parsing HTML has been an important aspect of web development since the early days of the internet. Before the creation of HTML parsers, web developers had to manually sift through HTML code to extract the information they needed. With the introduction of parsers, this process became much more efficient.

While there are other languages and libraries that offer HTML parsing functionality, Elm's built-in Html.Parser library is a great choice for those already working with Elm. Additionally, Elm is a functional language, which allows for concise and efficient parsing of HTML code.

When parsing HTML in Elm, the library uses its own custom data structures to represent the HTML code. This allows for a more strict and structured approach to parsing, ensuring that only valid HTML code is accepted.

## See Also:

- [Elm Documentation on Html.Parser](https://package.elm-lang.org/packages/elm/html/latest/Html-Parser)
- [MDN Web Docs on HTML Parsing](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction)