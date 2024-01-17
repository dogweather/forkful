---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the process of converting HTML code into structured data that can be understood and manipulated by computers. HTML is the standard markup language used to create web pages, so parsing HTML is essential for web developers to build and maintain websites.

## How to:

Gleam, a functional programming language, offers a built-in module for parsing HTML called html-parser. The following code block shows how to use this module to parse a simple HTML document and extract the title element:
```
Gleam code
[lang=gleam]
html <- `
<html>
<head>
<title>Hello, World!</title>
</head>
<body>
<h1>Welcome to My Website</h1>
<p>Thank you for visiting!</p>
</body>
</html>
`
document <- html_parser::parse_string(html)
title <- document["html"]["head"]["title"] |> html_parser::children |> List.head
```
The html variable contains the HTML code as a string, and the html-parser module's parse_string function is used to convert it into a structured data representation. The resulting document is a tree-like structure, and we can use the navigation syntax to access specific elements. In this case, we use the title element inside the head element. Finally, we use the children function to get the title's content, which is a list since there could be multiple elements with the same tag. We use the List.head function to extract the first (and only) element from the list.

The output of the code block above should be the string "Hello, World!" representing the title of the HTML document.

## Deep Dive:

Parsing HTML has a long history and has evolved with the development of the web. Before HTML5 was standardized in 2014, web developers had to deal with various versions of HTML and non-standard web browsers, making parsing a challenging task. However, as HTML5 became the dominant standard, parsing HTML has become more standardized and straightforward.

Apart from using the built-in html-parser module, web developers can also use libraries like xml-peek, htmlel, or elm-html-parsers, which offer more advanced features for parsing HTML and manipulating the resulting structured data.

The implementation details of parsing HTML can vary, but the general approach is to use a parser that reads the HTML code and creates a tree-like structure based on the elements, attributes, and their values. This structure can then be traversed to extract the desired data. Therefore, it is crucial to have a good understanding of HTML's structure and syntax to parse it correctly.

## See Also:

- Gleam html-parser module documentation: https://gleam.run/modules/html-parser.html
- xml-peek library for XML/HTML parsing: https://gleam.run/modules/xml-peek.html
- htmlel library for HTML parsing in Elm: https://gleam.run/modules/htmlel.html