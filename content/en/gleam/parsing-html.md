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

## Why
As a programmer, you may come across the need to extract specific information from a web page. This is where HTML parsing comes into play. By parsing HTML, you can easily extract the data you need and use it in your own applications.

## How To
To start parsing HTML with Gleam, you'll need to first import the `gleam/html` package. Then, you can use the `parse` function to parse the HTML content you want to extract data from.

```
Gleam.html.parse(html_content)
```

This will return a `Result` type with either an HTML tree if the parsing was successful, or an error if there were any parsing issues.

To extract information from the HTML tree, you can use the `find_all` function and specify the HTML tag you want to find. Here's an example of how to extract all the links from a web page:

```
Gleam.html.find_all(tree, "a")
```

This will return a list of all the `a` tags found in the HTML tree, which you can then loop through and extract the `href` attribute to get the link.

For more complex extractions, you can use the `find` function and specify a CSS selector to find specific elements. Here's an example of extracting all the paragraph text from a specific class:

```
Gleam.html.find(tree, ".class-name p")
```

This will return a list of all the paragraphs within the specified class.

## Deep Dive
Under the hood, Gleam uses the [html5ever](https://github.com/servo/html5ever) library for HTML parsing. This library has strong support for HTML5, making it a great choice for parsing modern web pages.

One of the unique features of Gleam's HTML parsing is the use of type annotations to ensure safety and prevent runtime errors. This makes parsing with Gleam a more reliable and enjoyable experience.

If you want to learn more about the inner workings of Gleam's HTML parser, be sure to check out the [documentation](https://gleam.run/packages/gleam/html/) for a complete list of functions and data types.

## See Also
- [Gleam HTML package documentation](https://gleam.run/packages/gleam/html/)
- [html5ever library on GitHub](https://github.com/servo/html5ever)
- [CSS selectors reference by Mozilla](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors)