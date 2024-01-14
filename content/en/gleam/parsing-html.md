---
title:                "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Why

Have you ever wanted to extract specific information from a website but found it too cumbersome to manually search for it? Well, with Gleam's HTML parsing capabilities, you can automate the process and easily retrieve the data you need.

## How To

To start parsing HTML with Gleam, you will first need to install the `gleam/html` package. Once installed, you can use the `parse_html` function to convert the HTML string into a structured data type. Let's look at an example:

```
Gleam import html

html.parse_html("<h1>Hello, world!</h1>")
```

This code will return a list containing a single element, which represents the `h1` tag with the text "Hello, world!" as its content. You can then use the `Node` type functions to extract specific information from the HTML code. For example, if we wanted to retrieve the content of the `h1` tag, we could use the `Node.text` function:

```
Gleam import html

parsed_html = html.parse_html("<h1>Hello, world!</h1>")
h1_tag = List.head(parsed_html)
text = Node.text(h1_tag) // "Hello, world!"
```

You can also use the `find_all` function to search for specific tags within the HTML code. For example, if we wanted to find all the `p` tags, we could use the following code:

```
Gleam import html

parsed_html = html.parse_html("<p>Hello</p><p>World</p>")
p_tags = Node.find_all(parsed_html, "p")
```

This will return a list containing two elements, each representing a `p` tag with the text "Hello" and "World" respectively.

## Deep Dive

Gleam's HTML parsing capabilities go beyond just extracting information from specific tags. You can also use the `html` module's functions to manipulate and transform the HTML code. For example, you can use the `Node.replace` function to replace a specific tag with another tag or text. Let's look at an example:

```
Gleam import html

html_str = "<p>Hello</p>"
parsed_html = html.parse_html(html_str)
new_html = Node.replace(parsed_html, "p", "h2")
result = html.to_html(new_html) // "<h2>Hello</h2>"
```

In this code, we parse the HTML string into a structured data type, replace the `p` tag with an `h2` tag, and then use the `html.to_html` function to convert the structured data back into HTML code.

## See Also

- [Gleam Documentation](https://gleam.run/documentation/)
- [HTML Parser Package](https://gleam.run/packages/gleam/html/)

And that's it! Now you have a basic understanding of how to parse and manipulate HTML using Gleam. Happy coding!