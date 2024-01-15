---
title:                "Parsing html"
html_title:           "Elixir recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Why

Parsing HTML, or converting raw HTML code into a structured document, is a crucial task in web development. It is necessary for building websites, web scrapers, and other web-related applications. With the rise of web technologies, knowing how to parse HTML can be a valuable skill for any programmer.

## How To

To parse HTML in Elixir, we can use the popular library, `Floki`. First, we need to install it in our project by adding `{:floki, "~> 0.28.0"}` to our `mix.exs` file's dependencies. Once installed, we can use the `Floki.parse/1` function to parse HTML code. Let's see an example:

```Elixir
html = "<div><p>Hello!</p></div>"

parsed_html = Floki.parse(html)
IO.inspect parsed_html
```

The output of this code would be a representation of the HTML code as a nested data structure, making it easier to manipulate and extract information from. In this case, the output would be:

```Elixir
[
  {:div, [], [
    {:p, [], ["Hello!"]}]}
]
```

We can also use `Floki.find/2` to search for specific elements within the HTML code. For example, if we want to find all `h1` tags within our HTML, we can use the following code:

```Elixir
html = "<h1>Welcome to my website</h1><div><h1>Some other heading</h1></div>"

h1_tags = Floki.find(html, "h1")
IO.inspect h1_tags
```

The output would be a list of all `h1` tags found in the HTML code, in this case, `["Welcome to my website", "Some other heading"]`.

## Deep Dive

Parsing HTML with Elixir is made even more powerful with features like patterns and queries. With patterns, we can search for specific attributes within HTML tags, while queries allow us to navigate the HTML structure more efficiently. Additionally, `Floki` supports CSS selectors, making it easier to locate specific elements within the HTML code.

To learn more about parsing HTML with `Floki`, check out the official documentation [here](https://hexdocs.pm/floki/api-reference.html).

## See Also

- [Elixir `Floki` Documentation](https://hexdocs.pm/floki/api-reference.html)
- [Elixir Official Website](https://elixir-lang.org/)
- [Elixir `mix` Documentation](https://hexdocs.pm/mix/Mix.html)