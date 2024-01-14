---
title:                "Elixir recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Why

Parsing HTML can be a tedious task, but it is a necessary one for anyone working with web development or data extraction. By understanding how to parse HTML, you can easily extract specific data from web pages, create web scrapers, or build web automation tools. This can save you time and effort in your development process.

## How To

To parse HTML in Elixir, we can use the `Floki` library. First, we need to add it to our `mix.exs` file:

```
defp deps do
  [
    {:floki, "~> 0.26.0"},
  ]
end
```

Next, we need to import the `Floki` module in our code:

```
import Floki
```

Now, let's say we want to extract the text from a `<p>` tag on a website:

```
html = """
<!DOCTYPE html>
<html>
  <body>
    <p>Hello world!</p>
  </body>
</html>
"""

text = html
      |> Floki.parse_document()
      |> Floki.find("p")
      |> Floki.text()
```

In this example, we used `Floki.parse_document()` to convert the HTML string into a Floki Document, then we used `Floki.find()` to find the `<p>` tag, and finally, we used `Floki.text()` to extract the text from the tag. The value of `text` would be "Hello world!".

We can also use `Floki.find_all()` to extract all `<a>` tags and their attributes:

```
html = """
<!DOCTYPE html>
<html>
  <body>
    <a href="https://elixir-lang.org">Elixir</a>
    <a href="https://phoenixframework.org">Phoenix</a>
    <a href="https://erlang.org">Erlang</a>
  </body>
</html>
"""

links = html
        |> Floki.parse_document()
        |> Floki.find_all("a")
        |> Enum.map(fn a -> Floki.attribute(a, "href") end)
```

The value of `links` would be ["https://elixir-lang.org", "https://phoenixframework.org", "https://erlang.org"].

## Deep Dive

Floki also provides functions like `Floki.raw_html()` and `Floki.raw_text()` to extract the raw HTML or text, without removing any tags or special characters. It also supports CSS selectors, allowing for easier and more specific extraction of data from the HTML.

Additionally, Floki is built on top of the `html5ever` library, which provides a robust HTML parser with better error handling. This can be useful when dealing with complex or malformed HTML.

## See Also

- Floki documentation: https://hexdocs.pm/floki/getting-started.html
- HTML5ever documentation: https://github.com/whatwg/html/tree/master/html5
- Elixir forum discussion on parsing HTML: https://elixirforum.com/t/how-to-parse-html-using-elixir/14377