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

# Diving into HTML Parsing with Elixir

## What & Why?
HTML parsing is the process of analyzing HTML code to extract meaningful structure - essentially turning HTML into data. Programmers do this to enable the extraction, automation, and manipulation of web content.

## How to:

Let's dive right into the action. In Elixir, we can use the package named `Floki` for parsing HTML. Assume that you have installed this dependency.

```elixir
def deps do
  [
    {:floki, "~> 0.30"}
  ]
end
```

Here is a basic example of how to extract all `href` links from a webpage:

```elixir
{:ok, body} = HTTPoison.get("https://www.example.com/")
links = body |> Floki.find("a[href]") |> Floki.attribute("href")
IO.inspect(links)
```

This script will print a list of all URLs found in the "href" attributes of anchor tags on "https://www.example.com".

## Deep Dive

HTML Parsing has been a popular task since the inception of the web. Initially, it was quite simple with basic HTML documents. However, as sites grew more complex, parsing became more involved.

As for alternatives, you might consider using `meeseeks` or `sweet_xml`. These offer similar parsing capabilities as `floki` but can differ in syntax and processing power.

An interesting point about `Floki`'s implementation is it leverages the underlying Cascading Style Sheets (CSS) selector engine; this is what allows for the querying of HTML elements using CSS-like selectors, leading to seamless, intuitive querying of HTML documents.

## See Also 

- `Floki` documentation: [hexdocs.pm/floki](https://hexdocs.pm/floki)
- `Meeseeks` documentation: [hexdocs.pm/meeseeks](https://hexdocs.pm/meeseeks)
- `SweetXML` documentation: [hexdocs.pm/sweet_xml](https://hexdocs.pm/sweet_xml) 
- Elixir Language Website: [elixir-lang.org](https://elixir-lang.org) 

Enjoy parsing!