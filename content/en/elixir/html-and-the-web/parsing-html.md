---
date: 2024-02-03 19:02:42.400756-07:00
description: "Parsing HTML in Elixir involves extracting information from HTML documents.\
  \ Programmers do this to programmatically interact with web pages, scrape data,\u2026"
lastmod: '2024-03-11T00:14:33.635554-06:00'
model: gpt-4-0125-preview
summary: "Parsing HTML in Elixir involves extracting information from HTML documents.\
  \ Programmers do this to programmatically interact with web pages, scrape data,\u2026"
title: Parsing HTML
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML in Elixir involves extracting information from HTML documents. Programmers do this to programmatically interact with web pages, scrape data, or automate web interactions, enabling applications to understand and utilize web content dynamically.

## How to:

Elixir, with its robust concurrency model and functional programming paradigm, doesn't include built-in HTML parsing capabilities. However, you can use popular third-party libraries like `Floki` for this purpose. Floki makes HTML parsing intuitive and efficient, leveraging Elixir's pattern matching and piping features.

First, add Floki to your mix.exs dependencies:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

Then, run `mix deps.get` to install the new dependency.

Now, let's parse a simple HTML string to extract data. We'll look for the titles inside `<h1>` tags:

```elixir
html_content = """
<html>
  <body>
    <h1>Hello, Elixir!</h1>
    <h1>Another Title</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**Sample Output:**

```elixir
["Hello, Elixir!", "Another Title"]
```

To dive deeper, say you want to extract links (`<a>` tags) alongside their href attributes. Here’s how you can achieve that:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Elixir's Official Website</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**Sample Output:**

```elixir
[{"Elixir's Official Website", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

This approach allows you to navigate and parse HTML documents efficiently, making web data extraction and manipulation tasks straightforward in Elixir applications.
