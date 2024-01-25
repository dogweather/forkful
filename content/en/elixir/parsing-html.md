---
title:                "Parsing HTML"
date:                  2024-01-20T15:30:48.429632-07:00
html_title:           "Bash recipe: Parsing HTML"
simple_title:         "Parsing HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML means sifting through HTML code to extract data or details programmatically. Programmers do it for tasks like web scraping, data mining, or automating interactions with websites.

## How to:

In Elixir, you can parse HTML with the Floki library. Here's a snippet:

```elixir
# First, add Floki to your mix.exs dependencies
{:floki, "~> 0.30.0"}

# Then, in your code

defmodule HTMLParser do
  alias Floki

  def parse_html(html) do
    {:ok, document} = Floki.parse(html)
    titles = Floki.find(document, "h1")
    IO.inspect(titles, label: "Titles")
  end
end

# Usage
html_content = "<html><body><h1>Hello, Elixir!</h1></body></html>"
HTMLParser.parse_html(html_content)

# Sample output
Titles: [{"h1", [], ["Hello, Elixir!"]}]
```

## Deep Dive

Historically, HTML parsing in languages like Python or JavaScript has been more common, but Elixir's concurrent features and scalability make it a strong alternative for modern web tasks. The Floki library uses the fast_html C parser underneath for speed, giving you best of both worlds: Elixir's concurrency and the performance of a compiled language.

Compared to other tools like BeautifulSoup in Python, Floki is less verbose and more functional in style - fitting well with Elixir's ethos. Plus, you have the entire might of the Erlang ecosystem for fault-tolerance and distribution, if you're thinking big.

## See Also

- [Floki on Hex](https://hex.pm/packages/floki) - Official Floki documentation.
- [HTML5ever](https://github.com/servo/html5ever) - Rust HTML parser that powers fast_html.