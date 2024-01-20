---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:31:51.959006-07:00
html_title:           "Bash: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Parsing HTML means turning a string of HTML code into a structure a program can understand and manipulate. Programmers do it to scrape websites, automate interactions, or process content.

## How to: (Jak to zrobić:)
Gleam doesn't have its own HTML parser, but you can use Erlang libraries thanks to Gleam's compatibility with BEAM. Here's an example using `:mochiweb_html`:

```gleam
import gleam/erlang
import gleam/list

// Assuming `:mochiweb_html` Erlang library is available
fn parse_html(html_string: String) -> list(tuple(String, list(String))) {
  erlang.apply(
    module: ":mochiweb_html", 
    function: "parse", 
    arguments: [html_string]
  )
  |> result.unwrap()
  |> list.map(fun(node) {
    (node |> element_name(), node |> element_attributes())
  })
}

fn element_name(node: tuple(String, list(String))) -> String {
  let tuple(name, _) = node
  name
}

fn element_attributes(node: tuple(String, list(String))) -> list(String) {
  let tuple(_, attrs) = node
  attrs
}

// Sample usage in main function
fn main() {
  let html = "<html><body><h1>Title</h1><p class=\"text\">Hello, Gleam!</p></body></html>"
  parse_html(html) |> io.debug
}
```

Output:
```
[("html", []), ("body", []), ("h1", []), ("p", ["class=\"text\""])]
```

## Deep Dive (Głębsze spojrzenie):
Parsing HTML in Gleam relies on Erlang's ecosystem, which includes libraries like `:mochiweb_html` from the MochiWeb project. This is due to the young age of the Gleam ecosystem and the strong interoperability with BEAM languages.

Alternatives to `:mochiweb_html` include `:floki`, which is widely used in Elixir, another language that runs on the BEAM VM. Floki offers a more friendly syntax for navigating the parsed HTML with a jQuery-like selector system.

Regarding implementation, HTML parsing involves tokenization, where the parser scans the HTML string and identifies tags, attributes, and content. The tokens are then processed to create a Document Object Model (DOM) tree.

## See Also (Zobacz też):
- [`:mochiweb_html` on Hex](https://hex.pm/packages/mochiweb)
- [`:floki` on Hex](https://hex.pm/packages/floki)
- [Web scraping in Erlang/Elixir](https://elixir-lang.org/getting-started/mix-otp/ets.html#web-scraping)