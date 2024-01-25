---
title:                "Parsing HTML"
date:                  2024-01-20T15:31:47.624458-07:00
html_title:           "Bash recipe: Parsing HTML"
simple_title:         "Parsing HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML means turning strings of HTML into structured data. Programmers do this to manipulate or extract info from web pages or to safely generate HTML from user input.

## How to:
Gleam doesn't have a built-in HTML parsing library, but you can use Erlang libraries through interop. Here's a basic example using the `meeseeks` package, an HTML/XML parser:

First, add `meeseeks` to your `rebar.config` deps, like this:

```erlang
{deps, [
    {meeseeks, "0.15.0"}
]}.
```

Here's how you might parse and search HTML in Gleam, assuming you've handled Erlang interop correctly:

```gleam
import gleam/erlang
import meeseeks/html
import meeseeks/css

pub fn parse_and_find() -> Result(String, Nil) {
  let html = "<html><body><h1>Hello, Gleam!</h1></body></html>"
  let doc = html |> html.parse
  let selector = css.parse("h1").unwrap()
  
  doc
  |> meeseeks.all(selector)
  |> meeseeks.text
  |> Result.map(lists.head)
}
```
This function parses the HTML, then queries it for `h1` tags and gets the text. Here's what running it might output:

```shell
> parse_and_find()
Ok("Hello, Gleam!")
```

## Deep Dive
Historically, parsing HTML in a new language meant writing a custom parser or wrapping an existing one. Alternatives include using regex (typically a bad idea due to HTML's complexity) or robust libraries like `meeseeks` based on proven parsers (like `html5ever` from Rust in `meeseeks`'s case).

Implementing HTML parsing can get hairy because HTML is often not well-formed or predictable. Libraries handle this by sanitizing and normalizing data. Interfacing with Erlang libraries from Gleam is straightforward thanks to the Erlang ecosystem's compatibility, giving access to mature libraries without having to reinvent the wheel.

## See Also
For further reading and resources, check out:

- Meeseeks library on Hex: https://hex.pm/packages/meeseeks
- The `html5ever` Rust parser: https://github.com/servo/html5ever
- Erlang interoperability guide for Gleam: https://gleam.run/book/tour/erlang-interop/