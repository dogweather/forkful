---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page refers to retrieving data from a specific URL and saving the resulting HTML file for later use. This lets programmers analyze, manipulate, or replicate the page's structure and content.

## How to:

While Gleam itself does not support HTTP requests natively, it can utilize the power of Elixir or even Erlang within its environment to do so. Below is an example using Erlang's 'httpc' module:

```Gleam
import gleam/erlang

pub fn download_page(url: String) -> Result(BitString, Nil) {
  let response = erlang.apply(
    "httpc",
    "request",
    [tuple("get", url)
  ])

  case response {
    Ok(tuple(_, tuple(_, _, body))) -> Ok(body)
    Error(err) -> Error(Nil)
  }
}
```

If you run this function with a valid URL, it will return the HTML contents of that page, or Error(Nil) if something went wrong.

## Deep Dive

In early days of the web, downloading the raw HTML of a page was a common way to parse data from the web. Modern solutions often use a combination of APIs and JSON to accomplish the same tasks, but in certain cases, or where an API doesn't exist, downloading a web page can still be necessary.

The code in Gleam is doing the heavy lifting using Erlang's httpc module underneath the hood, as Gleam runs on the Erlang virtual machine (BEAM). It is important to note that 'httpc' isn't necessarily the best HTTP client available, but it comes packaged with Erlang, and for simple GET requests like this, it's perfectly fine!

## See Also:

- Gleam Documentation: https://hexdocs.pm/gleam_erlang/gleam/erlang/index.html
- Erlang's 'httpc' HTTP client documentation: http://erlang.org/doc/man/httpc.html
- 'httpotion' - another popular HTTP client which can be used in Gleam: https://hexdocs.pm/httpotion/readme.html
- JSON Parsing in Gleam: https://hexdocs.pm/gleam_erlang/gleam/erlang/index.html#json_decode/1