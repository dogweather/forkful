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

## What & Why?

Parsing HTML is the decoding of HTML markup into a more structured format. Programmers do so to interact with, extract, modify, or render web content programmatically.

## How to:

In Gleam, parsing HTML is not idiomatic due to lack of HTML parsing library. But, let's hypothetically work with a simple string manipulation to retrieve content from HTML.

```gleam
pub fn parse(header: String) -> Result(String, Nil) {
  case String.split(header, "<title>") {
    [] -> Error(Nil)
    [_ | [pre, suf]] ->
      case String.split(suf, "</title>") {
        [] -> Error(Nil)
        [_ | [title, _]] -> Ok(title)
      }
  }
}

assert parse("<title>My Webpage</title>") == Ok("My Webpage")
assert parse("No title here") == Error(Nil)
```

The code above splits the input string around `<title>` and `</title>` tags and returns content between them.

## Deep Dive:

Historically, parsing HTML was crucial: web scrapers and SEO tools extract data, web servers render pages, and browsers present sites to users.

Alternatives to Gleam for HTML parsing involve languages with more robust libraries, such as BeautifulSoup in Python or Jsoup in Java.

In parsing HTML, notable concerns are tag nesting and error handling. Our hypothetical Gleam parsing doesn't address these issues but demonstrates the principle.

## See Also:

Gleam's String API: https://hexdocs.pm/gleam_stdlib/gleam/string/

A discussion on HTML parsing libraries in Rust, which could be a starting point once Gleam attains a wider ecosystem: https://users.rust-lang.org/t/html-parsing/1442

Python's BeautifulSoup Documentation for HTML parsing: https://www.crummy.com/software/BeautifulSoup/doc