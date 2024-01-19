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

# Learn Gleam by Parsing HTML

## What & Why?
Parsing HTML revolves around understanding and converting HTML strings into structured data. Programmers parse HTML to enable software to extract or read information embedded in web pages automatically.

## How to:
Here's a swift overview of how to parse HTML in Gleam:

```Gleam
import gleam/http.{Uri}
import gleam/string.{replace}

fn parse_html() {
  let html_response = "..."
  let parsed_html = parse_html_data(html_response)
  case parsed_html {
    Error(e) -> "HTML parsing error: " ++ e
    Ok(s) -> "Parsed successfully: " ++ s
  }
}

fn parse_html_data(response: String) -> Result(String, Nil) {
...
}
```
This simply parses HTML from an HTTP response and returns an error message if there's an issue, otherwise, a success message is output. You need to replace `"..."` with the actual HTML response.

## Deep Dive
HTML parsing's been around since the early web ages, with Perl's HTML::Parser being one of the archaic tools. Today, languages have rich libraries making this once arduous task less daunting.

Other Gleam alternatives include libraries like HtmlSport and HtmlTrance as well as tools like Beautiful Soup in Python or Nokogiri in Ruby.

The implementation of HTML parsing in Gleam primarily leverages pattern-matching, a core part of functional programming that Gleam thrives on. This is why the case expressions in the example above are integral.

## See Also
1. [Gleam Documentation](https://gleam.run/docs/)
2. [HtmlSport](https://github.com/warner/HtmlSport)
3. [HtmlTrance](https://github.com/rrva/htmltrance)

Check these resources to deep dive further into the Gleam landscape.