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

## What & Why?

Parsing HTML is the process of reading and analyzing HTML code to extract data and information from a webpage. This is an essential task for programmers as it allows them to access and manipulate the content of a webpage in their code. It is particularly useful for web scraping, data extraction, and web automation.

## How to:

To parse HTML in Elixir, we can use the `Floki` library, which provides a simple yet powerful interface for working with HTML documents. 

```
# Install `Floki` in your project's `mix.exs` file
def deps do
  [{:floki, "~> 0.26.0"}]
end
```

```
# Import `Floki` and fetch HTML document from a URL
import Floki
html = Floki.html("https://www.example.com")

# Find and extract information from the HTML document
titles = ["h1", "h2"] |> Floki.find(html) |> Floki.extract_text
```

The above code fetches the HTML document from a URL and uses `Floki.find` to find all the `h1` and `h2` elements in the document. Then, `Floki.extract_text` is used to extract the text content of these elements, which is assigned to the `titles` variable.

## Deep Dive:

Parsing HTML has been a fundamental task in web development since the early days of the internet. In the past, this was done mainly using regular expressions, but with the rise of structured data and complex web pages, specialized HTML parsing libraries like `Floki` have become essential.

Alternatives to `Floki` for HTML parsing in Elixir include `ExAgent`, `Meeseeks`, and `Erlsom`. However, `Floki` stands out for its simplicity and ease of use. 

Under the hood, `Floki` uses the `leex` and `yecc` parser generators from Erlang, making it efficient and robust. It also uses an internal data representation called FDOM (Floki Document Object Model) for faster and more flexible handling of HTML data.

## See Also:

- Official `Floki` documentation: https://hexdocs.pm/floki
- Source code for `Floki` on GitHub: https://github.com/philss/floki
- Alternatives to `Floki` for HTML parsing in Elixir: https://github.com/h4cc/awesome-elixir#xml--html-parsing