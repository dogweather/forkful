---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the process of analyzing HTML code to extract information contained within tags. This is useful in web scraping tasks, data extraction, or when you want to manipulate markup data.

## How to:

Elixir, coupled with the Floki library, provides a powerful way to parse HTML.

To begin, install the Floki library by adding to your mix.exs dependencies:
```Elixir
defp deps do
  [
    {:floki, "~> 0.30"}
  ]
end
```

Then fetch the dependencies using mix:
```Elixir
mix deps.get
```

Now you're able to use Floki. Let's try out a simple example of parsing HTML:

```Elixir
html = "<div><p>Hello, Elixir programmers!</p></div>"
{:ok, document} = Floki.parse_document(html)
text = document |> Floki.find("p") |> Floki.raw_html
```
The output would be `"Hello, Elixir programmers!"`.

## Deep Dive

Historically, because HTML parsing wasn't built into many languages, developers primarily relied on regular expressions or custom parsing functions, which can be quite error-prone and ineffective.

There are alternatives to Floki for parsing HTML in Elixir, such as Meeseeks or html_sax_parser, but Floki generally provides a simpler API and unifies parsing and querying, hence its popularity among Elixir enthusiasts.

Floki implementation uses a combination of different libraries like :mochiweb_html for parsing and :css_selector for handling CSS selection, offering a lightweight yet robust solution for manipulating HTML documents.

## See Also

- Official documentation for the Floki library: [https://hexdocs.pm/floki/readme.html](https://hexdocs.pm/floki/readme.html)
- A comprehensive tutorial on scraping with Elixir and Floki: [https://nerves.build/posts/elixir-scraping-with-floki](https://nerves.build/posts/elixir-scraping-with-floki)
- For an alternative look into html parsing with Meeseeks: [https://hexdocs.pm/meeseeks/readme.html](https://hexdocs.pm/meeseeks/readme.html)
- The history of parsing and more parsing in Elixir: [https://pragdave.me/blog/2010/12/16/parsing---a-timeline.html](https://pragdave.me/blog/2010/12/16/parsing---a-timeline.html)