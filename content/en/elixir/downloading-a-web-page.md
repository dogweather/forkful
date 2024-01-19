---
title:                "Downloading a web page"
html_title:           "Elixir recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means retrieving its content, typically an HTML file, over the internet. Programmers do it to automate data extraction (web scraping), run functional tests, or monitor up-time. 

## How to:

Elixir provides a high-level HTTP client called `:httpc` to download web pages. Here's an example:

```elixir
{:ok, {{’HTTP/1.1’, 200, ‘OK’}, _headers, body}} = :httpc.request('http://example.com')
IO.puts body
```

This code makes a GET request to 'http://example.com', and prints the response body, which is the content of the web page.

## Deep Dive:

`:httpc` is part of Erlang’s `inets` application, which Elixir inherits as it runs on the Erlang virtual machine, BEAM. 

There are alternative libraries to `:httpc`, such as `HTTPoison` and `Tesla`. They offer additional features like better error handling, instrumentation, and session support.

When you do `:httpc.request(url)`, BEAM spawns a new lightweight process, making a non-blocking HTTP request. It's highly concurrent, and it won't block the execution of your Elixir application.

## See Also: 

- Erlang `:httpc` documentation, to understand its full capabilities: http://erlang.org/doc/man/httpc.html
- Elixir `HTTPoison` library documentation, for a more feature-rich library: https://hexdocs.pm/httpoison/readme.html
- Elixir `Tesla` library documentation, for the swiss-army knife of HTTP clients in Elixir: https://hexdocs.pm/tesla/readme.html
- Elixir School, for learning more about Elixir: https://elixirschool.com/