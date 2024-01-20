---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page is the process of retrieving and storing the contents of a HTTP document (web page) from a web server. Programmers do this to extract and use data, automate tasks, or to save the web content offline for later use.

## How to:

Here's how you download a web page using the HTTPoison library in Elixir. First, add HTTPoison as a dependency in your mix.exs file:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

Fetch dependencies using the shell command:

```shell
$ mix deps.get
```

You're now set to retrieve a web page:

```elixir
defmodule MyScraper do
  def fetch_page(url) do
    case HTTPoison.get(url) do
      {:ok, response} -> IO.puts response.body
      {:error, reason} -> IO.puts "Error: #{reason}"
      end
  end
end
```

Call `fetch_page` function with a URL:

```elixir
MyScraper.fetch_page("https://www.example.com")
```

## Deep Dive

Downloading web pages isn't a new concept. Since the early days of the internet, we've been pulling data from servers. Elixir, an Erlang VM-based functional language, offers speed and concurrency that fits this task well.

While HTTPoison is a popular choice for HTTP requests, Elixir has other libraries too. :httpc, a built-in Erlang module, HTTPotion, HTTPipe etc., can serve similar purposes but with syntax variations and varying feature sets.

In downloading a web page, we essentially make a GET request to a server, then store the retrieved HTML response. The specific implementation varies based on whether we want to parse the HTML, download static assets, follow redirects, handle HTTP cookies, etc. 

## See Also

For more details:

- HTTPoison GitHub repo: [HTTPoison](https://github.com/edgurgel/httpoison)
- Other HTTPoison features: [HTTPoison advanced](https://hexdocs.pm/httpoison/HTTPoison.html)
- Elixir School for learning Elixir: [Elixir School](https://elixirschool.com/)
- Other Elixir HTTP libraries: [Awesome Elixir HTTP](https://github.com/h4cc/awesome-elixir#http)