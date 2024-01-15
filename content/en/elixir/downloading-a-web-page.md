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

## Why 

Are you tired of constantly clicking the refresh button on your browser to keep up with the latest updates on your favorite website? Look no further, because with Elixir programming, you can easily download web pages with just a few lines of code. No more manual refreshing, just sit back and let your program do the work for you.

## How To 

First, we need to install the HTTPoison library, which is a popular Elixir HTTP client. Open your terminal and run the following command: 

```Elixir
mix deps.get 
```

Next, we will use the `HTTPoison.get/2` function to make a GET request to the desired web page. 

```Elixir 
[status, body] = HTTPoison.get("https://example.com")
```

The `status` variable will contain the response status code, while the `body` variable will contain the HTML content of the page. 

We can also specify additional options, such as headers or query parameters, using a keyword list as the third argument. For example: 

```Elixir
options = [headers: [{"User-Agent", "Elixir"}]]
[status, body] = HTTPoison.get("https://example.com", options)
```

Once we have the HTML content, we can use any Elixir library such as `Floki` or `Scrape` to retrieve specific information from the webpage. 

```Elixir
# Using Floki
html = Floki.parse_document(body)
links = Floki.find(html, "a")
```

## Deep Dive 

Behind the scenes, `HTTPoison` uses the Erlang library `hackney` for low-level HTTP operations. This allows us to take advantage of `hackney`'s features such as connection pooling, SSL support, and streaming responses. To learn more about `hackney`, check out its documentation [here](https://github.com/benoitc/hackney).

Additionally, `HTTPoison` also has built-in support for parallel requests using Elixir's `Task` module. This allows us to make multiple requests at the same time, improving the performance of our application.

## See Also 

- [HTTPoison Documentation](https://hexdocs.pm/httpoison/) 
- [`hackney` Documentation](https://github.com/benoitc/hackney) 
- [Elixir Task Module](https://hexdocs.pm/elixir/Task.html)