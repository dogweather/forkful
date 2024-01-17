---
title:                "Sending an http request"
html_title:           "Elixir recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is a fundamental aspect of web development that allows communication between a client (such as a web browser) and a server. Programmers use this process to retrieve data, send information, and perform other actions within web applications.

## How to:

To send an HTTP request in Elixir, we can use the `HTTPoison` library, which provides a simple API for making HTTP requests.

First, we need to add `HTTPoison` as a dependency in our `mix.exs` file:
```Elixir
defp deps do
  [
    {:httpoison, "~> 1.7"}
  ]
end
```

Then, we can make a GET request by calling the `get/4` function from the `HTTPoison` module and passing in the URL we want to request from:
```Elixir
response = HTTPoison.get("https://example.com")
```

We can also make a POST request by passing in additional parameters in the `get/4` function:
```Elixir
response = HTTPoison.post("https://example.com", body: "Hello World") 
```

The `response` variable will contain the result of our HTTP request, which we can access through its `body` attribute. We can then manipulate and process the data as needed for our application.

## Deep Dive

HTTP (Hypertext Transfer Protocol) was first created in the 1990s as a way to transfer data between clients and servers on the World Wide Web. It has since evolved to become the standard protocol for communication on the internet.

While `HTTPoison` is a popular library for making HTTP requests in Elixir, there are also other options such as `Mint` and `HTTPotion`.

Under the hood, `HTTPoison` uses Elixir's `HTTP` module, which in turn uses Erlang's `Inets` library to handle the underlying network communication. This allows for a fast, efficient, and reliable way to send HTTP requests in Elixir.

## See Also

- [Official HTTPoison documentation](https://github.com/edgurgel/httpoison)
- [Alternative HTTP libraries for Elixir](https://elixir.libhunt.com/categories/122-http)
- [Elixir HTTP module documentation](https://hexdocs.pm/elixir/HTTP.html)