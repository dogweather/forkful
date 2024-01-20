---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is the act of asking a server for specific data. Programmers do this to interact with APIs or retrieve web content programmatically.

## How to:

Elixir uses the HTTPoison library for making HTTP requests. Here's a small example:

```Elixir
HTTPoison.start()

{:ok, response} = HTTPoison.get("http://httpbin.org/get")

IO.puts response.body
```

The output will be the HTTP response body from httpbin.org.

## Deep Dive

Elixir didn't always have HTTPoison. Elixir's http support evolved from Erlang's http modules. There are other libraries available like Tesla or Mint if HTTPoison doesn't float your boat.

HTTPoison is built on Hackney, a versatile HTTP library for Erlang. This shows how Elixir taps into the rich ecosystem of its parent language.

Under the hood, when you make a request with HTTPoison, it creates a process for handling that request. This leverages Erlang’s lightweight processes, allowing for many concurrent HTTP requests if your application requires it.

## See Also

- [HTTPoison Documentation](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Another useful article on using HTTPoison](https://medium.com/@n2oh/using-httpoison-to-make-http-requests-in-elixir-171a833e9508)
- [Hackney on GitHub](https://github.com/benoitc/hackney)
- [Comparison of HTTP clients in Elixir (Tesla vs HTTPoison)](https://pdgonzalez872.medium.com/comparing-http-clients-in-elixir-9e8a8b1f2f8f)